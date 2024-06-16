;;; cody-test-fixture.el --- Utility functions for Cody tests -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains utility functions to help with Cody's integration tests.

;;; Code:

(require 'ert)
(require 'cody)
(require 'cody-repo-util) ; for `cody--uri-for'

;; This is to get `cody--mode-startup' and other cody functions to behave correctly.
;; It ensures `selected-window' returns something useful, as we use it in several places.
;; TODO: Check whether this approach is responsible for the dreaded error,
;;    buttercup--run-spec: Device 1 is not a termcap terminal device
(defun cody--test-with-temp-window (fn)
  "Execute FN in a temporary window environment."
  (let ((buffer (get-buffer-create "*cody-temp-test-buffer*"))
        (no-save-hook (lambda () (set-buffer-modified-p nil) t)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            ;; Create a temporary window and set it as selected temporarily
            (let ((window (split-window)))
              (set-window-buffer window buffer)
              (select-window window)
              ;; Add the no-save hook to prevent save prompts
              (add-hook 'kill-buffer-query-functions no-save-hook nil t)
              ;; Call the provided test function
              (funcall fn)
              ;; Cleanup: remove the temporary window and buffer
              (delete-window window))))
      (kill-buffer buffer))))

(defun cody--test-get-sole-mirrored-doc-content (uri)
  "Assert the agent has one mirrored document, and return its contents.
URI is the uri for the file/buffer the agent is tracking."
  (let ((documents (cody--test-request-agent-mirror)))
    (should (listp documents))
    (should (= (length documents) 1))
    (plist-get (car documents) :content)))

(defun cody--test-request-agent-mirror ()
  "Request documents from the Agent so we can validate the contents.
Returns the list of `ProtocolTextDocument' plists currently open."
  (let* ((response (cody--request 'testing/workspaceDocuments nil))
         (documents (plist-get response :documents))) ; vector
    (append documents nil))) ; convert to a list

(defun cody--test-doc-sync-get-agent-mirror-for-uri (uri)
  "Request the `ProtocolTextDocument' plist for URI.
URI is the uri for a Cody-tracked buffer."
  (let* ((agent-doc (cody--request 'testing/workspaceDocuments
                                   (list :uris (vector uri)))))
    (should agent-doc)
    agent-doc))

(defun cody--test-doc-sync-cleanup (temp-file)
  "Perform cleanup actions after an erts transform runs.
TEMP-FILE is the temp file created for this test run."
  ;; Notify the agent that this document is closing. This is so that they don't
  ;; accumulate in AgentWorkspaceDocuments while multiple tests are running.
  ;; We have to do this because `kill-buffer-hook' is not run on temp buffers.
  (cody--handle-doc-closed)

  ;; - Unassociate the buffer from the temp file and delete the temp file.
  ;;   This is so that we aren't prompted to close every temp buffer.
  (set-visited-file-name nil)
  (when (file-exists-p temp-file)
    (delete-file temp-file)))

(defun cody--await-pending-promises ()
  "Calls `testing/awaitPendingPromises' and blocks until it returns.
This flushes all the promises on the agent so that tests can be more deterministic.
Signals an error if `cody--integration-testing-p' is nil."
  (unless cody--integration-testing-p
    (error "This method is for testing only."))
  (jsonrpc-request (cody--connection) 'testing/awaitPendingPromises nil))

(defun cody-test-wait-for (predicate timeout)
  "Wait for PREDICATE to return non-nil within TIMEOUT seconds.
Split the timeout into 0.05 second intervals."
  (let ((end-time (+ (float-time) timeout)))
    (while (and (not (funcall predicate))
                (< (float-time) end-time))
      (accept-process-output nil 0.05))
    (funcall predicate)))

;; We use ERT's "erts" mechanism for its declarative before/after tests. But erts
;; does not support JUnit-style before-all and after-all setup/teardown functions.
;; So we create our own poor-coder's version, by advising `ert-run-tests-batch'.

(defvar cody-ert-before-all-hook nil "Hook run before all tests.")

(defvar cody-ert-after-all-hook nil "Hook run after all tests.")

(defvar cody-ert-before-all-hook-called nil
  "Flag to prevent multiple calls within a single batch.")

(defun cody-run-before-all-hook-if-needed ()
  "Run the `cody-ert-before-all-hook' if it hasn't been run yet."
  (unless cody-ert-before-all-hook-called
    (when cody-ert-before-all-hook
      (run-hooks 'cody-ert-before-all-hook))
    (setq cody-ert-before-all-hook-called t)))

(defun cody-run-after-all-hook-if-needed ()
  "Run the `cody-ert-after-all-hook'."
  (when cody-ert-after-all-hook
    (run-hooks 'cody-ert-after-all-hook)))

(define-advice ert-run-tests-batch (:before (&rest _args) cody-run-before-all-hook)
  (cody-run-before-all-hook-if-needed))

(define-advice ert-run-tests-batch (:after (&rest _args) cody-run-after-all-hook)
  (cody-run-after-all-hook-if-needed))

(define-advice ert-run-tests (:before (&rest _args) cody-run-before-all-hook)
  (cody-run-before-all-hook-if-needed))

(define-advice ert-run-tests (:after (&rest _args) cody-run-after-all-hook)
  (cody-run-after-all-hook-if-needed))

(defmacro cody-define-test-fixtures (&rest body)
  "Define before-all and after-all hooks for Cody tests.

BODY should contain `:before-all` and/or `:after-all` keywords
followed by the hook functions."
  (let ((before-all nil)
        (after-all nil))
    (while body
      (let ((key (pop body))
            (val (pop body)))
        (cond
         ((eq key :before-all) (setq before-all val))
         ((eq key :after-all) (setq after-all val))
         (t (error "Invalid fixture form: %S" key)))))
    `(progn
       (when ,before-all
         (add-hook 'cody-ert-before-all-hook
                   (lambda () (funcall ,before-all))))
       (when ,after-all
         (add-hook 'cody-ert-after-all-hook
                   (lambda () (funcall ,after-all)))))))

;; It would have been nice to support multiple tests per file, as erts does.
;; But I wasn't able to get my own code scoped in the Code: blocks. This winds
;; up being pretty clean, though, with the erts tests in their own subdirectory.
(defmacro def-cody-doc-sync-erts-test (relative-path &rest body)
  "Define a Cody document synchronization test.
RELATIVE-PATH is the relative path to the test file.
BODY is the code to evaluate during the test."
  (let* ((base-name (file-name-nondirectory
                     (directory-file-name
                      (file-name-sans-extension relative-path))))
         (test-suffix "doc-sync-")
         (test-name (intern (concat "cody-" test-suffix
                                    (replace-regexp-in-string test-suffix
                                                              ""
                                                              base-name)))))
    `(ert-deftest ,test-name ()
       "Test the Cody document synchronization functionality."
       ;; Configure Cody for integration testing, trying to minimize differences.
       (let* ((cody--integration-testing-p t)
              (cody--dev-panic-on-doc-desync nil)
              (cody-completions-auto-trigger-p nil)
              ;; Locate the directory containing this fixture.
              (test-file-dir (cond
                              (load-file-name
                               (file-name-directory load-file-name))
                              (buffer-file-name
                               (file-name-directory buffer-file-name))
                              (t default-directory)))
              (absolute-test-path (expand-file-name ,relative-path test-file-dir)))
         (should (and absolute-test-path (file-exists-p absolute-test-path)))
         ;; We create a temp window because Cody's interaction with the Emacs window
         ;; hooks require an active window. This window doesn't pop up during testing,
         ;; but it does stubbornly appear while debugging tests with edebug.
         (cody--test-with-temp-window
          (lambda ()
            ;; We create an artificial temp file for the test, because Cody currently
            ;; requires the buffer to be visiting a file in order to track it. Even after
            ;; we add support for untitled documents, it probably makes sense for us to
            ;; test the visiting-file code path in the normal case.
            (let ((temp-file (make-temp-file "cody-")))
              (ert-test-erts-file
               absolute-test-path
               ;; Set up the environment for our transformer function, and run it.
               (lambda ()
                 (cody--test-doc-sync-erts-transform
                  (buffer-string)
                  temp-file
                  (lambda () ,@body)))))))))))

(put 'def-cody-doc-sync-erts-test 'lisp-indent-function 1)

(defmacro cody--test-with-transform (edit-body)
  "Macro to simplify writing transform functions.
EDIT-BODY is the body of the edit operation."
  `(cody--test-doc-sync-erts-transform
    (lambda () ,edit-body)))

(defun cody--test-doc-sync-erts-transform (initial-content temp-file edit-fn)
  "The main transform function for erts tests for document synchronization.
INITIAL-CONTENT is the initial buffer content before modifications.

The current buffer will be set to the temp buffer created by erts and
will contain the initial content, with point optionally set by the test.

INITIAL-CONTENT is the initial buffer content before modifications.
You can also get this from `buffer-string' in the current buffer.

TEMP-FILE is the temporary file used for the test.
EDIT-FN is the function that performs the edit in the current buffer.
Returns the agent's mirrored copy of the modified document."
  (let* ((old-workspace-root cody-workspace-root)
         (cody-workspace-root (file-name-directory temp-file))
         transformed-text)
    (unwind-protect
          (setq transformed-text
                (cody--test-doc-sync-erts-transform-helper
                 initial-content temp-file edit-fn))
      ;; Clean up.
      (setq cody--workspace-root old-workspace-root)
      (when (file-exists-p temp-file)
        (delete-file temp-file)))
    transformed-text))

(defun cody--test-doc-sync-erts-transform-helper (initial-content temp-file edit-fn)
  "Helper function for `cody--test-doc-sync-erts-transform'.
INITIAL-CONTENT, TEMP-FILE, and EDIT-FN are as for the caller, which see."
  (let ((point-in-original-buffer (point))
        (original-buffer (current-buffer)))
    ;; We copy the erts "before" content into a new working buffer with a temp file
    ;; associated, make the edits, and then copy the result back to the original.
    (with-temp-buffer
      (unwind-protect
          (progn
            ;; Copy in the initial test source and associate it with the temp file.
            (set-visited-file-name temp-file)
            (insert initial-content)
            (goto-char point-in-original-buffer)

            (should (not cody-mode))
            (cody-mode)
            (should (eq cody--buffer-state 'active))

            ;; Perform the edit function and wait for Agent to finish.
            (funcall edit-fn)
            (should (buffer-modified-p))  ; make sure we modified our copy
            (cody--await-pending-promises) ; wait for agent to settle

            ;; Transformed document content (for erts) is the agent's copy.
            (let* ((uri (cody--uri-for (buffer-file-name)))
                   (transformed-content
                    ;; These erts tests can only use one document at a time.
                    (cody--test-get-sole-mirrored-doc-content uri)))
              ;; Copy transform back to the erts buffer, where the runner
              ;; compares it to the expected after-text of the test.
              (with-current-buffer original-buffer
                (with-silent-modifications
                  (erase-buffer)
                  (insert transformed-content)))
              transformed-content))
        (cody--test-doc-sync-cleanup temp-file)))))

(provide 'cody-test-fixture)
;;; cody-test-fixture.el ends here
