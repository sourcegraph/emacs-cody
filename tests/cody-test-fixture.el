;;; cody-test-fixture.el --- Utility functions for Cody tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for Cody's integration tests.

;;; Code:

(require 'cody)


(defmacro cody--test-doc-sync (&rest args)
  "Macro to define a Cody document synchronization test for Buttercup.

ARGS is a plist containing the keys:
  :before - the initial content of the buffer.
  :after - the expected content of the buffer after transformation.
  :code - the code to run to perform the transformation.
  :point-char (optional) - the character in :before text marking
                           the cursor position. Default is ?^
  :mark-char (optional) - the character in :before text marking the
                          mark position. Default is ?$

The general test strategy is to run a transformation, await promises, and then
fetch the agent's copy of the content and compare it."
  (let ((before (plist-get args :before))
        (after (plist-get args :after))
        (code (plist-get args :code))
        (point-char (or (plist-get args :point-char) "^"))
        (mark-char (or (plist-get args :mark-char) "$")))
    `(cl-destructuring-bind (initial-content point-pos mark-pos)
         (cody--test-doc-sync-initialize-test ,before ,point-char ,mark-char)
         (cody--test-with-temp-window
          (lambda ()
            (cody--test-doc-sync-execute initial-content
                                         ;; Leading \n is for test readability.
                                         (string-remove-prefix "\n" ,after)
                                         point-pos
                                         mark-pos
                                         (lambda () ,code)))))))

(defun cody--test-doc-sync-initialize-test (input-str point-char mark-char)
  "Parse INPUT-STR, determining positions of POINT-CHAR and MARK-CHAR.
Return (list stripped-content point-pos mark-pos)."
  ;; The first "\n" is just there to improve test readability.
  (let* ((input (string-remove-prefix "\n" input-str))
         (case-fold-search nil)
         (point-pos (or (string-match (regexp-quote point-char) input)
                        (point-min)))
         (stripped-content (replace-regexp-in-string (regexp-quote point-char)
                                                     "" input))
         (mark-pos (string-match (regexp-quote mark-char) stripped-content)))
    ;; Adjust point-pos and mark-pos if either character is present.
    (setq point-pos (1+ point-pos))
    (when mark-pos
      (setq stripped-content
            (replace-regexp-in-string (regexp-quote mark-char) "" stripped-content))
      (setq mark-pos (1+ mark-pos))
      (when (< mark-pos point-pos)
        (setq point-pos (min (point-min) (1- mark-pos)))))
    (list stripped-content point-pos mark-pos)))

(defun cody--test-doc-sync-execute (initial-content
                                    expected-content
                                    point-pos
                                    mark-pos
                                    code)
  "Execute the test in a temp buffer in `cody-mode'.
Validate that Agent's contents and Emacs-side contents match EXPECTED-CONTENT.

INITIAL-CONTENT is the buffer text before the test operation.

POINT-POS where to place point in the buffer before the test begins.
Optional MARK-POS where to set mark before test begins.

CODE is the form to evaluate pseudo-interactively in a temp uffer
with the initial contents, to transform it into the expected contents.
"
  (let* ((temp-file (make-temp-file "cody-test-"))
         ;; Configure cody-mode for testing, minimizing differences.
         (cody-workspace-root (file-name-directory temp-file))
         (cody--integration-testing-p t)
         (cody--dev-panic-on-doc-desync nil)
         (cody-completions-auto-trigger-p nil))
    (with-current-buffer (get-buffer-create "*cody-test-buffer*")
      (unwind-protect
          (progn
            (erase-buffer)
            (set-visited-file-name temp-file)
            (insert initial-content)

            ;; Set initial selection.
            (goto-char (or point-pos (point-min)))
            (when mark-pos
              (set-mark (1+ mark-pos)))

            (expect (not cody-mode) :to-be-truthy)
            (cody-mode)
            (expect (eq cody--buffer-state 'active) :to-be-truthy)

            ;; Perform the edit function and wait for the Agent to finish.
            (funcall code)

            ;; Directly call the hook function to simulate interactive behavior.
            (run-hooks 'post-command-hook)

            (set-buffer-modified-p nil)
            (cody--await-pending-promises)

            ;; Get the agent's mirrored document/selection and compare.
            (let* ((uri (cody--uri-for (buffer-file-name)))
                   (agent-doc (cody--test-doc-sync-get-agent-mirror-for-uri uri))
                   (agent-content (plist-get agent-doc :content))
                   (agent-selection (plist-get agent-doc :selection)))
              (expect (stringp agent-content) :to-be-truthy)
              (expect (string= expected-content (cody--buffer-string)))
              (expect (string= expected-content agent-content) :to-be-truthy)
              (expect (cody--selection-get-current) :to-equal agent-selection)))
        ;; unwindw/finally forms:
        (cody--test-doc-sync-cleanup temp-file)))))

;; This is to get `cody--mode-startup' and other cody functions to behave correctly.
;; It ensures `selected-window' returns something useful, as we use it in several places.
;; TODO: Check whether this approach is responsible for the dreaded error,
;;    buttercup--run-spec: Device 1 is not a termcap terminal device
(defun cody--test-with-temp-window (fn)
  "Execute FN in a temporary window environment."
  (let* ((buffer (get-buffer-create "*cody-temp-test-buffer*"))
         (no-save-hook (lambda () (set-buffer-modified-p nil) t))
         (window-configuration (current-window-configuration)))
    (unwind-protect
        (condition-case err
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
          (error
           (cody--log "Error: %s" (error-message-string err))))
      ;; Restore window configuration
      (set-window-configuration window-configuration)
      ;; Kill the temporary buffer
      (kill-buffer buffer))))

(defun cody--test-request-agent-mirror ()
  "Request documents from the Agent so we can validate the contents.
Returns the list of `ProtocolTextDocument' plists currently open."
  (let* ((response (cody--request 'testing/workspaceDocuments nil))
         (documents (plist-get response :documents))) ; vector
    (append documents nil))) ; convert to a list

(defun cody--test-doc-sync-get-agent-mirror-for-uri (uri)
  "Request the agent's single `ProtocolTextDocument' for URI.
URI is the uri for a Cody-tracked buffer.
Return value is a plist representing the `ProtocolTextDocument'."
  (let* ((result (cody--request 'testing/workspaceDocuments
                                (list :uris (vector uri))))
         (documents (plist-get result :documents))
         (agent-doc (aref documents 0)))
    (should agent-doc)
    agent-doc))

(defun cody--test-doc-sync-cleanup (temp-file)
  "Perform cleanup actions after an erts transform runs.
TEMP-FILE is the temp file created for this test run."
  ;; Notify the agent that this document is closing. This is so that they don't
  ;; accumulate in AgentWorkspaceDocuments while multiple tests are running.
  ;; We have to do this because `kill-buffer-hook' is not run on temp buffers.
  (condition-case err
      (progn
        (cody--handle-doc-closed)
        ;; Unassociate the buffer from the temp file and delete the temp file.
        ;; This is so that we aren't prompted to close every temp buffer.
        (set-visited-file-name nil)
        (set-buffer-modified-p nil)
        (when (file-exists-p temp-file)
          (delete-file temp-file))
        (kill-buffer (current-buffer)))
    (error (cody--log "Error in cody--test-doc-sync-cleanup: %s" err))))

(defun cody--await-pending-promises ()
  "Call `testing/awaitPendingPromises' and block until it returns.
This flushes all the promises on the agent so that tests can be more deterministic.
Signals an error if `cody--integration-testing-p' is nil."
  (unless cody--integration-testing-p
    (error "This method is for testing only"))
  (jsonrpc-request (cody--connection) 'testing/awaitPendingPromises nil))

(provide 'cody-test-fixture)
;;; cody-test-fixture.el ends here
