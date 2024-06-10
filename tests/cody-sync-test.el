;;; cody-sync-test.el --- Tests for cody.el document synchronization -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for verifying document synchronization in the cody.el plugin.
;; Ensure `buttercup` is installed and added to the load path before running these tests.

;;; Code:

(require 'buttercup)
(require 'cody)

;; Define constants
(defconst cody--workspace-root (expand-file-name "../tests/src/")
  "Path to the workspace root for test purposes.")

;; Define helper functions
(defun cody-test-initialize ()
  "Initialize the Cody connection for testing."
  (cody--connection))

(defun cody-test-open-document (buffer)
  "Open a document in BUFFER and inform the agent."
  (with-current-buffer buffer
    (cody--notify-doc-did-open)))

(defun cody-test-change-document (buffer beg end text)
  "Make a change in BUFFER from BEG to END with TEXT and inform the agent."
  (with-current-buffer buffer
    (cody--notify-doc-did-change beg end text)))

(defun cody-test-fetch-agent-content (uri)
  "Fetch the current content of the document with URI from the agent."
  (cody--request 'textDocument/fetchContent (list :uri uri)))

;; Define buttercup tests
(describe "cody.el integration tests"
          (before-each
     (cody-test-initialize)
     (setq cody-test-buffer (get-buffer-create "*cody-test-buffer*"))
     (with-current-buffer cody-test-buffer
       (erase-buffer)
       (insert "initial content\n")))

  (after-each
    (kill-buffer cody-test-buffer))

  (it "should synchronize document changes with the agent"
    ;; Initialize the Cody connection for testing
    
    
    ;; Fetch the URI for the current test buffer
    (let ((uri (cody--uri-for (buffer-file-name cody-test-buffer))))
      ;; Open the document and inform the agent
      (cody-test-open-document cody-test-buffer)
      
      ;; Make a change to the document
      (cody-test-change-document cody-test-buffer (point-max) (point-max) "additional content\n")
      
      ;; Define an appropriate predicate function to check if the sync is complete
      (let ((sync-complete (lambda ()
                             ;; Check the fetched content against the buffer content
                             (let ((agent-content (cody-test-fetch-agent-content uri)))
                       (string= agent-content (with-current-buffer cody-test-buffer
                                                        (buffer-string)))))))
        ;; Wait for the sync to complete with a timeout
        (expect (wait-for sync-complete 30) :to-be-truthy)))))

;;; cody-sync-test.el ends here
