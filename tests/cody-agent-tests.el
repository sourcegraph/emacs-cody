;;; cody-tests.el --- Cody tests with Agent -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests that use the Cody Agent.

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'cody)
(require 'cody-test-fixture)

(defun cody--test-switch-workspaces-helper ()
  "Helper function to test switching workspaces and updating agent root."
  ;; Setup temporary workspace directories and files.
  (let* ((workspace-root-a (make-temp-file "cody-workspace-a" t))
         (workspace-root-b (make-temp-file "cody-workspace-b" t))
         (file-a (expand-file-name "A.java" workspace-root-a))
         (file-b (expand-file-name "B.java" workspace-root-b))
         (source "
class %s {
  public static void main(String[] args) {
    System.out.println(new Date());
  }
}")
         (a-source (format source "A"))
         (b-source (format source "B")))
    (unwind-protect
        (condition-case err
            (progn
              ;; Create temporary files
              (write-region "" nil file-a)
              (write-region "" nil file-b)

              (with-current-buffer (find-file-noselect file-a)
                (cody-mode)
                (expect (string= cody--workspace-root workspace-root-a)
                        :to-be-truthy))

              (with-current-buffer (find-file-noselect file-b)
                (cody-mode)
                (expect (not (string= cody--workspace-root workspace-root-b))
                        :to-be-truthy))

              ;; Programmatically switch workspace to B's root
              (setq current-prefix-arg workspace-root-b)
              (call-interactively 'cody-switch-workspace)

              (cody--await-pending-promises)

              (with-current-buffer (find-file-noselect file-b)
                ;; Now B.java's root should be the workspace root
                (expect (string= cody--workspace-root workspace-root-b)
                        :to-be-truthy))

              ;; Verify agent’s knowledge of the workspace root
              (let ((agent-docs (cody--test-request-agent-mirror)))
                (expect (length agent-docs) :to-be 1)
                (expect (string= (plist-get (nth 0 agent-docs) :uri)
                                 (concat "file://" file-b))
                          :to-be-truthy)))
          (error (cody--log "Error during test: %s" err)))
      (ignore-errors
        (delete-file file-a))
      (ignore-errors
        (delete-file file-b))
      (ignore-errors
        (delete-directory workspace-root-a))
      (ignore-errors
        (delete-directory workspace-root-b)))))

(describe "Cody workspace switching"
  (before-all (cody-shutdown))
  (after-all (cody-shutdown))

  (it "switches workspaces and updates agent root"
   (cody--test-switch-workspaces-helper))

  ) ; describe

(provide 'cody-tests)
;;; cody-tests.el ends here
