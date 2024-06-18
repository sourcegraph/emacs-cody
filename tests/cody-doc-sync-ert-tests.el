;;; cody-doc-sync-ert-tests.el --- Tests for Cody document synchronization -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains tests for the Cody document synchronization functionality.

;;; Code:

(require 'ert)
(require 'cody)
(require 'cody-test-fixture)

;; These are being ported over to buttercup.

;; (defun cody-doc-sync-before-all ()
;;   "Code that runs before `ert-run-tests-batch'."
;;   ;; Until we support multiple agents, use a pristine agent for testing.
;;   (cody-shutdown))

;; (defun cody-doc-sync-after-all ()
;;   "Code that runs after `ert-run-tests-batch'."
;;   ;; Clean up the testing agent after all tests run.
;;   (cody-shutdown))

;; (cody-define-test-fixtures
;;  :before-all 'cody-doc-sync-before-all
;;  :after-all 'cody-doc-sync-after-all)

;; (def-cody-doc-sync-erts-test "erts/doc-sync-insert-char.erts"
;;   (insert "!"))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-delete-char.erts"
;;   (delete-char 1))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-delete-range.erts"
;;   (kill-word 3))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-replace-range-atomic.erts"
;;   (replace-string "System.out.println" "console.log"))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-replace-range-nonatomic.erts"
;;   (kill-word 3)
;;   (insert "console.log"))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-insert-with-newlines.erts"
;;   (insert "\n  System.out.println(\"this is a test\")")
;;   (insert "\n  System.out.println(\"hello hello\")"))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-erase-document.erts"
;;   (delete-region (point-min) (point-max)))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-append-to-end.erts"
;;   (goto-char (point-max))
;;   (insert "// antidisestablishmentarianism\n")
;;   (insert "// pneumonoultramicroscopicsilicovolcanoconiosis\n"))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-delete-with-newlines.erts"
;;   (delete-region (point)
;;                  (save-excursion
;;                    (forward-line 3)
;;                    (1- (point)))))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-insert-emojis.erts"
;;   (insert "!🎉🎂\n🥳🎈"))

;; (def-cody-doc-sync-erts-test "erts/doc-sync-multiple-edits.erts"
;;   (goto-char (point-min))
;;   (insert "import com.foo.Bar;\n\n")
;;   (search-forward "{\n")
;;   (open-line 1)
;;   (insert "  // no comment")
;;   (forward-line 1)
;;   (goto-char (line-end-position))
;;   (insert ";")
;;   (goto-char (point-max))
;;   (insert "// end class Foo\n"))

(provide 'cody-doc-sync-ert-tests)
;;; cody-doc-sync-ert-tests.el ends here
