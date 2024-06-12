;;; cody-doc-sync-test.el --- Tests for Cody document synchronization -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains tests for the Cody document synchronization functionality.
;; It leverages erts files for defining and running the tests.

;;; Code:

(require 'ert)
(require 'cody)
(require 'cody-test-fixture)

(defun cody-doc-sync-before-all ()
  "Code that runs before `ert-run-tests-batch'."
  (cody-shutdown))

(defun cody-doc-sync-after-all ()
  "Code that runs after `ert-run-tests-batch'."
  (cody-shutdown))

(cody-define-test-fixtures
 :before-all 'cody-doc-sync-before-all
 :after-all 'cody-doc-sync-after-all)

(def-cody-doc-sync-test cody-doc-sync-test-insert-char
  "erts/doc-sync-insert-char.erts"
  (insert "!"))

(def-cody-doc-sync-test cody-doc-sync-test-delete-char
  "erts/doc-sync-delete-char.erts"
  (delete-char 1))

(def-cody-doc-sync-test cody-doc-sync-test-delete-range
  "erts/doc-sync-delete-range.erts"
  (kill-word 3)) ; this performs a single delete-region

(def-cody-doc-sync-test cody-doc-sync-test-replace-range-atomic
  "erts/doc-sync-replace-range-atomic.erts"
  (replace-string "System.out.println" "console.log"))

(def-cody-doc-sync-test cody-doc-sync-test-replace-range-nonatomic
  "erts/doc-sync-replace-range-nonatomic.erts"
  (kill-word 3)
  (insert "console.log"))

(def-cody-doc-sync-test cody-doc-sync-insert-with-newlines
  "erts/doc-sync-insert-with-newlines.erts"
  (insert "\n  System.out.println(\"this is a test\")")
  (insert "\n  System.out.println(\"hello hello\")"))

(def-cody-doc-sync-test cody-doc-sync-erase-document
  "erts/doc-sync-erase-document.erts"
  (delete-region (point-min) (point-max)))

(def-cody-doc-sync-test cody-doc-sync-append-to-end
  "erts/doc-sync-append-to-end.erts"
  (goto-char (point-max))
  (insert "// antidisestablishmentarianism\n")
  (insert "// pneumonoultramicroscopicsilicovolcanoconiosis\n"))
  
(def-cody-doc-sync-test cody-doc-sync-test-delete-with-newlines
  "erts/doc-sync-delete-with-newlines.erts"
  (delete-region (point)
                 (save-excursion
                   (forward-line 3)
                   (1- (point)))))

(def-cody-doc-sync-test cody-doc-sync-test-insert-emojis
  "erts/doc-sync-insert-emojis.erts"
  (insert "!🎉🎂\n🥳🎈"))

(def-cody-doc-sync-test cody-doc-sync-test-multiple-edits
  "erts/doc-sync-multiple-edits.erts"
  (goto-char (point-min))
  (insert "import com.foo.Bar;\n\n")
  (search-forward "{\n")
  (open-line 1)
  (insert "  // no comment")
  (forward-line 1)
  (goto-char (line-end-position))
  (insert ";")
  (goto-char (point-max))
  (insert "// end class Foo\n"))

(provide 'cody-doc-sync-test)
;;; cody-doc-sync-test.el ends here
