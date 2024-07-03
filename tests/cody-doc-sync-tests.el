;;; cody-doc-sync-tests.el --- Tests for Cody document synchronization -*- lexical-binding: t; -*-

;;; Commentary:
;; Document synchronization tests, to validate that Agent is tracking the
;; selection and buffer content of Cody-enabled buffers.

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'cody)
(require 'cody-test-fixture)

;; To run these tests:
;;   cd .. && eask run buttercup
;; or:
;;   M-x buttercup-run-at-point on this sexpr:
;;
;; To skip a test:
;;   - insert (buttercup-skip "reason") at the top, after the it-clause.

(describe "Cody doc-sync tests"
  (before-all (cody-shutdown))
  (after-all (cody-shutdown))

  (it "tracks caret/point moving"
    (cody--test-doc-sync
     :before "
class Foo {
  console.log(\"howdy^!\")
}
"
     :after "
class Foo {
  console.log(\"howdy!\")
}
"
     :code
     (backward-char 1)))


  (it "tracks adding a selection"
    (cody--test-doc-sync
     :before "
class Foo {
  ^console.log(\"hello\")
}
"
     ;; We don't need to specify the selection here, as the test will compare
     ;; the RPC result's selection to the temp buffer's current selection.
     :after "
class Foo {
  console.log(\"hello\")
}
"
     :code
     ;; Set the selection while in cody-mode, to trigger agent mirroring.
     (set-mark (line-end-position))))

  (it "tracks inserting a char"
    (cody--test-doc-sync
     :point-char "@"

     :before "
class Foo {
  console.log(\"hello there@\")
}
"
     :after "
class Foo {
  console.log(\"hello there!\")
}
"
     :code
     (insert "!")))

  (it "tracks deleting a char"
    (cody--test-doc-sync
     :before "
class Foo {
  console.log(\"hello there^!\")
}
"
     :after "
class Foo {
  console.log(\"hello there\")
}
"
     :code
     (progn
       (goto-char (point-min))
       (search-forward "!") ; leaves point after the "!"
       ;; This would ideally be `delete-backward-char' to simulate the more
       ;; typical usage, but the byte compiler complains that it's a no-no.
       (delete-char -1))))

  (it "tracks deleting a range"
    (cody--test-doc-sync
     :before "
class Foo {
  ^console.log(\"hello there!\")
}
"
     :after "
class Foo {
  (\"hello there!\")
}
"
     :code
     (kill-word 2)))

  (it "tracks replacing a range atomically"
    (cody--test-doc-sync
     :before "
class Foo {
  ^System.out.println(\"hello there\")
}
"
     :after "
class Foo {
  console.log(\"hello there\")
}
"
     :code
     (save-excursion
       (goto-char (point-min))
       (while (search-forward "System.out.println" nil t)
         (replace-match "console.log")))))

  (it "tracks replacing a range non-atomically"
    (cody--test-doc-sync
     :before "
class Foo {
  ^System.out.println(\"hullo\")
}
"
     :after "
class Foo {
  console.log(\"hullo\")
}
"
     :code
     (progn
       (kill-word 3)
       (insert "console.log"))))

  (it "tracks inserting with newlines"
    (cody--test-doc-sync
     :before "
class Foo {
  console.log(\"hello there!\")@
}
"
     :after "
class Foo {
  console.log(\"hello there!\")
  console.log(\"this is a test\")
  console.log(\"hello hello\")
}
"
     :code
     (progn
       (insert "\n  console.log(\"this is a test\")")
       (insert "\n  console.log(\"hello hello\")"))))

  (it "tracks erasing the document"
    (cody--test-doc-sync
     :before "
class Foo {
  ^console.log(\"hello there!\")
}
"
     :after "
"
     :code
     (delete-region (point-min) (point-max))))

  (it "syncs agent when appending to end of document"
    (cody--test-doc-sync
     :before "
class Foo {
  console.log(\"hello there!\")
}
"
     :after "
class Foo {
  console.log(\"hello there!\")
}
// antidisestablishmentarianism
// pneumonoultramicroscopicsilicovolcanoconiosis
"
     :code
     (progn
       (goto-char (point-max))
       (insert "// antidisestablishmentarianism\n")
       (insert "// pneumonoultramicroscopicsilicovolcanoconiosis\n"))))

  (it "tracks deleting ranges with newlines"
    (cody--test-doc-sync

     :point-char "@"

     :before "
class Foo {
  console.log(\"item 1\")@
  console.log(\"item 2\")
  console.log(\"item 3\")
  console.log(\"item 4\")
}
"
     :after "
class Foo {
  console.log(\"item 1\")
  console.log(\"item 4\")
}
"
     :code
     (delete-region (point)
                    (save-excursion
                      (forward-line 3)
                      (1- (point))))))

  (it "tracks inserting emojis"
    (cody--test-doc-sync
     :before "
class Foo {
  console.log(\"hello there^\")
}
"
     :after "
class Foo {
  console.log(\"hello there!🎉🎂
🥳🎈\")
}
"
     :code
     (insert "!🎉🎂\n🥳🎈")))

  (it "tracks multiple edits"
    (cody--test-doc-sync
     :before "
class Foo {
  console.log("hello there")
}
  "
     :after "
import com.foo.Bar;

class Foo {
  // no comment
  console.log("hello there");
}
// end class Foo
  "
     :code
     (progn
       (goto-char (point-min))
       (insert "import com.foo.Bar;\n\n")
       (search-forward "{\n")
       (open-line 1)
       (insert "  // no comment")
       (forward-line 1)
       (goto-char (line-end-position))
       (insert ";")
       (goto-char (point-max))
       (insert "// end class Foo\n"))))

  (it "tracks pasting/yanking"
    (cody--test-doc-sync
     :before "
class Foo {
  console.log(\"^\")
}
"
     :after "
class Foo {
  console.log(\"hello there\")
}
"
     :code
     (let ((original-point (point))
           (original-kill-ring kill-ring)
           (original-kill-ring-yank-pointer kill-ring-yank-pointer))
       (unwind-protect
           (progn
             (goto-char original-point)
             (let ((temporary-text "hello there"))
               (kill-new temporary-text)
               (yank)))
         ;; Restore global state
         (setq kill-ring original-kill-ring)
         (setq kill-ring-yank-pointer original-kill-ring-yank-pointer)
         (goto-char original-point)))))

  ) ; end describe

(provide 'cody-doc-sync-tests)
;;; cody-doc-sync-tests.el ends here
