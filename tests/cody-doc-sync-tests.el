;;; cody-doc-sync-tests.el --- Tests for Cody document synchronization -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains tests for the Cody document synchronization functionality.

;;; Code:

(require 'cl-lib)
(require 'buttercup)
(require 'cody)
(require 'cody-test-fixture)

(defmacro cody--test-doc-sync (&rest args)
  "Macro to define Cody document synchronization tests for Buttercup.

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
       (cody--test-doc-sync-execute initial-content
                                    ;; This \n is just for test readability.
                                    (string-remove-prefix "\n" ,after)
                                    point-pos
                                    mark-pos
                                    (lambda () ,code)))))

(defun cody--test-doc-sync-initialize-test (input-str point-char mark-char)
  "Parse INPUT-STR, determining positions of POINT-CHAR and MARK-CHAR.
Return (list stripped-content point-pos mark-pos)."
  ;; The first "\n" is just there to improve test readability.
  (let* ((input (string-remove-prefix "\n" input-str))
         (case-fold-search nil)
         (point-pos (string-match (regexp-quote point-char) input))
         (stripped-content (replace-regexp-in-string (regexp-quote point-char) "" input))
         (mark-pos (string-match (regexp-quote mark-char) stripped-content)))
    (when (and mark-pos (not point-pos))
      (error "Mark specified without point"))
    ;; Adjust point-pos and mark-pos if either character is present.
    (setq point-pos (when point-pos (1+ point-pos)))
    (when mark-pos
      (setq stripped-content
            (replace-regexp-in-string (regexp-quote mark-char) "" stripped-content))
      (setq mark-pos (1+ mark-pos))
      (when point-pos
        (when (< mark-pos point-pos)
          (setq point-pos (1- point-pos)))))
    (list stripped-content point-pos mark-pos)))

(defun cody--test-doc-sync-execute (initial-content
                                    expected-content
                                    point-pos
                                    mark-pos
                                    code)
  "Execute the document synchronization test.
INITIAL-CONTENT is the starting buffer content.
EXPECTED-CONTENT is the content expected after running CODE.
POINT-POS is the initial cursor position.
MARK-POS is the initial mark position.
CODE is a lambda function representing the test code to execute."
  (with-temp-buffer
    (let* ((temp-file (make-temp-file "cody-test-"))
           ;; Configure cody-mode for testing, minimizing differences.
           (cody-workspace-root (file-name-directory temp-file))
           (cody--integration-testing-p t)
           (cody--dev-panic-on-doc-desync nil)
           (cody-completions-auto-trigger-p nil))
      (unwind-protect
          (progn
            ;; Copy in the initial test source and associate it with a temp file.
            (set-visited-file-name temp-file)
            (insert initial-content)

            ;; Set initial selection. It's fine to do it before `cody-mode' starts.
            (goto-char (or point-pos (point-min)))
            (when mark-pos
              (set-mark (1+ mark-pos)))

            (expect (not cody-mode) :to-be-truthy)
            (cody-mode)
            (expect (eq cody--buffer-state 'active) :to-be-truthy)

            ;; Perform the edit function locally and wait for Agent to finish.
            (funcall code) ; this may initiate any number of rpcs to agent
            (cody--await-pending-promises) ; wait for agent to settle

            ;; Get the agent's mirrored document/selection, and compare
            ;; them to ours.
            (let* ((uri (cody--uri-for (buffer-file-name)))
                   (agent-doc (cody--test-doc-sync-get-agent-mirror-for-uri uri))
                   (agent-content (plist-get agent-doc :content))
                   (agent-selection (plist-get agent-doc :selection)))

              ;; Uncomment to compare the contents, as edebug struggles
              ;; with the `expect' forms below.

              ;; TODO:  Pass in the test name so we can print it here.

              ;; (message "Expected Content: '%s'" expected-content)
              ;; (message "Agent Content: '%s'" agent-content)

              (expect (stringp agent-content) :to-be-truthy)
              (expect (string= expected-content agent-content) :to-be-truthy)
              (expect (cody--selection-get-current) :to-equal agent-selection)))
        ;; Unwind/finally forms:
        (cody--test-doc-sync-cleanup temp-file)))))

;; To run these tests:
;;   cd .. && eask run buttercup
;; or:
;;   M-x buttercup-run-at-point on this sexpr:
;;
;; To skip a test:
;;   - insert (buttercup-skip "reason") at the top, after the it-clause.
;; 
(describe "Cody doc-sync tests"
  (before-all (cody-shutdown))
  (after-all (cody-shutdown))
  
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


  ;; TODO: More selection tests
  
  (it "tracks inserting a char"
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
    (insert "!"))

 (it "tracks deleting a char"
    :before "
lass Foo {
  console.log(\"hello there^!\")
}
"
    :after "
class Foo {
 console.log(\"hello there\")
}
"
    :code
    (delete-char 1))

  (it "tracks deleting a range"
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
    (kill-word 3))

  (it "tracks replacing a range atomically"
    :before "
class Foo {
  ^console.log(\"hello there!\")
}
! {
  console.log(\"hello there!\")
}
"
    :code
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "console.log" nil t)
        (replace-match "console.log"))))

  (it "tracks replacing a range non-atomically"
    :before "
class Foo {
  ^console.log(\"hello there!\")
}
"
    :after "
class Foo {
  console.log(\"hello there!\")
}
"
    :code
    (progn
      (kill-word 3)
      (insert "console.log")))

  (it "tracks inserting with newlines"
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
      (insert "\n  console.log(\"hello hello\")")))

  (it "tracks erasing the document"
    :before "
class Foo {
  ^console.log(\"hello there!\")
}
"
    :after "
"
    :code
    (delete-region (point-min) (point-max)))

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
                     (1- (point)))))

  (it "tracks inserting emojis"
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
    (insert "!🎉🎂\n🥳🎈"))

  (it "tracks multiple edits"
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
    (progn (goto-char (point-min))
           (insert "import com.foo.Bar;\n\n")
           (search-forward "{\n")
           (open-line 1)
           (insert "  // no comment")
           (forward-line 1)
           (goto-char (line-end-position))
           (insert ";")
           (goto-char (point-max))
           (insert "// end class Foo\n")))

  ) ; end describe

(provide 'cody-doc-sync-tests)
;;; cody-doc-sync-tests.el ends here
