;;; cody-tests.el --- Tests for Emacs-Cody -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Sourcegraph, Inc.

;; Version: 0.1
;; Author: Steve Yegge <steve.yegge@gmail.com>
;; URL: https://github.com/sourcegraph/emacs-cody

;;; Commentary:
;; It is best to start with a fresh copy of Emacs to run these tests,
;; but they are written to pass even if Cody has aready been started.
;;
;; To run them, first `M-x eval-buffer' to load the test definitions,
;; then `ert-run-tests-interactively RET RET' to run them.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'ert)
(require 'cody)

;;; System checks

(defmacro cody--node-version-test (&rest body)
  "Helper for testing `cody--check-node-version'.
The tests should set `test-version' to a node version string."
  ;; I had to brute-force this, so that the test runs even if you have
  ;; started Cody, even if it has since shut down. I was unable to
  ;; let-bind `shell-command-to-string' (e.g. with `cl-flet') and have it
  ;; take effect in the test, though it works in the *scratch*
  ;; buffer. Similarly, Cody was overriding the dynamic let-binding of
  ;; `cody--node-version-status', and both these issues would cause the
  ;; test to fail. So I had to replace the symbol definitions, and then
  ;; put them back after the test.
  (declare (indent defun) (debug body))
  `(let ((old-shell-command-fn (symbol-function 'shell-command-to-string))
         (old-node-version-status cody--node-version-status)
         test-version)
     (unwind-protect
         (progn
           (fset 'shell-command-to-string (lambda (cmd) test-version))
           ,@body)
       ;; Clean up.
       (fset 'shell-command-to-string old-shell-command-fn)
       (set cody--node-version-status old-node-version-status))))

(ert-deftest cody-test-node-v-check-fails ()
  "Test version check fails if major or minor version too low."
  (cody--node-version-test
    ;; Major version too low.
    (setq test-version "v18.17.1")
    (set 'cody--node-version-status nil)
    (should-error (cody--check-node-version))
    ;; Minor version too low.
    (setq test-version "v20.3.0")
    (set 'cody--node-version-status nil)
    (should-error (cody--check-node-version))))


(ert-deftest cody-test-node-v-check-succeeds ()
  "Test version check fails if major version too low."
  (cody--node-version-test
    (setq test-version "v20.4.0")
    (set 'cody--node-version-status nil)
    (should (cody--check-node-version))

    (setq test-version "v21.1.0")
    (set 'cody--node-version-status nil)
    (should (cody--check-node-version))))

;;; Completions tests

(defmacro cody--mock-completion-response (items)
  "Generate a custom mock completion jsonrpc response object.

You must specify one or more items in ITEMS, the list of
completion suggestions. Each spec in ITEMS has 4 elements:

  (id text range-beg range-end)

ID is a unique message ID, and TEXT is the suggested
replacement, which can be a multi-line string.
RANGE-BEG and RANGE-END are 0-based buffer bounds of the text
to replace with this completion suggestion upon acceptance.

The response object returned by the jsonrpc call is a plist:

  (:items (item-list) :completionEvent (event-params))

Currently you can only configure the `:completionEvent'
object by manipulating the returned tree manually."
  `(cl-labels
       ((convert-pos (pos)
          "Convert a 1-based buffer position to a 0-based line/character position."
          (let ((line (1- (line-number-at-pos pos)))
                (char (1- (save-excursion (goto-char pos) (current-column)))))
            `(:line ,line :character ,char)))
        (count-lines-and-chars (text)
          "Count the lines and characters in the completion text."
          (let ((lines (1+ (count-matches "\n" nil nil text)))
                (chars (length text)))
            `(:lineCount ,lines :charCount ,chars :stopReason "stop"
                         :lineTruncatedCount 0 :truncatedWith "indentation"))))
     (let ((items-vector (cl-loop for (id text range-beg range-end) in ,items
                                  vconcat `(( :id ,id
                                              :insertText ,text
                                              :range ( :start ,(convert-pos range-beg)
                                                       :end ,(convert-pos range-end))))))
           (event-items (cl-loop for (_ text _ _) in ,items
                                 vconcat (list (count-lines-and-chars text)))))
       `(:items ,items-vector
                :completionEvent ( :id "test-completion-hash-id"
                                   :params ( :multiline t
                                             :triggerKind "Manual"
                                             :providerIdentifier "fireworks"
                                             :providerModel "starcoder-hybrid"
                                             :languageId "typescript"
                                             :artificialDelay 0
                                             :multilineMode "block"
                                             :id "3948fdc3-317b-48d1-be79-734628c94e94"
                                             :contextSummary
                                             ( :strategy "jaccard-similarity"
                                               :duration 21.32
                                               :totalChars
                                               ,(apply '+ (mapcar (lambda (item)
                                                                    (plist-get item :charCount))
                                                                  event-items))
                                               retrieverStats nil)
                                             :source "Network")
                                   :startedAt 2149
                                   :items ,event-items
                                   :loggedPartialAcceptedLength 0)))))

(defvar cody--tests-sample-completion-response-1
  (list
   (list "1" "Call me Ishmael" 500 505)
   (list "2" "Hear Me, Oh Muse" 500 505)
   (list "3" "Rosebud" 500 505))
  "3 single-line suggestions included")

(defun cody--test-whitespace-before-caret (str caret-pos)
  "Return the whitespace before the first caret in STR.
Returns ws up to the beginning of the line, given CARET-POS."
  (let ((start-pos (string-match "\\(^\\|[^[:space:]]\\)[[:space:]]*$"
                                 str 0 caret-pos)))
    (if start-pos
        (substring str start-pos caret-pos)
      "")))

(defun cody--test-set-up-caret-test (test-spec)
  "Set up test parameters given a DSL in TEST-SPEC.

Currently the DSL has only one element, the ^, which
is removed in the result, and that position is returned
as the point at which the completion is requested.

Return value is (updated-text . point)."
  (let* ((caret (or (string-match "\\^" test-spec)
                    (error "No ^ character found in test-spec")))
         (text (concat (substring test-spec 0 caret) ; remove caret
                       (substring test-spec (1+ caret)))))
    (cons text (1+ caret)))) ; convert string pos to buffer pos

(ert-deftest cody-test-single-line-completion ()
  "Run checks for a single-line completion response."
  ;; TODO: Make a new macro to reduce the boilerplate below.
  (let* ((pretext "// file hello.c
#include <stdio.h>
int main() {
   ^
   return 0;
}
")
         (params (cody--test-set-up-caret-test pretext))
         (insert-text (car params))
         (pos (cdr params))
         (cody--unit-testing-p t)) ; don't talk to real agent
    (with-temp-buffer
      (erase-buffer)
      (insert insert-text)
      (goto-char pos)
      (cody-mode)
      (let ((response (cody--mock-completion-response
                       `(("id-1" "printf(\"Hello, world!\");" ,pos ,pos)))))
        ;; Make sure cody-mode starts up without failing.
        (should cody-mode)
        ;; Should pass if the response looks good.
        ;; TODO: Finish getting this test working.
        ;;   - there is a 0-indexing problem happening in the test code
        (when nil
          (should (cody--handle-completion-result
                   response (current-buffer) pos "Automatic"))
          ;; Check that the event is dropped if the point was moved.
          (should-not (cody--handle-completion-result
                       response
                       (current-buffer)
                       (- pos 2) ; point has moved 2 spaces ahead of request
                       "Invoke"))) ; or Automatic
        ;; It should error if items is not a vector.
        (should 'finish-this-test)))))


(ert-deftest cody-test-diff-lists ()
  "Tests for `cody-diff-lists' Myers-diff functionality."
  (should (null (cody-diff-lists nil nil)))
  (let* ((a '(1 2 3))
         (b a))
    (should (equal (cody-diff-lists a b) a)))
  ;; Test James Coglan's example of ABCABBA and CBABAC.
  ;; https://blog.jcoglan.com/2017/02/12/the-myers-diff-algorithm-part-1/
  (should (equal (cody-diff-lists '(A B C A B B A) '(C B A B A C))
                 '((- A B) C (- A) B (+ A) B A (+ C)))))

(ert-deftest cody-test-diff-strings ()
  "Tests for `cody-diff-strings' Myers-diff functionality."
  (should (null (cody-diff-strings nil nil)))
  (should (equal (cody-diff-strings
                  "ABCABBA" ; start string
                  "CBABAC") ; final string
                 '((- "A" "B")
                   "C" (- "A")
                   "B" (+ "A")
                   "B" "A" (+ "C"))))
  ;; Check that it works with cursor at point-min.
  (should (equal (cody-diff-strings-with-positions
                  "AAAA"
                  "BABABAABB" ; insert a bunch of B's
                  0)
                 '((0 . "B") (2 . "B") (4 . "B") (7 . "BB"))))
  ;; Check that it works after point-min.
  (should (equal (cody-diff-strings-with-positions
                  "foofoo"
                  "foolfoolery"
                  10)
                 '((13 . "l") (17 . "lery"))))
  ;; Check that a deletion results in a nil.
  (should (null (cody-diff-strings-with-positions
                 "foobar"
                 "fooarb"
                 42))))


(provide 'cody-tests)
;;; cody-tests.el ends here
