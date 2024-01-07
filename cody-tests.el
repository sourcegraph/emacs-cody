;;; cody-tests.el --- tests for Emacs-Cody -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sourcegraph, Inc.

;; Version: 0.1
;; Author: Steve Yegge <steve.yegge@gmail.com>
;; URL: https://github.com/sourcegraph/emacs-cody

(require 'cody)
(require 'cody-diff)
(require 'ert)

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
