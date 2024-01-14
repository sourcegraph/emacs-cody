;;; cody-tests.el --- tests for Emacs-Cody

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

(require 'cody)
(require 'cody-diff)
(require 'ert)
(eval-when-compile
  (require 'cl-lib))

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
