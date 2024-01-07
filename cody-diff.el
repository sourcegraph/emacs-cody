;;; cody-diff.el -- Provides Myers diff functionality -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Sourcegraph, Inc.

;; Version: 0.1
;; Author: Steve Yegge <steve.yegge@gmail.com>
;; URL: https://github.com/sourcegraph/emacs-cody

;;; Commentary:
;; This provides a very quick-and-dirty list differ. It is intended
;; only for diffing within relatively short single sequences of under a
;; few hundred characters, for code autocompletion use cases.

(require 'cl-lib)

(defvar cody--list-diff-cache (make-hash-table :test 'equal)
  "Memoization cache for `cody--list-diff'.")

(defun cody-diff-lists (a b)
  "Compute Myers diff of lists A and B.
Returns a list of `patches' consisting of either atoms, which are
unchanged regions, deletions of the form (- atom atom? ...), and
insertions of the form (+ atom atom? ...)."
  (clrhash cody--list-diff-cache)
  (let ((max-lisp-eval-depth 25000)) ; allow deeper recursion
    (cody--diff-worker a b)))

(defun cody-diff-strings (a b)
  "Compute Myers diff at character level on two strings A and B.
Returns same as `cody-diff-lists' except with single-character
strings as the atoms in the patch."
  (cody-diff-lists (mapcar #'string (string-to-list a))
                   (mapcar #'string (string-to-list b))))

(defun cody-diff-strings-with-positions (a b start-pos)
  "Postprocesses the `cody-diff-strings' output for display.
Removes all unchanged characters, and prepends the buffer position
to each diff chunk. Return values are for the form `(POS . STRING)'.
Returns nil if there are any deletions in the diff."
  (cody--diff-get-patch-insertions (cody-diff-strings a b)
                                   start-pos))

(defun cody--diff-get-patch-insertions (patch start-pos)
  "Convert PATCH, a list of patch chunks, into position-based inserts.

START-POS is the buffer position corresponding to the beginning of the
code rewrites for the line, and may be at the beginning of the current line,
before point, but not before `line-beginning-position'.

The Myers diff algorithm returns patches that take the form of a
list of chunks which are either atoms representing unchanged
characters, or flat lists representing insertions (car is `+'),
with no support currently for deletions.

Example:
    (C B (+ A) B A (+ C))

We rewrite this list to compute the character position of each
insertion, discarding the unchanged characters, which are used
to count the buffer position for each patch chunk.

If any element of the passed PATCH is a deletion such as `(- A B)',
this function returns nil."
   (cl-loop with pos = start-pos
           and inserts = nil
           for chunk in patch
           do (pcase chunk
                (`(+ . ,ins)
                 (let ((insertion-str (apply 'concat ins)))
                   (push (cons pos insertion-str) inserts)
                   (cl-incf pos (length insertion-str))))
                (`(- . ,_) (cl-return nil)) ; Return nil immediately if a deletion is found
                (_ (cl-incf pos))) ; Handle unchanged characters
           finally return (nreverse inserts)))

;; I found this code here: http://perma-curious.eu/post-elisp-diff/
;; There is no license referenced with it. I've contacted the author
;; and asked if we can use it; if that falls through it's easy to reimplement;
;; it's just a memoized port of http://www.lispology.com/show?1H95
;; which is a dirt-simple recursive expression of the Myers algorithm.
;; I fixed a bug with diffing lists of strings, and renamed the cache var.

(defun cody--diff-worker (a b)
  "Diff A and B.
Recursively computes the longest common subsequence and while
doing so assembles the returned diff."
    ;; f:first r:rest
    (with-memoization (gethash (cons a b) cody--list-diff-cache)
      (cl-labels
          ((choose (a b)
             ;; choose diff with longer lcs
             (let ((la (seq-count #'atom a))
                   (lb (seq-count #'atom b)))
               (if (> la lb) a b)))
           (merge (x)
             ;; merge consecutive add or rem
             (pcase x
               (`((,op ,f) (,op . ,r) . ,d)
                `((,op ,f . ,r) . ,d))
               (d d))))
        (cond
         ((and a b)
          (pcase-let ((`(,af . ,ar) a)
                      (`(,bf . ,br) b))
            (if (equal af bf)
                ;; same
                (cons af (cody--diff-worker ar br))
              ;; different
              (merge (choose (cons (list '+ bf) (cody--diff-worker a br))
                             (cons (list '- af) (cody--diff-worker ar b)))))))
         ;; rest
         (a `((- . ,a)))
         (b `((+ . ,b)))))))


(provide 'cody-diff)
;;; cody-diff.el ends here
