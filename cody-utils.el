;;; cody-utils.el --- Utility functions for Sourcegraph Cody in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sourcegraph, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; URL: https://github.com/sourcegraph/emacs-cody
;; Version: 0.1

;;; Commentary:
;; Pure utilities for the `cody' package, with no dependencies.

;;; Code:

(defsubst cody--bol ()
  "Alias for `line-beginning-position'."
  ;; Why did they retire `point-at-bol'? :-(
  (line-beginning-position))

(defsubst cody--eol ()
  "Alias for `line-end-position'."
  (line-beginning-position))

(defsubst cody--key-for-command (command &optional keymap)
  "Get user-visible key sequence for COMMAND."
  (when-let ((keys (where-is-internal command keymap)))
    (key-description (car keys))))

(defun cody--buffer-active-p (&optional buf)
  "Return non-nil if BUF is active. BUF defaults to the current buffer."
  (let ((buffer (or buf (current-buffer))))
    (and (eq buffer (window-buffer (selected-window)))
         (get-buffer-window buffer t))))

;; Development utilities.

(defun cody--edebug-inspect (obj)
  "Pretty-print OBJ, one of our EIEIO objects while debugging.
It pops the pretty-printed object tree into a separate buffer.
Invoke this while debugging from an *edebug* buffer, e.g. with:

  (cody--edebug-inspect (cody--cc))

to see the current completion response object in detail.
"
  (let ((buf (get-buffer-create "*Edebug Object Dump*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (pp-to-string
               (mapcar (lambda (slot)
                         (cons (eieio-slot-descriptor-name slot)
                               (eieio-oref obj (eieio-slot-descriptor-name slot))))
                       (eieio-class-slots (eieio-object-class obj))))))
    (pop-to-buffer buf)))

(provide 'cody-utils)
;;; cody-utils.el ends here
