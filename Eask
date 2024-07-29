;; Eask file for project Cody.
(source "gnu")
(source "melpa")

(package "cody"
         "0.2.0"
         "Sourcegraph Cody in Emacs")

(website-url "https://github.com/sourcegraph/emacs-cody")
(keywords "completion" "convenience" "languages" "programming" "tools")

(package-file "cody.el")

(depends-on "emacs" "26.3")
(depends-on "jsonrpc" "1.0.16")
(depends-on "uuidgen" "20240201.2318")

(development
 (depends-on "buttercup" "1.35"))

(put 'script 'lisp-indent-function '1)
(script "test"
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; Bootstrap straight.el
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'uuidgen)
  (straight-use-package 'buttercup)

  (require 'uuidgen)
  (require 'buttercup)

  ;; TODO: Manually set PATH for the CI environment
  ;;(setenv "PATH" (concat "/custom/path/to/node/bin:" (getenv "PATH")))

  ;; Output the path variables for debugging
  (message "load-path: %s" load-path)
  (message "default-directory: %s" default-directory)

  (message "Starting test loading.")
  (let* ((default-directory (file-name-directory (or load-file-name buffer-file-name)))
         (project-root (file-name-directory (locate-dominating-file default-directory "Eask")))
         (test-dir (expand-file-name "tests" project-root))
         (buttercup-files (directory-files test-dir t "\\.el$")))
    (message "Project root: %s" project-root)
    (message "buttercup-files: %s" buttercup-files)
    (add-to-list 'load-path project-root)
    (add-to-list 'load-path test-dir)

    ;; Load and run Buttercup tests.
    (dolist (file buttercup-files)
      (message "Loading Buttercup test file: %s" file)
      (load file t t t))
    (buttercup-run-discover)))
