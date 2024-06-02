;;; cody-repo-util.el --- Sourcegraph Cody repository utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Sourcegraph, Inc.

;; Version: 0.1
;; Maintainer: Steve Yegge <stevey@sourcegraph.com>
;; URL: https://github.com/sourcegraph/emacs-cody
;; Package-Requires: ((emacs "26.3") (cl-lib "0.5"))

;;; Commentary:
;;
;; This package provides utility functions to interact with version control
;; systems within Sourcegraph Cody for Emacs.
;;
;; Functions in this package are used to retrieve repository information,
;; handle URL replacements, and determine the VCS type for files in a project.

;;; Code:

(require 'cl-lib)
(require 'vc)
(require 'vc-git)
(require 'url-parse)

(defun cody--uri-for (buffer-or-file)
  "Convert the BUFFER-OR-FILE to a URI string.
Handles Windows drive letters and formats them appropriately.
If BUFFER-OR-FILE is a buffer, retrieves its associated file name.
Returns BUFFER-OR-FILE in case of errors or if conversion is not possible,
or `buffer-name' as a last resort, to avoid sending a nil path to the agent."
  (let* ((buffer (if (bufferp buffer-or-file) buffer-or-file (current-buffer)))
         (file (or (when (bufferp buffer-or-file)
                    (buffer-file-name buffer-or-file))
                   buffer-or-file))
         (file (or file (buffer-name buffer))))
    (condition-case err
        (let* ((expanded (expand-file-name file))
               (uri (if (file-remote-p expanded)
                        expanded
                      (concat "file:///"
                              (replace-regexp-in-string
                               (regexp-quote "/") "/"
                               (url-hexify-string expanded))))))
          ;; Handle Windows drive letters
          (if (string-match "^file:///\\([a-zA-Z]\\):/" uri)
              (let ((drive-letter (downcase (match-string 1 uri))))
                (replace-match (concat "file:///" drive-letter "%3A/") t t uri))
            uri))
      (error
       (cody--log "Error converting file to URI: %s" err)
       (buffer-name buffer)))))

(defun cody--repo-info (project file)
  "Return repository info for PROJECT and FILE."
  (let* ((vcs-type (cody--get-vcs-type project file))
         (relative-path "")
         (remote-url "")
         (remote-branch-name "")
         (repo-root-path (cody--get-repo-root-path project file)))
    (when repo-root-path
      (setq relative-path (if (> (length (file-relative-name file repo-root-path)) 0)
                              (file-relative-name file repo-root-path)
                            ""))
      (when (and (eq vcs-type 'perforce) (string-match "/" relative-path))
        (setq relative-path (substring relative-path (match-end 0))))
      (setq remote-url (cody--get-remote-repo-url project file))
      (setq remote-url (cody--url-do-replacements remote-url))
      (setq remote-branch-name (cody--get-remote-branch-name project file))
      (when (or (not remote-branch-name)
                (not (string-match-p remote-branch-name remote-url)))
        (setq remote-branch-name cody-default-branch-name)))
    (list :vcs-type vcs-type
          :remote-url remote-url
          :remote-branch-name (or remote-branch-name cody--default-branch-name)
          :relative-path relative-path)))

(defun cody--find-repository-name (project current-file)
  "Find the repository name for PROJECT and CURRENT-FILE."
  (when-let ((file-from-repo (or current-file
                                 (cody--get-root-file-from-first-git-repo project))))
    (condition-case nil
        (cody--get-remote-repo-url project file-from-repo)
      (error
       (cody--get-simple-repository-name project file-from-repo)))))

(defun cody--get-simple-repository-name (project file)
  "Get the simple repository name for PROJECT and FILE."
  (when-let ((repository (vc-call-backend (vc-backend file) 'root file)))
    (file-name-nondirectory (directory-file-name repository))))

(defun cody--url-do-replacements (remote-url)
  "Perform URL replacements for REMOTE-URL"
  (let* ((replacements
          (split-string cody--remote-url-replacements "\\s*,\\s*"))
         (remote-url-with-replacements remote-url))
    (when (zerop (% (length replacements) 2))
      (cl-loop for i from 0 to (1- (/ (length replacements) 2))
               do (setq remote-url-with-replacements
                        (replace-regexp-in-string
                         (nth (* 2 i) replacements)
                         (nth (1+ (* 2 i)) replacements)
                         remote-url-with-replacements))))
    remote-url-with-replacements))

(defun cody--get-remote-repo-url (project file)
  "Get the remote repository URL for PROJECT and FILE.
Returned format: github.com:sourcegraph/sourcegraph.git"
  (let ((repository (vc-call-backend (vc-backend file) 'root file))
        (vcs-type (cody--get-vcs-type project file)))
    (cond
     ((and (eq vcs-type 'git) repository)
      (cody--convert-git-clone-url-to-codebase-name (vc-git-repository-url repository)))
     ((eq vcs-type 'perforce)
      (cody--get-perforce-remote-repo-url project file))
     (t (error "Unsupported VCS or repository not found for file %s" file)))))

(defun cody--get-repo-root-path (project file)
  "Get the repository root directory for PROJECT and FILE."
  (let ((vcs-root (vc-call-backend (vc-backend file) 'root file)))
    (when vcs-root
      (file-name-directory vcs-root))))

(defun cody--get-remote-branch-name (project file)
  "Get the remote branch name for PROJECT and FILE."
  (let ((repository (vc-call-backend (vc-backend file) 'root file)))
    (when (and repository (eq (cody--get-vcs-type project file) 'git))
      (vc-git-working-revision repository))))

(defun cody--get-vcs-type (project file)
  "Get the VCS type for PROJECT and FILE."
  (let ((repository (vc-call-backend (vc-backend file) 'root file)))
    (cond
     ((and (require 'vc-git nil t)
           (vc-call-backend 'Git 'root file)) 'git)
     ((and (require 'p4 nil t)
           (p4-get-client-name)) 'perforce)
     (t 'unknown))))

(defun cody--get-root-file-from-first-git-repo (project)
  "Get the root file from the first Git repository in PROJECT."
  (let ((repo (locate-dominating-file project ".git")))
    (when repo
      (expand-file-name repo))))

(defun cody--convert-git-clone-url-to-codebase-name (url)
  "Convert Git clone URL to codebase name or raise an error."
  (let* ((url (downcase url))
         (ssh-url-regex "^[\\w-]+@\\([^:]+\\):\\([\\w-]+\\)/\\([\\w-]+\\(\\.git\\)?\\)$")
         (parsed-url (url-generic-parse-url url))
         (scheme (url-type parsed-url))
         (host (url-host parsed-url))
         (path (url-filename parsed-url)))
    (if (string-match ssh-url-regex url)
        (cl-destructuring-bind (_ whole host owner repo &rest)
            (match-data)
          (format "%s/%s/%s"
                  (match-string 1 url)
                  (match-string 2 url)
                  (replace-regexp-in-string "\\.git$" "" (match-string 3 url))))
      (when (null scheme)
        (setq parsed-url (url-generic-parse-url (concat "http://" url)))
        (setq scheme (url-type parsed-url))
        (setq host (url-host parsed-url))
        (setq path (url-filename parsed-url)))
      ;; Handle known host patterns
      (cond
       ((and host (string-match-p "dev.azure" host))
        (replace-regexp-in-string "/_git" "" (concat host path)))
       ((and scheme (string-prefix-p "github" scheme))
        (format "github.com/%s" (replace-regexp-in-string "\\.git$" "" path)))
       ((and scheme (string-prefix-p "gitlab" scheme))
        (format "gitlab.com/%s" (replace-regexp-in-string "\\.git$" "" path)))
       ((and (string-prefix-p "http" scheme) host path)
        (format "%s%s" host (replace-regexp-in-string "\\.git$" "" path)))
       ((and host path)
        (format "%s%s" host (replace-regexp-in-string "\\.git$" "" path)))
       (t (error "Cody could not extract repo name from clone URL %s" url))))))

(provide 'cody-repo-util)

;;; cody-repo-util.el ends here
