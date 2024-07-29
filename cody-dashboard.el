;;; cody-dashboard.el --- Cody Dashboard for Emacs -*- lexical-binding: t; -*-
;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 1.1
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides a dashboard for displaying Cody configuration
;; and usage information in Emacs.  It shows server info, authentication
;; status, active workspaces, and Cody settings with clickable links.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'browse-url)

(defface cody-dashboard-status-success
  '((t (:foreground "green")))
  "Face for successful status in Cody dashboard.")

(defface cody-dashboard-status-error
  '((t (:foreground "red")))
  "Face for error status in Cody dashboard.")

(defface cody-dashboard-status-warning
  '((t (:inherit italic :foreground "red")))
  "Face for warning labels in Cody dashboard.")

(defface cody-dashboard-status-disabled
  '((t :inherit font-lock-comment-face))
  "Face for disabled or non-interactible text in the Cody dashboard.")

(defvar cody--dashboard-avatar-inserted nil
  "Private variable, set when avatar has been inserted in the dashboard.")

(defun cody-dashboard ()
  "Show a console buffer with info about Cody configuration and usage."
  (interactive)
  (let ((buf (get-buffer-create "*cody-dashboard*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (inhibit-redisplay t)
            (inhibit-modification-hooks t))
        (setq buffer-read-only nil)
        (erase-buffer)
        (save-excursion
          (with-silent-modifications
            (cody--dashboard-build)))
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (pop-to-buffer buf)))))

(defun cody--dashboard-build ()
  "Insert the dashboard contents into the current buffer."
  (let*((image-marker (make-marker))
        (workspace-counts (cody--dashboard-count-workspaces))
        (any-live-connection (nth 0 workspace-counts))
        (active-count (nth 1 workspace-counts))
        (disconnected-count (nth 2 workspace-counts))
        (error-count (nth 3 workspace-counts)))
    (cody--dashboard-insert-global-info
     active-count (+ error-count disconnected-count))

    (cody--dashboard-insert-node-status)

    (setq cody--dashboard-avatar-inserted nil)
    (cody--dashboard-insert-server-info-and-auth-status image-marker)

    (when cody--connection-global-error
      (cody--dashboard-insert-global-error-banner))

    (cody--dashboard-insert-header "Active Workspaces")
    (insert "\n")
    (cody--dashboard-insert-active-workspaces)

    (cody--dashboard-insert-header "Cody Settings")
    (insert "\n")
    (cody--dashboard-display-settings)))

(defun cody--dashboard-insert-node-status ()
  "Tell the user if the Node.js version is wrong."
  (when (null cody--node-version-status)
    (ignore-errors
      (cody--check-node-version)))
  (when (not (eq cody--node-version-status 'good))
    (cody--dashboard-insert-text "Warning: " 'cody-dashboard-status-warning t)
    (cody--dashboard-insert-text
     "Your Node.js version does not match required version " 'error t)
    (cody--dashboard-insert-text (format "%s" cody-node-min-version) 'success t)
    (cody--dashboard-insert-text
     ".\nFor now, you will need to customize `" 'error t)
    (insert-text-button "cody-node-executable"
                      'face 'link
                      'follow-link t
                      'action (lambda (_) (describe-variable 'cody-node-executable)))
    (cody--dashboard-insert-text
     "' to the\nabsolute path to your node binary.\n" 'error)))
          
(defun cody--dashboard-insert-global-info (active-count error-count)
  "Insert global info section of the dashboard, including workspace counts.
ACTIVE-COUNT is the number of active workspaces.
Argument ERROR-COUNT is the number of error or unconnected workspaces."
  (cody--dashboard-insert-header "Global Info")
  (insert "\n")
  (cody--dashboard-insert-field "  Cody Global Mode"
                                (if cody--global-mode "Enabled" "Disabled"))
  (cody--dashboard-insert-field "  Number of Cody Workspaces"
                                (number-to-string (hash-table-count cody-workspaces)))
  (cody--dashboard-insert-field "  Active Workspaces"
                                (number-to-string active-count))
  (cody--dashboard-insert-field "  Error/Disconnected Workspaces"
                                (number-to-string error-count))
  (insert "\n"))

(defun cody--dashboard-insert-server-info-and-auth-status (image-marker)
  "Insert server info and auth status into the dashboard."
  (let* ((any-live-connection (cody--dashboard-has-live-connection))
         (server-info (and any-live-connection (cody--get-server-info))))
    (cody--dashboard-insert-header "Server Info")
    (insert "\n")
    (if server-info
        (progn
          (cody--dashboard-insert-field
           "  Name" (cody--server-info-name server-info))
          (cody--dashboard-insert-field
           "  Type" (if cody-use-remote-agent
                        (format "remote on port %s" cody-remote-agent-port)
                      "local"))

          (cody--dashboard-insert-field
           "  Authenticated"
           (if (cody--server-info-authenticated server-info)
               "Yes" "No")
           (if (cody--server-info-authenticated server-info)
               'cody-dashboard-status-success 'cody-dashboard-status-error))

          (cody--dashboard-insert-field
           "  Cody Enabled"
           (if (cody--server-info-cody-enabled server-info)
               "Yes" "No")
           (if (cody--server-info-cody-enabled server-info)
               'cody-dashboard-status-success 'cody-dashboard-status-error))

          (cody--dashboard-insert-field
           "  Cody Version" (cody--server-info-cody-version server-info)))
      (cody--dashboard-insert-text "  (not available)"))
    (insert "\n")
    (cody--dashboard-insert-header "Auth Status")
    (if server-info
        (when-let ((auth-status (cody--server-info-auth-status server-info)))
          ;; Insert avatar marker
          (cody--dashboard-insert-avatar-once image-marker server-info)
          (insert "\n")

          (cody--dashboard-insert-field
           "  Username" (cody--auth-status-username auth-status))

          (cody--dashboard-insert-field
           "  Is Logged In"
           (if (cody--auth-status-is-logged-in auth-status)
               "Yes" "No")
           (if (cody--auth-status-is-logged-in auth-status)
               'cody-dashboard-status-success 'cody-dashboard-status-error))

          (cody--dashboard-insert-field
           "  Site Has Cody Enabled"
           (if (cody--auth-status-site-has-cody-enabled auth-status)
               "Yes" "No")
           (if (cody--auth-status-site-has-cody-enabled auth-status)
               'cody-dashboard-status-success 'cody-dashboard-status-error))

          (cody--dashboard-insert-field
           "  Site Version" (cody--auth-status-site-version auth-status))
          (cody--dashboard-insert-field
           "  Display Name" (cody--auth-status-display-name auth-status))
          (cody--dashboard-insert-field
           "  Primary Email" (cody--auth-status-primary-email auth-status)))
      (cody--dashboard-insert-text "  (not available)"))
    (insert "\n")))

(defun cody--dashboard-insert-avatar-once (image-marker server-info)
  "Call `cody--dashboard-insert-avatar', but only one time."
  (unless cody--dashboard-avatar-inserted
    (set-marker image-marker (point))
    (cody--dashboard-insert-avatar image-marker server-info)
    (setq cody--dashboard-avatar-inserted t)))

(defun cody--dashboard-insert-avatar (image-marker server-info)
  "Insert avatar image from SERVER-INFO at the current position."
  (when-let* ((auth-status (cody--server-info-auth-status server-info))
              (avatar-url (cody--auth-status-avatar-url auth-status))
              (dashboard (current-buffer)))
    (let ((inhibit-message t))
      (with-temp-message ""
        (url-retrieve
         avatar-url
         (lambda (_status)
           (goto-char (point-min))
           (re-search-forward "\n\n")
           (when (image-type-available-p 'png)
             (let ((image-data (buffer-substring-no-properties (point) (point-max))))
               (when (buffer-live-p dashboard) ; may have been killed
                 (with-current-buffer dashboard
                   (let ((buffer-read-only nil))
                     (goto-char image-marker)
                     (insert "  ")
                     (insert-image (create-image image-data 'png t
                                                 :ascent 'center
                                                 :max-height 40
                                                 :max-width 40))
                     (insert "\n"))))))))))))

(defun cody--get-server-info ()
  "Get the server info from any workspace if available."
  (let ((server-info nil))
    (maphash (lambda (_uri workspace)
               (unless server-info
                 (setq server-info (cody-workspace-server-info workspace))))
             cody-workspaces)
    server-info))

(defun cody--dashboard-has-live-connection ()
  "Check if any live connection exists in the active workspaces."
  (let ((workspace-counts (cody--dashboard-count-workspaces)))
    (nth 0 workspace-counts)))

(defun cody--dashboard-insert-workspace-summary (active-count
                                                 error-count)
  "Insert the summary section of the dashboard with workspace counts."
  (cody--dashboard-insert-field "  Active Workspaces"
                                (number-to-string active-count))
  (cody--dashboard-insert-field "  Error/Disconnected Workspaces"
                                (number-to-string error-count))
  (insert "\n"))

(defun cody--dashboard-insert-active-workspaces ()
  "Insert information about active workspaces."
  (maphash
   (lambda (_uri workspace)
     (cody--dashboard-insert-workspace workspace))
   cody-workspaces))

(defun cody--dashboard-insert-global-error-banner ()
  "Insert a banner indicating a global connection error."
  (cody--dashboard-insert-text
   "Cody is unable to connect. Please check your configuration"
   'cody-dashboard-status-error)
  (cody--dashboard-insert-text "and then use M-x cody-restart to try again."
                               'cody-dashboard-status-error)
  (insert "\n  ")
  (insert-text-button "Restart"
                      'face 'button
                      'button t
                      'follow-link t
                      'action (lambda (_) (cody-restart)))
  (insert "\n\n"))

(defun cody--dashboard-display-workspace (uri workspace)
  "Display information about a WORKSPACE in the dashboard."
  (let ((root (cody-workspace-root workspace))
        (status (cody-workspace-status workspace))
        (error (cody-workspace-error workspace))
        (buffers (length (cody--workspace-buffers workspace)))
        (events-buffer (cody-workspace-events-buffer workspace))
        (stderr-buffer (cody-workspace-stderr-buffer workspace)))
    (cody--dashboard-insert-link "  - Workspace" uri)
    (insert (propertize "    Root: " 'face 'font-lock-keyword-face))
    (insert (format " %s\n" root))
    (insert "    Status: "
            (propertize (symbol-name status)
                        'face (cond
                               ((eq status 'error) 'cody-dashboard-status-error)
                               ((eq status 'connected) 'cody-dashboard-status-success)
                               (t 'cody-dashboard-status-disabled)))
            "\n")
    (when (eq status 'error)
      (insert "    Error: "
              (propertize (or error "(unspecified)") 'face 'italic)
              "\n"))
    (insert (format "    Buffers: %d\n" buffers))
    (if (and events-buffer (buffer-live-p events-buffer))
        (progn
          (insert "    Events buffer: ")
          (insert-text-button (buffer-name events-buffer)
                              'face 'link
                              'follow-link t
                              'action (lambda (_) (switch-to-buffer events-buffer)))
          (insert "\n")))
    (if (and stderr-buffer (buffer-live-p stderr-buffer))
        (progn
          (insert "    Stderr buffer: ")
          (insert-text-button (buffer-name stderr-buffer)
                              'face 'link
                              'follow-link t
                              'action (lambda (_) (switch-to-buffer stderr-buffer)))
          (insert "\n")))
    (if error
        (progn
          (insert (propertize (format "    Error: %s\n" error) 'face 'warning))))
    (insert "\n")))

(defun cody--dashboard-count-workspaces (&optional active-count
                                                   disconnected-count
                                                   error-count)
  "Count active, disconnected, and error workspaces.
Updates the given ACTIVE-COUNT, DISCONNECTED-COUNT, and ERROR-COUNT variables.
Return non-nil if any live connection exists."
  (let ((any-live-connection nil)
        (active-count (or active-count 0))
        (disconnected-count (or disconnected-count 0))
        (error-count (or error-count 0)))
    (maphash
     (lambda (_uri workspace)
       (let ((status (cody-workspace-status workspace))
             (error (cody-workspace-error workspace)))
         (cond
          ((eq status 'disconnected)
           (setq disconnected-count (1+ disconnected-count)))
          ((or error (eq status 'error))
           (setq error-count (1+ error-count)))
          (t
           (setq any-live-connection t)
           (setq active-count (1+ active-count))))))
     cody-workspaces)
    (list any-live-connection active-count disconnected-count error-count)))

(defun cody--dashboard-insert-workspace (workspace)
  "Insert information about WORKSPACE in the dashboard."
  (cody--dashboard-display-workspace (cody-workspace-uri workspace) workspace))

(defun cody--dashboard-valid-variable-p (member)
  "Check if MEMBER is a valid custom variable for the dashboard.
Return non-nil if MEMBER is a custom variable and not marked with `no-cody-dashboard'."
  (let ((symbol (if (consp member) (car member) member)))
    (and (custom-variable-p symbol)
         (not (get symbol 'no-cody-dashboard)))))

(defun cody--dashboard-custom-variables (group)
  "Get all custom variables for the provided GROUP and its subgroups.
Excludes variables marked with `no-cody-dashboard`.
Return an alist where each element is (GROUP . VARIABLES)."
  (let (results)
    (letrec ((collect-group
              (lambda (grp)
                (let ((variables
                       (cl-loop for member in (custom-group-members grp nil)
                                when (cody--dashboard-valid-variable-p member)
                                collect (if (consp member) (car member) member))))
                  ;; Add group and its variables to results
                  (when variables (push (cons grp variables) results))
                  ;; Recur for subgroups
                  (dolist (subgroup (custom-group-members grp 'groups))
                    (funcall collect-group
                             (if (consp subgroup) (car subgroup) subgroup)))))))
      (funcall collect-group group))
    ;; Put dev group last and top-level group first, which conveniently can be
    ;; accomplished by sorting the group names, at least for now.
    (cl-sort results (lambda (a b)
                       (string< (symbol-name (car a)) (symbol-name (car b)))))))

(defun cody--dashboard-display-settings ()
  "Insert Cody settings into the dashboard buffer."
  (condition-case err
      (dolist (group-var (cody--dashboard-custom-variables 'cody))
        (let ((group (car group-var))
              (variables (cdr group-var)))
          (cody--dashboard-insert-header (symbol-name group))
          (dolist (var variables)
            (insert (propertize (format "  %s: " var) 'face 'font-lock-builtin-face))
            (insert (propertize (format "%s\n" (symbol-value var))
                                'face 'font-lock-variable-name-face)))
          (insert "\n")))
    (error (insert (format "Error generating Cody settings: %s" err)))))

(defun cody--dashboard-show-disconnected-screen ()
  "Show that Cody is disconnected, with help on starting it."
  (save-excursion
    (cody--dashboard-insert-text "Cody is not connected" 'warning)
    (cody--dashboard-insert-text "")
    (cody--dashboard-insert-text " - `M-x cody-login' or click below to start Cody")
    (cody--dashboard-insert-text " - `M-x cody-help' for more information")
    (cody--dashboard-insert-text "")
    (insert-text-button "Start Cody"
                        'face 'button
                        'button t
                        'follow-link t
                        'action (lambda (_) (call-interactively 'cody-login))))
  (forward-line 1)) ;; don't obscure the main message with the cursor

(defun cody--dashboard-insert-link (label path &optional face)
  "Insert clickable link with LABEL pointing to PATH."
  (insert (propertize (concat label ": ") 'face 'font-lock-builtin-face))
  (insert-text-button path
                      'face (or face 'link)
                      'follow-link t
                      'action (lambda (_) (browse-url path)))
  (insert "\n"))

(defun cody--dashboard-insert-text (text &optional face no-newline-p)
  "Insert TEXT with an optional FACE."
  (insert (propertize
           (if no-newline-p
               text
             (concat text "\n"))
           'face face)))

(defun cody--dashboard-insert-header (header)
  "Insert section HEADER."
  (insert (propertize header 'face '(:underline t)))
  (insert "\n"))

(defun cody--dashboard-insert-field (label value &optional face)
  "Insert field with LABEL and VALUE, optionally with FACE."
  (insert (propertize (concat label ": ") 'face 'font-lock-builtin-face))
  (insert (propertize (concat (format "%s" value) "\n") 'face face)))

(provide 'cody-dashboard)
;;; cody-dashboard.el ends here
