;;; cody.el --- Sourcegraph Cody in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sourcegraph, Inc.

;; Version: 0.1
;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Maintainer: Steve Yegge <steve.yegge@gmail.com>
;; URL: https://github.com/sourcegraph/emacs-cody
;; Package-Requires: ((emacs "26.3") (jsonrpc "1.0.16") (uuidgen "1.2"))

;;; Commentary:
;; Add something like `(add-hook 'prog-mode-hook 'cody-mode)' to start
;; cody automatically in code buffers.  Make sure your nodejs "node"
;; executable is in your Emacs `exec-path'.

;;; Code:
(eval-when-compile (require 'cl-lib))
(eval-and-compile (require 'eieio-base))
(require 'auth-source)
(require 'jsonrpc)
(require 'uuidgen)
(require 'cody-diff)

(defgroup cody nil
  "Sourcegraph Cody."
  :group 'programming)

(defcustom cody-workspace-root (getenv "HOME")
  "Directory which Cody considers your current project root."
  :group 'cody
  :type 'string)

(defcustom cody-enable-completion-cycling t
  "Non-nil to allow cycling among alternative completion suggestions.
These are not always available, but when they are, you can cycle
between them with `cody-next-completion' and `cody-prev-completion'.
When nil, cycling is completely disabled, and only the first option
returned by the server is ever displayed or interactible."
  :group 'cody
  :type 'boolean)

(defcustom cody-enable-completion-cycling-help t
  "Non-nil to show a message in the minibuffer for cycling completions.
If non-nil, and multiple completion suggestions are returned from the
server, it will show how many are available and how to cycle them.
If nil, no messages are printed when cycling is available or used."
  :type 'boolean
  :group 'cody)

(defcustom cody-enable-telemetry t
  "Non-nil to allow anonymized event/usage telemetry.
This information is used by Sourcegraph to improve the product."
  :type 'boolean
  :group 'cody)

(defcustom cody--anonymized-uuid nil
  "A generated ID for telemetry, to tie usage events together.
This is generated and cached on first use, if telemetry is enabled."
  :type 'string
  :group 'cody)

(defcustom cody-enable-automatic-completions t
  "Non-nil to have Cody prompt with suggestions automatically.
Completion suggestions will appear after you stop typing for a while,
if any suggestions are available.  Set this to nil if you prefer to
use manual completion triggering with `cody-request-completion'."
  :type 'boolean
  :group 'cody)

;;; Completions "API" for the rest of the lisp code:

(defclass cody-completion-item ()
  ((insertText :initarg :insertText
               :initform nil
               :type (or null string)
               :accessor cody--completion-item-original-text
               :documentation "The original suggested text.")
   (range :initarg :range
          :initform nil
          :type (or null list)
          :accessor cody--completion-item-range
          :documentation "The range where the completion applies.")
   (-displayText :initarg :displayText
                 :initform nil
                 :type (or null string)
                 :accessor cody--display-text
                 :documentation "The updated text as the user types."))
  "Represents a single alternative/suggestion in a completion response.")

(defclass cody-completion ()
  ((items :initarg :items
          :initform nil
          :type (or null vector)
          :accessor cody--completion-items
          :documentation "Vector of `cody-completion-item' instances.")
   (completionEvent :initarg :completionEvent
                    :initform nil
                    :type (or null list)
                    :documentation "Event associated with the completion.")
   (response :initarg :response
             :initform nil
             :type (or null list)
             :accessor cody--completion-response
             :documentation "Top-level jsonrpc protocol response object.")
   (-current-item-index :initform 0
                        :type integer
                        :accessor cody--current-item-index
                        :documentation "Index of currently selected completion alternative."))
  "Represents the entire JSON-RPC response for a requested completion.")

(cl-defmethod cody--num-items ((cc cody-completion))
  "Return the total number of alternative items for the completion CC."
  (length (oref cc items)))

(cl-defmethod cody--multiple-items-p ((cc cody-completion))
  "Return non-nil if the completion CC has two or more completion items."
  (> (cody--num-items cc) 1))

(cl-defmethod cody--current-item ((cc cody-completion))
  "Return the currently displayed variable `cody-completion-item', or nil.
Argument CC is the completion object."
  (ignore-errors
    (aref (cody--completion-items cc) (cody--current-item-index cc))))

(cl-defmethod cody--completion-original-text ((cc cody-completion))
  "Return the original text for the currently displayed item from CC.
Returns nil if it cannot find it for any reason."
  (when-let ((item (cody--current-item cc)))
    (cody--completion-item-original-text item)))

(cl-defmethod cody--completion-display-text ((cc cody-completion))
  "Return the up-to-date text for the currently displayed item from CC.
Returns nil if it cannot find it for any reason."
  (when-let ((item (cody--current-item cc)))
    (oref item -displayText)))

(cl-defmethod cody--completion-text ((cc cody-completion))
  "Retrieve the text of the selected CC item."
  (when-let ((item (cody--current-item cc)))
    (or (cody--display-text item)
        (oref item insertText))))

(cl-defmethod cody-set-completion-event-prop ((cc cody-completion) prop value)
  "Update the completion event in CC, setting PROP to VALUE."
  (oset cc completionEvent
        (plist-put (oref cc completionEvent) prop value)))

(cl-defmethod cody-completion-event-prop ((cc cody-completion) prop)
  "Retrieve PROP from the completion event of CC."
  (plist-get (oref cc completionEvent) prop))

;;; State variables.

(defconst cody--cody-agent
  (file-name-concat (file-name-directory (or load-file-name
                                             (buffer-file-name)))
                    "dist" "cody-agent.js")
  "Path to bundled cody agent.")

(defconst cody--node-min-version "20.4.0"
  "The minimum required version of node.js for Cody.")

(defvar cody--node-version-status nil
  "Non-nil after `cody--check-node-version' is called.
The node version is only checked on Cody startup.
You can call `cody-restart' to force it to re-check the version.")

(defcustom cody-node-executable nil
  "Hardwired path to the nodejs binary to use for Cody.
If nil, Cody will search for node using variable `exec-path'."
  :type 'string
  :group 'cody)

(defcustom cody--node-min-version "20.4.0"
  "The minimum required version of Node.js."
  :type 'string
  :group 'cody)

(defun cody--agent-command ()
  "Command and arguments for running agent."
  (list (or cody-node-executable "node") cody--cody-agent))

(defvar cody--connection nil "Global jsonrpc connection to Agent.")
(defvar cody--message-in-progress nil "Chat message accumulator.")

(defvar cody--sourcegraph-host "sourcegraph.com" "Sourcegraph host.")
(defvar cody--access-token nil "LLM access token for `cody--sourcegraph-host'.")

(defconst cody-log-buffer-name "*cody-log*" "Cody log messages.")
(defconst cody--chat-buffer-name "*cody-chat*" "Cody chat Buffer.")

(defvar cody-prefix-map nil "Map for bindings with Cody's prefix.")
(define-prefix-command 'cody-prefix-map)
(define-key cody-prefix-map (kbd "c") 'cody-request-completion)
(define-key cody-prefix-map (kbd "x") 'cody-mode) ; toggle cody-mode off for buffer

(defvar cody-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c /") cody-prefix-map)
    (define-key map (kbd "M-\\") 'cody-request-completion) ; for IntelliJ users
    (define-key map (kbd "TAB") 'cody--tab-key) ; accept completions
    (define-key map (kbd "C-g") 'cody--ctrl-g-key)
    (define-key map (kbd "ESC ESC ESC") 'cody--ctrl-g-key)
    (when cody-enable-completion-cycling
      (define-key map (kbd "M-n") 'cody-next-completion)
      (define-key map (kbd "M-p") 'cody-prev-completion))
    map)
  "Keymap for `cody-mode'.")

(defvar cody-completion-map (make-sparse-keymap)
  "Keymap for cody completion overlay.")

(defvar cody--use-threads t
  "Non-nil to use threads; set to false for easier debugging.")

(defvar cody--typewriter-effect nil
  "Non-nil to use typewriter effect in Cody chat output.")

(defconst cody--mode-hooks
  '((after-change-functions . cody--after-change)
    (kill-buffer-hook . cody--kill-buffer-function)
    (post-command-hook . cody--post-command-function))
  "List of buffer-local hooks that Cody registers on in `cody-mode'.
These hooks enable it to keep buffers and selections synced
with the Cody Agent.")

(defvar-local cody--overlay-deltas nil
  "List of overlays for Cody current completion suggestion.")

(defvar-local cody--completion nil
  "Most recent completion response object from the Cody Agent.
This is an instance of a variable `cody-completion' object, which see.
Each time we request a new completion, it gets discarded and replaced.")

(defvar cody--completion-timer nil
  "Maybe trigger a completion each time Emacs goes idle.")

(defvar cody--last-completion-trigger-spot nil "Temp variable.")

(defsubst cody--cc ()
  "Return the current buffer-local version of `cody--completion'."
  cody--completion)

(defvar-local cody--completion-timestamps nil
  "Tracks event timestamps for telemetry, as plist properties.")

;; These are for keeping the agent notified about current file/selection.
(defvar-local cody--last-point nil "The last point position.")
(defvar-local cody--last-mark nil "The last mark position.")

(defvar-local cody--update-debounce-timer nil
  "Delay for batching buffer-modification updates to the Agent.")

(defconst cody--debounce-timer-delay 0.2
  "Wait at least this long between notifications of buffer content changes.")

(defface cody-completion-face
  '((t :inherit shadow :slant italic :foreground "#4c8da3" :background "#ffffcd"))
  "Face for Cody completion overlay.")

(defsubst cody--timestamp ()
  "Return seconds since epoch."
  (float-time (current-time)))

;; Add to your ~/.authinfo.gpg something that looks like
;;   machine `cody--sourcegraph-host' login apikey password sgp_SECRET
(defun cody--access-token ()
  "Fetch and cache the access token from ~/.authinfo.gpg."
  (or cody--access-token
      ;; We are looking for an API key, so look for the first entry
      ;; where the secret starts with sgp_
      (setq cody--access-token
            (seq-some (lambda (found)
                        (let ((token (auth-info-password found)))
                          (if (string-prefix-p "sgp_" token) token)))
                      (auth-source-search
                       :max 10
                       :host cody--sourcegraph-host
                       :require '(:secret :host))))))

(defun cody--extension-configuration ()
  "Which `ExtensionConfiguration' parameters to send on Agent handshake."
  (list :accessToken (cody--access-token)
        :serverEndpoint (concat "https://" cody--sourcegraph-host)
        ;; Note there is a bug currently where the agent initialize request
        ;; fails if the serverEndpoint doesn't know the codebase.
        :codebase "https://github.com/sourcegraph/cody"))

(defun cody--request (method &rest params)
  "Helper to send a Cody request for METHOD with PARAMS."
  ;; TODO: Make requests cancellable and implement $/cancelRequest.
  (jsonrpc-request (cody--connection) method params))

(defun cody--alive-p ()
  "Return non-nil if the jsonrpc connection is still running."
  (and cody--connection
       (cody--check-node-version)
       (zerop (process-exit-status (jsonrpc--process cody--connection)))))

(defun cody--connection ()
  "Return the agent process, starting one if it is not already running."
  (unless (cody--alive-p)
    (setq cody--connection
          (make-instance
           'jsonrpc-process-connection
           :name "cody"
           :events-buffer-scrollback-size nil
           :notification-dispatcher #'cody--handle-agent-notification
           :process (make-process
                     :name "cody"
                     :command (cody--agent-command)
                     :coding 'utf-8-emacs-unix
                     :connection-type 'pipe
                     :stderr (get-buffer-create "*cody stderr*")
                     :noquery t)))
    (jsonrpc-request cody--connection 'initialize
                     (list
                      :name "Emacs"
                      :version "0.1"
                      :workspaceRootUri (cody--workspace-root)
                      :extensionConfiguration (cody--extension-configuration)))
    (jsonrpc-notify cody--connection 'initialized nil))
  cody--connection)

(defun cody--check-node-version ()
  "Signal an error if the default node.js version is too low.
Min version is configurable with `cody--node-min-version'."
  (cl-case cody--node-version-status
    (good t)
    (bad (error "Installed nodejs must be at least %s." cody--node-min-version))
    (otherwise
     (let* ((cmd (concat (or cody-node-executable "node") " -v"))
            (node-version (string-trim (shell-command-to-string cmd)))
            minor major patch)
       (if (not (string-match "^v\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"
                              node-version))
           (progn
             (message "Error: Could not parse nodejs version string: %s"
                      node-version)
             nil)
         (setq major (string-to-number (match-string 1 node-version))
               minor (string-to-number (match-string 2 node-version))
               patch (string-to-number (match-string 3 node-version)))
         (let* ((min-version-parts (split-string cody--node-min-version "\\."))
                (min-major (string-to-number (nth 0 min-version-parts)))
                (min-minor (string-to-number (nth 1 min-version-parts)))
                (min-patch (string-to-number (nth 2 min-version-parts))))
           (if (or (> major min-major)
                   (and (= major min-major) (> minor min-minor))
                   (and (= major min-major)
                        (= minor min-minor)
                        (>= patch min-patch)))
               (setq cody--node-version-status 'good)
             (setq cody--node-version-status 'bad)
             (error
              "Error: Installed nodejs version %s is lower than min version %s"
              node-version cody--node-min-version))))))))

(defun cody--workspace-root ()
  "Return the workspace root for the Agent.
You can override it with `cody-workspace-root'."
  (or cody-workspace-root
      (and buffer-file-name (file-name-directory buffer-file-name))
      (getenv "HOME")))

;;; Code for cody minor mode:

(defun cody-logo ()
  "Return the Cody large logo image, memoizing on first call."
  (or (get 'cody-logo 'cached-image)
      (put 'cody-logo 'cached-image
           (create-image (cody-logo-file "cody-logo.png")))))

(defun cody-logo-small ()
  "Return the Cody modeline image, memoizing on first call."
  (or (get 'cody-logo-small 'cached-image)
      (put 'cody-logo-small 'cached-image
           (create-image (cody-logo-file "cody-logo-small.png")))))

(defun cody-logo-file (file-base)
  "Construct path to bundled cody image file.
Argument FILE-BASE is the file base name sans directory."
  (file-name-concat ; hack the Cody logo path
   ;; wtf emacs why is there no parent-directory function?
   (file-name-directory (directory-file-name
                         (file-name-directory cody--cody-agent)))
   file-base))

(defvar cody-mode-menu)

(defun cody--mode-line-click (_event)
  "Handle mouse click EVENT on Cody mode line item."
  (interactive "e")
  (popup-menu cody-mode-menu))

(defvar cody-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'cody--mode-line-click)
    (easy-menu-define cody-mode-menu map "Cody Mode Menu"
      '("Cody"
        ["Say Hello" (message "Hello from Cody!")]
        ["Turn Off" cody-mode]
        ["Help" (describe-function 'cody-mode)]))
    map)
  "Keymap for Cody mode line button.")

(defun cody-propertize-icon (text-or-image)
  "Return propertized string or image for `cody--minor-mode-icon`.
Argument TEXT-OR-IMAGE is the string or image to propertize."
  (propertize (if (stringp text-or-image) text-or-image " ")
              'display (if (stringp text-or-image) nil text-or-image)
              'help-echo "Cody mode - click for menu"
              'mouse-face 'mode-line-highlight
              'keymap cody-mode-line-map))

(defun cody--decorate-mode-line-lighter (image)
  "Use the passed IMAGE for the mode line lighter."
  (if-let ((img (and (display-graphic-p) image)))
      ;; Hack - bump the image up a bit vertically using :ascent, to center it.
      (cody-propertize-icon (cons 'image (plist-put (cdr img) :ascent 80)))
    (cody-propertize-icon " Cody")))

(defvar cody--minor-mode-icon
  (cody--decorate-mode-line-lighter (cody-logo-small))
  "Mode line lighter for Cody minor-mode.")

(put 'cody--minor-mode-icon 'risky-local-variable t)

;;;###autoload
(define-minor-mode cody-mode
  "Minor mode for interacting with the Cody coding assistant.
Changes to the buffer will be tracked by the Cody agent"
  ;; Never did figure out how to get this option to display a custom menu.
  ;; Currently going with a hack that puts it up at a higher level in the
  ;; mode line, but does produce a custom menu when mouse-1 clicked.
  ;; Bounty to anyone who can figure out how to do it correctly.
  ;;
  ;;:lighter cody--minor-mode-icon
  :keymap cody-mode-map
  (if cody-mode
      (cody--minor-mode-startup)
    (cody--minor-mode-shutdown)))

(defvar-local cody--mode-line-icon-evaluator
    '(:eval (when cody-mode cody--minor-mode-icon))
  "Descriptor for producing a custom menu in the mode line lighter.")

(defun cody--minor-mode-startup ()
  "Code to run when `cody-mode' is enabled in a buffer."
  (cl-loop for (hook . func) in cody--mode-hooks
           do (add-hook hook func nil t))
  (cody--sync-buffer-to-agent 'textDocument/didOpen)
  (add-to-list 'mode-line-modes cody--mode-line-icon-evaluator)
  (force-mode-line-update t)
  (message "Cody mode enabled"))

(defun cody--minor-mode-shutdown ()
  "Code to run when `code-mode' is disabled in a buffer."
  (cody--discard-completion)
  (cl-loop for (hook . func) in cody--mode-hooks
           do (remove-hook hook func t))
  (setq cody-mode nil) ; this clears the modeline and other vars
  (cody--cancel-completion-timer)
  (message "Cody mode disabled"))

(defun cody--text-file-p (buf)
  "Return non-nil if BUF is visiting a text file.
A heuristic to see if we should notify the agent about it."
  ;; TODO: Maybe just ask the filesystem if it's a text file, instead?
  (and (buffer-file-name buf)
       (not
        (memq (with-current-buffer buf buffer-file-coding-system)
              '(nil raw-text binary no-conversion)))))

(defun cody--maybe-turn-on-cody-mode ()
  "Maybe enable `cody-mode' on a newly opened file.
Current buffer is visiting the file."
  (when (cody--enable-for-buffer-p)
    (cody--sync-buffer-to-agent 'textDocument/didOpen)
    (cody-mode)))

(defun cody--enable-for-buffer-p ()
  "Return non-nil if Cody should be enabled for current buffer.
Currently it means the buffer is visiting a text file on disk."
  (and (cody--alive-p)
       ;; TODO: Some way to customize how cody-mode is chosen for a buffer.
       (cody--text-file-p (current-buffer))))

;;; Code for change notifications and syncing with Agent.

(defun cody--after-change (&rest _)
  "Implement the `textDocument/didChange' notification.
Installed on `after-change-functions' buffer-local hook in `cody-mode'."
  (unless cody--update-debounce-timer
    (setq cody--update-debounce-timer
          (run-with-idle-timer cody--debounce-timer-delay nil
                               #'cody--flush-pending-changes))))

(defun cody--cancel-debounce-timer ()
  "Cancel debounce timer, returning non-nil if a timer was cancelled."
  (when cody--update-debounce-timer
    (cancel-timer cody--update-debounce-timer)
    (setq cody--update-debounce-timer nil)
    'cancelled))

(defun cody--flush-pending-changes ()
  "If there is pending data, send it to the agent."
  (when-let ((had-pending-changes-p (cody--cancel-debounce-timer)))
    (cois the protocol operation, e.g. `textDocument/didChange'.dy--sync-buffer-to-agent 'textDocument/didChange)))

(defun cody--sync-buffer-to-agent (operation)
  "Send the full file contents to the agent with OPERATION."
  (cody--send-file-to-agent (current-buffer) operation))

(defun cody--send-file-to-agent (buf op)
  "Make the jsonrpc call to notify agent of opened/changed file.
Argument BUF is the buffer whose contents will be sent.
Argument OP is the protocol operation, e.g. `textDocument/didChange'."
  (condition-case err
      (when cody-mode
        (let ((happy nil))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (jsonrpc-notify
                   (cody--connection) op
                   (list
                    :filePath (buffer-file-name buf)
                    :content (buffer-substring-no-properties (point-min)
                                                             (point-max))
                    :selection (cody--selection buf)))))
            (setq happy t))
          (unless happy
            (cody--log "Unable to update Cody agent for %s" buffer-file-name))))
    (error (cody--log "Error sending file %s: %s" buffer-file-name err))))

(defun cody--post-command-function ()
  "If point or mark has moved, update selection/focus with agent.
Installed on `post-command-hook', which see."
  (ignore-errors
    (when cody-mode
      (cody--debuggable-post-command))))

(defun cody--debuggable-post-command ()
  "Set your breakpoint for `cody--post-command-function` here instead."
  (ignore-errors
    (cody--maybe-clear-completion))
  (cody--handle-typing-in-completion)
  (cody--notify-if-focus-changed))

(defun cody--handle-typing-in-completion ()
  "If the user is typing within the completion, keep it in sync."
  (when-let ((o (cody--point-at-overlay-p)))
    (cody--shorten-completion o)))

(defun cody--notify-if-focus-changed ()
  "Check whether the Agent should be notified of a focus/selection change."
  (let ((point-unequal (not (eq cody--last-point (point))))
        ;; If the mark isn't set (nil), we pretend it is set at point,
        ;; yielding a zero-width range for the current selection.
        (mark-unequal (not (eq cody--last-mark (or (mark) (point))))))
    (if point-unequal
        (setq cody--last-point (point)))
    (if mark-unequal
        (setq cody--last-mark (or (mark) (point))))
    (if (or point-unequal mark-unequal)
        (cody--handle-focus-change point-unequal))))

(defun cody--handle-focus-change (cursor-moved-p)
  "Notify agent that cursor or selection has changed in current buffer.
If CURSOR-MOVED-P then we may also trigger a completion timer."
  ;; Does not send the file contents. You can leave it undefined when
  ;; you're just updating the selection or caret.
  (when cody-mode
    (jsonrpc-notify (cody--connection) 'textDocument/didFocus
                    (list
                     :filePath (buffer-file-name (current-buffer))
                     ;; Specifically leave :content undefined here.
                     :selection (cody--selection (current-buffer))))
    ;; Maybe set a timer to trigger an automatic completion.
    ;; Do some trivial rejects here before setting the timer.
    (when (and cursor-moved-p
               (cody--buffer-active-p)
               (not (cody--overlay-visible-p)))
      (cody--start-completion-timer))))

(defun cody--start-completion-timer ()
  "Set a cancellable timer to check for an automatic completion."
  (setq cody--completion-timer
        (run-with-idle-timer 0 nil #'cody--maybe-trigger-completion)))

(defun cody--cancel-completion-timer ()
  "Cancel any pending timer to check for automatic completions."
  (when cody--completion-timer
    (cancel-timer cody--completion-timer)
    (setq cody--completion-timer nil)))

(defun cody--selection (buf)
  "Return the jsonrpc parameters representing the selection in BUF.
BUF can be a buffer or buffer name, and we return a Range with `start'
and `end' parameters, each a Position of 1-indexed `line' and `character'.
The return value is appropiate for sending directly to the rpc layer."
  (cl-flet ((pos-parameters (pos)
              (list :line (line-number-at-pos pos)
                    :character (save-excursion
                                 (goto-char pos)
                                 (1+ (current-column))))))
    (with-current-buffer buf
      (let* ((mark (if mark-active (mark) (point)))
             (point (point))
             (beg (min mark point))
             (end (max mark point))
             (beg-pos (pos-parameters beg))
             (end-pos (pos-parameters end)))
        (list :start beg-pos :end end-pos)))))

;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/JSONRPC-Overview.html
;; for a description of the parameters for this jsonrpc notification callback.
(defun cody--handle-agent-notification (_ method params)
  "Handle notifications from the agent, e.g. shutdown."
  (cl-case method
    (chat/updateMessageInProgress
     (cody--handle-chat-update params))
    (shutdown ; Server initiated shutdown.
     (cody-logout))))

(defun cody--kill-buffer-function ()
  "If we are killing the last buffer visiting this file, notify agent."
  (let ((bufname buffer-file-name))
    (condition-case err
        (when (and cody-mode
                   bufname
                   (cody--last-buffer-for-file-p))
          (jsonrpc-notify (cody--connection) 'textDocument/didClose
                          (list :filePath bufname))
          (cody--log "Notified agent closed %s" bufname))
      (error "Error notifying agent closing %s" bufname))))

(defun cody--last-buffer-for-file-p ()
  "Check if the current buffer is the last one visiting its file.
It returns non-nil if the buffer being killed is the last one
visiting its associated file."
  (when-let ((current-file (buffer-file-name))
             (current-buf (current-buffer)))
    (not (cl-some (lambda (buf)
                    (and (not (eq buf current-buf))
                         (string-equal current-file (buffer-file-name buf))))
                  (buffer-list)))))

(defun cody--handle-chat-update (params)
  "Handler for `chat/updateMessageInProgress'."
  (if cody--typewriter-effect
      (progn
        (message nil)
        (if params
            (cody--chat-insert-msg-tail params)
          (cody--chat-insert "\n")))
    (if params
        ;; Replace the accumulator with the updated generated output.
        (setq cody--message-in-progress params)
      ;; Null params --> Message is complete.
      (message nil) ; clear 'Awaiting' message
      (cody--chat-insert
       (plist-get cody--message-in-progress :text) "\n\n")
      (setq cody--message-in-progress nil))))

(defun cody--chat-insert (&rest args)
  "Insert ARGS at the end of the Cody chat buffer."
  (ignore-errors ; don't throw errors in process filter
    (with-current-buffer (cody--chat-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (when args (apply #'insert args))
        (set-buffer-modified-p nil)
        (cody--chat-scroll-to-bottom)))))

(defun cody--chat-scroll-to-bottom ()
  "Move cursor to bottom of chat buffer."
  ;; If the buffer is visible in a window, then the point doesn't move
  ;; even after we insert text, as windows keep their own copy of point.
  ;; We want it to act more like a shell.
  (with-current-buffer (cody--chat-buffer)
    (let ((win (get-buffer-window)))
      (when (window-live-p win)
        (set-window-point win (point-max))))))

;; The agent sends an update with increasingly long hunks of the response,
;; e.g. "Here", "Here is", "Here is an", "Here is an explanation", ...
;; This permits a typewriter effect.
(defun cody--chat-insert-msg-tail (params)
  "Insert only the most recent update to the message output.
This allows you to see the output stream in as it is generated.
Cody chat buffer should be current, and PARAMS non-nil."
  (let* ((old-text cody--message-in-progress)
         (new-text (plist-get params :text))
         (tail
          (cond
           ((null old-text) new-text)
           ((>= (length new-text) (length old-text))
            (substring new-text (length old-text)))
           (t
            ;; This could happen if the generator "backs up", which I've
            ;; seen happen with ChatGPT, so we'll likely need to handle
            ;; this at some point.
            ""))))
    (let ((inhibit-read-only t))
      (insert tail))
    (setq cody--message-in-progress new-text)))

(defun cody-logout ()
  "Stop the Cody agent process and turn Cody off globally."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when cody-mode
        (cody--minor-mode-shutdown))))
  (remove-hook 'find-file-hook #'cody--maybe-turn-on-cody-mode)
  (when (cody--alive-p)
    (ignore-errors
      (cody--request 'shutdown)) ; Required by the protocol
    (ignore-errors
      (cody--kill-process))
    (ignore-errors ; sometimes jsonrpc doesn't clean this one up
      (kill-buffer (get-buffer "*cody events*")))
    (message "Cody has shut down."))
  (setq cody--message-in-progress nil
        ;; Force re-check of node version on Cody startup.
        cody--node-version-status nil))

(defun cody-force-unload ()
  "Shut down Cody and remove all Cody-related buffers."
  (interactive)
  (cody-logout)
  (ignore-errors
    (kill-buffer (cody--chat-buffer)))
  (ignore-errors
    (kill-buffer cody-log-buffer-name)))

(defun cody--kill-process ()
  "Shut down the jsonrpc connection."
  (when cody--connection
    (ignore-errors
      (jsonrpc-shutdown cody--connection 'cleanup-buffers))
    (setq cody--connection nil)))

(defun cody ()
  "Prompt for a recipe and arguments."
  (interactive)
  (let* ((recipes (cody--request 'recipes/list))
         (recipe (completing-read "recipe: "
                                  (seq-map (lambda (elt)
                                             (plist-get elt :id))
                                           recipes)
                                  nil
                                  t))
         (chat (read-from-minibuffer (concat recipe ": "))))
    (cody--request 'recipes/execute
                   :id recipe
                   :humanChatInput chat))
  (message "Cody recipe sent."))

;;;###autoload
(defun cody-login (&optional quiet)
  "Start the Cody agent. Optionally enables `cody-mode' in buffers.
This function is idempotent and only starts a new connection if needed.
Turning on `cody-mode' is set by the `cody-auto-enable-cody-mode'
customization option. This function essentially starts up the Cody
system, and you can call it from any Cody command wrapper to ensure
there is a connection."
  (interactive)
  (if (cody--alive-p)
      (unless quiet
        (message "Cody agent is already started."))
    (setq cody--node-version-status nil) ; re-check node version on start
    (message "Initializing Cody connection...")
    (cody--connection)
    (message "Cody connection initialized.")))

(defun cody-restart ()
  "Shut down and restart Cody. Mostly for debugging."
  (interactive)
  (let ((cody--node-version-status 'good))
    (ignore-errors
      (cody-logout)))
  (setq cody--node-version-status nil)
  (cody-login))

(defun cody-unload-function ()
  "Handle `unload-feature' for this package."
  (cody-logout))

(defun cody--log (msg &rest args)
  "Log MSG with ARGS, currently just for debugging."
  (with-current-buffer (get-buffer-create cody-log-buffer-name)
    (goto-char (point-max))
    (insert (apply #'format msg args) "\n")))

;;;###autoload
(defun cody-chat ()
  "Shorthand for the chat recipe.
Query and output go into the *cody-chat* buffer."
  (interactive)
  (cody-login)
  (display-buffer (cody--chat-buffer))
  (let ((query (read-from-minibuffer "Ask Cody: ")))
    (cody--chat-insert-query query)
    (cody--request 'recipes/execute
                   :id "chat-question"
                   :humanChatInput query))
  (message "Awaiting response from Cody..."))

(defun cody--chat-insert-query (query)
  "Insert the user QUERY into the chat output buffer."
  (with-current-buffer (cody--chat-buffer)
    (goto-char (point-max))
    (cody--chat-insert "> " query "\n\n")
    (goto-char (point-max))))

(defun cody--chat-buffer ()
  "Return Cody chat output buffer, initializing if necessary."
  (let* ((probe (get-buffer cody--chat-buffer-name)))
    (if (buffer-live-p probe)
        probe
      ;; Create and initialize the chat buffer.
      (with-current-buffer (get-buffer-create cody--chat-buffer-name)
        (buffer-disable-undo)
        (let ((inhibit-modification-hooks t))
          (insert-image (cody-logo) "Cody")
          (insert "Welcome to Cody. Type `M-x cody-help` for more info.\n")
          (set-buffer-modified-p nil))
        (current-buffer)))))

(defun cody--overlay-visible-p ()
  "Return non-nil if Cody is displaying a suggestion in the current buffer."
  (when-let ((o (car-safe cody--overlay-deltas)))
    (and (overlayp o)
         (overlay-buffer o) ; overlay is positioned somewhere
         (cody--cc))))

(defun cody--make-overlay (text pos)
  "Create a new overlay for displaying part of a completion suggestion.
Gives it TEXT as the `after-string' and sets it at POS.
Pushes the new overlay onto the front of `cody--overlay-deltas'."
  (save-excursion
    (let ((o (make-overlay pos pos nil nil t)))
      (overlay-put o 'cody t)
      (overlay-put o 'keymap cody-completion-map)
      (overlay-put o 'priority '(nil . 50))
      (overlay-put o 'help-echo "TAB to accept")
      (overlay-put o 'insert-in-front-hooks '(cody--overlay-insert-front))
      (cody--overlay-set-text o text)
      (push o cody--overlay-deltas)
      o)))

(defun cody--overlay-set-text (overlay text)
  "Update TEXT for OVERLAY."
  (let ((suggestion (propertize text 'face 'cody-completion-face)))
    (put-text-property 0 1 'cursor t suggestion)
    (overlay-put overlay 'after-string suggestion)))

(defsubst cody--overlay-get-text (overlay)
  "Return the text of the specified OVERLAY."
  (overlay-get overlay 'after-string))

(defun cody--point-at-overlay-p (&optional fuzzy)
  "Return the cody completion fragment overlay at point, if any.
If FUZZY is non-nil, returns the overlay if there is only whitespace
between point and the start of the completion. Returns nil if point
is not currently at the start of a completion suggestion delta."
  (when (and cody-mode (cody--overlay-visible-p))
    (cl-loop for o in cody--overlay-deltas
             when (or (and fuzzy
                           (>= (overlay-start o) (point))
                           (string-match-p "\\`[[:space:]]*\\'"
                                           (buffer-substring-no-properties
                                            (point) (overlay-start o))))
                      (and (not fuzzy) ; strict
                           (= (overlay-start o) (point))))
             return o)))

(defmacro cody--call-if-at-overlay (func key &optional fuzzy)
  "If point is at the start of the completion overlay, invoke FUNC.
Otherwise, call the default non-Cody key binding for KEY.
If FUZZY is non-nil, then the check ignores whitespace between point
and the start of the overlay."
  `(if (cody--point-at-overlay-p ,fuzzy)
       (funcall ,func)
     (let (cody-mode) ; avoid recursion
       (call-interactively (key-binding (kbd ,key))))))

(defun cody--execute-default-keybinding ()
  "Execute default command for the key sequence invoking current command."
  (when-let ((cmd (lookup-key global-map (this-command-keys))))
    (call-interactively cmd)))

(defun cody--tab-key ()
  "Handler for TAB key."
  (interactive)
  (cody--call-if-at-overlay 'cody--accept-completion "TAB"))

(defun cody--ctrl-g-key ()
  "Handler for quit; clears/rejects the completion suggestion."
  (interactive)
  (cody--call-if-at-overlay 'cody--discard-completion "C-g" 'fuzzy))

(defun cody--maybe-trigger-completion ()
  "Under the right conditions, trigger an automatic completion request."
  (when (and cody-mode
             cody-enable-automatic-completions
             (not (cody--overlay-visible-p))
             (not mark-active)
             (cody--completion-syntactically-eligible-p))
    (cody-request-completion)))

(defun cody--keyboard-event-p (event)
  "Return t if EVENT is a keyboard event."
  (and (symbolp event)
       (not (mouse-event-p event))))

(defun cody--mouse-click-event-p (event)
  "Return non-nil if EVENT is a mouse click event."
  (and (mouse-event-p event)
       (memq (event-basic-type event) '(mouse-1 mouse-2 mouse-3))))

(defun cody--completion-syntactically-eligible-p ()
  "Return non-nil if this is a valid location for a completion trigger.
Does syntactic smoke screens before requesting completion from Agent."
  (not (or
        ;; user is in the middle of a word (from jetbrains cody client)
        (looking-back "\\s*[A-Za-z]+" (line-beginning-position))
        ;; suffix of the current line contains any word characters
        (looking-at ".*\\w.*"))))

(defun cody-request-completion ()
  "Request manual autocompletion in current buffer at point."
  (interactive)
  (unless cody-mode
    (error "Cody-mode not enabled in this buffer."))
  (let* ((buf (current-buffer))
         (file (buffer-file-name buf))
         (line (1- (line-number-at-pos)))
         (col (current-column))
         (cursor (point))
         (trigger-kind (if (called-interactively-p 'interactive)
                           "Invoke" "Automatic")))
    (when (and (not buffer-read-only)
               (or (string= trigger-kind "Invoke")
                   ;; Avoid spamming if the cursor hasn't moved.
                   (not (eql cursor cody--last-completion-trigger-spot))))
      (cody--discard-completion) ; Clears telemetry from previous request.
      (cody--flush-pending-changes)
      (cody--update-completion-timestamp :triggeredAt)
      (setq cody--last-completion-trigger-spot cursor)
      (jsonrpc-async-request
       (cody--connection) 'autocomplete/execute
       (list :filePath file
             :position (list :line line :character col)
             :triggerKind trigger-kind)
       :deferred 'cody  ; have new requests replace pending ones
       :success-fn (lambda (response)
                     (cody--handle-completion-result
                      response buf cursor trigger-kind))
       :error-fn (lambda (err) (cody--log "Error requesting completion: %s" err))
       :timeout-fn (lambda () (cody--log "Error: request-completion timed out"))))))

(defun cody--handle-completion-result (response buf request-spot kind)
  "Dispatches completion result based on jsonrpc RESPONSE.
BUF and REQUEST-SPOT specify where the request was initiated.
KIND specifies whether this was requested manually or automatically"
  (when (cody--buffer-active-p buf)
    (with-current-buffer buf
      (let ((items (plist-get response :items))
            (manual (equal kind "Invoke")))
        (cond
         ((not (eql (point) request-spot)) nil) ; Drop event if point has moved.
         ((not (vectorp items))
          (let ((msg (format "Unexpected response format: %s" response)))
            (cody--log msg)
            (when manual (message msg))))
         ((zerop (length items))
          (when manual
            (message "No completions returned")))
         (t
          (let (cody--completion-timestamps) ; preserve the trigger time from request
            (ignore-errors (cody--discard-completion))
            (setq cody--completion (cody--populate-from-response response)))
          (cody--update-completion-timestamp :displayedAt)
          (condition-case err
              (cody--display-completion 0)
            (error
             (cody--log "Error displaying completion: %s" err)))))))))

(defun cody--populate-from-response (response)
  "Parse RESPONSE and return a populated `cody-completion' object."
  (make-instance
   'cody-completion
   :items (vconcat
           (mapcar (lambda (item)
                     (make-instance 'cody-completion-item
                                    :insertText (plist-get item :insertText)
                                    :range (plist-get item :range)))
                   (plist-get response :items)))
   :response response
   :completionEvent (plist-get response :completionEvent)))

(defsubst cody--key-for-command (command &optional keymap)
  "Get user-visible key sequence for COMMAND."
  (when-let ((keys (where-is-internal command keymap)))
    (key-description (car keys))))

(defun cody--position-to-point (pos)
  "Convert Cody Agent line/char position to a buffer position."
  (save-excursion
    (goto-char (point-min))
    (forward-line (plist-get pos :line))
    (forward-char (plist-get pos :character))
    (point)))

(defsubst cody--range-start (range)
  "Fetches START from RANGE, a `cody--completion-item-range' object.
Converts from line/char to buffer positions."
  (cody--position-to-point (plist-get range :start)))

(defsubst cody--range-end (range)
  "Fetches END from RANGE, a `cody--completion-item-range' object.
Converts from line/char to buffer positions."
  (cody--position-to-point (plist-get range :end)))

(defun cody--display-completion (index)
  "Show the server's code autocompletion suggestion.
RESPONSE is the entire jsonrpc response.
INDEX is the completion alternative to display from RESPONSE."
  (when-let*
      ((cc (cody--cc))
       (item (cody--current-item cc))
       (range (cody--completion-item-range item))
       (range-start (cody--range-start range))
       (original-text (buffer-substring-no-properties
                       range-start (cody--range-end range)))
       (insert-text (let ((text (cody--completion-text cc)))
                      (cond
                       ((null text) nil)
                       ((string-match-p "[[:graph:]]" text) text)
                       ;; Bail if suggestion is all-whitespace; the team
                       ;; says this is a bug.  If it repros reliably,
                       ;; they have asked that you let them know.
                       (t (error "Completion text is blank: %s" text)))))
       (lines (split-string insert-text "\r?\n"))
       ;; We treat the 1st line specially: the completion may rewrite it.
       (insert-text-1st-line (car-safe lines))
       ;; We treat the 2nd and following lines as a single unit of text.
       (multiline-insert-text (mapconcat #'identity (cdr lines) "\n"))
       ;; Skip completions that need to delete or change characters in the
       ;; existing document. Harder to support, and most clients don't.
       (only-inserts
        (cl-loop for chunk in (cody-diff-strings original-text
                                                 insert-text-1st-line)
                 never (eq (car-safe chunk) '-)))
       ;; Run Myers diff between the existing text in the document and the
       ;; first line of the `insertText` that is returned from the agent.
       ;; The diff algorithm returns a list of "deltas" that give us the
       ;; minimal number of additions we need to make to the buffer text.
       ;; Insertions are a list of the form '((buffer-pos . text) ...)
       (inserts (cody-diff-strings-with-positions original-text
                                                  insert-text-1st-line
                                                  range-start)))
    ;; TODO: log completion suggested with item's id
    (condition-case err
        ;; Add one overlay span per delta in the first line.
        (cl-loop
         for (pos . text) in inserts
         for rest = (when (> (length multiline-insert-text) 0)
                      multiline-insert-text)
         do (cody--make-overlay text pos)
         finally do
         ;; Add Cody marker to the end of the first line.
         (cody--add-completion-marker (car-safe (last cody--overlay-deltas)))
         ;; Insert following lines, if any, as a single block.
         (when rest (cody--make-overlay rest pos))
         (setf (cody--current-item-index cc) index)
         (when (and cody-enable-completion-cycling-help
                    (cody--multiple-items-p cc))
           (message "Showing suggestion %s of %s (%s/%s to cycle)"
                    (1+ index)
                    (cody--num-items cc)
                    (cody--key-for-command #'cody-next-completion)
                    (cody--key-for-command #'cody-prev-completion))))
      (error
       (cody--log "Error setting completion text: %s" err)
       (cody--hide-completion)))))

(defun cody--add-completion-marker (ovl)
  "Put a Cody symbol at the end of overlay OVL."
  (condition-case err
      (when (and t (overlayp ovl))
        (overlay-put ovl 'after-string
                     (concat (or (overlay-get ovl 'after-string) "")
                             (propertize " " 'display (cody-logo-small)))))
    (error (cody--log "Error showing completion marker: %s" err))))

(defun cody--overlay-insert-front (ovl after-p beg _end &optional len)
  "Allow user to type over the overlay character by character."
  (when (and after-p
             (cody--overlay-visible-p)
             (numberp len)
             (zerop len)) ; Length 0 means it is an insertion.
    (when-let ((text (cody--overlay-get-text ovl))
               (first (string-to-char text))
               (typed (char-after beg))) ; char they just typed
      (cond
       ;; TODO: Handle backspace.
       ((/= first typed) ; typed something different
        ;; Allow them to type whitespace and push the suggestion over
        (unless (eq (char-syntax typed) ?\s)
          (cody--hide-completion))) ; hide if not whitespace
       ((= 1 (length text))
        (cody--accept-completion)) ; fully consumed
       (t
        (cody--shorten-completion ovl))))))

(defun cody--shorten-completion (overlay)
  "Consume the first character of the current completion.
OVERLAY is the relevant completion fragment."
  (cody--overlay-set-text overlay
                          (substring
                           (cody--overlay-get-text overlay) 1)))

(defun cody--hide-completion ()
  "Stop showing the completion suggestion."
  (mapc #'delete-overlay cody--overlay-deltas)
  (setq cody--overlay-deltas nil)
  (when cody--completion
    (setf (cody--current-item-index (cody--cc)) 0)))

(defun cody--maybe-clear-completion ()
  "Implements `post-command-hook' to clear completion if applicable."
  ;; For now, clear it if we're no longer at or before overlay start.
  (unless (cody--point-at-overlay-p 'fuzzy)
    (cody--hide-completion)))

(defun cody--graphql-log-event (event)
  "Send a `telemetry/recordEvent' to the agent.
EVENT is a Sourcegraph GraphQL event."
  ;; TODO: Need to switch this to the new 'telemetry/recordEvent'
  (condition-case err
      (jsonrpc-notify (cody--connection) 'telemetry/recordEvent
                      (list :publicArgument event))
    (error (cody--log "Error logging graphql event: %s" err))))

(defun cody--accept-completion ()
  (let* ((cc (cody--cc))
         (item (cody--current-item cc))
         (text (cody--completion-item-original-text item))
         (range (cody--completion-item-range item))
         (range-start (cody--range-start range))
         (range-end (cody--range-end range)))
    (when (and range-start range-end text)
      (undo-boundary)
      (delete-region range-start range-end)
      (goto-char range-start)
      (insert text)
      (indent-region range-start (point)))
    (cody--telemetry-completion-accepted)
    (jsonrpc-notify cody--connection 'autocomplete/clearLastCandidate nil)
    (cody--discard-completion)))

(defun cody--discard-completion ()
  "Discard/reset the current completion overlay and suggestion data.
Sends telemetry notifications when telemetry is enabled."
  (cody--cancel-completion-timer)
  (unless (eq (cody--completion-status) 'triggered)
    (cody--telemetry-completion-suggested))
  (cody--hide-completion)
  (setq cody--completion nil
        cody--completion-timestamps nil
        ;; Just to be clear, don't change this or the completion will resurrect.
        cody--last-completion-trigger-spot cody--last-completion-trigger-spot
        cody--last-point nil
        cody--last-mark nil
        cody--update-debounce-timer nil))

(defun cody--key-msg-for-command (command)
  "Return message about the key to press for a given COMMAND."
  (if-let ((key (cody--key-for-command command)))
      (format "%s to accept" key)
    "No key is bound to accept"))

(defun cody--check-cycle-preconditions ()
  "Return non-nil if we fail the preconditions for cycling.
Returns nil if we meet all the preconditions. If not, then it
handles logging and messaging."
  (let ((verbose (or cody-enable-completion-cycling-help
                     (and (called-interactively-p 'interactive)
                          (memq this-command
                                '(cody-next-completion cody-prev-completion)))))
        (cc (cody--cc)))
    (cl-labels ((explain (msg &rest args)
                  (let ((output (apply #'message msg args)))
                    (if verbose (message output)
                      (cody--log output))
                    'fail))) ; return non-nil to signal failed check
      (cond
       ((not cody-enable-completion-cycling)
        (explain "Set `cody-enable-completion-cycling' to enable cycling."))
       ((not (and (cody--cc) (cody--current-item (cody--cc))))
        (explain "No completion here"))
       ((not (cody--multiple-items-p cc))
        (explain "No other suggestions; %s"
                 (cody--key-msg-for-command 'cody--tab-key)))
       ;; Disallow cycling if they have typed into the completion prefix.
       ((not (string= (cody--completion-display-text cc)
                      (cody--completion-original-text cc)))
        (explain "Cycling currently unavailable."))
       (t nil))))) ; cycling is available

(defun cody--cycle-completion (direction)
  "Cycle through the completion items in the specified DIRECTION.
DIRECTION should be 1 for next, and -1 for previous."
  (unless (cody--check-cycle-preconditions)
    (if-let* ((cc (cody--cc))
              (items (cody--completion-items cc))
              (index (cody--current-item-index cc))
              (num (cody--num-items cc))
              (next (% (+ index direction num) num)))
        (condition-case err
            (cody--display-completion next)
          (error
           (cody--log "Error displaying completion: %s \ncompletion: %s" err cc)))
      (when cody-enable-completion-cycling-help
        (message "Error cycling through completions"))
      (cody--log "Error cycling through completions: items= %s" items))))

(defun cody-next-completion ()
  "Move to the next completion alternative."
  (interactive)
  (if (and cody-enable-completion-cycling
           (cody--point-at-overlay-p))
      (cody--cycle-completion 1)
    (cody--execute-default-keybinding)))

(defun cody-prev-completion ()
  "Move to the previous completion alternative."
  (interactive)
  (if (and cody-enable-completion-cycling
           (cody--point-at-overlay-p))
      (cody--cycle-completion -1)
    (cody--execute-default-keybinding)))

(defun cody-dashboard ()
  "Show a console with data about Cody configuration and usage."
  (interactive)
  ;; TODO: Agent should collect stats and provide them for us.
  (let ((buf (get-buffer-create "*cody-dashboard*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (erase-buffer)
        (if (cody--alive-p)
            (insert "Cody is connected")
          (insert "Cody is not connected"))))
    (pop-to-buffer buf)))

(defun cody-doctor ()
  "Diagnose and troubleshoot Cody issues."
  (interactive)
  ;; Obviously lots more to do here.
  (if (cody--alive-p)
      (message "Cody is running! Try `M-x cody-chat' to get started.")
    (message "Cody is not running. 'M-x cody-login' to start hacking.")))

;;;==== Telemetry ==============================================================

;; TODO: Agent has a new API that theoretically greatly simplifies all this.

(defun cody--update-completion-timestamp (property)
  "Set the given timestamp named PROPERTY to the current time."
  (setq cody--completion-timestamps
        (plist-put cody--completion-timestamps property (cody--timestamp))))

(defun cody--completion-timestamp (property)
  "Return the given completion timestamp named PROPERTY."
  (or (plist-get cody--completion-timestamps property) 0.0))

(defun cody--completion-status ()
  "Return `triggered', `displayed', `hidden', or nil."
  (cond
   ((null cody--completion-timestamps) nil)
   ((zerop (or (cody--completion-timestamp :displayedAt) 0))
    'triggered)
   ((zerop (or (cody--completion-timestamp :hiddenAt) 0))
    'displayed)
   (t 'hidden)))

(defun cody--anonymized-uuid ()
  "Return, generating if needed, `cody--anonymized-uuid'."
  (or cody--anonymized-uuid
      (setq cody--anonymized-uuid (uuidgen-4))
      (custom-save-all)
      cody--anonymized-uuid))

(defun cody--telemetry-completion-accepted ()
  "Notify the telemetry collector that a completion was accepted."
  (when cody-enable-telemetry
    (when-let ((cc (cody--cc)))
      (cody-set-completion-event-prop cc :acceptedAt (cody--timestamp))
      (condition-case err
          (jsonrpc-notify
           (cody--connection) 'telemetry/recordEvent
           (cody--create-graphql-event "CodyEmacsPlugin:completion:accepted"
                                       (cody--add-completion-event-params
                                        nil (cody-completion-event-prop cc :params))))
        (error (cody--log "Telemetry error in completion:accepted: %s" err))))))

(defun cody--telemetry-completion-suggested ()
  (when cody-enable-telemetry
    (when-let* ((cc (cody--cc))
                (latency (max (- (cody--completion-timestamp :displayedAt)
                                 (cody--completion-timestamp :triggeredAt)) 0))
                (duration (max (- (cody--timestamp) ; :hiddenAt
                                  (cody--completion-timestamp :displayedAt)) 0))
                (params (cody-completion-event-prop cc :params)))
      (condition-case err
          (jsonrpc-notify
           (cody--connection) 'telemetry/recordEvent
           (cody--create-graphql-event
            "CodyEmacsPlugin:completion:suggested"
            (cody--add-completion-event-params
             (list
              :latency latency
              :displayDuration duration
              :isAnyKnownPluginEnabled (cody--other-plugins-enabled-p))
             params)))
        (error (cody--log "Telemetry error in completion:suggested: %s" err))))))

(defun cody--add-completion-event-params (event-params &optional params)
  "Add common completion event telemetry to the passed params.
Both parameters are plists representing json objects."
  (let ((updated (copy-sequence event-params))
        (properties (list :id :languageId :source :charCount
                          :lineCount :multilineMode :providerIdentifier)))
    (when-let ((summary (plist-get params :contextSummary)))
      (setq updated (plist-put updated :contextSummary summary)))
    (dolist (prop properties)
      (setq updated (plist-put updated prop (plist-get params prop))))
    updated))

(defun cody--other-plugins-enabled-p ()
  "Return non-nil if another coding assistant is active concurrently."
  (let ((features-to-check '(copilot tabnine)))
    (cl-some (lambda (feature) (featurep feature)) features-to-check)))

(defun cody--create-graphql-event (event-name params)
  "Return a Sourcegraph GraphQL logging event for telemetry."
  (let ((uuid (cody--anonymized-uuid)))
    (list
     :event event-name
     :userCookieID uuid
     :url cody-workspace-root
     :source "IDEEXTENSION"
     :argument nil
     :publicArgument params
     :client "EMACS_CODY_EXTENSION"
     :deviceID uuid)))

;;; Utilities

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

(defun cody--buffer-active-p (&optional buf)
  "Return non-nil if BUF is active. BUF defaults to the current buffer."
  (let ((buffer (or buf (current-buffer))))
    (and (eq buffer (window-buffer (selected-window)))
         (get-buffer-window buffer t))))

(provide 'cody)
;;; cody.el ends here
