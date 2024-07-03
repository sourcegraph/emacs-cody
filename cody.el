;;; cody.el --- Sourcegraph Cody in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sourcegraph, Inc.

;; Version: 0.1
;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Maintainer: Steve Yegge <steve.yegge@gmail.com>
;; URL: https://github.com/sourcegraph/emacs-cody
;; Package-Requires: ((emacs "26.3") (jsonrpc "1.0.16") (uuidgen "1.2"))

;;; Commentary:
;;
;; Make sure your nodejs "node" executable is in your Emacs `exec-path'.
;;
;; Add something like `(add-hook 'prog-mode-hook 'cody-mode)' to start
;; cody automatically in new code buffers after logging into Cody.
;;
;; `M-x cody-login` to start using Cody.

;;; Code:
(eval-when-compile (require 'cl-lib))
(eval-and-compile (require 'eieio-base))
(require 'auth-source)
(require 'jsonrpc)
(require 'uuidgen)
(require 'cody-diff)

(defgroup cody nil
  "Sourcegraph Cody."
  :group 'programming
  :prefix "cody-")

(defcustom cody-telemetry-enable-p t
  "Non-nil to allow anonymized event/usage telemetry.
This information is used by Sourcegraph to improve the product."
  :group 'cody
  :type 'boolean)

(defcustom cody-workspace-root (getenv "HOME")
  "Directory which Cody considers your current project root.
You can override this to tell Cody to load up and focus on a
specific project, or by default Cody will attempt to infer it
from common project structures."
  :group 'cody
  :type 'string)

(defcustom cody-node-executable nil
  "Hardwired path to the nodejs binary to use for Cody.
If nil, Cody will search for node using variable `exec-path'."
  :group 'cody
  :type 'string)

(defcustom cody--node-min-version "20.4.0"
  "The minimum required version of Node.js."
  :group 'cody
  :type 'string)

(defcustom cody--anonymized-uuid nil
  "A generated ID for telemetry, to tie usage events together.
This is generated and cached on first use, if telemetry is enabled."
  :type 'string
  :group 'cody)

(defgroup cody-completions nil
  "Code autocompletion options."
  :group 'cody
  :prefix "cody-completion-"
  :prefix "cody-completions-")

(defcustom cody-completions-auto-trigger-p t
  "Non-nil to have Cody prompt with suggestions automatically.
Completion suggestions will appear after you stop typing for a while,
if any suggestions are available.  Set this to nil if you prefer to
use manual completion triggering with `cody-request-completion'."
  :group 'cody-completions
  :type 'boolean)

(defcustom cody-completions-display-marker-p t
  "Non-nil to display completion markers in the minibuffer.
This helps distinguish Cody's completions from other packages."
  :group 'cody-completions
  :type 'boolean)

(defface cody-completion-face
  '((t :inherit shadow
       :slant italic
       ;;:background "#ffffcd" ; uncomment for debugging overlay spans
       :foreground "#4c8da3"))
  "Face for Cody completion overlay."
  :group 'cody-completions)

(defcustom cody-completions-enable-cycling-p t
  "Non-nil to allow cycling among alternative completion suggestions.
These are not always available, but when they are, you can cycle
between them with `cody-completion-cycle-next' and
`cody-completion-cycle-prev'.  When nil, cycling is completely
disabled, and only the first option returned by the server is
ever displayed or interactible."
  :group 'cody
  :type 'boolean)

(defcustom cody-completions-cycling-help-p t
  "Non-nil to show a message in the minibuffer for cycling completions.
If non-nil, and multiple completion suggestions are returned from the
server, it will show how many are available and how to cycle them.
If nil, no messages are printed when cycling is available or used."
  :group 'cody
  :type 'boolean)

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
          :documentation "The range where the completion applies."))
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
                    :accessor cody--completion-event
                    :documentation "Event associated with the completion.")
   (response :initarg :response
             :initform nil
             :type (or null list)
             :accessor cody--completion-response
             :documentation "Top-level jsonrpc protocol response object.")
   (-current-item-index :initform 0
                        :type integer
                        :accessor cody--current-item-index
                        :documentation "Currently displayed alternative."))
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

(cl-defmethod cody--completion-text ((cc cody-completion))
  "Retrieve the text of the selected variable `cody-completion-item'.
Argument CC is the completion object."
  (when-let ((item (cody--current-item cc)))
    (oref item insertText)))

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

(defvar cody--unit-testing-p nil
  "Set to non-nil during unit testing.
When testing, all calls to the agent are diverted.")

(defun cody--agent-command ()
  "Command and arguments for running agent."
  (list (or cody-node-executable "node") cody--cody-agent))

(defvar cody--connection nil "Global jsonrpc connection to Agent.")
(defvar cody--message-in-progress nil "Chat message accumulator.")

(defvar cody--sourcegraph-host "sourcegraph.com" "Sourcegraph host.")
(defvar cody--access-token nil "Access token for `cody--sourcegraph-host'.")

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
    (define-key map (kbd "TAB") 'cody-completion-accept-key-dispatch)
    (define-key map (kbd "C-g") 'cody-quit-key-dispatch)
    (define-key map (kbd "ESC ESC ESC") 'cody-quit-key-dispatch)
    (when cody-completions-enable-cycling-p
      (define-key map (kbd "M-n") 'cody-completion-cycle-next-key-dispatch)
      (define-key map (kbd "M-p") 'cody-completion-cycle-prev-key-dispatch))
    map)
  "Keymap for `cody-mode'.")

(defvar cody-completion-map (make-sparse-keymap)
  "Keymap for cody completion overlay.")

(defvar cody--typewriter-effect nil
  "Non-nil to use typewriter effect in Cody chat output.")

(defconst cody--mode-hooks
  '((before-change-functions . cody--before-change)
    (after-change-functions . cody--after-change)
    (kill-buffer-hook . cody--kill-buffer-function)
    (pre-command-hook . cody--pre-command-function)
    (post-command-hook . cody--post-command-function))
  "List of buffer-local hooks that Cody registers on in `cody-mode'.
These hooks enable it to keep buffers and selections synced
with the Cody Agent.")

(defvar-local cody--overlay-deltas nil
  "List of overlays for Cody current completion suggestion.")

(defvar-local cody--completion nil
  "Most recent completion response object from the Cody Agent.
This is an instance of a variable `cody-completion' object.
Each time we request a new completion, it gets discarded and replaced.")

(defvar cody--completion-timer nil
  "Maybe trigger a completion each time Emacs goes idle.")

(defvar cody--last-completion-trigger-spot nil "Temp variable.")

(defsubst cody--cc ()
  "Return the current buffer-local version of `cody--completion'."
  cody--completion)

(defvar-local cody--completion-timestamps nil
  "Tracks event timestamps for telemetry, as plist properties.")

(defvar-local cody--update-debounce-timer nil
  "Delay for batching buffer-modification updates to the Agent.")

(defvar cody--vars nil
  "Symbol used as scratch space for ephemeral temp variables.
Typically used for allowing before/after hooks to communicate data.
Symbol properties are used reduce namespace clutter.")

(defconst cody--debounce-timer-delay 0.05
  "Wait at least this long between notifications of buffer content changes.")

(defsubst cody--timestamp ()
  "Return seconds since epoch."
  (float-time (current-time)))

;; Utilities

(defsubst cody--bol ()
  "Alias for `line-beginning-position'."
  ;; Why did they retire `point-at-bol'? :-(
  (line-beginning-position))

(defsubst cody--eol ()
  "Alias for `line-end-position'."
  (line-beginning-position))

(defun cody--buffer-active-p (&optional buf)
  "Return non-nil if BUF is active. BUF defaults to the current buffer."
  (or cody--unit-testing-p
      (let ((buffer (or buf (current-buffer))))
        (and (eq buffer (window-buffer (selected-window)))
             (get-buffer-window buffer t)))))

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

(defun cody--alive-p ()
  "Return non-nil if the jsonrpc connection is still running."
  (or cody--unit-testing-p
      (and cody--connection
           (cody--check-node-version)
           (zerop (process-exit-status
                   (jsonrpc--process cody--connection))))))

(defun cody--connection ()
  "Return the agent process, starting one if it is not already running."
  (unless (cody--alive-p)
    (setq cody--connection
          (make-instance
           'jsonrpc-process-connection
           :name "cody"
           :events-buffer-scrollback-size nil
           ;; TODO: add :events-buffer-config back in conditionally if available
           ;; This will fix a warning on every test we run with buttercup.
           :notification-dispatcher #'cody--handle-agent-notification
           :process (make-process
                     :name "cody"
                     :command (cody--agent-command)
                     :coding 'utf-8-emacs-unix
                     :connection-type 'pipe
                     :stderr (get-buffer-create "*cody stderr*")
                     :noquery t)))
    (cody--request 'initialize
                   (list
                    :name "Emacs"
                    :version "0.1"
                    :workspaceRootUri (cody--workspace-root)
                    :extensionConfiguration (cody--extension-configuration)))
    (cody--notify 'initialized nil))
  cody--connection)

(defun cody--request (method params &rest args)
  "Wrapper for `jsonrpc-request' that makes it testable."
  (unless cody--unit-testing-p
    (apply #'jsonrpc-request (cody--connection) method params args)))

(defun cody--notify (method params &rest args)
  "Helper to send a Cody request for METHOD with PARAMS."
  (unless cody--unit-testing-p
    (apply #'jsonrpc-notify (cody--connection) method params args)))

(defun cody--async-request (method params &rest args)
  "Wrapper for `jsonrpc-async-request' that makes it testable."
  (unless cody--unit-testing-p
    (apply #'jsonrpc-async-request method params args)))

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
  ;; Kill any zombie overlays; it can happen.
  (cl-loop for overlay being the overlays in (current-buffer)
           when (overlay-get overlay 'cody)
           do (delete-overlay overlay))
  (setq cody-mode nil) ; this clears the modeline and other vars
  (cody--cancel-completion-timer)
  (message "Cody mode disabled"))

(defun cody--before-change (beg end)
  "Before the buffer is changed, remember the old contents.
This is primarily so that we can detect when the user deleted
whitespace backward before the point at the completion suggestion.
BEG and END are as per the contract of `before-change-functions'."
  ;; This is the only way to know what text was removed in deletions.
  ;; Store it as a symbol property to avoid cluttering the namespace.
  ;; I'm going to leave it here as it's generally useful, though it
  ;; will be ignored until we get typing-in-completions working.
  ;; TODO: typing in completions
  (condition-case err
      (when (cody--overlay-visible-p)
        (when (and
               (/= beg end) ; if the change is to be a deletion
               ;; We only support deletions limited to within the line.
               (not (string-match-p "\n" (buffer-substring beg end))))
          (put 'cody--vars 'deleted-text
               (buffer-substring-no-properties beg end))))
    (error (cody--log "Error in `cody-before-change': %s" err))))

(defun cody--after-change (beg end len)
  "Respond to any change made in the buffer.
BEG, END and LEN are all defined by `after-change-hooks'.
Installed on `after-change-functions' buffer-local hook in `cody-mode'."
  ;; Kick off the `textDocument/didChange' notification.
  (unless cody--update-debounce-timer
    (setq cody--update-debounce-timer
          (run-with-idle-timer cody--debounce-timer-delay nil
                               #'cody--flush-pending-changes)))
  (if (cody--overlay-visible-p)
      ;; Either recompute or hide the overlay, based on the change.
      (cody--handle-typing-while-suggesting beg end len)
    (cody--start-completion-timer)))

(defun cody--cancel-debounce-timer ()
  "Cancel debounce timer, returning non-nil if a timer was cancelled."
  (when cody--update-debounce-timer
    (cancel-timer cody--update-debounce-timer)
    (setq cody--update-debounce-timer nil)
    'cancelled))

(defun cody--flush-pending-changes ()
  "If there is pending data, send it to the agent."
  (when-let ((had-pending-changes-p (cody--cancel-debounce-timer)))
    (cody--sync-buffer-to-agent 'textDocument/didChange)))

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
                  (cody--notify
                   op
                   (list
                    :filePath (buffer-file-name buf)
                    :content (buffer-substring-no-properties (point-min)
                                                             (point-max))
                    :selection (cody--selection-get-current buf)))))
            (setq happy t))
          (unless happy
            (cody--log "Unable to update Cody agent for %s" buffer-file-name))))
    (error (cody--log "Error sending file %s: %s" buffer-file-name err))))

(defun cody--pre-command-function ()
  "Save the current point and mark position before each command.
To cut down on namespace pollution they are stored as symbol
properties of the symbol `cody--post-command-function'."
  (condition-case err
      (when (cody--overlay-visible-p)
        ;; We save these three to check after the command completes.
        (put 'cody--vars 'last-point (point))
        (put 'cody--vars 'last-mark (mark))
        (put 'cody--vars 'last-hash (buffer-hash)))
    (error (cody--log "Error in pre-command function: %s" err))))

(defun cody--post-command-function ()
  "If point or mark has moved, update selection/focus with agent.
Installed on `post-command-hook', which see."
  (condition-case err
      (when (and (cody--overlay-visible-p)
                 ;; Buffer changes are handled by `cody--after-change'.
                 (equal (get 'cody--vars 'last-hash) (buffer-hash)))
        (let* ((last-point (get 'cody--vars 'last-point))
               (last-mark (get 'cody--vars 'last-mark))
               (point-moved (not (eq last-point (point))))
               ;; If the mark isn't set, pretend it is set at point,
               ;; yielding a zero-width range for the current selection.
               (mark-moved (not (eq last-mark (or (mark) (point))))))
          (when (or point-moved mark-moved)
            (cody--handle-focus-changed last-point))))
    (error
     (ignore-errors ; don't risk an error or our hook deregisters
       (cody--log "error in post-command-function %s: %s" last-command err)))))

(defun cody--handle-focus-changed (cursor-moved-p)
  "Notify agent that cursor or selection has changed in current buffer.
If CURSOR-MOVED-P then we may also trigger a completion timer."
  ;; Does not send the file contents. You can leave it undefined when
  ;; you're just updating the selection or caret.
  (when cody-mode
    (cody--notify 'textDocument/didFocus
                  (list
                   :filePath (buffer-file-name (current-buffer))
                   ;; Specifically leave :content undefined here.
                   :selection (cody--selection-get-current
                               (current-buffer))))
    ;; Maybe set a timer to trigger an automatic completion.
    ;; Do some trivial rejects here before setting the timer.
    (when (and cursor-moved-p
               (cody--buffer-active-p))
      (if (cody--overlay-visible-p)
          (cody--hide-completion)
        (cody--start-completion-timer)))))

(defun cody--start-completion-timer ()
  "Set a cancellable timer to check for an automatic completion."
  ;; This is a debounce so we don't request one until they stop typing.
  ;; It requests immediately after they go idle.
  (setq cody--completion-timer
        (run-with-idle-timer 0.05 nil #'cody--maybe-trigger-completion)))

(defun cody--cancel-completion-timer ()
  "Cancel any pending timer to check for automatic completions."
  (when cody--completion-timer
    (cancel-timer cody--completion-timer)
    (setq cody--completion-timer nil)))

(defun cody--selection-get-current (buf)
  "Return the jsonrpc parameters representing the selection in BUF.
If the region is not active, the selection is zero-width at point.
BUF can be a buffer or buffer name, and we return a Range with `start'
and `end' parameters, each a Position of 1-indexed `line' and `character'.
The return value is appropiate for sending directly to the rpc layer."
  (cl-flet ((pos-parameters (pos)
              ;; agent protocol range line/char are always 0-indexed
              (list :line (1- (line-number-at-pos pos))
                    :character (save-excursion
                                 (goto-char pos)
                                 (current-column)))))
    (with-current-buffer buf
      (let* ((mark (if mark-active (mark) (point)))
             (point (point))
             (beg (min mark point))
             (end (max mark point))
             (beg-pos (pos-parameters beg))
             (end-pos (pos-parameters end)))
        (list :start beg-pos :end end-pos)))))

(defun cody--handle-typing-while-suggesting (beg end len)
  "Either recompute completion or dispel it, depending on the change.
This can be called for either insertions or deletions, and we allow
the completion suggestion overlay to remain active for certain instances
of both. If it is a deletion, we recorded the deleted text in our
before-change function.
BEG and END are the region that changed, and LEN is its length."
  (condition-case err
      (if-let*
          ;; TODO: We ought to get typing the completion prefix working,
          ;; as it works in other clients. But our zero-width overlays
          ;; are making it a very slippery problem that needs revisiting.
          ((enable-this-code-branch nil) ; t to turn back on
           ;; Make sure there's a suggestion at the end of the change.
           (ovl (cody--overlay-delta-at end))
           (suggested-text (overlay-get ovl 'after-string))
           ;; BEG and END are as per contract of `after-change-functions'.
           (inserted-text (buffer-substring beg end)) ; "" for deletions
           (ignored (or
                     ;; Inserted text exactly matches the front of what's
                     ;; left of the suggestion string at point.
                     (and (zerop len)
                          (string-prefix-p inserted-text suggested-text))
                     ;; User deleted only spaces and tabs on the current line.
                     (and (plusp len) ; deletion (len is num chars deleted)
                          (string-match "^[ \t]*$"
                                        (get 'cody--vars 'deleted-text))))))
          (progn
            ;; This should recompute the necessary deltas.
            (cody--hide-completion)
            (cody--completion-display))
        ;; All other buffer changes make the overlay go away.
        (cody--hide-completion))
    (error (cody--log "Error in after-change function: %s" err))))

(defun cody--next-overlay-delta (beg)
  "Return the next overlay delta at or after BEG on the current line.
Returns nil if it finds any non-whitespace characters before finding
the Cody overlay, as that is a case which I think does not occur."
  (ignore-errors
    (cl-loop for pos = beg then (next-overlay-change pos)
             while (<= pos (cody--eol))
             if (string-match-p "^[ \t]*$"
                                (buffer-substring-no-properties beg pos))
             for o = (cody--overlay-delta-at pos)
             when o return o)))

(defun cody--overlay-delta-at (pos)
  "Return the Cody delta overlay at POS, if any."
  ;; Searching the overlays at POS is fraught with weird emacs bugs.
  (cl-find-if (lambda (ovl) ; Instead, search our own overlay list.
                (and (overlay-get ovl 'after-string) ; not e.g. logo ovl
                     (= pos (overlay-start ovl))))
              cody--overlay-deltas))

;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/JSONRPC-Overview.html
;; for a description of the parameters for this jsonrpc notification callback.
(defun cody--handle-agent-notification (_ method params)
  "Handle notifications from the agent, e.g. shutdown.
Optional argument METHOD is the agent protocol method with PARAMS."
  (cl-case method
    (chat/updateMessageInProgress
     (cody--handle-chat-update params))
    (shutdown ; Server initiated shutdown.
     (cody-logout))))

(defun cody--kill-buffer-function ()
  "If we are killing the last buffer visiting this file, notify agent."
  (condition-case err
      (when (and cody-mode
                 buffer-file-name
                 (cody--last-buffer-for-file-p))
        (cody--notify 'textDocument/didClose `(:filePath ,buffer-file-name)))
    (error (cody--log "Error notifying agent closing %s: err"
                      buffer-file-name err))
    (:success (cody--log "Notified agent closed %s" buffer-file-name))))


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
  "Handler for `chat/updateMessageInProgress'.
Argument PARAMS is the chat response object."
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
  (when (cody--alive-p)
    (ignore-errors
      (cody--request 'shutdown nil)) ; Required by the protocol
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
  (unless cody--unit-testing-p
    (when cody--connection
      (ignore-errors
        (jsonrpc-shutdown cody--connection 'cleanup-buffers))
      (setq cody--connection nil))))

(defun cody ()
  "Prompt for a recipe and arguments."
  (interactive)
  (let* ((recipes (cody--request 'recipes/list nil))
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
  "Shut down and restart Cody.  Mostly for debugging."
  (interactive)
  (let ((buffers (cl-loop for buf in (buffer-list)
                          when (with-current-buffer buf cody-mode)
                          collect buf)))
    (let ((cody--node-version-status 'good))
      (ignore-errors (cody-logout)))
    (setq cody--node-version-status nil)
    (cody-login)
    (dolist (buf buffers)
      (with-current-buffer buf
        (cody-mode)))))

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
  (when cody-mode
    (when-let ((o (car-safe cody--overlay-deltas)))
      (and (overlayp o)
           (overlay-buffer o) ; overlay is positioned/visible somewhere
           (cody--cc))))) ; not a zombie

(defun cody--make-overlay (text pos)
  "Create a new overlay for displaying part of a completion suggestion.
Gives it TEXT as the `after-string' and sets it at POS.
Pushes the new overlay onto the front of `cody--overlay-deltas'."
  (save-excursion
    ;; No front/rear advance - the overlay should never be nonempty.
    (let ((o (make-overlay pos pos)))
      (overlay-put o 'cody t)
      (overlay-put o 'keymap cody-completion-map)
      (overlay-put o 'priority '(nil . 50))
      (overlay-put o 'help-echo
                   "\\[cody-completion-accept-key-dispatch] to accept")
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
between point and the start of the completion.  Returns nil if point
is not currently at the start of a completion suggestion delta."
  (when (cody--overlay-visible-p)
    (cl-loop for o in cody--overlay-deltas
             when (or (and fuzzy
                           (>= (overlay-start o) (point))
                           (string-match-p "\\`[[:space:]]*\\'"
                                           (buffer-substring-no-properties
                                            (point) (overlay-start o))))
                      (and (not fuzzy) ; strict
                           (= (overlay-start o) (point))))
             return o)))

(defmacro cody--call-if-at-overlay (func &optional fuzzy)
  "If point is at the start of the completion overlay, invoke FUNC.
Otherwise, call the default non-Cody key binding for KEY.
If FUZZY is non-nil, then the check ignores whitespace between point
and the start of the overlay."
  `(if (cody--point-at-overlay-p ,fuzzy)
       (funcall ,func)
     (let (cody-mode) ; avoid recursion
       (call-interactively (key-binding (this-command-keys))))))

(defun cody--execute-default-keybinding ()
  "Execute default command for the key sequence invoking current command."
  (when-let ((cmd (lookup-key global-map (this-command-keys))))
    (call-interactively cmd)))

(defun cody-completion-accept-key-dispatch ()
  "Handler for the key that accepts a completion.
If we are at the Cody overlay, use the Cody binding; otherwise use
the default binding."
  (interactive)
  (cody--call-if-at-overlay 'cody--accept-completion))

(defun cody-quit-key-dispatch ()
  "Handler for `keyboard-quit'; clears the completion suggestion.
Also calls the default key binding."
  (interactive)
  (cody--call-if-at-overlay 'cody--discard-completion)
  ;; Also call default binding.
  (let (cody-mode) ; avoid recursion
    (call-interactively (key-binding (this-command-keys)))))

(defun cody--maybe-trigger-completion ()
  "Under the right conditions, trigger an automatic completion request."
  (when (and cody-mode
             cody-completions-auto-trigger-p
             (not (cody--overlay-visible-p))
             (not (use-region-p))
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
        (looking-back "\\s*[A-Za-z]+" (cody--bol))
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
       ;; have new requests replace pending ones
       :deferred 'cody
       :success-fn (lambda (response)
                     (cody--handle-completion-result
                      response buf cursor trigger-kind))
       :error-fn
       (lambda (err) (cody--log "Error requesting completion: %s" err))
       :timeout-fn
       (lambda () (cody--log "Error: request-completion timed out"))))))

(defun cody--handle-completion-result (response buf request-spot kind)
  "Dispatches completion result based on jsonrpc RESPONSE.
BUF and REQUEST-SPOT specify where the request was initiated.
KIND specifies whether this was triggered manually or automatically"
  ;; TODO: Handle rate-limiting.
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
              (cody--completion-display)
            (error
             (cody--log "Error displaying completion: %s" err)))))))))

(defun cody--populate-from-response (response)
  "Parse RESPONSE and return a populated variable `cody-completion' object."
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

(defun cody--position-to-point (pos)
  "Convert Cody Agent line/char position to a buffer position.
Argument POS is a plist with the line and char."
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

(defun cody--completion-display ()
  "Show the server's code autocompletion suggestion.
RESPONSE is the entire jsonrpc response."
  (when-let*
      ((cc (cody--cc))
       (item (cody--current-item cc))
       (index (cody--current-item-index cc))
       (range (cody--completion-item-range item))
       (range-start (cody--range-start range))
       (buffer-text (buffer-substring-no-properties
                     range-start (cody--range-end range)))
       (insert-text-chunks (cody--construct-line-chunks))
       (first-line (car insert-text-chunks))
       (next-lines (cdr insert-text-chunks))
       ;; Skip completions that need to delete or change characters in the
       ;; existing document. Harder to support, and most clients don't.
       (only-inserts-p (cody--check-only-insertions buffer-text first-line))
       (insertions (cody--compute-diff-deltas buffer-text
                                              first-line
                                              range-start)))
    (cody--completion-log-event 'autocomplete/completionSuggested)
    (condition-case err
        ;; Add one overlay span per delta in the first line.
        (cl-loop
         for (pos . text) in insertions
         do (cody--make-overlay text pos)
         finally do
         (cody--add-completion-marker (car-safe (last cody--overlay-deltas)))
         ;; Insert following lines, if any, as a single block.
         (when (and next-lines (cl-plusp (length next-lines)))
           (cody--make-overlay next-lines pos))
         (when (and cody-completions-cycling-help-p
                    (cody--multiple-items-p cc))
           (message
            (substitute-command-keys
             (concat "Showing suggestion %s of %s "
                     "(\\[cody-completion-cycle-next-key-dispatch]/"
                     "\\[cody-completion-cycle-prev-key-dispatch] to cycle)"))
            (1+ index) (cody--num-items cc))))
      (error
       (cody--log "Error setting completion text: %s" err)
       (cody--hide-completion)))))

(defun cody--construct-line-chunks ()
  "Split suggestion into first and remaining lines.
Returns (FIRST . REST) as strings."
  (let* ((insert-text (cody--get-insert-text-for-display))
         ;; We treat the 1st line specially: the completion may rewrite it.
         ;; So it gets a separate text chunk for the caller.
         (lines (split-string insert-text "\r?\n"))
         (multiline-p (> (length lines) 1))
         (first-line (car lines))
         ;; We treat any 2nd and following lines as a single unit of text.
         (more-lines
          (if multiline-p
              (concat "\n" (mapconcat #'identity (cdr lines) "\n"))
            "")))
    (cons first-line more-lines)))

(defun cody--get-insert-text-for-display ()
  "Return the suggested text for the current completion.
Returns nil if the text does not pass the sanitizing checks."
  (let ((text (cody--completion-text (cody--cc))))
    (cond
     ((null text) nil)
     ((string-match-p "[[:graph:]]" text) ; contains non-whitespace
      ;; Check if it is just giving us the code that's already there.
      ;; Can happen with any model, even fitm-trained.
      (if (string= text
                   (buffer-substring-no-properties (point) (length text)))
          nil
        text)) ; Otherwise, looks legit.
     ;; Bail if suggestion is all-whitespace; the team says this is a
     ;; bug.  If it repros reliably, they have asked that you let them know.
     (t
      (error "Completion text is blank: %s" text)))))

(defun cody--add-completion-marker (ovl)
  "Put a Cody symbol at the end of overlay OVL."
  (when (and cody-completions-display-marker-p
             (overlayp ovl))
    (condition-case err
        (let* ((text (or (overlay-get ovl 'after-string) ""))
               (ends-in-newline (string-match-p "\n\\'" text))
               (marker (propertize " " 'display (cody-logo-small)))
               ;; Make sure it precedes any trailing \n, to stay on line.
               (text-with-marker (if ends-in-newline
                                     (concat (substring text 0 -1) marker "\n")
                                   (concat text marker))))
          (overlay-put ovl 'after-string text-with-marker))
      (error (cody--log "Error adding completion marker: %s" err)))))

(defun cody--check-only-insertions (buffer-text first-line)
  "Return non-nil if the diffs contain only insertions.
BUFFER-TEXT and FIRST-LINE are the strings to diff."
  (condition-case err
      (cl-loop for chunk in (cody-diff-strings buffer-text first-line)
               never (eq (car-safe chunk) '-))
    (error
     (cody--log "Error chunking completion: %s" err)
     nil)))

(defun cody--compute-diff-deltas (buffer-text first-line range-start)
  "Diff completion against first line and compute deltas.
BUFFER-TEXT is the text being replaced by the suggested completion.
FIRST-LINE is the first line of the suggested completion.
RANGE-START is the start buffer position of the text being replaced.
Returns a list of insertions of the form ((buffer-pos . text) ...)."
  ;; Agent protocol necessitates computing the character diffs between
  ;; the current line in the buffer, and the suggested "completion",
  ;; which acts more like a rewrite of the current line.
  (condition-case err
      (cody-diff-strings-with-positions buffer-text
                                        first-line
                                        range-start)
    (error
     (cody--log "Error computing diff deltas: %s" err))))

(defun cody--hide-completion ()
  "Stop showing the current completion suggestion overlays.
`cody--completion' is left alone; use `cody--discard-completion' to
remove all traces of the last code completion response."
  (mapc #'delete-overlay cody--overlay-deltas)
  (setq cody--overlay-deltas nil)
  (when cody--completion
    (setf (cody--current-item-index (cody--cc)) 0)))

(defun cody--completion-log-event (notification)
  "Sends a required autocomplete notification to the agent.
NOTIFICATION is the fire-and-forget protocol message to send.
Does nothing if custom option `cody-telemetry-enable-p' is nil."
  (when cody-telemetry-enable-p
    (let ((event-id (plist-get (cody--completion-event (cody--cc)) :id)))
      (condition-case err
          (cody--notify notification (list :completionID event-id))
        (error (cody--log "Error on id=%s %s: %s" event-id notification err))))))

(defun cody--accept-completion ()
  "Accept and discard the completion, and notify server."
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
    (cody--completion-log-event 'autocomplete/completionSuggested)
    (cody--discard-completion)))

(defun cody--discard-completion ()
  "Discard/reset the current completion overlay and suggestion data.
Sends telemetry notifications when telemetry is enabled."
  (cody--cancel-completion-timer)
  (cody--hide-completion)
  (setq cody--completion nil
        cody--completion-timestamps nil
        ;; Just to be clear, don't change this or the completion will resurrect.
        cody--last-completion-trigger-spot cody--last-completion-trigger-spot
        cody--update-debounce-timer nil)
  (setplist 'cody--vars nil))

(defun cody--check-cycle-preconditions ()
  "Return non-nil if we meet all the preconditions for cycling.
If not, then it handles logging and messaging."
  (let ((verbose (or cody-completions-cycling-help-p
                     (and (called-interactively-p 'interactive)
                          (memq this-command
                                '(cody-completion-cycle-next cody-completion-cycle-prev)))))
        (cc (cody--cc)))
    (cl-labels ((explain (msg &rest args)
                  (let ((output (apply #'message msg args)))
                    (if verbose (message output)
                      (cody--log output))
                    nil))) ; return nil to signal failed check
      (cond
       ((not cody-completions-enable-cycling-p)
        (explain "Set `cody-completions-enable-cycling-p' to enable cycling"))
       ((not (and cc (cody--current-item cc)))
        (explain "No completion here"))
       ((not (cody--multiple-items-p cc))
        (explain "No other suggestions"))
       (t 'success)))))

(defun cody--completion-cycle (direction)
  "Cycle through the completion items in the specified DIRECTION.
DIRECTION should be 1 for next, and -1 for previous."
  (if (cody--check-cycle-preconditions)
      (if-let* ((cc (cody--cc))
                (items (cody--completion-items cc))
                (index (cody--current-item-index cc))
                (num (cody--num-items cc))
                (next (% (+ index direction num) num)))
          (condition-case err
              (progn
                (cody--hide-completion) ; This sets index to 0.
                (setf (cody--current-item-index cc) next) ; Set index to new val.
                (cody--completion-display))
            (error
             (cody--log "Error displaying completion: %s \ncompletion: %s"
                        err cc)))
        (when cody-completions-cycling-help-p
          (message "Error cycling through completions"))
        (cody--log "Error cycling through completions: items= %s" items))
    (when cody-completions-cycling-help-p
      (message
       (concat "No more suggestions. "
               (substitute-command-keys
                "\\[cody-completion-accept-key-dispatch] to accept."))))))

(defun cody-completion-cycle-next-key-dispatch ()
  "Cycles to next completion only if the point is at an overlay.
If point is not at the overlay, dispatches to the default binding."
  (interactive)
  (cody--call-if-at-overlay #'cody-completion-cycle-next))

(defun cody-completion-cycle-prev-key-dispatch ()
  "Cycles to prev completion only if the point is at an overlay.
If point is not at the overlay, dispatches to the default binding."
  (interactive)
  (cody--call-if-at-overlay #'cody-completion-cycle-prev))

(defun cody-completion-cycle-next ()
  "Move to the next completion alternative."
  (interactive)
  (if (and cody-completions-enable-cycling-p
           (cody--point-at-overlay-p))
      (cody--completion-cycle 1)
    (cody--execute-default-keybinding)))

(defun cody-completion-cycle-prev ()
  "Move to the previous completion alternative."
  (interactive)
  (if (and cody-completions-enable-cycling-p
           (cody--point-at-overlay-p))
      (cody--completion-cycle -1)
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

(defun cody--other-plugins-enabled-p ()
  "Return non-nil if another coding assistant is active concurrently."
  (let ((features-to-check '(copilot tabnine)))
    (cl-some (lambda (feature) (featurep feature)) features-to-check)))

(defun cody--anonymized-uuid ()
  "Return, generating if needed, variable `cody--anonymized-uuid'."
  (or cody--anonymized-uuid
      (setq cody--anonymized-uuid (uuidgen-4))
      (custom-save-all)
      cody--anonymized-uuid))

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

(provide 'cody)
;;; cody.el ends here
