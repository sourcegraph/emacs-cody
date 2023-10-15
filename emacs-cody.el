;;; emacs-cody.el --- Sourcegraph Cody in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sourcegraph, Inc.

;; Version: 0.1
;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Maintainer: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; URL: https://github.com/keegancsmith/emacs-cody
;; Package-Requires: ((emacs "26.3") (jsonrpc "1.0.16"))


;;; Commentary:
;; Load this package and then add `(cody-start)' to your .emacs

;;; Code:
(require 'cl-lib)
(require 'auth-source)
(require 'jsonrpc)
(require 'dash)
(require 'uuidgen)
(eval-when-compile (require 'subr-x))

(defgroup cody nil
  "Sourcegraph Cody."
  :group 'programming)

(defcustom cody-workspace-root (getenv "HOME")
  "Directory which Cody considers your current project root."
  :group 'cody
  :type 'string)

(defcustom cody-auto-enable-cody-mode t
  "Non-nil to enable Cody commands in all relevant buffers.
You can set this to nil if you need to run Cody only in a chat session,
or if you prefer to set up your own rules for enabling `cody-mode'."
  :group 'cody
  :type 'boolean)

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

(defconst cody--cody-agent
  (file-name-concat (file-name-directory
                     (or load-file-name
                         (buffer-file-name)))
                    "dist" "cody-agent.js")
  "Path to bundled cody agent.")

(defconst cody--node-min-version "20.4.0"
  "The minimum required version of node.js for Cody.")

(defvar cody--node-version-status nil
  "Non-nil after `cody--check-node-version' is called.
The node version is only checked on Cody startup.
You can call `cody-restart' to force it to re-check the version.")

(defvar cody-node-path-override nil
  "Hardwired path to the node.js binary to use for Cody.")

(defun cody--agent-command ()
  "Command and arguments for running agent."
  (list (or cody-node-path-override "node") cody--cody-agent ""))

(defvar cody--connection nil "Global jsonrpc connection to Agent.")
(defvar cody--message-in-progress nil "Chat message accumulator.")
(defvar cody--access-token nil "LLM access token.")

(defconst cody-log-buffer-name "*cody-log*" "Cody log messages.")
(defconst cody-chat-buffer-name "*cody-chat*" "Cody chat Buffer.")

(defvar cody-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'cody-request-completion)
    (define-key map (kbd "M-\\") 'cody-request-completion) ; for IntelliJ users
    (define-key map (kbd "TAB") 'cody--tab-key)
    (define-key map (kbd "C-g") 'cody--ctrl-g-key)
    (define-key map (kbd "ESC ESC ESC") 'cody--ctrl-g-key)
    (when cody-enable-completion-cycling
      (define-key map (kbd "M-n") 'cody-next-completion)
      (define-key map (kbd "M-p") 'cody-prev-completion))
    map)
  "Keymap for `cody-mode'.")

(defvar cody-completion-map (make-sparse-keymap)
  "Keymap for cody overlay.")

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

(defvar-local cody--overlay nil
  "Overlay for Cody current completion suggestion.")

(defvar-local cody--completion nil
  "Most recent completion result from Cody Agent.
This is a plist whose `:insertText' property holds the text
of the last suggested autocompletion.  It can be overridden
by a `:displayText' property on the same plist; the `:insertText'
property should be considered immutable.")

(defvar-local cody--completion-response nil
  "Most recent completion `jsonrpc' response from Cody.")

(defvar-local cody--completion-timestamps nil
  "Tracks event timestamps for telemetry, as plist properties.")

;; These are for keeping the agent notified about current file/selection.
(defvar-local cody--last-point nil "The last point position.")
(defvar-local cody--last-mark nil "The last mark position.")

(defvar-local cody--last-index nil
  "Index of last displayed completion alternative.")

(defface cody-completion-face
  '((t :inherit shadow :slant italic))
  "Face for Cody completion overlay.")

(defsubst cody--completion-text (&optional completion)
  "Text of the most recent COMPLETION result."
  (let ((c (or completion cody--completion)))
    (or (plist-get c :displayText)   ; updated via user mutations
        (plist-get c :insertText)))) ; original from RPC

(defsubst cody--timestamp ()
  "Return seconds since epoch."
  (float-time (current-time)))

(defsubst cody--completion-alternatives ()
  "Return the list of alternatives from the last completion response.
It is a vector of plists, one per completion alternative.
Returns nil if there is no current completion being displayed."
  (plist-get cody--completion-response :items))

(defsubst cody--trim-leading (s)
  "Trim leading whitespace from S."
  (replace-regexp-in-string "^\\s-+" "" s))

(defsubst cody--completion-set-prop (prop value)
  "Mutate the completion by setting plist PROP to VALUE.
Useful for recording metadata for the completion during its lifecycle."
  (setq cody--completion (plist-put cody--completion prop value)))

;; Add to your ~/.authinfo.gpg something that looks like
;;   machine sourcegraph.sourcegraph.com login apikey password sgp_SECRET
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
                       :host "sourcegraph.sourcegraph.com"
                       :require '(:secret :host))))))

(defun cody--extension-configuration ()
  "Which `ExtensionConfiguration' parameters to send on Agent handshake."
  (list :accessToken (cody--access-token)
        :serverEndpoint "https://sourcegraph.sourcegraph.com"
        ;; Note there is a bug currently where the agent initialize request
        ;; fails if the serverEndpoint doesn't know the codebase.
        :codebase "https://github.com/sourcegraph/cody"))

(cl-defun cody--request (method &rest params &allow-other-keys)
  "Helper to send a Cody request."
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
    (cody--log "Sending 'initialize' request to agent")
    (jsonrpc-request cody--connection 'initialize
                     (list
                      :name "emacs"
                      :version "0.1"
                      :workspaceRootPath (cody--workspace-root)
                      :extensionConfiguration (cody--extension-configuration)))
    (jsonrpc-notify cody--connection 'initialized nil))
  cody--connection)

(defcustom cody--node-min-version "20.4.0"
  "The minimum required version of Node.js."
  :type 'string
  :group 'cody)

(defun cody--check-node-version ()
  "Signal an error the default node.js is not a high enough version.
Version is configurable with `cody--node-min-version'."
  (cl-case cody--node-version-status
    (good t)
    (bad (error "Installed Node.js must be at least %s - see `cody-node-path-override'"
                cody--node-min-version))
    (otherwise
     (let* ((cmd (concat (or cody-node-path-override "node") " -v"))
            (node-version (string-trim (shell-command-to-string cmd)))
            minor major patch)
       (if (not (string-match "^v\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"
                              node-version))
           (progn
             (message "Error: Could not parse node.js version string: %s" node-version)
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
                   (and (= major min-major) (= minor min-minor) (>= patch min-patch)))
               (setq cody--node-version-status 'good)
             (setq cody--node-version-status 'bad)
             (error
              "Error: Installed Node.js version %s is lower than min version %s"
              node-version cody--node-min-version))))))))

(defun cody--workspace-root ()
  "Return the workspace root for the Agent.
You can override it with `cody-workspace-root'."
  (or cody-workspace-root
      (and buffer-file-name (file-name-directory buffer-file-name))
      (getenv "HOME")))

(defun cody-logo ()
  "Return the Cody logo image via `create-image'."
  (create-image (cody-logo-file "cody-logo.png")))

(defun cody-logo-small ()
  "Return the Cody modeline image."
  (create-image (cody-logo-file "cody-logo-small.png")))

(defun cody-logo-file (file-base)
  "Construct path to bundled cody image file."
  (file-name-concat ; hack the Cody logo path
   ;; wtf emacs why is there no parent-directory function?
   (file-name-directory
    (directory-file-name
     (file-name-directory cody--cody-agent)))
   file-base))

(defvar cody--minor-mode-icon
  (let ((img (cody-logo-small)))
    (if img
        (progn
          (plist-put (cdr img) :ascent 80)  ; Raise the image up a bit.
          (list " " (propertize " " 'display img)))
      " Cody"))
  "Mode line lighter for Cody minor-mode.")

(put 'cody--minor-mode-icon 'risky-local-variable t)

(define-minor-mode cody-mode
  "Minor mode for interacting with the Cody coding assistant.
Changes to the buffer will be tracked by the Cody agent"
  :lighter cody--minor-mode-icon
  :keymap cody-mode-map
  (if cody-mode
      (cody--minor-mode-startup)
    (cody--minor-mode-shutdown)))

(defun cody--minor-mode-startup ()
  "Code to run when `cody-mode' is enabled in a buffer."
  (cl-loop for (hook . func) in cody--mode-hooks
           do (add-hook hook func nil t))
  (cody--send-file-to-agent (current-buffer) 'textDocument/didOpen)
  (message "Cody mode enabled"))

(defun cody--minor-mode-shutdown ()
  "Code to run when `code-mode' is disabled in a buffer."
  (cody--discard-completion)
  (cl-loop for (hook . func) in cody--mode-hooks
           do (remove-hook hook func t))
  (setq cody-mode nil) ; this clears the modeline and other vars
  (message "Cody mode disabled"))

(defun cody--text-file-p (buf)
  "Return non-nil if BUF is visiting a text file.
A heuristic to see if we should notify the agent about it."
  ;; TODO: Maybe just ask the filesystem if it's a text file, instead?
  (and (buffer-file-name buf)
       (not
        (memq (with-current-buffer buf buffer-file-coding-system)
              '(nil raw-text binary no-conversion)))))

(defun cody--enable-cody-mode ()
  "Enables `cody-mode' for any applicable open buffers.
This may be on a background thread, since there may be many files open
in the user's Emacs session when they start or restart Cody."
  ;; Selectively turn on `cody-mode' for newly opened files.
  (add-hook 'find-file-hook #'cody--maybe-turn-on-cody-mode)
  ;; Selectively turn on `cody-mode' for applicable open buffers.
  (let ((file-count 0)
        (cody-count 0))
    (cl-loop
     for buf being the buffers
     do
     (with-current-buffer buf
       (cody--maybe-turn-on-cody-mode)
       (if cody-mode (cl-incf cody-count)))
     (cl-incf file-count)
     finally (cody--log "Scanned %s buffers, enabled Cody in %s"
                        file-count cody-count))))

(defun cody--maybe-turn-on-cody-mode ()
  "Maybe enable `cody-mode' on a newly opened file.
Current buffer is visiting the file."
  (when (cody--enable-for-buffer-p)
    (cody-mode)))

(defun cody--enable-for-buffer-p ()
  "Return non-nil if Cody should be enabled for current buffer.
Currently it means the buffer is visiting a text file on disk."
  (and (cody--alive-p)
       ;; TODO: Need ways to whitelist & blacklist cody-mode for various
       ;; buffer/file patterns.
       (cody--text-file-p (current-buffer))))

(defun cody--send-file-to-agent (buf op)
  "Make the jsonrpc call to notify agent of opened/changed file."
  (with-current-buffer buf
    (jsonrpc-notify (cody--connection) op
                    (list
                     :filePath (buffer-file-name buf)
                     :content (buffer-substring-no-properties (point-min)
                                                              (point-max))
                     :selection (cody--get-selection buf)))))

(defun cody--after-change (&rest _)
  "Implement the `textDocument/didChange' notification.
Installed on `after-change-functions' buffer-local hook in `cody-mode'."
  ;; N.B. Emacs calls this on every keystroke as you type, and also on other mods.
  (ignore-errors
    (when cody-mode
      (let ((happy nil))
        (unwind-protect
            (progn
              ;; Dispense with optimization and just blast the whole file over. Vavoom.
              (cody--send-file-to-agent (current-buffer)
                                        'textDocument/didChange)
              (setq happy t))
          (unless happy
            (cody--log "Unable to update Cody agent for %s" buffer-file-name)))))))

(defun cody--post-command-function ()
  "If point or mark has moved, update selection/focus with agent.
Installed on `post-command-hook', which see."
  (ignore-errors
    (when cody-mode
      (cody--debuggable-post-command))))

(defun cody--debuggable-post-command ()
  "Set your breakpoint for `cody--post-command-function` here instead."
  (ignore-errors ; Fail somewhat gracefully here; completion will eventually die.
    (cody--maybe-clear-completion))
  (let ((point-unequal (neq cody--last-point (point)))
        ;; If the mark isn't set (nil), we pretend it is set at point,
        ;; yielding a zero-width range for the current selection.
        (mark-unequal (neq cody--last-mark (or (mark) (point)))))
    (if point-unequal
        (setq cody--last-point (point)))
    (if mark-unequal
        (setq cody--last-mark (or (mark) (point))))
    (if (or point-unequal mark-unequal)
        (cody--handle-focus-change))))

(defun cody--handle-focus-change ()
  "Notify agent that cursor or selection has changed in current buffer."
  ;; Does not send the file contents. Olaf assured me you can leave it undefined
  ;; when you're just updating the selection or caret.
  (when cody-mode ; sanity check
    (jsonrpc-notify (cody--connection) 'textDocument/didFocus
                    (list
                     :filePath (buffer-file-name (current-buffer))
                     ;; Specifically leave :content undefined here.
                     :selection (cody--get-selection (current-buffer))))))

(defun cody--get-selection (buf)
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
     (cody-shutdown))))

(defun cody--kill-buffer-function ()
  "If we are killing the last buffer visiting this file, notify agent."
  (when (and cody-mode
             buffer-file-name
             (cody--last-buffer-for-file-p))
    (jsonrpc-notify (cody--connection) 'textDocument/didClose
                    (list :filePath buffer-file-name))
    (cody--log "Notified agent closed %s" buffer-file-name)))

(defun cody--last-buffer-for-file-p ()
  "Check if the current buffer is the last one visiting its file.
It returns non-nil if the buffer being killed is the last one
visiting its associated file."
  (let ((current-file (buffer-file-name))
        (current-buf (current-buffer)))
    (when current-file
      (not (cl-some (lambda (buf)
                      (and (not (eq buf current-buf))
                           (string-equal current-file (buffer-file-name buf))))
                    (buffer-list))))))

(defun cody--handle-chat-update (params)
  "Handler for `chat/updateMessageInProgress'."
  (message nil) ; clear the "Awaiting Cody response..." message
  (if cody--typewriter-effect
      (if params
          (cody-chat-insert-msg-tail params)
        (cody-chat-insert "\n"))
    (if params
        ;; Replace the accumulator with the updated generated output.
        (setq cody--message-in-progress params)
      ;; Null params --> Message is complete.
      (cody-chat-insert
       (plist-get cody--message-in-progress :displayText)
       "\n\n")
      (setq cody--message-in-progress nil))))

(defun cody-chat-insert (&rest args)
  "Insert ARGS at the end of the Cody chat buffer."
  (with-current-buffer (cody-chat-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (apply #'insert args)
      (set-buffer-modified-p nil)
      (cody-chat-scroll-to-bottom))))

(defun cody-chat-scroll-to-bottom ()
  "Move cursor to bottom of chat buffer."
  ;; If the buffer is visible in a window, then the point doesn't move
  ;; even after we insert text, as windows keep their own copy of point.
  ;; We want it to act more like a shell.
  (with-current-buffer (cody-chat-buffer)
    (let ((win (get-buffer-window)))
      (if (window-live-p win)
          (set-window-point win (point-max))))))

;; The agent sends an update with increasingly long hunks of the response,
;; e.g. "Here", "Here is", "Here is an", "Here is an explanation", ...
;; This permits a typewriter effect.
(defun cody-chat-insert-msg-tail (params)
  "Insert only the most recent update to the message output.
This allows you to see the output stream in as it is generated.
Cody chat buffer should be current, and PARAMS non-nil."
  (let* ((old-text cody--message-in-progress)
         (new-text (plist-get params :displayText))
         (tail
          (cond
           ((null old-text) new-text)
           ((>= (length new-text) (length old-text))
            (substring new-text (length old-text)))
           (t
            ;; This could happen if the generator "backs up",
            ;; which I've seen happen with ChatGPT, so we'll
            ;; likely need to handle this at some point.
            ""))))
    (let ((inhibit-read-only t))
      (insert tail))
    (setq cody--message-in-progress new-text)))

(defun cody-shutdown ()
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
  (cody-shutdown)
  (ignore-errors
    (kill-buffer (cody-chat-buffer)))
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

(defun cody-start (&optional quiet)
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
    (message "Cody connection initialized.")
    (when cody-auto-enable-cody-mode
      (cody--init-cody-mode))))

(defun cody--init-cody-mode ()
  "Start an agent if needed, and enable Cody in applicable buffers."
  ;; TODO: Add a customization option to not auto-enable cody-mode
  ;; for all buffers, and instead the user can invoke cody-mode manually
  ;; or set up their own rules for toggling it. As one example driving this
  ;; use case, the user might only want to have a chat session open, but
  ;; not want Cody tracking all their other buffers.
  (if cody--use-threads
      ;; N.B. Debugger is unable to exit if there's an error in the thread function.
      (make-thread (lambda ()
                     (cody--enable-cody-mode))
                   "Cody startup thread")
    (cody--enable-cody-mode)))

(defun cody-restart ()
  "Shut down and restart Cody.  Mostly for debugging."
  (interactive)
  (let ((cody--node-version-status 'good))
    (ignore-errors
      (cody-shutdown)))
  (setq cody--node-version-status nil)
  (cody-start))

(defun emacs-cody-unload-function ()
  "Handle `unload-feature' for this package."
  (cody-shutdown))

(defun cody--log (msg &rest args)
  "Log MSG with ARGS, currently just for debugging."
  ;; TODO: Use a real logging package
  (with-current-buffer (get-buffer-create cody-log-buffer-name)
    (goto-char (point-max))
    (insert (apply #'format msg args) "\n")))

(defun cody-chat ()
  "Shorthand for the chat recipe.
Query and output go into the *cody-chat* buffer."
  (interactive)
  (cody-start)
  (display-buffer (cody-chat-buffer))
  (let ((query (read-from-minibuffer "Ask Cody: ")))
    (cody-chat-insert-query query)
    (cody--request 'recipes/execute
                   :id "chat-question"
                   :humanChatInput query))
  (message "Awaiting response from Cody..."))

(defun cody-chat-insert-query (query)
  "Insert the user QUERY into the chat output buffer."
  (with-current-buffer (cody-chat-buffer)
    (goto-char (point-max))
    (cody-chat-insert "> " query "\n\n")
    (goto-char (point-max))))

(defun cody-chat-buffer ()
  "Return Cody chat output buffer, initializing if necessary."
  (let* ((probe (get-buffer cody-chat-buffer-name)))
    (if (buffer-live-p probe)
        probe
      ;; Create and initialize the chat buffer.
      (with-current-buffer (get-buffer-create cody-chat-buffer-name)
        (buffer-disable-undo)
        (let ((inhibit-modification-hooks t))
          (insert-image (cody-logo) "Cody")
          (insert "Welcome to Cody. Type `M-x cody-help` for more info.\n")
          (set-buffer-modified-p nil))
        (current-buffer)))))

(defun cody--overlay-visible-p ()
  "Return non-nil if Cody is displaying a suggestion in the current buffer."
  (and cody--completion
       (overlayp cody--overlay)
       ;; Make sure we have set a nonzero-length string as the visible string.
       ;; Otherwise the overlay may be hidden. We could also check that it's at bob,
       ;; but that would lead to false positives in completing in an empty buffer.
       (plusp (length (overlay-get cody--overlay 'after-string)))))

(defun cody--overlay ()
  "Get Cody completion overlay, creating if needed."
  (unless (overlayp cody--overlay)
    (let ((o (setq cody--overlay (make-overlay 1 1 nil nil t))))
      (overlay-put o 'keymap cody-completion-map)
      (overlay-put o 'priority '(nil . 50))
      (overlay-put o 'help-echo "TAB to accept")
      (overlay-put o 'insert-in-front-hooks '(cody--overlay-insert-front))))
  cody--overlay)

(defun cody--point-at-overlay-p (&optional fuzzy)
  "Return non-nil if point is at the start of a completion suggestion.
If FUZZY is non-nil, returns non-nil if there is only whitespace between
point and the start of the completion."
  (and cody-mode
       (cody--overlay-visible-p)
       (let ((beg (overlay-start cody--overlay)))
         (if fuzzy
             (save-excursion
               (save-match-data
                 (string-match-p "\\`[[:space:]\r]*\\'"
                                 (buffer-substring (point) beg))))
           (= beg (point)))))) ; strict

(defmacro cody--call-if-at-overlay (func key &optional fuzzy)
  "If point is at the start of the completion overlay, invoke FUNC.
Otherwise, call the default non-Cody key binding for KEY.
If FUZZY is non-nil, then the check ignores whitespace between point
and the start of the overlay."
  `(if (cody--point-at-overlay-p ,fuzzy)
       (funcall ,func)
     (let (cody-mode) ; avoid recursion
       (call-interactively (key-binding (kbd ,key))))))

(defun cody--tab-key ()
  "Handler for TAB key."
  (interactive)
  (cody--call-if-at-overlay 'cody--accept-completion "TAB"))

(defun cody--ctrl-g-key ()
  "Handler for quit; clears/rejects the completion suggestion."
  (interactive)
  (cody--call-if-at-overlay 'cody--discard-completion "C-g" 'fuzzy))

(defun cody-request-completion ()
  "Request manual autocompletion in current buffer at point."
  (interactive)
  (unless cody-mode
    (error "Cody-mode not enabled in this buffer."))
  (condition-case err
      (let* ((buf (current-buffer))
             (file (buffer-file-name buf))
             (line (1- (line-number-at-pos)))
             (col (current-column)))
        (cody--discard-completion) ; Clears telemetry from previous request.
        (cody--update-completion-timestamp :triggeredAt)
        (cody--handle-completion-result
         (jsonrpc-request (cody--connection) 'autocomplete/execute
                          (list :filePath file
                                :position (list :line line :character col)
                                :triggerKind "Invoke"))))
    (error (cody--log "Error requesting completion: %s" err))))

(defun cody--handle-completion-result (response)
  "Dispatches completion result based on jsonrpc RESPONSE."
  (if-let* ((item-vec (plist-get response :items))
            (_ (vectorp item-vec))
            (count (length item-vec)))
      (if (zerop count)
          (message "No completions returned")
        (let (cody--completion-timestamps) ; preserve the trigger time
          (cody--set-completion (aref item-vec 0) response))
        (cody--update-completion-timestamp :displayedAt)
        (cody--display-completion 0))
    (message "Unexpected response format: %s" response)))

(defun cody--set-completion (completion &optional response)
  "Set a new COMPLETION, from any source, deleting any existing one(s).
If RESPONSE is not passed in, it is retained, e.g. when cycling completions."
  (cody--discard-completion)
  (setq cody--completion completion)
  (when response
    (setq cody--completion-response response)))

(defun cody--display-completion (index)
  "Show the server's code autocompletion suggestion.
RESPONSE is the entire jsonrpc response.
INDEX is the completion alternative to display from RESPONSE."
  (when-let ((text (cody--completion-text)))
    (setq cody--last-index index)
    (cody--overlay-set-text (cody--trim-leading text))
    (when (and cody-enable-completion-cycling-help
               (cody--multiple-alternatives-p))
      (message "Showing suggestion %s of %s (M-n/M-p to cycle)"
               (1+ index) (length (cody--completion-alternatives))))))

(defun cody--overlay-set-text (text)
  "Update overlay TEXT and redisplay it at point."
  ;; Record it, since 'after-string is cleared if we hide the completion.
  (cody--completion-set-prop :displayText text)
  (let ((pretty-text (propertize text 'face 'cody-completion-face)))
    (put-text-property 0 1 'cursor t pretty-text)
    (overlay-put (cody--overlay) 'after-string pretty-text))
  (cody--move-overlay (point) (point)))

(defun cody--move-overlay (beg end)
  "Safely move cody overlay to BEG and END, creating it if needed."
  (save-excursion
    (move-overlay (cody--overlay) beg end)))

(defun cody--overlay-insert-front (overlay after beg end &optional len)
  "Allow user to type over the overlay character by character."
  (save-excursion
    (when (and after
               (cody--overlay-visible-p)
               (numberp len)
               (zerop len)) ; Length 0 means it is an insertion.
      (let* ((text (cody--completion-text))
             (c (char-after beg))) ; char they just typed
        (cond
         ((!= c (aref text 0)) ; typed something different
          (unless (eq (char-syntax c) ?\s)
            (cody--hide-completion))) ; hide if not whitespace
         ((= 1 (length text))
          (cody--accept-completion)) ; fully consumed
         (t
          (cody--shorten-completion)))))))

(defun cody--shorten-completion ()
  "Consume the first character of the current completion."
  (let* ((completion cody--completion)
         (text (cody--completion-text completion))
         (shortened (substring text 1)))
    (cody--overlay-set-text shortened)))

(defun cody--hide-completion ()
  "Stop showing the completion suggestion."
  (overlay-put (cody--overlay) 'after-string "")
  (cody--move-overlay 1 1)
  (setq cody--last-index 0))

(defun cody--maybe-clear-completion ()
  "Implements `post-command-hook' to clear completion if applicable."
  ;; For now, clear it if we're no longer at or before overlay start.
  (unless (cody--point-at-overlay-p 'fuzzy)
    (cody--hide-completion)))

(defun cody--graphql-log-event (event)
  "Send a `graphql/log-event' to the agent.
EVENT is a Sourcegraph GraphQL event."
  (condition-case err
      (jsonrpc-notify (cody--connection) 'graphql/log-event
                      (list :publicArgument event))
    (error (cody--log "Error logging graphql event: %s" err))))

(defun cody--accept-completion ()
  "Record the completion as accepted."
  (let ((text (cody--completion-text))
        (start (line-beginning-position)))
    (insert text)
    (indent-region start (point)))
  (cody--telemetry-completion-accepted)
  (jsonrpc-notify cody--connection 'autocomplete/clearLastCandidate nil)
  (cody--discard-completion))

(defun cody--discard-completion ()
  "Discard/reset the current completion overlay and suggestion data.
Sends telemetry notifications when telemetry is enabled."
  (unless (eq (cody--completion-status) 'triggered)
    (cody--telemetry-completion-suggested))
  (when-let ((o cody--overlay))
    (delete-overlay o))
  (setq cody--overlay nil
        cody--completion nil
        cody--completion-response nil
        cody--completion-timestamps nil
        cody--last-point nil
        cody--last-mark nil
        cody--last-index nil))

(defun cody--multiple-alternatives-p ()
  "Return non-nil if the last completion response had multiple options."
  (when-let* ((item-vec (cody--completion-alternatives)))
    (> (length item-vec) 1)))

(defun cody--key-for-command (command &optional keymap)
  "Get user-visible key sequence for COMMAND."
  (when-let ((keys (where-is-internal command keymap)))
    (key-description (car keys))))

(defun cody--key-msg-for-command (command)
  "Return message about the key to press for a given COMMAND."
  (if-let ((key (cody--key-for-command command)))
      (format "%s to accept" key)
    "No key is bound to accept"))

(defun cody--cycle-completion (direction)
  "Cycle through the completion alternatives in the specified DIRECTION.
DIRECTION should be 1 for next, and -1 for previous."
  (if (and cody-enable-completion-cycling
           (cody--multiple-alternatives-p))
      (if-let* ((items (cody--completion-alternatives))
                (index (or cody--last-index 0))
                (num (length items))
                (next (% (+ index direction num) num)))
          (progn
            (cody--set-completion (aref items next))
            (cody--display-completion next))
        (when cody-enable-completion-cycling-help
          (message "Error cycling through completions"))
        (cody--log "Error cycling through completions: items= %s" items))
    (when cody-enable-completion-cycling-help
      (message "No other suggestions; %s"
               (cody--key-msg-for-command 'cody--tab-key)))))

(defun cody-next-completion ()
  "Move to the next completion alternative."
  (interactive)
  (when cody-enable-completion-cycling
    (cody--cycle-completion 1)))

(defun cody-prev-completion ()
  "Move to the previous completion alternative."
  (interactive)
  (when cody-enable-completion-cycling
    (cody--cycle-completion -1)))

(defun cody-dashboard ()
  "Show a console with data about Cody configuration and usage."
  (interactive)
  ;; TODO: Lots more information here.
  (let ((buf (get-buffer-create "*cody-dashboard*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (erase-buffer)
        (if (cody--alive-p)
            (insert "Cody is connected")
          (insert "Cody is not connected"))))
    (pop-to-buffer buf)))

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

(defun cody--update-completion-timestamp (property)
  "Set the given timestamp named PROPERTY to the current time."
  (setq cody--completion-timestamps
        (plist-put cody--completion-timestamps property (cody--timestamp))))

(defun cody--set-completion-response-prop (prop value)
  "Update `cody--completion-response' setting PROP to VALUE."
  (setq cody--completion-response
        (plist-put cody--completion-response prop value)))

(defun cody--set-completion-event-prop (prop value)
  "Update the completion event in the response setting PROP to VALUE."
  (cody--set-completion-response-prop
   :completionEvent (plist-put (cody--completion-event) prop value)))

(defun cody--completion-event-prop (prop)
  "Retrieve PROP from the completion event, if present."
  (plist-get (cody--completion-event) prop))

(defun cody--completion-event ()
  "Return the completion event passed along with the response.
As part of the completion lifecycle, we fill in some of its fields,
and we pass it back during telemetry logging."
  (plist-get cody--completion-response :completionEvent))

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
    (cody--set-completion-event-prop :acceptedAt (cody--timestamp))
    (condition-case err
        (jsonrpc-notify
         (cody--connection) 'graphql/log-event
         (cody--create-graphql-event "CodyEmacsPlugin:completion:accepted"
                                     (cody--add-completion-event-params
                                      nil (cody--completion-event-prop :params))))
      (error (cody--log "Telemetry error in completion:accepted: %s" err)))))

(defun cody--telemetry-completion-suggested ()
  (and cody-enable-telemetry cody--completion
    (let ((latency (max (- (cody--completion-timestamp :displayedAt)
                           (cody--completion-timestamp :triggeredAt)) 0))
          (duration (max (- (cody--timestamp) ; :hiddenAt
                            (cody--completion-timestamp :displayedAt)) 0))
          (params (cody--completion-event-prop :params)))
      (condition-case err
          (jsonrpc-notify
           (cody--connection) 'graphql/log-event
           (cody--create-graphql-event
            "CodyEmacsPlugin:completion:suggested"
            (cody--add-completion-event-params
             (list
              :latency latency
              :displayDuration duration
              :isAnyKnownPluginEnabled (cody--other-plugins-enabled-p))
             params)))
        (error (cody--log "Telemetry error in completion:suggested: %s" err))))))

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

(provide 'emacs-cody)
;;; emacs-cody.el ends here
