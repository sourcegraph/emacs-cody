;;; emacs-cody.el --- Sourcegraph Cody in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sourcegraph, Inc.

;; Version: 0.1
;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Maintainer: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; URL: https://github.com/keegancsmith/emacs-cody
;; Package-Requires: ((emacs "26.3") (jsonrpc "1.0.16"))

;;; Code:
(require 'cl-lib)
(require 'auth-source)
(require 'jsonrpc)

;;; TODO: Tests!

(defconst cody--cody-worker
  (file-name-concat (file-name-directory
                     (or load-file-name
                         (buffer-file-name)))
                    "dist" "cody-agent.js")
  "Path to bundled cody agent.
Customizing `cody-worker-binary` will override this default.")

(defvar cody--connection nil "")
(defvar cody--message-in-progress nil "")
(defvar cody--access-token nil "")

(defconst cody-log-buffer-name "*cody-log*" "")
(defconst cody-chat-buffer-name "*cody-chat*" "")

(defvar cody-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c") 'cody-request-completion)
    map)
  "Keymap for `cody-mode'.")

(defvar cody--use-threads nil
  "True to use threads; set to false for easier debugging.")

(defvar cody--typewriter-effect nil
  "True to use typewriter effect in Cody chat output.")

(defgroup cody nil
  "Sourcegraph Cody coding assistant"
  :group 'programming)

(defcustom cody-workspace-root (getenv "HOME")
  "Directory which Cody considers your current project root."
  :group 'cody
  :type 'string)

(defcustom cody-worker-binary nil
  "Location of pre-built Cody worker binary.
Setting this to non-nil overrides the default distributed Cody worker."
  :group 'cody
  :type 'string)

(defcustom cody-auto-enable-cody-mode t
  "Non-nil to enable Cody commands in all relevant buffers.
You can set this to nil if you need to run Cody only in a chat session,
or if you prefer to set up your own rules for enabling `cody-mode'."
  :group 'cody
  :type 'boolean)

(defconst cody--mode-hooks
  '((after-change-functions . cody--after-change)
    (kill-buffer-hook . cody--kill-buffer-function)
    (post-command-hook . cody--post-command-function))
  "List of buffer-local hooks that Cody registers on in `cody-mode'.
These hooks enable it to keep buffers and selections synced
with the Cody Worker.")

;; These are for keeping the worker notified about current file/selection.
(defvar cody--last-point nil "The last point position.")
(make-variable-buffer-local 'cody--last-point)
(defvar cody--last-mark nil "The last mark position.")
(make-variable-buffer-local 'cody--last-mark)

;; Add to your ~/.authinfo.gpg something that looks like
;;
;;  machine sourcegraph.sourcegraph.com login apikey password sgp_SECRET
(defun cody--access-token ()
  "Fetch and cache the access token from ~/.authinfo.gpg"
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

(defun cody--connection-configuration ()
  ""
  (list :accessToken (cody--access-token)
        :serverEndpoint "https://sourcegraph.sourcegraph.com"
        ;; Note there is a bug currently where the worker initialize request
        ;; fails if the serverEndpoint doesn't know the codebase.
        :codebase "https://github.com/sourcegraph/cody"))

(cl-defun cody--request (method &rest params &allow-other-keys)
  "Helper to send a Cody request."
  ;; TODO: Make requests cancellable and implement $/cancelRequest.
  (jsonrpc-request (cody--connection) method params))

(defun cody--alive-p ()
  "Return non-nil if the jsonrpc connection is still running."
  (and cody--connection
       (zerop (process-exit-status (jsonrpc--process cody--connection)))))

(defun cody--connection ()
  "Return the worker process, starting one if it is not already running."
  (unless (cody--alive-p)
    (let ((cody-command (if (and (boundp 'cody-worker-binary)
                                 (stringp 'cody-worker-binary)
                                 (file-executable-p cody-worker-binary))
                            (list cody-worker-binary "")
                          ;; TODO allow overriding location of node
                          (list "node" cody--cody-worker))))
      (setq cody--connection
            (make-instance
             'jsonrpc-process-connection
             :name "cody"
             :events-buffer-scrollback-size nil
             :notification-dispatcher #'cody--handle-worker-notification
             :process (make-process
                       :name "cody"
                       :command cody-command
                       :coding 'utf-8-emacs-unix
                       :connection-type 'pipe
                       :stderr (get-buffer-create "*cody stderr*")
                       :noquery t))))
    (message "Initializing Cody worker")

    ;; The 'initialize' request must be sent at the start of the connection
    ;; before any other request/notification is sent.
    (jsonrpc-request cody--connection 'initialize
                     (list
                      :name "emacs"
                      :version "0.1"
                      :workspaceRootPath (cody--workspace-root)
                      :connectionConfiguration (cody--connection-configuration)))

    ;; The 'initialized' notification must be sent after receiving the
    ;; 'initialize' response.
    (jsonrpc-notify cody--connection 'initialized nil))
  cody--connection)

(defun cody--workspace-root ()
  ;; TODO: Onboarding!
  ;; For now, you can configure cody-workspace-root to your project dir.
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
     (file-name-directory cody--cody-worker)))
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
Changes to the buffer will be tracked by the Cody worker"
  :lighter cody--minor-mode-icon
  :keymap cody-mode-map
  (if cody-mode
      (cody--minor-mode-startup)
    (cody--minor-mode-shutdown)))

(defun cody--minor-mode-startup ()
  "Code that runs when `cody-mode' is enabled in a buffer."
  (cl-loop for (hook . func) in cody--mode-hooks
           do (add-hook hook func nil t))
  (cody--send-file-to-worker (current-buffer) 'textDocument/didOpen)
  (message "Cody mode enabled"))

(defun cody--minor-mode-shutdown ()
  "Code that runs when `code-mode' is disabled in a buffer."
  (cl-loop for (hook . func) in cody--mode-hooks
           do (remove-hook hook func t))
  (setq cody-mode nil) ; this clears the modeline and other vars
  (message "Cody mode disabled"))

(defun cody--text-file-p (buf)
  "Return non-nil if BUF is visiting a text file.
A heuristic to see if we should notify the worker about it."
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

(defun cody--send-file-to-worker (buf op)
  "Make the jsonrpc call to notify worker of opened/changed file."
  (with-current-buffer buf
    (jsonrpc-notify (cody--connection) op
                    (list
                     :filePath (buffer-file-name buf)
                     :content (buffer-substring-no-properties (point-min)
                                                              (point-max))
                     :selection (cody--get-selection buf)))))

(defun cody--after-change (&rest _)
  "Implement `textDocument/didChange' notification.
Installed on `after-change-functions' buffer-local hook in `cody-mode'."
  (ignore-errors
    (when cody-mode
      (let ((happy nil))
        (unwind-protect
            (progn
              ;; Dispense with optimization and just blast the whole file over. Vavoom.
              (cody--send-file-to-worker (current-buffer)
                                        'textDocument/didChange)
              (setq happy t))
          (if happy
              (cody--log "Blasted whole file %s on small change" buffer-file-name)
            (cody--log "Unable to update Cody worker for %s" buffer-file-name)))))))


(defun cody--post-command-function ()
  "If point or mark has moved, update selection/focus with worker.
Installed on `post-command-hook', which see."
  (ignore-errors
    (when cody-mode
      (cody--debuggable-post-command))))

(defun cody--debuggable-post-command ()
  "Set your breakpoint for `cody--post-command-function` here instead."
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
  "Notify worker that cursor or selection has changed in current buffer."
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
BUF can be a buffer or buffer-name, and we return a Range with `start'
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
(defun cody--handle-worker-notification (_ method params)
  "Handle notifications from the worker, e.g. shutdown."
  (cl-case method
    (chat/updateMessageInProgress
     (cody--handle-chat-update params))
    (shutdown ; Server initiated shutdown.
     (cody-shutdown))))

(defun cody--kill-buffer-function ()
  "If we are killing the last buffer visiting this file, notify worker."
  (when (and cody-mode
             buffer-file-name
             (cody--last-buffer-for-file-p))
    (jsonrpc-notify (cody--connection) 'textDocument/didClose
                    (list :filePath buffer-file-name))
    (cody--log "Notified worker closed %s" buffer-file-name)))

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

;; The worker sends an update with increasingly long hunks of the response,
;; e.g. "Here", "Here is", "Here is an", "Here is an explanation", ...
;; This permits a typewriter effect.
(defun cody-chat-insert-msg-tail (params)
  "Insert only the most recent update to the message output.
This allows you to see the output stream in as it is generated.
Cody chat buffer should be current, and params non-nil."
  ;; TODO: Generated markdown breaks with this approach.
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
  "Stop the Cody worker process and turn Cody off globally."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when cody-mode
        (cody--minor-mode-shutdown))))
  (remove-hook 'find-file-hook #'cody--maybe-turn-on-cody-mode)
  (when (cody--alive-p)
    (cody--request 'shutdown) ; Required by the protocol
    (cody--kill-process)
    (message "Cody has shut down."))
  (setq cody--message-in-progress nil))

(defun cody-force-unload ()
  "Shut down Cody and remove all Cody-related buffers."
  (interactive)
  (cody-shutdown)
  (ignore-errors
    (kill-buffer (cody-chat-buffer)))
  (ignore-errors
    (kill-buffer cody-log-buffer-name)))

(defun cody--kill-process ()
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
  "Start the Cody worker. Optionally enables `cody-mode' in buffers.
This function is idempotent and only starts a new connection if needed.
Turning on `cody-mode' is set by the `cody-auto-enable-cody-mode'
customization option. This function essentially starts up the Cody
system, and you can call it from any Cody command wrapper to ensure
there is a connection."
  (interactive)
  (if (cody--alive-p)
      (unless quiet
        (message "Cody worker is already started."))
    (message "Initializing Cody worker...")
    (cody--connection)
    (message "Cody connection initialized.")
    (when cody-auto-enable-cody-mode
      (cody--init-cody-mode))))

(defun cody--init-cody-mode ()
  "Start a worker if needed, and enable Cody in applicable buffers."
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

(defun cody--log (msg &rest args)
  "Log a message, currently just for debugging"
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
  "Insert the user query into the chat output buffer."
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
          (set-buffer-modified-p nil)
          (markdown-mode))
        (current-buffer)))))

(defun cody-request-completion ()
  "Request autocompletion for the current buffer at the current point."
  (interactive)
  (unless cody-mode
    (error "Cody-mode not enabled in this buffer."))
  (let* ((buf (current-buffer))
         (file (buffer-file-name buf))
         (line (1- (line-number-at-pos)))
         (col (current-column))
         (result (jsonrpc-request (cody--connection)
                                  'autocomplete/execute
                                  (list :filePath file
                                        :position (list :line line :character col)))))
    (message "%s" result)))

(defun emacs-cody-unload-function ()
  "Handle `unload-feature' for this package."
  (cody-shutdown))

(provide 'emacs-cody)

;;; emacs-cody.el ends here
