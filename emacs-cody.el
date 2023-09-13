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

(defconst cody--cody-agent
  (file-name-concat (file-name-directory
                     (or load-file-name
                         (buffer-file-name)))
                    "dist" "cody-agent.js")
  "Path to distributed cody-agent.js.
Customizing `cody-agent-binary` will override this default.")

(defvar cody--connection nil "")
(defvar cody--message-in-progress nil "")
(defvar cody--access-token nil "")
(defvar cody--initialized-p nil "")

(defvar cody--last-point nil "The last point position.")
(make-variable-buffer-local 'cody--last-point)
(defvar cody--last-mark nil "The last mark position.")
(make-variable-buffer-local 'cody--last-mark)

(defconst cody-log-buffer-name "*cody-log*" "")
(defconst cody-chat-buffer-name "*cody-chat*" "")

(defvar cody--use-threads t
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

(defcustom cody-agent-binary nil
  "Location of pre-built Cody agent binary.
Setting this to non-nil overrides the default distributed Cody agent."
  :group 'cody
  :type 'string)

(defconst cody--standard-hooks
  '((after-change-functions . cody--after-change)
    (kill-buffer-hook . cody--kill-buffer-function)
    (post-command-hook . cody--post-command-function)
    (find-file-hook . cody--notify-find-file))
  "List of global hooks for which we register and unregister.")

;; These is primarily for keeping the agent notified about current file adn r positions.")

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
       (zerop (process-exit-status (jsonrpc--process cody--connection)))))

(defun cody--connection ()
  "Start the agent process if it is not already running."
  (unless (cody--alive-p)
    (let ((cody-command (if (and (boundp 'cody-agent-binary)
                                 (stringp 'cody-agent-binary)
                                 (file-executable-p cody-agent-binary))
                            (list cody-agent-binary "")
                          ;; TODO allow overriding location of node
                          (list "node" cody--cody-agent))))
      (setq cody--connection
            (make-instance
             'jsonrpc-process-connection
             :name "cody"
             :events-buffer-scrollback-size nil
             :notification-dispatcher #'cody--handle-notification
             :process (make-process
                       :name "cody"
                       :command cody-command
                       :coding 'utf-8-emacs-unix
                       :connection-type 'pipe
                       :stderr (get-buffer-create "*cody stderr*")
                       :noquery t))))
    (message "Initializing Cody agent")

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
    (jsonrpc-notify cody--connection 'initialized nil)
    (cody--initialize))
  cody--connection)

(defun cody--workspace-root ()
  ;; TODO: Onboarding!
  ;; For now, you can configure cody-workspace-root to your project dir.
  (or cody-workspace-root
      (and buffer-file-name (file-name-directory buffer-file-name))
      (getenv "HOME")))

(defun cody--initialize ()
  (unless cody--initialized-p
    (setq cody--initialized-p t)
    (cody--update-hooks 'add-hook)
    (if cody--use-threads
        ;; N.B. Debugger is unable to exit if there's an error in the thread function.
        (make-thread (lambda ()
                       (cody--startup-notifications))
                     "Cody startup thread")
      (cody--startup-notifications))))

(defun cody--notify-find-file ()
  "Notify Cody when the user loads a file into a buffer.
Current buffer will be the newly opened file."
  (when (cody-tracking-buffer-p) ;; Ensures Cody does not auto-start on file open.
    (cody--notify-file)))

(defun cody--startup-notifications ()
  "Tell the new Cody agent process about all currently visited files.
This is done on a background thread, since there may be many files open
in the user's Emacs session when they start or restart Cody."
  (let ((file-count 0))
    (cl-loop
     for buf being the buffers
     when (cody-tracking-buffer-p buf)
     do
     (add-hook 'post-command-hook 'cody--post-command-function)
     (cody--notify-file buf)
     (cl-incf file-count)
     finally (cody-log "Cody agent notified of %s open files" file-count))))

(cl-defun cody--notify-file (&optional (buf (current-buffer)))
  "Tell the agent that we are visiting a file.
This has the side effect of starting the agent if it is not running."
  (cody--send-file-to-agent buf 'textDocument/didOpen))

(defun cody--send-file-to-agent (buf op)
  (with-current-buffer buf
    (jsonrpc-notify (cody--connection) op
                    (list
                     :filePath (buffer-file-name buf)
                     :content (buffer-substring-no-properties (point-min)
                                                              (point-max))
                     :selection (cody--get-selection buf)))))

(defun cody--after-change (start end old-len)
  "Implement `textDocument/didChange` notification.
Installed on `after-change-functions'.
START and END are the start and end of the changed text.  OLD-LEN
is the pre-change length."
  (ignore-errors
    (when (cody-tracking-buffer-p)
      (let ((happy nil))
        (unwind-protect
            (progn
              ;; Dispense with optimization and just blast the whole file over. Vavoom.
              (cody--send-file-to-agent (current-buffer)
                                        'textDocument/didChange)
              (setq happy t))
          (if happy
              (cody-log "Blasted whole file %s on small change" buffer-file-name)
            (cody-log "Unable to update Cody agent for %s" buffer-file-name)))))))
    

(defun cody--post-command-function ()
  "If point or mark has moved, update selection/focus with agent.
Installed on `post-command-hook', which see."
  (ignore-errors
    (when (cody-tracking-buffer-p)
      cody--debuggable-post-command)))

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
  "Notify agent that cursor or selection has changed in current buffer."
  ;; Does not send the file contents. Olaf assured me you can leave it undefined
  ;; when you're just updating the selection or caret.
  (jsonrpc-notify (cody--connection) 'textDocument/didFocus
                  (list
                   :filePath (buffer-file-name buf)
                   ;; Specifically leave :content undefined here.
                   :selection (cody--get-selection buf))))

;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/JSONRPC-Overview.html
;; for a description of the parameters for this jsonrpc notification callback.
(defun cody--handle-notification (_ method params)
  "Handle notifications from the agent, e.g. shutdown."
  (cl-case method
    (chat/updateMessageInProgress
     (cody--handle-chat-update params))
    (shutdown
     ;; Server initiated shutdown.
     (cody--kill-process)
     (cody--request 'exit)
     (message "Cody has shut down."))))

(defun cody--kill-buffer-function ()
  "If we are killing the last buffer visiting this file, notify agent."
  (when (and buffer-file-name
             (cody--last-buffer-for-file-p))
      (jsonrpc-notify (cody--connection) 'textDocument/didClose
                      (list :filePath buffer-file-name))
      (cody-log "Notified agent closed %s" buffer-file-name)))


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

;; The agent sends an update with increasingly long hunks of the response,
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
  "Stop the Cody agent process."
  (interactive)
  (cody--update-hooks 'remove-hook)
  (when (cody--alive-p)
    (cody--request 'shutdown) ; Required by the protocol
    (cody--kill-process))
  (setq cody--initialized-p nil
        cody--message-in-progress nil))

(defun cody--update-hooks (op)
  "Add or remove each of our hooks.
OP is `add-hook' or `remove-hook'."
  ;; First add or remove us from the global hooks.
  (cl-loop for (hook . func) in cody--standard-hooks
           do (funcall op hook func))
  ;; Now go through every Cody-tracked buffer and add/remove the post-command-hook,
  ;; which for some reason gets set and wholly overrides/shadows the global value.
  (dolist (buffer (buffer-list))
    (when (cody-tracking-buffer-p buffer)
      (with-current-buffer buffer
        (funcall op 'post-command-hook #'cody--post-command-function)))))

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
  "Start the Cody agent. Mostly useful for debugging."
  (interactive)
  (if (cody--alive-p)
      (unless quiet
        (message "Cody is already started."))
    (message "Initializing Cody...")
    (cody--connection)
    (message "done")))

(defun cody-log (msg &rest args)
  "Log a message, currently just for debugging"
  ;; TODO: Use a real logging package
  (with-current-buffer (get-buffer-create cody-log-buffer-name)
    (goto-char (point-max))
    (insert (apply #'format msg args) "\n")))

(defun cody-chat ()
  "Shorthand for the chat recipe.
Query and output go into the *cody-chat* buffer."
  (interactive)
  (unless (cody--alive-p)
    (cody-start))
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
      (let ((cody-logo (file-name-concat ; hack the Cody logo path
                        ;; wtf emacs why is there no parent-directory function?
                        (file-name-directory
                         (directory-file-name
                          (file-name-directory cody--cody-agent)))
                        "cody-logo.png")))
        (with-current-buffer (get-buffer-create cody-chat-buffer-name)
          (buffer-disable-undo)
          (let ((inhibit-modification-hooks t))
            (insert-image (create-image cody-logo) "Cody")
            (insert "Welcome to Cody. Type `M-x cody-help` for more info.\n")
            (set-buffer-modified-p nil)
            (markdown-mode))
          (current-buffer))))))

(cl-defun cody-tracking-buffer-p (&optional (buf (current-buffer)))
  "Return non-nil if Cody is active/available in this buffer.
Currently it means the buffer is visiting a text file on disk."
  (and (cody--alive-p)
       (cody--text-file-p buf)))

(defun cody--text-file-p (buf)
  "Return non-nil if BUF is visiting a text file.
A heuristic to see if we should notify the agent about it."
  ;; TODO: Maybe just ask the filesystem instead?
  (and (buffer-file-name buf)
       (not
        (memq (with-current-buffer buf buffer-file-coding-system)
              '(nil raw-text binary no-conversion)))))

(defun cody-request-autocomplete ()
  "Request autocompletion for the current buffer at the current point."
  (interactive)
  (let* ((buf (current-buffer))
         (file (buffer-file-name buf))
         (line (1- (line-number-at-pos)))
         (col (current-column)))
    (jsonrpc-request (cody--connection)
                     'autocomplete/execute
                     (list :filePath file
                           :position (list :line line :character col)))))

;;; emacs-cody.el ends here
