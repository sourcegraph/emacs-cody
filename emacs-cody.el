;;; emacs-cody.el --- Sourcegraph Cody in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sourcegraph, Inc.

;; Version: 0.1
;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Maintainer: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; URL: https://github.com/keegancsmith/emacs-cody
;; Package-Requires: ((emacs "26.3") (jsonrpc "1.0.16"))

;;; Code:
(require 'auth-source)
(require 'jsonrpc)

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

(defconst cody-log-buffer-name "*cody-log*" "")
(defconst cody-chat-buffer-name "*cody-chat*" "")

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

(defcustom cody-agent-binary nil
  "Location of pre-built Cody agent binary.
Setting this to non-nil overrides the default distributed Cody agent."
  :group 'cody
  :type 'string)

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
    (add-hook 'find-file-hook 'cody--notify-find-file)
    (if cody--use-threads
        ;; TODO: Debugger is unable to exit if there's an error in the thread function.
        (make-thread (lambda ()
                       (cody--startup-notifications))
                     "Cody startup thread")
      (cody--startup-notifications))))

(defun cody--notify-find-file ()
  "Notify Cody when the user loads a file into a buffer.
Current buffer will be the newly opened file."
  (when (and (cody--alive-p) ;; Or Cody will resurrect every time you open a file...
             (cody--text-file-p (current-buffer)))
    (cody--notify-file)))

(defun cody--startup-notifications ()
  "Tell the new Cody agent process about all currently visited files.
This is done on a background thread, since there may be many files open
in the user's Emacs session when they start or restart Cody."
  (let ((file-count 0))
    (cl-loop
     for buf being the buffers
     when (cody--text-file-p buf)
     do
     (cody--notify-file buf)
     (cl-incf file-count)
     finally (message "Cody has been told about %s files" file-count))))

(cl-defun cody--notify-file (&optional (buf (current-buffer)))
  "Tell the agent that we are visiting a file.
This has the side effect of starting the agent if it is not running."
  (save-excursion
    (set-buffer buf)
    ;; Get the current selection, which is from the point to the point
    ;; (zero-width) if the mark is not set.
    (let ((start-line (1- (line-number-at-pos (point)))) ; 0-indexed
          (start-col (current-column)) ; 0-indexed
          (end-line (1- (line-number-at-pos (mark))))
          (end-col (save-excursion
                     (goto-char (or (mark) (point)))
                     (current-column))))
      (jsonrpc-notify (cody--connection) 'textDocument/didOpen
                      (list
                       :name (buffer-file-name buf)
                       :content (buffer-substring-no-properties (point-min)
                                                                (point-max))
                       :selection (list
                                   :start `(:line ,start-line :character ,start-col)
                                   :end `(:line ,end-line :character ,end-col))))
      (cody-log "Notified agent of %s (size %s)"
                (buffer-file-name buf)
                (point-max)))))

;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/JSONRPC-Overview.html
;; for a description of the parameters for this jsonrpc notification callback.
(defun cody--handle-notification (_ method params)
  "Handle notifications from the agent, e.g. shutdown."
  (cl-case method
    ('chat/updateMessageInProgress
     (cody--handle-chat-update params))
    ('shutdown
     ;; Server initiated shutdown.
     (cody--kill-process)
     (cody--request 'exit)
     (message "Cody has shut down."))))

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
    (let ((inhibit-read-only t))
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
  (when (cody--alive-p)
    (cody--request 'shutdown) ; Required by the protocol
    (cody--kill-process)
    (remove-hook 'find-file-hook 'cody--notify-find-file)
    (setq cody--initialized-p nil
          cody--message-in-progress nil)))

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
          (insert-image (create-image cody-logo) "Cody")
          (insert "Welcome to Cody. Type `M-x cody-help` for more info.\n")
          (set-buffer-modified-p nil)
          (markdown-mode)
          (current-buffer))))))

(defun cody--text-file-p (buf)
  "Return non-nil if BUF is visiting a text file.
A heuristic to see if we should notify the agent about it."
  ;; TODO: Maybe just ask the filesystem instead?
  (and (buffer-file-name buf)
       (not
        (memq (with-current-buffer buf buffer-file-coding-system)
              '(nil raw-text binary no-conversion)))))

;;; emacs-cody.el ends here
