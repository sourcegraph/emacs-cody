;;; cody.el --- Sourcegraph Cody in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sourcegraph, Inc.

;; Version: 0.2.0
;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Maintainer: Steve Yegge <steve.yegge@gmail.com>
;; URL: https://github.com/sourcegraph/emacs-cody
;; Package-Requires: ((emacs "26.3") (jsonrpc "1.0.16") (uuidgen "20240201.2318"))
;; Keywords: completion convenience languages programming tools

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
(require 'project)
(require 'ansi-color)
(require 'cody-diff)
(require 'cody-repo-util)

;;; Custom variables.

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

(defcustom cody--internal-anonymized-uuid nil
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

(defcustom cody-completions-display-marker-p nil
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

(defcustom cody-default-branch-name "main"
  "Default branch name for the current projectp."
  :group 'cody
  :type 'string)

(defcustom cody-remote-url-replacements ""
  "Whitespace-separated pairs of replacements for repo URLs."
  :group 'cody
  :type 'string)

(defgroup cody-dev nil
  "Cody developer/contributor configuration settings."
  :group 'cody
  :prefix "cody--dev-")

(defcustom cody--dev-node-executable nil
  "Hardwired path to the nodejs binary to use for Cody.
If nil, Cody will search for node using variable `exec-path'."
  :group 'cody
  :type 'string)

(defcustom cody--dev-node-min-version "20.4.0"
  "The minimum required version of Node.js."
  :group 'cody-dev
  :type 'string)

(defcustom cody--dev-use-remote-agent nil
  "Non-nil to connect to an agent running on `cody--dev-remote-agent-port`.
This is a setting for contributors to Cody-Emacs."
  :group 'cody-dev
  :type 'boolean)

(defcustom cody--dev-remote-agent-port 3113
  "The port on which to attach to a remote Agent.
The remote Agent is typically started by an IDE such as VS code,
and enables you to set breakpoints on both sides of the protocol."
  :group 'cody-dev
  :type 'number)

;; TODO: When this value changes, if cody is alive, notify the agent.
(defcustom cody--dev-enable-agent-debug-p nil
  "Non-nil to enable debugging in the agent.
Sends this flag as part of the agent extension configuration."
  :group 'cody-dev
  :type 'boolean)

(defcustom cody--dev-enable-agent-debug-verbose-p nil
  "Non-nil to enable verbose debugging in the agent.
Sends this flag as part of the agent extension configuration."
  :group 'cody-dev
  :type 'boolean)

(defcustom cody--dev-panic-on-doc-desync nil
  "Non-nil to ask the Agent to panic if we discover it is desynced.
De-syncing is when the Agent's copy of a document is out of sync with
the actual document in Emacs. Setting this customn variable to non-nil,
which should only be done in development, sends extra metadata along
with document changes, which the Agent will compare against."
  :group 'cody-dev
  :type 'boolean)

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

(defclass cody-llm-site-configuration ()
  ((chatModel :initarg :chatModel
              :initform nil
              :type (or null string)
              :accessor cody--llm-chat-model
              :documentation "Chat model.")
   (chatModelMaxTokens :initarg :chatModelMaxTokens
                       :initform nil
                       :type (or null integer)
                       :accessor cody--llm-chat-model-max-tokens
                       :documentation "Chat model max tokens.")
   (fastChatModel :initarg :fastChatModel
                  :initform nil
                  :type (or null string)
                  :accessor cody--llm-fast-chat-model
                  :documentation "Fast chat model.")
   (fastChatModelMaxTokens :initarg :fastChatModelMaxTokens
                           :initform nil
                           :type (or null integer)
                           :accessor cody--llm-fast-chat-model-max-tokens
                           :documentation "Fast chat model max tokens.")
   (completionModel :initarg :completionModel
                    :initform nil
                    :type (or null string)
                    :accessor cody--llm-completion-model
                    :documentation "Completion model.")
   (completionModelMaxTokens :initarg :completionModelMaxTokens
                             :initform nil
                             :type (or null integer)
                             :accessor cody--llm-completion-model-max-tokens
                             :documentation "Completion model max tokens.")
   (provider :initarg :provider
             :initform nil
             :type (or null string)
             :accessor cody--llm-provider
             :documentation "Provider.")
   (smartContextWindow :initarg :smartContextWindow
                       :initform nil
                       :type (or null boolean)
                       :accessor cody--llm-smart-context-window
                       :documentation "Smart context window."))
  "Class representing Cody LLM site configuration.")

;; Update the cody-auth-status class
(defclass cody-auth-status ()
  ((username :initarg :username
             :initform ""
             :type string
             :accessor cody--auth-status-username
             :documentation "The username of the authenticated user.")
   (endpoint :initarg :endpoint
             :initform nil
             :type (or null string)
             :accessor cody--auth-status-endpoint
             :documentation "The endpoint used for authentication.")
   (isDotCom :initarg :isDotCom
             :initform nil
             :type boolean
             :accessor cody--auth-status-is-dotcom
             :documentation "Whether the server is dot-com.")
   (isLoggedIn :initarg :isLoggedIn
               :initform nil
               :type boolean
               :accessor cody--auth-status-is-logged-in
               :documentation "Whether the user is logged in.")
   (showInvalidAccessTokenError :initarg :showInvalidAccessTokenError
                                :initform nil
                                :type boolean
                                :accessor cody--auth-status-show-invalid-token-error
                                :documentation "Show invalid access token error.")
   (authenticated :initarg :authenticated
                  :initform nil
                  :type boolean
                  :accessor cody--auth-status-authenticated
                  :documentation "Whether the user is authenticated.")
   (hasVerifiedEmail :initarg :hasVerifiedEmail
                     :initform nil
                     :type boolean
                     :accessor cody--auth-status-has-verified-email
                     :documentation "Whether the user has a verified email.")
   (requiresVerifiedEmail :initarg :requiresVerifiedEmail
                          :initform nil
                          :type boolean
                          :accessor cody--auth-status-requires-verified-email
                          :documentation "Whether user requires a verified email.")
   (siteHasCodyEnabled :initarg :siteHasCodyEnabled
                       :initform nil
                       :type boolean
                       :accessor cody--auth-status-site-has-cody-enabled
                       :documentation "Whether the site has Cody enabled.")
   (siteVersion :initarg :siteVersion
                :initform ""
                :type string
                :accessor cody--auth-status-site-version
                :documentation "The version of the site.")
   (codyApiVersion :initarg :codyApiVersion
                   :initform 1
                   :type integer
                   :accessor cody--auth-status-cody-api-version
                   :documentation "The API version of Cody.")
   (configOverwrites :initarg :configOverwrites
                     :initform nil
                     :type (or null cody-llm-site-configuration)
                     :accessor cody--auth-status-config-overwrites
                     :documentation "LLM configuration overwrites.")
   (showNetworkError :initarg :showNetworkError
                     :initform nil
                     :type (or null boolean)
                     :accessor cody--auth-status-show-network-error
                     :documentation "Show network error status.")
   (primaryEmail :initarg :primaryEmail
                 :initform ""
                 :type string
                 :accessor cody--auth-status-primary-email
                 :documentation "The primary email of the authenticated user.")
   (displayName :initarg :displayName
                :initform nil
                :type (or null string)
                :accessor cody--auth-status-display-name
                :documentation "The display name of the authenticated user.")
   (avatarURL :initarg :avatarURL
              :initform ""
              :type string
              :accessor cody--auth-status-avatar-url
              :documentation "The avatar URL of the authenticated user.")
   (userCanUpgrade :initarg :userCanUpgrade
                   :initform nil
                   :type boolean
                   :accessor cody--auth-status-user-can-upgrade
                   :documentation "Whether the user can upgrade their plan."))
  "Class representing authentication status.")

(defclass cody-server-info ()
  ((name :initarg :name
         :initform ""
         :type string
         :accessor cody--server-info-name
         :documentation "The name of the server.")
   (authenticated :initarg :authenticated
                  :initform nil
                  :type (or null boolean)
                  :accessor cody--server-info-authenticated
                  :documentation "Whether the server is authenticated.")
   (codyEnabled :initarg :codyEnabled
                :initform nil
                :type (or null boolean)
                :accessor cody--server-info-cody-enabled
                :documentation "Whether Cody is enabled on the server.")
   (codyVersion :initarg :codyVersion
                :initform nil
                :type (or null string)
                :accessor cody--server-info-cody-version
                :documentation "The version of Cody on the server.")
   (authStatus :initarg :authStatus
               :initform nil
               :type (or null cody-auth-status)
               :accessor cody--server-info-auth-status
               :documentation "The authentication status of the server."))
  "Class representing server information.")

(defconst cody--dotcom-url "https://sourcegraph.com/")

(defconst cody--cody-agent
  (file-name-concat (file-name-directory (or load-file-name
                                             (buffer-file-name)))
                    "dist" "index.js")
  "Path to bundled cody agent.")

(defconst cody--dev-node-min-version "20.4.0"
  "The minimum required version of node.js for Cody.")

(defvar cody--node-version-status nil
  "Non-nil after `cody--check-node-version' is called.
The node version is only checked on Cody startup.
You can call `cody-restart' to force it to re-check the version.")

(defvar cody--unit-testing-p nil
  "Set to non-nil during unit testing.
When testing, all calls to the agent are diverted.")

(defvar cody--integration-testing-p nil
  "Set to non-nil during integration tests.
When testing, the calls go through to the LLM.")

(defvar cody--connection nil "Main jsonrpc connection to Agent.")

(defvar cody--server-info nil
  "ServerInfo struct sent from the `initialize' handshake.")

(defvar cody--status 'disconnected
  "Current status of Cody connection.
If the status is `error', then `cody--status' will have an `error'
property whose value is the last error received.")

(defvar cody--message-in-progress nil "Chat message accumulator.")

(defvar cody--sourcegraph-host "sourcegraph.com"
  "Sourcegraph host.")

(defvar cody--access-token nil
  "Access token for `cody--sourcegraph-host'.")

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
    (window-selection-change-functions . cody--handle-focus-changed)
    (window-buffer-change-functions . cody--handle-focus-changed)
    (post-command-hook . cody--post-command)
    (kill-buffer-hook . cody--handle-doc-closed)
    (activate-mark-hook . cody--handle-selection-change)
    (deactivate-mark-hook . cody--handle-selection-change))
  "List of buffer-local hooks that Cody registers on in `cody-mode`.
These hooks enable it to keep buffers and selections synced
with the Cody Agent.")

(defvar cody--overlay-deltas nil
  "List of overlays for Cody current completion suggestion.")

(defvar cody--completion nil
  "Most recent completion response object from the Cody Agent.
This is an instance of a variable `cody-completion' object.
Each time we request a new completion, it gets discarded and replaced.")

(defvar cody--completion-timer nil
  "Maybe trigger a completion each time Emacs goes idle.")

(defvar cody--completion-last-trigger-spot nil "Temp variable.")

(defsubst cody--cc ()
  "Return the current buffer-local version of `cody--completion'."
  cody--completion)

(defvar cody--completion-timestamps nil
  "Tracks event timestamps for telemetry, as plist properties.")

(defvar cody--vars nil
  "Symbol used as scratch space for ephemeral temp variables.
Typically used for allowing before/after hooks to communicate data.
Symbol properties are used reduce namespace clutter.")

(defvar cody-connection-initialized-hook nil
  "Hook run after `cody--connection' initializes the connection.
If the connection failed, then `cody--status' will be `error',
and the symbol `cody--status' will have an `error' property.")

(defvar cody--custom-variables
  '(cody-telemetry-enable-p
    cody-workspace-root
    cody-completions-auto-trigger-p
    cody-completions-display-marker-p
    cody-completions-enable-cycling-p
    cody-completions-cycling-help-p
    cody-default-branch-name
    cody-remote-url-replacements
    cody--internal-anonymized-uuid
    cody--dev-node-executable
    cody--dev-node-min-version
    cody--dev-use-remote-agent
    cody--dev-remote-agent-port
    cody--dev-enable-agent-debug-p
    cody--dev-enable-agent-debug-verbose-p)
  "List of custom variables to display in `cody-dashboard'.")

(defconst cody--defgroups '(cody cody-completions cody-dev)
  "List of Cody-related `defgroup's to include in `cody-dashboard'.")

(defsubst cody--timestamp ()
  "Return seconds since epoch."
  (float-time (current-time)))

(defvar-local cody--buffer-state nil
  "The state of Cody in the current buffer.
Can be any of nil, `active', `inactive', 'error', or `ignored'.

If nil, the buffer's state has not yet been evaluated.  Inactive means
its workspace is not currently live, so Cody is not tracking changes to
the file nor using it for context. Ignored means that an admin has
configured the file or repo to be excluded from Cody. The `error' state
usually means communication with the backend is down.")

(defvar-local cody--last-selection nil
  "Stores the last known selection range to detect changes.")

(defvar cody-mode-menu)

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

(defun cody--compute-logo-height ()
  "Compute the desired height for the Cody logo as 85% of the mode line height."
  (truncate (* 0.85 (frame-char-height))))

(defvar-local cody--mode-line-icon-evaluator
  '(:eval (condition-case err
              (when cody-mode
                (let ((icon
                       (cl-case cody--buffer-state
                         (active (cody--icon-cody-logo-small))
                         (inactive (cody--icon-logo-monotone))
                         (error (cody--icon-logo-disabled))
                         (ignored (cody--icon-logo-disabled))
                         (otherwise nil))))
                  (cody--decorate-mode-line-lighter icon)))
            (error (cody--log "Error in mode line evaluator: %s" err))))
  "Descriptor for producing a custom menu in the mode line lighter.")

;; Utilities

(defmacro cody--call-safely (func &rest args)
  "Call FUNC with ARGS safely. Report an error if the call fails."
  `(condition-case err
       (apply ,func ,args)
     (error (cody--log "Error calling %s: %s" (if (symbolp ,func)
                                                  (symbol-name ,func)
                                                "a lambda")
                       err))))

(defsubst cody--bol ()
  "Alias for `line-beginning-position'."
  (line-beginning-position))

(defsubst cody--eol ()
  "Alias for `line-end-position'."
  (line-beginning-position))

(defsubst cody--buffer-string ()
  "Return the entire current buffer's contents as a string."
  (without-restriction
    (buffer-substring-no-properties (point-min) (point-max))))

(defsubst cody--convert-json-false (val)
  "Convert JSON false (:json-false) to nil. Leave other values unchanged."
  (if (eq val :json-false) nil val))

(defun cody--buffer-visible-p (&optional buf)
  "Return non-nil if BUF is active. BUF defaults to the current buffer."
  (or cody--unit-testing-p
      (let ((buffer (or buf (current-buffer))))
        (and (eq buffer (window-buffer (selected-window)))
             (get-buffer-window buffer t)))))

(defun cody--agent-command ()
  "Command and arguments for running agent."
  (let ((node-executable (or cody--dev-node-executable (executable-find "node"))))
    (unless node-executable
      (error "Node.js executable not found in exec-path"))
    (list node-executable
          "--inspect"
          "--enable-source-maps"
          cody--cody-agent)))

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
    (setq cody--status 'disconnected)
    (cody--connection-create-process)
    (if (cody--alive-p)
        (progn
          (cody--activate-event-log-mode)
          (cody--initialize-connection))
      (cody--log "Failed to start cody agent process: %s" cody--connection)
      (setq cody--connection nil)))
  cody--connection)

(defun cody--connection-create-process ()
  "Create a local or network process for talking to the agent.
Sets the `cody--connection' variable to the resulting process,
and returns it."
  (let ((process-environment (cody--agent-process-environment)))
    (setq cody--connection
          (make-instance
           'jsonrpc-process-connection
           :name "cody"
           :events-buffer-config nil
           :notification-dispatcher #'cody--handle-agent-notification
           :process
           (if cody--dev-use-remote-agent
               (make-network-process
                :name "cody"
                :host 'local
                :service cody--dev-remote-agent-port
                :coding 'utf-8-emacs-unix
                :noquery t)
             (make-process
              :name "cody"
              :command (cody--agent-command)
              :coding 'utf-8-emacs-unix
              :connection-type 'pipe
              :stderr (get-buffer-create "*cody stderr*")
              :noquery t))))))

(defun cody--agent-process-environment ()
  "Return the environment variables to set in the Agent."
  (list
   (format "PATH=%s" (getenv "PATH")) ; Propagate PATH
   (concat "CODY_CLIENT_INTEGRATION_TESTING="
           (if cody--integration-testing-p "true" "false"))))

(defun cody-populate-server-info (response)
  "Populate the ServerInfo instance from the RESPONSE.
Returns a `cody-server-info' instance."
  (cl-labels ((cjf (val) (cody--convert-json-false val))
              (create-cody-llm-site-configuration (config)
                (when config
                  (make-instance
                   'cody-llm-site-configuration
                   :chatModel (plist-get config :chatModel)
                   :chatModelMaxTokens (cjf (plist-get config :chatModelMaxTokens))
                   :fastChatModel (plist-get config :fastChatModel)
                   :fastChatModelMaxTokens (cjf (plist-get config
                                                           :fastChatModelMaxTokens))
                   :completionModel (plist-get config :completionModel)
                   :completionModelMaxTokens (cjf (plist-get
                                                   config :completionModelMaxTokens))
                   :provider (plist-get config :provider)
                   :smartContextWindow (cjf (plist-get config :smartContextWindow))))))
    (let* ((auth-status (plist-get response :authStatus))
           (config-overwrites (create-cody-llm-site-configuration
                               (plist-get auth-status :configOverwrites)))
           (auth-status-instance
            (condition-case err
                (make-instance
                 'cody-auth-status
                 :username (plist-get auth-status :username)
                 :endpoint (cjf (plist-get auth-status :endpoint))
                 :isDotCom (cjf (plist-get auth-status :isDotCom))
                 :isLoggedIn (cjf (plist-get auth-status :isLoggedIn))
                 :showInvalidAccessTokenError (cjf (plist-get
                                                    auth-status
                                                    :showInvalidAccessTokenError))
                 :authenticated (cjf (plist-get auth-status :authenticated))
                 :hasVerifiedEmail (cjf (plist-get auth-status :hasVerifiedEmail))
                 :requiresVerifiedEmail (cjf (plist-get auth-status
                                                        :requiresVerifiedEmail))
                 :siteHasCodyEnabled (cjf (plist-get auth-status :siteHasCodyEnabled))
                 :siteVersion (plist-get auth-status :siteVersion)
                 :codyApiVersion (cjf (plist-get auth-status :codyApiVersion))
                 :configOverwrites config-overwrites
                 :showNetworkError (cjf (plist-get auth-status :showNetworkError))
                 :primaryEmail (plist-get auth-status :primaryEmail)
                 :displayName (plist-get auth-status :displayName)
                 :avatarURL (plist-get auth-status :avatarURL)
                 :userCanUpgrade (cjf (plist-get auth-status :userCanUpgrade)))
              (error
               (message "Error creating cody-auth-status instance: %s" err)
               (cody--set-error "Failed to create cody-auth-status instance" err)
               (signal 'error err)))))
      (make-instance 'cody-server-info
                     :name (plist-get response :name)
                     :authenticated (cjf (plist-get response :authenticated))
                     :codyEnabled (cjf (plist-get response :codyEnabled))
                     :codyVersion (plist-get response :codyVersion)
                     :authStatus auth-status-instance))))

(defun cody--initialize-connection ()
  "Required handshake exchanging ClientInfo and ServerInfo."
  (condition-case err
      (let ((response
             (cody--request 'initialize
                            (list
                             :name "Emacs"
                             :version "0.2"
                             :workspaceRootUri (cody--workspace-root)
                             :capabilities (cody--client-capabilities)
                             :extensionConfiguration (cody--extension-configuration)))))
        (setq cody--server-info (cody-populate-server-info response)))
    (error (cody--set-error err)))
  (condition-case err
      (run-hooks 'cody-connection-initialized-hook)
    (error (cody--log "Error in `cody--initialize-connection': %s" err)))
  (condition-case err
      (cody--notify 'initialized nil)
    (error (cody--log "Error calling 'initialize': %s" err))))

(defun cody--client-capabilities ()
  "Return the features that we support in the Emacs client."
  (list :edit "enabled"
        :editWorkspace "enabled"
        :codeLenses "enabled"
        :showDocument "enabled"
        :ignore "none"
        :untitledDocuments "enabled"
        :progressBars "none"))

(defun cody--extension-configuration ()
  "Which `ExtensionConfiguration' parameters to send on Agent handshake."
  (list :anonymousUserID (cody--internal-anonymized-uuid)
        :serverEndpoint (concat "https://" cody--sourcegraph-host "/")
        :accessToken (cody--access-token)
        :debug cody--dev-enable-agent-debug-p
        :debug-verbose cody--dev-enable-agent-debug-verbose-p
        :codebase (cody--uri-for (cody--workspace-root))
        :customConfiguration (list (cons :cody.experimental.foldingRanges
                                         "indentation-based"))))

(defun cody--notify-configuration-changed ()
  "Notify the agent that the extension configuration has changed."
  (let ((auth-status
         (cody--request 'extensionConfiguration/change
                        (cody--extension-configuration))))
    (unless (plist-get auth-status :authenticated)
      (cody--log "Cody is no longer authenticated after config change: %s"
                 auth-status)
      (message "Cody needs to re-authenticate")
      (cody-logout))))

(defun cody--set-workspace-root (new-root)
  "Instruct the Agent to switch to NEW-ROOT as its workspace.
NEW-ROOT is an absolute path to a directory containing the workspace.
This discards all mirrored documents from the previous workspace."
  (setq cody-workspace-root new-root)
  (cody--notify-configuration-changed))

(defun cody--set-error (msg &optional err)
  "Sets the plugin into an error state with MSG and ERR."
  (cody--log "Error: %s %s" msg (if err (format "(%s)" err) ""))
  (setq cody--status 'error)
  (when err (put 'cody--status 'error err))
  (force-mode-line-update t))

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
    (apply #'jsonrpc-async-request (cody--connection) method params args)))

(defun cody--check-node-version ()
  "Signal an error if the default node.js version is too low.
Min version is configurable with `cody--dev-node-min-version'."
  (cond
   (cody--dev-use-remote-agent
    t)
   ((eq cody--node-version-status 'good)
    t)
   ((eq cody--node-version-status 'bad)
    (error "Installed nodejs must be at least %s." cody--dev-node-min-version))
   (t
    (let* ((cmd (concat (or cody--dev-node-executable "node") " -v"))
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
        (let* ((min-version-parts (split-string cody--dev-node-min-version "\\."))
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
             node-version cody--dev-node-min-version))))))))

(defun cody--workspace-root ()
  "Return the workspace root for the Agent.
You can override it with `cody-workspace-root'.
Will always return a value that is non-nil, and tries hard to
make it a reasonable root for the current project."
  (or cody-workspace-root
      (ignore-errors
        (project-root (project-current)))
      (let ((home-from-env (getenv "HOME")))
        (if (and home-from-env (file-accessible-directory-p home-from-env))
            home-from-env
          ;; Inline locate-dominating-file to find a readable directory
          (or (locate-dominating-file default-directory
                                      (lambda (dir)
                                        (file-accessible-directory-p dir)))
              "/tmp")))))

(defun cody--get-custom-request-headers-as-map (custom-request-headers)
  "Convert CUSTOM-REQUEST-HEADERS string to a map."
  (let ((pairs (split-string custom-request-headers ",")))
    (cl-loop for (key value) on pairs by #'cddr
             collect (cons (string-trim key) (string-trim value)))))

;;; Code for cody minor mode:

(defun cody--logo-file (file-base)
  "Construct path to bundled cody image file.
Argument FILE-BASE is the file base name sans directory."
  (file-name-concat
   (file-name-directory (directory-file-name
                         (file-name-directory cody--cody-agent)))
   "icons"
   file-base))

(defun cody-current-theme-dark-p ()
  "Return t if the current theme is considered dark."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defmacro define-cached-icon (base-name &optional dual-theme)
  "Define a function that returns a cached image for BASE-NAME.
This function considers the current theme (light or dark) and returns
an appropriate version of the icon. If DUAL-THEME is non-nil,
it indicates that the icon works for both light and dark themes."
  (let* ((name (intern (format "cody--icon-%s" base-name)))
         (light-filename (concat base-name ".png"))
         (dark-filename (if dual-theme
                            (concat base-name ".png")
                          (concat base-name "_dark.png"))))
    `(defun ,name ()
       (let* ((file (if (cody-current-theme-dark-p) ,dark-filename ,light-filename))
              (img (or (get ',name 'cached-image)
                       (put ',name 'cached-image
                            (create-image (cody--logo-file file))))))
         (plist-put (cdr img) :height (cody--compute-logo-height))
         img))))

(defmacro create-icon-functions ()
  "Create functions for all necessary icons in the icons directory."
  (let ((theme-dependent-icons '("logo-disabled"
                                 "logo-monotone"
                                 "error"))
        (fixed-icons '("cody-logo-small"
                       "cody-logo"
                       "logo-mono-unavailable")))
    `(progn
       ;; Generate functions for theme-dependent icons
       ,@(mapcar (lambda (name)
                   `(define-cached-icon ,name))
                 theme-dependent-icons)
       ;; Generate functions for fixed icons
       ,@(mapcar (lambda (name)
                   `(define-cached-icon ,name t))
                 fixed-icons))))

;; Execute the macro to create the necessary functions
(create-icon-functions)

(defun cody--mode-line-click (_event)
  "Handle mouse click EVENT on Cody mode line item."
  (interactive "e")
  (popup-menu cody-mode-menu))

(defun cody-propertize-icon (text-or-image)
  "Return propertized string or image for `cody--minor-mode-icon`.
Argument TEXT-OR-IMAGE is the string or image to propertize."
  (let ((buffer-state (buffer-local-value 'cody--buffer-state (current-buffer)))
        (help-echo (if cody--buffer-state
                       (format "Cody mode (%s) - click for menu" cody--buffer-state)
                     "Cody mode - click for menu")))
    (propertize (if (stringp text-or-image) text-or-image " ")
                'display (if (stringp text-or-image) nil text-or-image)
                'help-echo help-echo
                'mouse-face 'mode-line-highlight
                'keymap cody-mode-line-map)))

(defun cody--decorate-mode-line-lighter (image)
  "Use the passed IMAGE for the mode line lighter."
  (let ((icon
         (if-let ((img (and (display-graphic-p) image)))
             ;; Hack - bump the image up a bit vertically using :ascent, to center it.
             (cody-propertize-icon (cons 'image (plist-put (cdr img) :ascent 80)))
           (cody-propertize-icon " Cody")))
        (red-exclamation (if (eq cody--status 'error)
                             (propertize "!" 'face '(:foreground "red"))
                           "")))
    (concat icon red-exclamation)))

;;;###autoload
(define-minor-mode cody-mode
  "Minor mode for interacting with the Cody coding assistant."
  ;; Never did figure out how to get this option to display a custom menu.
  ;; Currently going with a hack that puts it up at a higher level in the
  ;; mode line, but does produce a custom menu when mouse-1 clicked.
  ;; Bounty to anyone who can figure out how to do it correctly.
  ;;
  ;;:lighter cody--minor-mode-icon
  :keymap cody-mode-map
  (if cody-mode
      (cody--mode-startup)
    (cody--mode-shutdown)))

(defun cody--mode-startup ()
  "Code to run when `cody-mode' is turned on in a buffer."
  (unless (cody--alive-p)
    (cody-login))
  (when (cody--alive-p)
    (cl-loop for (hook . func) in cody--mode-hooks
             do (add-hook hook func nil 'local))
    (add-to-list 'mode-line-modes cody--mode-line-icon-evaluator)
    (force-mode-line-update t)
    (cody--handle-focus-changed (selected-window)))
  (cody--buffer-init-state))

(defun cody--mode-shutdown ()
  "Code to run when `cody-mode' is turned off in a buffer."
  (cody--completion-discard)
  (cl-loop for (hook . func) in cody--mode-hooks
           do (remove-hook hook func 'local))
  (cody--completion-cancel-timer)
  (setq cody-mode nil)) ; clears the modeline and buffer-locals

;;;###autoload
(define-global-minor-mode cody--global-mode
  cody-mode cody--turn-on-if-applicable)

(defun cody--turn-on-if-applicable ()
  "Turn on `cody-mode' in this buffer if it satisfies the preconditions."
  (when (cody--should-auto-enable-cody-p (current-buffer))
    (cody-mode 1)))

(defun cody--should-auto-enable-cody-p (buffer)
  "Check if Cody mode should be automatically enabled in BUFFER."
  (and buffer-file-name ; TODO: support untitled documents
       ;; Don't automatically turn on in buffers during integration testing.
       (not cody--integration-testing-p)
       (with-current-buffer buffer
         (derived-mode-p 'text-mode 'prog-mode))))

(defun cody--mode-active-p ()
  "Return non-nil if `cody-mode' is enabled and `cody--buffer-state' is `active'."
  (and cody-mode (eq cody--buffer-state 'active)))

(defun cody--before-change (beg end)
  "Handle deletions, which are only visible before the change.
BEG and END are as per the contract of `before-change-functions'."
  (condition-case err
      (progn
        (when (cody--overlay-visible-p)
          (without-restriction
            ;; Store the range and old content from before the change.
            (put 'cody--vars 'cody--before-change-state
                 (list :range (list :start beg :end end)
                       :old-content (buffer-substring-no-properties beg end)))))
        (when (< beg end) ; handle deletions only
          (cody--on-doc-change beg end "")))
    (error (cody--log "Error in ``cody--before-change: %s" err))))

(defun cody--after-change (beg end old-length)
  "Handle insertions, which are only visible after the change is applied.
BEG and END are the range of the changed text; they are the same for deletions.
OLD-LENGTH is the length of the pre-change text replaced by that range."
  (condition-case err
      (when (< beg end) ; insert/replace
        (cody--on-doc-change
         ;; Although beg==end means deletions to Emacs here, the protocol
         ;; expects beg==end for insertions, so we use beg for both:
         beg beg
         ;; The change has been applied, so we can get the inserted text.
         (without-restriction
           (buffer-substring-no-properties beg end))))
    (error (cody--log "Error in `cody--after-change': %s" err))))

(defun cody--on-doc-change (beg end text)
  "Common code for `cody--before-change' and `cody--after-change'."
  (when (cody--mode-active-p)
    (cody--notify-doc-did-change beg end text)
    (cody--recompute-or-hide-overlay)))

(defun cody--recompute-or-hide-overlay ()
  ;; Either recompute or hide the overlay, based on the change.
  (when-let* ((before-state (get 'cody--vars 'cody--before-change-state))
              (range (plist-get before-state :range))
              (deleted-text (plist-get before-state :old-content))
              (before-beg (plist-get range :start))
              (before-end (plist-get range :end))
              (len (length deleted-text)))
    (if (cody--overlay-visible-p)
        (cody--handle-typing-while-suggesting before-beg before-end len)
      (cody--completion-start-timer))))

(defun cody--handle-doc-closed ()
  "Notify agent that the document has been closed."
  (when (and cody-mode
             buffer-file-name
             (eq cody--buffer-state 'active))
    (cody--notify 'textDocument/didClose
                  (list :uri (cody--uri-for (buffer-file-name))))))

(defun cody--handle-focus-changed (window)
  "Notify agent that a document has been focused or opened."
  ;; N.B. Don't check `cody--mode-active-p' here, as it causes tests to fail.
  ;; We can safely assume that we're only called when `cody-mode' is active.
  (when (eq window (selected-window))
    (cody--focus-or-open)
    (if (cody--overlay-visible-p)
        (cody--completion-hide)
      (cody--completion-start-timer))))

(defun cody--focus-or-open ()
  "Initialize the state, and decide whether to tell agent doc focused or opened.
Current buffer is visiting the document under consideration."
  (let* ((old-state cody--buffer-state)
         (new-state (cody--buffer-init-state))
         (already-open (eq old-state new-state)))
    (when (eq new-state 'active)
      (if already-open
          (cody--notify-doc-did-focus)
        (cody--notify-doc-did-open)))))

(defun cody--handle-selection-change ()
  "Handle changes in the selection or region."

  ;; To CHOP:
  ;;  - fix this to hide the completion only if point has gone off the line
  ;;  - maybe have design discussion first
  ;;  - you may need to have it implement a function returning the line of the current overlay

  ;; (when (cody--overlay-visible-p)
  ;;   (cody--completion-hide))

  (cody--completion-start-timer)
  (let ((current-selection (cody--selection-get-current)))
    (unless (equal current-selection cody--last-selection)
      (setq cody--last-selection current-selection)
      (condition-case err
          (cody--notify-doc-did-focus)
        (error (cody--log "Error in `cody--handle-selection-change': %s" err))))))

(defun cody--buffer-init-state ()
  "Initialize `cody--buffer-state' for the current buffer.
Returns the new value."
  (let ((alive (cody--alive-p))
        (uri (cody--uri-for (current-buffer)))
        (workspace-uri (cody--uri-for (cody--workspace-root))))
    (setq cody--buffer-state
          (cond
           ((not alive) 'error)
           ;; Until we get multiple workspaces supported, only make
           ;; Cody "active" in files under the current workspace root.
           ((and uri (string-prefix-p workspace-uri uri))
            'active)
           (t 'inactive)))))

(defun cody--notify-doc-did-open ()
  "Inform the agent that the current buffer's document just opened."
  (cody--notify 'textDocument/didOpen
                (list
                 :uri (cody--uri-for (buffer-file-name))
                 :content (buffer-substring-no-properties (point-min) (point-max))
                 :selection (cody--selection-get-current))))

(defun cody--notify-doc-did-focus ()
  "Inform the agent that the current buffer's document was just focusd."
  (cody--notify 'textDocument/didFocus
                (list ; Don't include :content, as it didn't change.
                 :uri (cody--uri-for (buffer-file-name))
                 :selection (cody--selection-get-current))))

(defun cody--notify-doc-did-change (beg end text)
  "Inform the agent that the current buffer's document changed.
This is a synchronous call that suspends the caller until it completes."
  (let* ((uri (cody--uri-for (buffer-file-name)))
         (selection (cody--selection-get-current))
         (content-change (list
                          :range (list :start (cody--position-from-offset beg)
                                       :end (cody--position-from-offset end))
                          :text text))
         (params (list
                  :uri uri
                  :selection selection
                  :contentChanges (vector content-change))))
    (when cody--dev-panic-on-doc-desync
      (setq params (plist-put params :testing
                              (list :selectedText selection
                                    :sourceOfTruthDocument
                                    (list
                                     :uri uri
                                     :selection selection
                                     :content (cody--buffer-string))))))
    (let ((result (cody--request 'textDocument/change params)))
      (unless (plist-get result :success)
        (cody--log "Failed to apply textDocument/change request: %s" result)))))

(defun cody--position-from-offset (offset)
  "Convert OFFSET to a protocol Position struct."
  (save-excursion
    (goto-char offset)
    ;; The protocol uses 0-based line and column numbers.
    (list :line (1- (line-number-at-pos (point)))
          :character (current-column))))

(defun cody--completion-start-timer ()
  "Set a cancellable timer to check for an automatic completion."
  (cody--maybe-trigger-completion))

(defun cody--completion-cancel-timer ()
  "Cancel any pending timer to check for automatic completions."
  (when cody--completion-timer
    (cancel-timer cody--completion-timer)
    (setq cody--completion-timer nil)))

(defun cody--selection-get-current (&optional buf)
  "Return the jsonrpc parameters representing the selection in BUF.
Uses current buffer if BUF is nil. If the region is not active,
the selection is zero-width at point. BUF can be a buffer or
buffer name,and we return a Range with `start' and `end' parameters,
each a Position of 1-indexed `line' and `character'. The return
value is appropriate for sending directly to the rpc layer."
  (or buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (let* ((begin (if (or (use-region-p) mark-active)
                      (region-beginning)
                    (point)))
           (end (if (or (use-region-p) mark-active)
                    (region-end)
                  (point))))
      (list :start (cody--position-from-offset begin)
            :end (cody--position-from-offset end)))))

(defun cody--post-command ()
  "If point or mark has moved, update selection/focus with agent.
Installed on `post-command-hook'."
  (condition-case err
      (cody--handle-selection-change)
    (error (cody--log "Error in `cody--post-command': %s: %s"
                      buffer-file-name err))))

(defun cody--handle-typing-while-suggesting (beg end len)
  "Either recompute completion or dispel it, depending on the change.
This can be called for either insertions or deletions, and we allow
the completion suggestion overlay to remain active for certain instances
of both. If it is a deletion, we recorded the deleted text in our
`cody--before-change' function.
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
            (cody--completion-hide)
            (cody--completion-display))
        ;; All other buffer changes make the overlay go away.
        (cody--completion-hide))
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
      (when (and (cody--mode-active-p)
                 buffer-file-name
                 (cody--last-buffer-for-file-p))
        (cody--notify 'textDocument/didClose
                      (list :uri (cody--uri-for (buffer-file-name)))))
    (error (cody--log "Error notifying agent closing %s: %s"
                      buffer-file-name err))))

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
      (with-silent-modifications
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
  (when (cody--alive-p)
    (ignore-errors
      (cody--global-mode -1)) ; mode shutdown in all buffers
    (ignore-errors
      (cody--request 'shutdown nil)) ; Required by the protocol
    (ignore-errors
      (cody--kill-process))
    (unless cody--integration-testing-p
      (ignore-errors ; sometimes jsonrpc doesn't clean this one up
        (kill-buffer (get-buffer "*cody events*"))))
    (message "Cody has shut down."))
  (setq cody--message-in-progress nil
        cody--status 'disconnected
        ;; Force re-check of node version on Cody startup.
        cody--node-version-status nil
        cody--server-info nil))

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
        (message "Cody active and ready. M-x `cody-dashboard' for details."))
    (setq cody--node-version-status nil) ; re-check node version on start
    (message "Initializing Cody connection...")
    (cody--connection)
    (message "Cody connection initialized."))
  (when (cody--alive-p)
    (cody--global-mode 1)))

(defun cody-restart ()
  "Shut down and restart Cody."
  (interactive)
  (let ((cody--node-version-status 'good))
    (ignore-errors (cody-logout)))
  (setq cody--node-version-status nil)
  (cody-login))

(defun cody-unload-function ()
  "Handle `unload-feature' for this package."
  ;; TODO: Write integration test for this.
  (cody-logout))

(defun cody--log (msg &rest args)
  "Log MSG with ARGS, currently just for debugging."
  (with-current-buffer (get-buffer-create cody-log-buffer-name)
    (goto-char (point-max))
    (let ((timestamp (format-time-string "[%Y-%m-%d %H:%M:%S] ")))
      (insert timestamp (apply #'format msg args) "\n"))))

(defun cody--clear-log ()
  "Clear the *cody-log* debugging message buffer."
  (interactive)
  (with-current-buffer (get-buffer-create cody-log-buffer-name)
    (erase-buffer)))

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
          (insert-image (cody--icon-cody-logo) "Cody")
          (insert "Welcome to Cody. Type `M-x cody-help` for more info.\n")
          (set-buffer-modified-p nil))
        (current-buffer)))))

(defun cody--overlay-visible-p ()
  "Return non-nil if Cody is displaying a suggestion in the current buffer."
  (when-let* ((active (cody--mode-active-p))
              (o (car-safe cody--overlay-deltas))
              (_ (overlayp o))
              (_ (overlay-buffer o)) ; overlay is positioned/visible somewhere
              (_ (cody--cc))) ; not a zombie
    t))

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
  (cody--call-if-at-overlay 'cody--completion-accept))

(defun cody-quit-key-dispatch ()
  "Handler for `keyboard-quit'; clears the completion suggestion.
Also calls the default key binding."
  (interactive)
  (cody--call-if-at-overlay 'cody--completion-discard)
  ;; Also call default binding.
  (let (cody-mode) ; avoid recursion
    (call-interactively (key-binding (this-command-keys)))))

(defun cody--maybe-trigger-completion ()
  "Possibly trigger an immediate automatic completion request."
  (when (and cody-completions-auto-trigger-p
             (cody--mode-active-p)
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
  "Return non-nil if this is a valid location for a completion trigger."
  (not
   ;; User is in the middle of a word (from jetbrains cody client)
   (looking-back "\\s*[A-Za-z]+" (cody--bol))))

(defun cody-request-completion ()
  "Request manual autocompletion in current buffer at point."
  (interactive)
  (when (and (called-interactively-p 'any)
             (not (cody--mode-active-p)))
    (error "Cody-mode not enabled in this buffer."))
  (let* ((buf (current-buffer))
         (line (1- (line-number-at-pos)))
         (col (current-column))
         (cursor (point))
         (trigger-kind (if (called-interactively-p 'interactive)
                           "Invoke" "Automatic")))
    (when (and (not buffer-read-only)
               (or (string= trigger-kind "Invoke")
                   ;; Avoid spamming if the cursor hasn't moved.
                   (not (eql cursor cody--completion-last-trigger-spot))))
      (cody--completion-discard) ; Clears telemetry from previous request.
      (cody--completion-update-timestamp :triggeredAt)
      (setq cody--completion-last-trigger-spot cursor)
      (jsonrpc-async-request
       (cody--connection) 'autocomplete/execute
       (list :uri (cody--uri-for (buffer-file-name))
             :position (list :line line :character col)
             :triggerKind trigger-kind)
       ;; have new requests replace pending ones
       :deferred 'cody
       :success-fn (lambda (response)
                     (cody--completion-handle-result
                      response buf cursor trigger-kind))
       :error-fn
       (lambda (err) (cody--log "Error requesting completion: %s" err))
       :timeout-fn
       (lambda () (cody--log "Error: request-completion timed out"))))))

(defun cody--completion-handle-result (response buf request-spot kind)
  "Dispatches completion result based on jsonrpc RESPONSE.
BUF and REQUEST-SPOT specify where the request was initiated.
KIND specifies whether this was triggered manually or automatically"
  ;; TODO: Handle rate-limiting.
  (when (cody--buffer-visible-p buf)
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
            (ignore-errors (cody--completion-discard))
            (setq cody--completion (cody--populate-from-response response)))
          (cody--completion-update-timestamp :displayedAt)
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
         (cody--completion-add-marker (car-safe (last cody--overlay-deltas)))
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
       (cody--completion-hide)))))

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

(defun cody--completion-add-marker (ovl)
  "Put a Cody symbol at the end of overlay OVL."
  (when (and cody-completions-display-marker-p
             (overlayp ovl))
    (condition-case err
        (let* ((text (or (overlay-get ovl 'after-string) ""))
               (ends-in-newline (string-match-p "\n\\'" text))
               (marker (propertize " " 'display (cody--icon-cody-logo-small)))
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

(defun cody--completion-hide ()
  "Stop showing the current completion suggestion overlays.
`cody--completion' is left alone; use `cody--completion-discard' to
remove all traces of the last code completion response."
  (mapc #'delete-overlay cody--overlay-deltas)
  (cody--remove-all-overlays)
  (setq cody--overlay-deltas nil)
  (when cody--completion
    (setf (cody--current-item-index (cody--cc)) 0)))

(defun cody--remove-all-overlays ()
  "Remove all Cody overlays in the current buffer."
  ;; Cleans up any zombies, which does seem to happen, though rarely.
  (cl-loop for overlay being the overlays in (current-buffer)
           when (overlay-get overlay 'cody)
           do (delete-overlay overlay)))

(defun cody--completion-log-event (notification)
  "Sends a required autocomplete notification to the agent.
NOTIFICATION is the fire-and-forget protocol message to send.
Does nothing if custom option `cody-telemetry-enable-p' is nil."
  (when cody-telemetry-enable-p
    (when-let ((event-id (condition-case err
                             (plist-get (cody--completion-event (cody--cc)) :id)
                           (error
                            (cody--log "Malformed completion event: no :id param: %s "
                                       err)
                            nil))))
      (condition-case err
          (cody--notify notification (list :completionID event-id))
        (error (cody--log "Error on id=%s %s: %s" event-id notification err))))))

(defun cody--completion-accept ()
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
    (cody--completion-discard)))

(defun cody--completion-discard ()
  "Discard/reset the current completion overlay and suggestion data.
Sends telemetry notifications when telemetry is enabled."
  (cody--completion-cancel-timer)
  (cody--completion-hide)
  (setq cody--completion nil
        cody--completion-timestamps nil
        ;; Just to be clear, don't change this or the completion will resurrect.
        cody--completion-last-trigger-spot cody--completion-last-trigger-spot
        cody--update-debounce-timer nil)
  (setplist 'cody--vars nil))

(defun cody--check-cycle-preconditions ()
  "Return non-nil if we meet all the preconditions for cycling.
If not, then it handles logging and messaging."
  (let ((verbose (or cody-completions-cycling-help-p
                     (and (called-interactively-p 'interactive)
                          (memq this-command
                                '(cody-completion-cycle-next
                                  cody-completion-cycle-prev)))))
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
                (cody--completion-hide) ; This sets index to 0.
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

(defun cody--filtered-custom-variables (groups)
  "Get all custom variables in the provided GROUPS
Excludes those marked with `no-cody-dashboard`.
Returns an alist where each element is (GROUP . VARIABLES)."
  (cl-loop for group in groups
           collect (cons group
                         (cl-loop for symbol being the symbols
                                  when (and (custom-variable-p symbol)
                                            (not (get symbol 'no-cody-dashboard))
                                            (string-prefix-p (symbol-name group)
                                                             (symbol-name symbol)))
                                  collect symbol into vars
                                  finally return (sort vars #'string<)))))

(defun cody-dashboard ()
  "Show a console with data about Cody configuration and usage."
  (interactive)
  (let ((buf (get-buffer-create "*cody-dashboard*"))
        (image-marker (make-marker))
        (grouped-vars (cody--filtered-custom-variables cody--defgroups)))
    (with-current-buffer buf
      (cl-labels ((itext (text &optional face)
                    (insert (propertize (concat text "\n") 'face face)))
                  (isec (header)
                    (insert (propertize header 'face '(:underline t)))
                    (insert "\n"))
                  (ifield (label value &optional face)
                    (insert (propertize (concat label ": ")
                                        'face 'font-lock-builtin-face))
                    (insert (propertize (concat value "\n") 'face face)))
                  (fbuf (buffer)
                    (setq buffer-read-only t)
                    (pop-to-buffer buffer)
                    (goto-char (point-max))))
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (erase-buffer)
          (setq buffer-read-only nil)

          (unless (cody--alive-p)
            (itext "Cody is not connected" 'warning)
            (fbuf buf)
            (cl-return-from cody-dashboard))

          (itext "Cody is connected" 'success)
          (insert "\n\n")

          (isec "Server Info")

          ;; Server Info fields
          (ifield "  Name" (cody--server-info-name cody--server-info)
                  'font-lock-type-face)
          (ifield "  Authenticated"
                  (if (cody--server-info-authenticated cody--server-info)
                      "Yes" "No")
                  (if (cody--server-info-authenticated cody--server-info)
                      'success 'error))
          (ifield "  Cody Enabled"
                  (if (cody--server-info-cody-enabled cody--server-info)
                      "Yes" "No")
                  (if (cody--server-info-cody-enabled cody--server-info)
                      'success 'error))
          (ifield "  Cody Version" (cody--server-info-cody-version
                                    cody--server-info))

          (insert "\n")
          (isec "Auth Status")

          ;; Auth Status fields
          (when-let ((auth-status (cody--server-info-auth-status
                                   cody--server-info)))
            ;; Insert avatar marker
            (set-marker image-marker (point))
            (insert "\n")
            (ifield "  Username" (cody--auth-status-username auth-status))
            (ifield "  Is Logged In"
                    (if (cody--auth-status-is-logged-in auth-status)
                        "Yes" "No")
                    (if (cody--auth-status-is-logged-in auth-status)
                        'success 'error))
            (ifield "  Site Has Cody Enabled"
                    (if (cody--auth-status-site-has-cody-enabled auth-status)
                        "Yes" "No")
                    (if (cody--auth-status-site-has-cody-enabled auth-status)
                        'success 'error))
            (ifield "  Site Version" (cody--auth-status-site-version
                                      auth-status))
            (ifield "  Display Name" (cody--auth-status-display-name
                                      auth-status))
            (let ((email (cody--auth-status-primary-email auth-status)))
              (ifield "  Primary Email" email 'font-lock-string-face))
            (insert "\n"))

          ;; Cody Settings fields
          (insert "\n")
          (isec "Cody Settings")
          (insert "\n")
          (dolist (group-vars grouped-vars)
            (let ((group (car group-vars))
                  (variables (cdr group-vars)))
              (isec (format "%s" group))
              (dolist (var variables)
                (ifield (format "  %s" var) (format "%s" (symbol-value var))
                        'font-lock-variable-name-face))))
          (fbuf buf))
        (goto-char (point-min))
        (forward-line 1))
      (cody--dashboard-insert-avatar image-marker))))

(defun cody--dashboard-insert-avatar (image-marker)
  "Insert avatar image at the position marked by IMAGE-MARKER."
  (let* ((auth-status (cody--server-info-auth-status cody--server-info))
         (avatar-url (when auth-status
                       (cody--auth-status-avatar-url auth-status))))
    (when avatar-url
      (url-retrieve
       avatar-url
       (lambda (_status)
         (goto-char (point-min))
         (re-search-forward "\n\n")
         (let ((image-data (buffer-substring-no-properties
                            (point) (point-max))))
           (when (image-type-available-p 'png)
             (with-current-buffer (marker-buffer image-marker)
               (save-excursion
                 (goto-char image-marker)
                 (let ((inhibit-read-only t))
                   (insert "  ")
                   ;; Insert image at marker with size limit
                   (insert-image (create-image image-data 'png t
                                               :ascent 'center
                                               :max-height 40
                                               :max-width 40))
                   (insert "\n")))))))))))

(defun cody-doctor ()
  "Diagnose and troubleshoot Cody issues."
  (interactive)
  ;; Obviously lots more to do here.
  (if (cody--alive-p)
      (message "Cody is running! Try `M-x cody-chat' to get started.")
    (message "Cody is not running. 'M-x cody-login' to start hacking.")))

;;; Telemetry

(defun cody--completion-update-timestamp (property)
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

(defun cody--internal-anonymized-uuid ()
  "Return, generating if needed, variable `cody--internal-anonymized-uuid'."
  (or cody--internal-anonymized-uuid
      (setq cody--internal-anonymized-uuid (uuidgen-4))
      (custom-save-all)
      cody--internal-anonymized-uuid))

(defun cody--create-graphql-event (event-name params)
  "Return a Sourcegraph GraphQL logging event for telemetry."
  (let ((uuid (cody--internal-anonymized-uuid)))
    (list
     :event event-name
     :userCookieID uuid
     :url (cody--workspace-root)
     :source "IDEEXTENSION"
     :argument nil
     :publicArgument params
     :client "EMACS_CODY_EXTENSION"
     :deviceID uuid)))

;;; Event log

(defvar cody-event-log-keywords
  (let* ((x-keywords '("initialize" "initialized"
                       "debug/message" "remoteRepo/didChange"))
         (x-keywords-regexp (regexp-opt x-keywords 'words)))
    `((,x-keywords-regexp . font-lock-keyword-face))))

(defun cody-event-log-shortening-handler ()
  "Hide long lines in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\"content\":\"[^\"]+\"" nil t)
      (ignore-errors
        (let* ((max-length 150)
               (beginning (match-beginning 0))
               (end (match-end 0))
               (overlay (make-overlay beginning (min end (+ beginning 100)))))
          (overlay-put overlay
                       'display (concat
                                 (buffer-substring beginning (+ beginning max-length))
                                 "... [truncated]"))
          (overlay-put overlay 'cody-original-content (buffer-substring beginning end))
          (overlay-put overlay 'face 'font-lock-comment-face)
          (overlay-put overlay 'cody-shortened t)
          (add-text-properties beginning end '(read-only t)))))))

(defun cody-event-log-expand-handler (point)
  "Expand the content at POINT if it's shortened."
  (interactive "d")
  (let ((overlay (car (overlays-at point))))
    (when (and overlay (overlay-get overlay 'cody-shortened))
      (let ((original (overlay-get overlay 'cody-original-content)))
        (delete-overlay overlay)
        (remove-text-properties (point-min) (point-max) '(read-only t))
        (goto-char point)
        (delete-region (point) (+ point 100))
        (insert original)
        (add-text-properties (point-min) (point-max) '(read-only t))
        (message "Expanded content")))))

(define-derived-mode cody-event-log-mode fundamental-mode "Cody Event Log"
  "Major mode for viewing Cody event logs with special features."
  (setq font-lock-defaults '(cody-event-log-keywords))
  (cody-event-log-shortening-handler))

(define-key cody-event-log-mode-map (kbd "C-c C-e") 'cody-event-log-expand-handler)

(defun cody--activate-event-log-mode ()
  "Activate `cody-event-log-mode' in the `*cody events*' buffer."
  (run-with-idle-timer
   0.1 nil
   (lambda ()
     (condition-case err
         (let ((inhibit-read-only t))
           (with-current-buffer (get-buffer-create "*cody events*")
             (cody-event-log-mode)))
       (error (cody--log "Failed to activate cody-event-log-mode: %s"
                         (error-message-string err)))))))

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
