;; cody.el --- Sourcegraph Cody in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sourcegraph, Inc.

;; Version: 0.2.0
;; Author: Keegan Carruthers-Smith <keegan.csmith@gmail.com>
;; Maintainer: Steve Yegge <steve.yegge@gmail.com>
;; URL: https://github.com/sourcegraph/emacs-cody
;; Package-Requires: ((emacs "26.3") (jsonrpc "1.0.16") (uuidgen "20240201.2318"))
;; Keywords: completion convenience languages programming tools

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; See ./README.org for installation information.
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
(require 'cody-dashboard)

;;; Custom variables.

(defgroup cody nil
  "Sourcegraph Cody settings."
  :group 'programming
  :prefix "cody-")

(defcustom cody-telemetry-enable-p nil
  "Non-nil to allow anonymized event/usage telemetry.
This information is used by Sourcegraph to improve Cody."
  :group 'cody
  :type 'boolean)

(defcustom cody-gc-workspace-timeout-sec 15 ;; 300
  "Time to wait before garbage collecting an unused workspace."
  :type 'integer
  :group 'cody)

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
  "Default branch name for the current project."
  :group 'cody
  :type 'string)

(defcustom cody-remote-url-replacements ""
  "Whitespace-separated pairs of replacements for repo URLs."
  :group 'cody
  :type 'string)

(defgroup cody-dev nil
  "Cody developer/contributor configuration settings."
  :group 'cody
  :prefix "cody-dev-")

(defcustom cody-node-executable nil
  "Hardwired path to the nodejs binary to use for Cody.
If nil, Cody will search for node using variable `exec-path'."
  :group 'cody
  :type 'string)

(defcustom cody-node-min-version "20.4.0"
  "The minimum required version of Node.js."
  :group 'cody-dev
  :type 'string)

(defcustom cody-max-workspaces 1
  "Maximum number of active workspace connections Cody will create.
Each workspace connection spins up its own Cody agent subprocess.
You can view all Cody workspaces with `cody-dashboard'."
  :type 'number
  :group 'cody-dev)

(defcustom cody-use-remote-agent nil
  "Non-nil to connect to an agent running on `cody--dev-remote-agent-port`.
This is a setting for contributors to Cody-Emacs."
  :group 'cody-dev
  :type 'boolean)

(defcustom cody-remote-agent-port 3113
  "The port on which to attach to a remote Agent.
The remote Agent is typically started by an IDE such as VS Code,
and enables you to set breakpoints on both sides of the protocol."
  :group 'cody-dev
  :type 'number)

(defcustom cody-enable-agent-debug nil
  "Non-nil to enable debugging in the agent.
Sends this flag as part of the agent extension configuration."
  :group 'cody-dev
  :type 'boolean)

(defcustom cody-enable-agent-debug-verbose nil
  "Non-nil to enable verbose debugging in the agent.
Sends this flag as part of the agent extension configuration."
  :group 'cody-dev
  :type 'boolean)

(defcustom cody-panic-on-doc-desync nil
  "Non-nil to ask the Agent to panic if we discover it is desynced.
De-syncing is when the Agent's copy of a document is out of sync with
the actual document in Emacs.

Setting this custom variable to non-nil, which should only be done in
development, sends extra metadata along with document changes, which the
Agent will compare against. Performance will be heavily impacted, with
the entire buffer being sent to Agent on every small change."
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

;; It might seem odd to have a separate Node process for each workspace,
;; but it makes things more flexible in general; e.g. integration testing
;; without interfering with your normal Cody session, or hitting dev backends.
;; Other Cody clients (e.g. JetBrains) also have per-workspace agents.
(cl-defstruct cody-workspace
  "Data associated with a workspace root and its connection.
Each Cody-enabled workspace has a separate Agent instance."
  (root (getenv "HOME") :type string)
  (uri (cody--uri-for (getenv "HOME")) :type string)
  (connection nil :type (or null jsonrpc-process-connection))
  (server-info nil :type (or null cody-server-info))
  ;; Possible values are: `unconnected', `connected', `error', 'closed'.
  (status 'unconnected :type symbol)
  (error nil :type (or null string)) ; last error encountered
  (events-buffer nil :type (or null buffer))
  (stderr-buffer nil :type (or null buffer)))

(defvar cody-workspaces (make-hash-table :test 'equal)
  "Hash table mapping workspace root uris to `cody-workspace' structs.
Workspace roots are typically repo roots.")

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

(defvar cody--sourcegraph-host "sourcegraph.com"
  "Sourcegraph host.")

(defvar cody--access-token nil
  "Access token for `cody--sourcegraph-host'.")

(defconst cody-log-buffer-name "*cody-log*"
  "Cody log messages.
This log is shared across all agent connections.")

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

(defconst cody--mode-hooks
  '((before-change-functions . cody--before-change)
    (after-change-functions . cody--after-change)
    (window-selection-change-functions . cody--handle-focus-changed)
    (window-buffer-change-functions . cody--handle-focus-changed)
    (post-command-hook . cody--post-command)
    (kill-buffer-hook . cody--kill-buffer-function)
    (activate-mark-hook . cody--handle-selection-change)
    (deactivate-mark-hook . cody--handle-selection-change)
    (after-revert-hook . cody--after-revert))
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

(defvar cody--connection-global-error nil
  "Non-nil when Cody fails to start an Agent subprocess.
Until this flag is cleared, it will not attempt to create any
new Agent processes.")

(defvar cody--vars nil
  "Symbol used as scratch space for ephemeral temp variables.
Typically used for allowing before/after hooks to communicate data.
Symbol properties are used reduce namespace clutter.")

(defvar cody-connection-initialized-hook nil
  "Hook run after `cody--connection' initializes a connection.
Each workspace has its own node subprocess and jsonrpc connection,
so this hook is run once per workspace that Cody opens.
If the connection failed, then the workspace status field will be `error'.")

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
usually means communication with the backend is down. The only way to
recover from this state is to restart it with `cody-workspace-reopen'.")

(defvar-local cody--buffer-document-state nil
  "State machine to ensure that we sequence open/close/focus events.
States: nil = unopened, `opened' = opened, and `closed' = closed/error.
States must progress from nil -> opened -> closed, and didFocus events
can only be sent while the document state is `opened'.")

(defvar-local cody--last-selection nil
  "Stores the last known selection range to detect changes.")

(defvar cody-mode-menu)

(defvar cody-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'cody--mode-line-click)
    (easy-menu-define cody-mode-menu map "Cody Mode Menu"
      '("Cody"
        ["Dashboard" (cody-dashboard)]
        ["Turn Off" cody-mode]
        ["Help" (describe-function 'cody-mode)]))
    map)
  "Keymap for Cody mode line button.")

(defun cody--compute-logo-height ()
  "Compute the desired height for the Cody logo as 85% of the mode line height."
  (truncate (* 0.85 (frame-char-height))))

(defvar-local cody--mode-line-icon-evaluator
    '(:eval (cody--evaluate-mode-line-icon))
  "Descriptor for producing a custom menu in the mode line lighter.")

(defvar cody--post-command-debounce-timer nil)

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
  (let ((node-executable (or cody-node-executable (executable-find "node"))))
    (unless node-executable
      (error "Node.js executable not found in exec-path"))
    (list node-executable
          "--enable-source-maps"
          cody--cody-agent
          "api"
          "jsonrpc-stdio")))

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

(defun cody--alive-p (&optional connection)
  "Return non-nil if CONNECTION is a live process.
CONNECTION defaults to the current workspace's connection, if any."
  (or cody--unit-testing-p
      ;; Don't call `cody--connection' here since it will spawn one.
      ;; Check the workspace to see if it has a live connection.
      (when-let ((conn (or connection
                           (when-let* ((workspace-root (cody--workspace-root))
                                       (workspace (gethash workspace-root cody-workspaces)))
                             (cody-workspace-connection workspace)))))
        (let ((process (jsonrpc--process conn)))
          (and process (zerop (process-exit-status process)))))))

(defun cody--connection (&optional restart-if-needed)
  "Get or create the agent connection for the current workspace.

RESTART-IF-NEEDED, if non-nil, will attempt to start or restart the
connection if it is anything other than connected and active.

Return nil if there is no connection available, either because it is
still nil, or because it has failed in some way. Will not return a
dead connection."
  (let* ((workspace (cody--current-workspace))
         (connection (and workspace (cody-workspace-connection workspace))))
    (cond
     ((cody--error-p) nil)
     ((cody--alive-p connection) connection)
     ;; All the error conditions are handled identically for now.
     (t
      (if (not restart-if-needed)
          nil
        (condition-case err
            ;; Restart and initialize.
            (progn
              (setq connection
                    (cody--connection-create-process-safe workspace))
              ;; Required before calling `cody--initialize-connection'.
              (setf (cody-workspace-connection workspace) connection)
              (with-timeout (10
                             (signal 'connection-timeout '("Timed out during handshake")))
                (cody--initialize-connection workspace))
              (if (cody--alive-p connection)
                  connection
                nil))
          (error
           (cody--workspace-set-error
            (format "initializing connection: %s" err)))))))))

(defun cody--connection-create-process-safe (workspace)
  "Handle errors and timeouts around attaching to agent process for WORKSPACE.
If `cody--connection-global-error' is non-nil, return nil immediately.
Within a timeout of 10 seconds, attempt to create the process
using `cody--connection-create-process'.
Set a global error on timeout or if any other error occurs.
Return value, if non-nil, is a `jsonrpc-process-connection'."
  (when (not cody--connection-global-error)
    (condition-case err
        (with-timeout (10
                       (signal 'connection-timeout '("Connection timed out")))
          ;; This is the meat; everything below is error handling.
          (cody--connection-create-process workspace))
      (error
       ;; Making the assumption that timing out means we'll never succeed -
       ;; the user will have to change something and explicitly reset.
       (when (eq (car err) 'connection-timeout)
         (setq cody--connection-global-error 'timed-out))
       ;; Whereas other errors _may_ only apply to this workspace, so we'll
       ;; just flag this one as being in error.
       (cody--workspace-set-error
        (if cody-use-remote-agent
            (format "Could not attach to agent on port %d: %s"
                    cody-remote-agent-port
                    (error-message-string err))
          (format "Could not create process: %s"
                  (error-message-string err)))
        nil ; no error for format string; we already handled it.
        workspace)
       nil))))

(defun cody--connection-create-process (workspace)
  "Create a local or network process for talking to the agent for WORKSPACE.
Return value is a `jsonrpc-process-connection'."
  (let* ((workspace-root (cody-workspace-root workspace))
         (events-buffer
          (let ((buf (get-buffer-create (format "*cody events[%s]*"
                                                workspace-root))))
            (with-current-buffer buf
              (buffer-disable-undo)
              buf)))
         (process-name (cody--workspace-process-name workspace))
         (process-environment (cody--agent-process-environment))
         (process (if cody-use-remote-agent
                      (make-network-process
                       :name process-name
                       :host 'local
                       :service cody-remote-agent-port
                       :coding 'utf-8-emacs-unix
                       :noquery t)
                    (make-process
                     :name process-name
                     :command (cody--agent-command)
                     :coding 'utf-8-emacs-unix
                     :connection-type 'pipe
                     :noquery t)))
         (connection (make-instance
                      'jsonrpc-process-connection
                      :name process-name
                      :notification-dispatcher #'cody--notification-dispatcher
                      :request-dispatcher #'cody--request-dispatcher
                      :process process)))
    (setf (cody-workspace-status workspace) 'connected)
    (setf (jsonrpc--events-buffer connection) events-buffer)
    (setf (cody-workspace-events-buffer workspace) events-buffer)
    (setf (cody-workspace-stderr-buffer workspace)
          (jsonrpc-stderr-buffer connection))
    connection))

(defun cody--workspace-process-name (workspace)
  "Create the process name for WORKSPACE.
/Path/to/workspace/root is changed to cody-path-to-workspace-root.
This name can be directly looked up more quickly on the workspace after it is set,
since it is immutable, using `(process-name (cody-workspace-process workspace))'."
  (let ((root (replace-regexp-in-string "^/+\\|/+$" ""
                                        (cody-workspace-root workspace))))
    (setq root (replace-regexp-in-string "/" "-" root))
    (setq root (replace-regexp-in-string " " "-" root))
    (downcase (concat "cody-" root))))

(defun cody--initialize-connection (workspace)
  "Send the protocol handshake requests for WORKSPACE."
  ;; client -> 'initialize -> host
  (condition-case err
      (let ((response
             (cody--request 'initialize
                            (list
                             :name "Emacs"
                             :version "0.2"
                             :workspaceRootUri (cody--workspace-uri)
                             :capabilities (cody--client-capabilities)
                             :extensionConfiguration (cody--extension-configuration)))))
        (when response
          (setf (cody-workspace-server-info workspace)
                (cody-populate-server-info response))))
    (error (cody--workspace-set-error err)))
  (condition-case err
      (run-hooks 'cody-connection-initialized-hook)
    (error (cody--log "Error in `cody--initialize-connection': %s" err)))
  ;; client -> 'initialized -> host
  (condition-case err
      (cody--notify 'initialized nil)
    (error (cody--workspace-set-error err))))

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
               (cody--workspace-set-error "Failed to create cody-auth-status instance" err)
               (signal 'error err)))))
      (make-instance 'cody-server-info
                     :name (plist-get response :name)
                     :authenticated (cjf (plist-get response :authenticated))
                     :codyEnabled (cjf (plist-get response :codyEnabled))
                     :codyVersion (plist-get response :codyVersion)
                     :authStatus auth-status-instance))))

(defun cody--client-capabilities ()
  "Return the Cody features that we support in the Emacs client."
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
        :debug cody-enable-agent-debug
        :debug-verbose cody-enable-agent-debug-verbose
        :codebase (cody--workspace-uri)
        :customConfiguration (list (cons :cody.experimental.foldingRanges
                                         "indentation-based"))))

;; This was used to switch workspaces before multi-workspace support.
;; Keeping it because there are other situations where we will need it.
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

(defun cody--format-jsonrpc-error (jsonrpc-error)
  "Format JSONRPC-ERROR as a concise string."
  (let ((request-id (nth 1 jsonrpc-error))
        (error-message (alist-get 'jsonrpc-error-message (nth 2 jsonrpc-error))))
    (format "request id=%s failed: %s" request-id error-message)))

(defun cody--request (method params &rest args)
  "Wrapper for `jsonrpc-request' that makes it testable."
  (unless (or cody--unit-testing-p (cody--error-p))
    (condition-case err
        (when-let ((connection (cody--connection)))
          (apply #'jsonrpc-request (cody--connection) method params args))
      (error (cody--log "Unable to send request %s: %s" method err)))))

(defun cody--notify (method params &rest args)
  "Helper to send a Cody request for METHOD with PARAMS."
  (unless (or cody--unit-testing-p (cody--error-p))
    (condition-case err
        (if-let ((connection (cody--connection)))
            (apply #'jsonrpc-notify connection method params args)
          (cody--log "Skipped sending notification %s: null connection" method))
      (error (cody--log "Unable to send %s: %s" method err)))))

(defun cody--check-node-version ()
  "Signal an error if the default node.js version is too low.
Min version is configurable with `cody-node-min-version'."
  (cond
   (cody-use-remote-agent
    t)
   ((eq cody--node-version-status 'good)
    t)
   ((eq cody--node-version-status 'bad)
    (error "Installed nodejs must be at least %s." cody-node-min-version))
   (t
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
        (let* ((min-version-parts (split-string cody-node-min-version "\\."))
               (min-major (string-to-number (nth 0 min-version-parts)))
               (min-minor (string-to-number (nth 1 min-version-parts)))
               (min-patch (string-to-number (nth 2 min-version-parts))))
          ;; For now, as of July 07 2024, make it exact, since 22.1.0 hangs Agent.
          (if (and (= major min-major) (= minor min-minor) (= patch min-patch))
              (setq cody--node-version-status 'good)
            (setq cody--node-version-status 'bad)
            (error
             "Error: Installed nodejs version %s is not the required version %s"
             node-version cody-node-min-version))))))))

(defun cody--get-custom-request-headers-as-map (custom-request-headers)
  "Convert CUSTOM-REQUEST-HEADERS string to a map."
  (let ((pairs (split-string custom-request-headers ",")))
    (cl-loop for (key value) on pairs by #'cddr
             collect (cons (string-trim key) (string-trim value)))))

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

;; Execute the macro to create the necessary functions; e.g.,
;; `cody--logo-cody-logo-small'. They load their images lazily.
(create-icon-functions)

(defun cody--evaluate-mode-line-icon ()
  "Decide the mode line icon based on the current buffer state."
  (condition-case err
      (when (and cody-mode (symbol-function 'cody--icon-cody-logo-small))
        (let ((icon
               (cl-case cody--buffer-state
                 (active (cody--icon-cody-logo-small))
                 (inactive (cody--icon-logo-monotone))
                 (error (cody--icon-logo-disabled))
                 (ignored (cody--icon-logo-disabled))
                 (untrackable (cody--icon-logo-disabled))
                 (otherwise nil))))
          (cody--decorate-mode-line-lighter icon)))
    (error (cody--log "Error in mode line evaluator: %s" err))))

(defun cody--mode-line-click (_event)
  "Handle mouse click EVENT on Cody mode line item."
  (interactive "e")
  (popup-menu cody-mode-menu))

(defun cody-propertize-icon (text-or-image)
  "Return propertized string or image for `cody--minor-mode-icon`.
Argument TEXT-OR-IMAGE is the string or image to propertize."
  (let ((help-echo (if cody--buffer-state
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
        (red-exclamation (if (cody--error-p)
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
  (if (bound-and-true-p cody-mode)
      (cody--mode-startup)
    (cody--mode-shutdown)))

(defun cody--mode-startup ()
  "Code to run when `cody-mode' is turned on in a buffer."
  (if (not (cody--alive-p))
      (when (called-interactively-p 'interactive)
        (message "Use `cody-login' to start Cody"))
    (cl-loop for (hook . func) in cody--mode-hooks
             do (add-hook hook func nil 'local))
    (add-to-list 'mode-line-modes cody--mode-line-icon-evaluator)
    (force-mode-line-update t)
    ;; Normally the hooks handle notifications, but we need to do this one.
    (cody--buffer-update-state)
    (cody--notify-doc-did-open)))

(defun cody--mode-shutdown ()
  "Code to run when `cody-mode' is turned off in a buffer."
  (cody--completion-discard)
  (cl-loop for (hook . func) in cody--mode-hooks
           do (remove-hook hook func 'local))
  (cody--completion-cancel-timer)
  (setq cody-mode nil ; clears the modeline and buffer-locals
        cody--buffer-document-state nil))

;; ;;;###autoload
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
  (and cody-mode
       (cody--alive-p)
       (eq cody--buffer-state 'active)))

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

(defun cody--after-revert ()
  "Update Agent after buffer is reverted."
  (condition-case err
      (when (cody--mode-active-p)
        (cody--buffer-update-state)
        (cody--notify-doc-did-change
         (point-min) (point-max) (buffer-substring-no-properties (point-min) (point-max))))
    (error (cody--log "Error in `cody--after-revert': %s" err))))

(defun cody--on-doc-change (beg end text)
  "Common code for `cody--before-change' and `cody--after-change'."
  (when (and (cody--mode-active-p)
             ;; Don't request a completion after an undo operation.
             (not (memq last-command '(undo undo-only advertised-undo))))
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
  (when (and buffer-file-name
             (cody--mode-active-p))
    (cody--notify 'textDocument/didClose
                  (list :uri (cody--workspace-uri)))))

(defun cody--handle-focus-changed (window)
  "Notify agent that a document has been focused or opened."
  ;; N.B. Don't check `cody--mode-active-p' here, as it causes tests to fail.
  ;; We can safely assume that we're only called when `cody-mode' is active.
  (unless (cody--error-p)
    (condition-case err
        (when (eq window (selected-window))
          (cody--focus-or-open)
          (when (cody--overlay-visible-p)
            (cody--completion-hide))
          (cody--completion-start-timer))
      (error (cody--log "cody--handle-focus-changed: %s" err)))))

(defun cody--focus-or-open ()
  "Initialize the state, and decide whether to tell agent doc focused or opened.
Current buffer is visiting the document under consideration."
  (cond
   ((null cody--buffer-document-state) ; unopened
    (cody--notify-doc-did-open))
   ((eq cody--buffer-document-state 'opened)
    (cody--notify-doc-did-focus))
   ;; Otherwise it's closed. We drop any spurious events silently.
   (t nil)))

(defun cody--handle-selection-change ()
  "Handle changes in the selection or region.
If point has moved off an overlay line, hide the completion and start a timer."
  (let ((current-selection (cody--selection-get-current)))
    (unless (or (equal current-selection cody--last-selection)
                (cody--error-p))
      (setq cody--last-selection current-selection)
      (condition-case err
          (progn
            (unless (cody--point-on-overlay-line-p)
              (when (cody--overlay-visible-p)
                (cody--completion-hide))
              (cody--completion-start-timer))
            (cody--notify-doc-did-focus))
        (error (cody--log "Error in `cody--handle-selection-change': %s" err))))))

(defun cody--point-on-overlay-line-p ()
  "Return non-nil if point is on same line as a completion overlay fragment."
  (let ((point-line (line-number-at-pos (point))))
    (cl-some (lambda (overlay)
               (let ((overlay-line (line-number-at-pos (overlay-start overlay))))
                 (= point-line overlay-line)))
             cody--overlay-deltas)))

(defun cody--buffer-update-state ()
  "Initialize or update `cody--buffer-state' for the current buffer.
Return the new value."
  (let* ((workspace (cody--current-workspace))
         (workspace-uri (and workspace (cody--workspace-uri workspace)))
         (alive (and workspace (cody--alive-p
                                (cody-workspace-connection workspace)))))
    (setq cody--buffer-state
          (cond
           ;; We can't have an Agent+connection without a workspace.
           ((not (cody-workspace-p workspace)) 'untrackable)
           ((cody-workspace-error workspace) 'error)
           (alive 'active)
           (t 'inactive)))))

(defun cody--notify-doc-did-open ()
  "Inform the agent that the current buffer's document just opened."
  (unless cody--buffer-document-state ; already opened
    (cody--notify 'textDocument/didOpen
                  (list
                   :uri (cody--uri-for (buffer-file-name))
                   :content (buffer-substring-no-properties (point-min)
                                                            (point-max))
                   :selection (cody--selection-get-current)))
    (setq cody--buffer-document-state 'opened)))

(defun cody--notify-doc-did-close ()
  "Inform the agent that the current buffer's document just closed.
This is sent when the last buffer visiting the file is killed."
  (if (eq cody--buffer-document-state 'opened)
      (progn
        (cody--notify 'textDocument/didClose
                      (list :uri (cody--uri-for (buffer-file-name))))
        (setq cody--buffer-document-state 'closed))
    (cody--log "textDocument/didClose skipped because document is in %s state"
               cody--buffer-document-state)))

(defun cody--notify-doc-did-focus ()
  "Inform the agent that the current buffer's document was just focusd."
  (when (eq cody--buffer-document-state 'opened) ; else drop silently
    (cody--notify 'textDocument/didFocus
                  (list ; Don't include :content, as it didn't change.
                   :uri (cody--uri-for (buffer-file-name))
                   :selection (cody--selection-get-current)))))

(defun cody--notify-doc-did-change (beg end text)
  "Inform the agent that the current buffer's document changed.
This is a synchronous call that suspends the caller until it completes."
  (unless (or (cody--error-p)
              (not (eq cody--buffer-document-state 'opened)))
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
      (when cody-panic-on-doc-desync
        (setq params (plist-put params :testing
                                (list :selectedText selection
                                      :sourceOfTruthDocument
                                      (list
                                       :uri uri
                                       :selection selection
                                       :content (cody--buffer-string))))))
      (let ((result (cody--request 'textDocument/change params)))
        (unless (plist-get result :success)
          (cody--log "Failed to apply textDocument/change request: %s" result))))))

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
      (progn
        ;; Have a new request replace any pending request.
        (when cody--post-command-debounce-timer
          (cancel-timer cody--post-command-debounce-timer))
        (run-with-idle-timer 0 nil #'cody--handle-selection-change))
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
                     (and (cl-plusp len) ; deletion (len is num chars deleted)
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

(defun cody--kill-buffer-function ()
  "Check whether to notify agent and/or schedule workspace GC.
If this is the last buffer visiting a file with an active workspace,
notify the agent that the document has closed. If this is the last
file open in the workspace, schedule a workspace garbage collection."
  (condition-case err
      (when (and (cody--mode-active-p)
                 buffer-file-name
                 (cody--last-buffer-for-file-p))
        ;; Last buffer visiting the file - we consider the document closed.
        (cody--notify-doc-did-close)
        ;; See if this is the last file in its workspace.
        (when-let ((workspace-root (cody--workspace-root)))
          ;; Check if no other buffers are visiting this workspace.
          (unless (cl-some (lambda (buf)
                             (with-current-buffer buf
                               (and (cody--mode-active-p)
                                    (equal workspace-root
                                           (cody--workspace-root)))))
                           (buffer-list))
            (message "starting Cody workspace GC timer")
            (run-at-time cody-gc-workspace-timeout-sec nil
                         #'cody--garbage-collect-workspaces))))
    (error (cody--log "Error notifying agent closing %s: %s"
                      buffer-file-name err))))

(defun cody--workspace-root (&optional buffer-or-file)
  "Return the workspace root for the Agent.
BUFFER-OR-FILE is a buffer visiting a file or a file being visited
by a buffer, and if omitted defaults to the current buffer.
Will return nil if the target buffer is not visiting a file, or
the project root cannot be determined from the visited file path."
  (when-let
      ((result
        (condition-case err
            (let* ((default-directory ; used by `project-current'
                    (cond
                     ((bufferp buffer-or-file)
                      (with-current-buffer buffer-or-file
                        default-directory))
                     ((and (stringp buffer-or-file)
                           (get-file-buffer buffer-or-file))
                      (with-current-buffer (get-file-buffer buffer-or-file)
                        default-directory))
                     (buffer-or-file
                      (file-name-directory buffer-or-file))
                     (t default-directory)))
                   (project (project-current))
                   (root (when project (project-root project))))
              (and root (expand-file-name root)))
          (error
           (cody--log "Error determining project root: %s" err)
           nil))))
    (when result (expand-file-name result))))

(defun cody--workspace-uri (&optional buffer-or-file)
  "Return the canonical URI for the current or passed workspace.
BUFFER-OR-FILE can be a buffer, buffer name, or file path.
Defaults to the current workspace."
  (if buffer-or-file
      (cody--uri-for buffer-or-file)
    (when-let ((workspace (cody--current-workspace)))
      (cody-workspace-uri workspace))))

(defun cody--workspace-buffers (&optional workspace)
  "Return the buffers in WORKSPACE."
  (cl-loop for buf being the buffers
           when (with-current-buffer buf
                  (and (cody--mode-active-p) ; or maybe just cody-mode?
                       (equal workspace (cody--workspace-root))))
           collect buf))

(defun cody--error-p ()
  "Return non-nil if there is a global error or workspace error."
  (or cody--connection-global-error
      (let ((workspace (cody--current-workspace)))
        (and workspace (cody--workspace-get-error workspace)))))

(defun cody--current-workspace (&optional buffer)
  "Get or create the `cody-workspace' struct for the current project root.
Does not initialize a connection. If BUFFER is passed, uses its workspace root.
Return nil if there is no workspace root for the current buffer."
  (when-let ((workspace-root (cody--workspace-root buffer))
             (count (hash-table-count cody-workspaces)))
    (or (gethash workspace-root cody-workspaces)
        (when (< count cody-max-workspaces)
          (puthash workspace-root (make-cody-workspace
                                   :root workspace-root
                                   :uri (cody--uri-for workspace-root))
                   cody-workspaces)))))

(defun cody--garbage-collect-workspaces ()
  "Garbage collect workspaces that have no active buffers."
  (maphash (lambda (root workspace)
             (unless (cl-plusp (length (cody--workspace-buffers workspace)))
               (cody-workspace-close workspace)))
           cody-workspaces))

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

(defun cody-workspace-reopen ()
  "Attempt to force-restart the connection for the current workspace."
  (interactive)
  (when-let ((workspace (cody--current-workspace)))
    (cody-workspace-close workspace))
  (cody--workspace-open))

(defun cody--workspace-open (&optional buffer-or-name)
  "Point Cody at the specified BUFFER-OR-NAME's workspace.
Defaults to current buffer if not specified.
This is found via `cody--workspace-root', which looks for the repo root
or equivalent. Return nil if BUFFER-OR-NAME is in a valid workspace."
  (with-current-buffer (or buffer-or-name (current-buffer))
    (when-let ((workspace-root (cody--workspace-root)))
      (cody--connection))))

(defun cody--workspace-close-all (&optional kill-buffers)
  "Shut down all jsonrpc connections and clean up buffers.
If kill-BUFFERS is non-nil, kills the event and stderr buffers."
  (maphash (lambda (_key workspace)
             (condition-case err
                 (cody-workspace-close workspace kill-buffers)
               (error (cody--log "Error closing workspace: %s %s" workspace err))))
           cody-workspaces)
  (clrhash cody-workspaces))

(defun cody-workspace-close (workspace &optional kill-buffers)
  "Shut down the WORKSPACE and reset its fields, but keep buffers.
If kill-BUFFERS is non-nil, kills the event and stderr buffers."
  (interactive)
  (let ((workspace-root (cody-workspace-root workspace)))
    (cody--log "Closing workspace: %s" workspace-root)
    (condition-case err
        (with-timeout (5 (cody--log "Timeout sending 'shutdown to %s"
                                    workspace-root))
          (cody--request 'shutdown nil)))
    (condition-case err
        (when-let ((connection (cody-workspace-connection workspace)))
          (with-timeout (10 (cody--log
                             "Timeout shutting down jsonrpc connection"))
            (jsonrpc-shutdown connection 'cleanup-buffers)))
      (error
       (cody--log "Error closing workspace: %s" err)))
    (cody-workspace-reset workspace kill-buffers)
    (remhash workspace-root cody-workspaces)))

(defun cody-workspace-reset (workspace &optional kill-buffers)
  "Reset all fields of WORKSPACE to their initial values, except buffers.
If kill-BUFFERS is non-nil, kills the event and stderr buffers."
  (setf (cody-workspace-root workspace) (getenv "HOME"))
  (setf (cody-workspace-uri workspace) (cody--uri-for (getenv "HOME")))
  (setf (cody-workspace-connection workspace) nil)
  (setf (cody-workspace-server-info workspace) nil)
  (setf (cody-workspace-status workspace) 'unconnected)
  (setf (cody-workspace-error workspace) nil)
  (when-let ((buf (cody-workspace-events-buffer workspace)))
    (when kill-buffers
      (kill-buffer buf))
    (setf (cody-workspace-events-buffer workspace) nil))
  (when-let ((buf (cody-workspace-stderr-buffer workspace)))
    (when kill-buffers
      (kill-buffer buf))
    (setf (cody-workspace-stderr-buffer workspace) nil))
  workspace)

(defun cody--workspace-set-error (msg &optional err workspace)
  "Set the current workspace into an error state with MSG and ERR.
Uses WORKSPACE instead of current workspace, if provided."
  (let ((formatted-msg (if (and (listp msg)
                                (eq (car msg) 'jsonrpc-error))
                           (cody--format-jsonrpc-error msg)
                         msg)))
    (cody--log "Error: %s %s" formatted-msg (if err (format "(%s)" err) ""))
    (when-let ((ws (or workspace (cody--current-workspace))))
      (setf (cody-workspace-status ws) 'error)
      (when err (setf (cody-workspace-error ws) formatted-msg))
      (force-mode-line-update t))))

(defun cody--workspace-get-error (&optional workspace)
  "Get the error message for WORKSPACE, defaulting to the current workspace.
Will return nil if there is no error."
  (when-let ((ws (or workspace (cody--current-workspace))))
    (or (cody-workspace-error ws)
        (when (eq (cody-workspace-status ws) 'error)
          "connection error"))))

(defun cody--error-any-workspace-p ()
  "Return non-nil if any workspace is in an error state."
  (cl-loop for workspace being the hash-values of cody-workspaces
           thereis (cody--workspace-get-error workspace)))

(defun cody--any-workspace-active-p ()
  "Return non-nil if any Cody workspace has an active connection."
  (cl-some (lambda (workspace)
             (let ((connection (cody-workspace-connection workspace)))
               (and connection (cody--alive-p connection))))
           (hash-table-values cody-workspaces)))

(defun cody--enable-for-workspace-buffers (workspace-root)
  "Turn on `cody-mode' in all open buffers that are part of WORKSPACE-ROOT
   and qualify for Cody tracking."
  (cl-loop for buf being the buffers
           when (with-current-buffer buf
                  (and (cody--should-auto-enable-cody-p buf)
                       (equal workspace-root (cody--workspace-root))))
           do (cody-mode 1)))

;;;###autoload
(defun cody-login (&optional quiet)
  "Start the Cody agent. Optionally enables `cody-mode' in buffers.
This function starts up the Cody system, and you can call it from any
Cody command wrapper to ensure there is a connection.
This function is idempotent and only starts a new connection if needed."
  (interactive)
  (if (cody--any-workspace-active-p)
      (unless quiet
        (message "Cody active and ready. M-x `cody-dashboard' for details."))
    (cody--log "------------- Beginning Cody login sequence -------------")
    (setq cody--node-version-status nil)
    (cody--check-node-version)
    (message "Starting Cody...")
    (cody--connection 'start))
  (if (not (cody--alive-p))
      (progn
        (message "Cody failed to connect. See *cody-log* for details.")
        (cody--log "***** Cody login failed *****"))
    (let ((workspace-root (cody--workspace-root)))
      (cody--enable-for-workspace-buffers workspace-root)
      (message "Cody is now tracking %s" workspace-root))))

(defun cody-logout (&optional quiet)
  "Stop the Cody agent process and turn Cody off globally."
  (interactive)
  (ignore-errors
    ;; Don't kill the event buffers; let them truncate automatically.
    ;; They often have useful debugging information.
    (cody--workspace-close-all))
  (when (bound-and-true-p cody--global-mode)
    (cody--global-mode -1))
  (cody--mode-turn-off-in-all-buffers) ; catch any stragglers
  (unless quiet
    (message "Cody has shut down."))
  ;; Force re-check of node version on Cody startup.
  (setq cody--node-version-status nil))

(defun cody--mode-turn-off-in-all-buffers ()
  "Sweep through all buffers and turn off `cody-mode' if it is on."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (bound-and-true-p cody-mode)
          (ignore-errors
            (cody-mode -1)))))))

(defun cody-restart ()
  "Shut down and restart Cody."
  (interactive)
  (let ((cody--node-version-status 'good))
    (ignore-errors (cody-logout)))
  (cody-login))

(defun cody-unload ()
  "Try to remove all traces of Cody, including symbols, hooks, and buffers."
  (interactive)
  ;; Unload the feature, removing the symbol and definitions.
  (when (featurep 'cody)
    (unload-feature 'cody t))
  (message "Cody has been unloaded and all traces have been removed."))

(defun cody-unload-function ()
  "Custom unload function for `cody` package.
Perform custom cleanup and then allow the standard unload process to proceed."
  ;; Close all workspaces and related buffers.
  (ignore-errors
    (cody--workspace-close-all 'kill-buffers))
  ;; Turn off Cody mode in all buffers and globally.
  (cody-logout t)
  (cody--delete-buffer cody-log-buffer-name)

  ;; Cancel any active timers that might still be running.
  (dolist (var '(cody--completion-timer))
    (when (boundp var)
      (let ((timer (symbol-value var)))
        (when (timerp timer)
          (cancel-timer timer)))))

  ;; Reset customizable variables to their default values.
  (dolist (sym (apropos-internal "^cody-" #'boundp))
    (when (custom-variable-p sym)
      (custom-reevaluate-setting sym)))

  ;; Remove mode line icon functions
  (dolist (func (apropos-internal "^cody--" #'fboundp))
    (fmakunbound func))

  ;; Return nil to allow the standard unload process to proceed
  nil)

(defvar cody--inhibit-log-messages nil "Internal variable")

(defun cody--log (msg &rest args)
  "Log MSG with ARGS, currently just for debugging."
  (unless cody--inhibit-log-messages  ; avoid infinite recursion
    (let* ((cody--inhibit-log-messages t)
           (workspace-root (let ((root (cody--workspace-root)))
                             (if root (abbreviate-file-name root)
                               "[no workspace]"))))
      (with-current-buffer (get-buffer-create cody-log-buffer-name)
        (goto-char (point-max))
        (let ((timestamp (format-time-string "[%Y-%m-%d %H:%M:%S] ")))
          (insert workspace-root)
          (insert timestamp (apply #'format msg args) "\n"))))))

(defun cody--clear-log ()
  "Clear the *cody-log* debugging message buffer."
  (interactive)
  (with-current-buffer (get-buffer-create cody-log-buffer-name)
    (let ((inhibit-read-only t))
      (with-silent-modifications
        (erase-buffer)))))

(defun cody--overlay-visible-p ()
  "Return non-nil if Cody is displaying a suggestion in the current buffer."
  (condition-case err
      (when-let* ((active (cody--mode-active-p))
                  (o (car-safe cody--overlay-deltas))
                  (_ (overlayp o))
                  (_ (overlay-buffer o)) ; overlay is positioned/visible somewhere
                  (_ (cody--cc))) ; not a zombie
        t)
    (error (cody--log "Error in cody--overlay-visible-p: %s" err))))

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
  (cody--completion-discard)
  ;; Also call default binding.
  (let (cody-mode) ; avoid recursion
    (call-interactively (key-binding (this-command-keys)))))

(defun cody--maybe-trigger-completion ()
  "Possibly trigger an immediate automatic completion request."
  (when (and cody-completions-auto-trigger-p
             (cody--mode-active-p)
             (not (cody--overlay-visible-p))
             (not (use-region-p))
             (cody--completion-syntactically-eligible-p)
             (not (cody--error-p)))
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
    (error (if (cody--error-p)
               "Cody is experiencing connection errors; try M-x cody-dashboard"
             "Cody-mode not enabled in this buffer.")))
  (unless (or (cody--error-p)
              (not (eq cody--buffer-document-state 'opened)))
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
         ;; Have new requests replace pending ones.
         :deferred 'cody
         :success-fn (lambda (response)
                       (cody--completion-handle-result
                        response buf cursor trigger-kind))
         :error-fn (lambda (err)
                     (cody--log "Error requesting completion: %s" err))
         :timeout-fn (lambda ()
                       (cody--workspace-set-error
                        "Error: request-completion timed out")))))))

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
        cody--completion-last-trigger-spot cody--completion-last-trigger-spot)
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

;;;###autoload
(defun cody-help ()
  "Display help information about Cody in a dedicated buffer."
  (interactive)
  (let ((help-buffer (get-buffer-create "*Cody Help*"))
        (help-text
         "Cody is an AI-powered coding assistant.

Here are some key features and commands to get you started:

* Autocompletion: Cody suggests code as you type (M-\\ or TAB)
* Manual Completion Request: `M-x cody-request-completion`
* Toggle Cody Mode: `M-x cody-mode`
* View Configuration, Troubleshooting: `M-x cody-dashboard`

For more detailed documentation and customization options, please visit:
"))
    (with-current-buffer help-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (ansi-color-apply "Welcome to Cody for Emacs!"))
      (when (display-graphic-p)
        (insert "\n\n")
        (insert-image (cody--icon-cody-logo))
        (insert "\n\n"))
      (insert help-text)
      (help-insert-xref-button "https://github.com/sourcegraph/emacs-cody"
                               'help-url "https://github.com/sourcegraph/emacs-cody")
      (insert "\n\n")
      (goto-char (point-min))
      (help-mode)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer help-buffer))))

(defun cody--delete-buffer (buffer-or-name)
  "Delete the buffer given BUFFER-OR-NAME, which can be a string or a buffer."
  (ignore-errors
    (when buffer-or-name
      (let ((buffer (get-buffer buffer-or-name)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

;; TODO: Switch to telemetry v2.

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
     :url (cody--workspace-uri)
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

;; Server (agent) requests and notifications.

(defun cody--notification-dispatcher (conn method &rest params)
  "Dispatch JSON-RPC notifications from the server based on METHOD and PARAMS.
CONN is the connection to the agent."
  (pcase method
    (`debug/message
     (cody--handle-debug-message (car params)))
    (`editTask/didUpdate
     (cody--handle-edit-task-did-update (car params)))
    (`editTask/didDelete
     (cody--handle-edit-task-did-delete (car params)))
    (`codeLenses/display
     (cody--handle-code-lenses-display (car params)))
    (`ignore/didChange
     (cody--handle-ignore-did-change (car params)))
    (`webview/postMessage
     (cody--handle-webview-post-message (car params)))
    (`webview/postMessageStringEncoded
     (cody--handle-webview-post-message-string-encoded (car params)))
    (`progress/start
     (cody--handle-progress-start (car params)))
    (`progress/report
     (cody--handle-progress-report (car params)))
    (`progress/end
     (cody--handle-progress-end (car params)))
    (`remoteRepo/didChange
     (cody--handle-remote-repo-did-change (car params)))
    (`remoteRepo/didChangeState
     (cody--handle-remote-repo-did-change-state (car params)))
    (_
     (error "Received unknown notification from server: %s" method))))

(defun cody--request-dispatcher (conn method &rest params)
  "Dispatch JSON-RPC requests from the server based on METHOD and PARAMS.
CONN is the connection to the agent."
  (pcase method
    (`window/showMessage
     (cody--handle-window-show-message (car params)))
    (`textDocument/edit
     (cody--handle-text-document-edit (car params)))
    (`textDocument/openUntitledDocument
     (cody--handle-text-document-open-untitled-document (car params)))
    (`textDocument/show
     (cody--handle-text-document-show (car params)))
    (`workspace/edit
     (cody--handle-workspace-edit (car params)))
    (`webview/create
     (cody--handle-webview-create (car params)))
    (_
     (error "Received unknown method from server: %s" method))))

(defun cody--handle-text-document-edit (params)
  "Handle the 'textDocument/edit' request with PARAMS from the server."
  (let((uri (plist-get params :uri))
       (edits (plist-get params :edits)))
    (cody--log "Editing document at %s with %d edits." uri (length edits))
    ;; Example: Applying edits (this is simplified for demonstration purposes)
    (dolist (edit edits)
      (let ((edit-type (plist-get edit :type)))
        (pcase edit-type
          ("replace" (cody--log "Replace text in range %s with %s"
                                (plist-get edit :range)
                                (plist-get edit :value)))
          ("insert" (cody--log "Insert text %s at position %s"
                               (plist-get edit :value)
                               (plist-get edit :position)))
          ("delete" (cody--log "Delete text in range %s"
                               (plist-get edit :range))))))
    t)) ;; Stub implementation returning success

(defun cody--handle-text-document-open-untitled-document (params)
  "Handle the 'textDocument/openUntitledDocument' request with PARAMS from the server."
  (let ((uri (plist-get params :uri))
        (content (plist-get params :content))
        (language (plist-get params :language)))
    (cody--log "Opening untitled document: %s\nContent: %s\nLanguage: %s"
               uri content language)
    t)) ;; Stub implementation returning success

(defun cody--handle-text-document-show (params)
  "Handle the 'textDocument/show' request with PARAMS from the server."
  (let ((uri (plist-get params :uri))
        (options (plist-get params :options)))
    (cody--log "Showing document: %s\nOptions: %s" uri options)
    ;; Example: Focus the specified document (this is simplified for demonstration purposes)
    (when options
      (when (plist-get options :preserveFocus)
        (cody--log "Preserving focus for document: %s" uri))
      (when (plist-get options :preview)
        (cody--log "Showing preview for document: %s" uri))
      (when (plist-get options :selection)
        (cody--log "Selecting range: %s in document: %s" (plist-get options :selection) uri)))
    t)) ;; Stub implementation returning success

(defun cody--handle-workspace-edit (params)
  "Handle the 'workspace/edit' request with PARAMS from the server."
  (let ((operations (plist-get params :operations))
        (metadata (plist-get params :metadata)))
    (cody--log "Applying workspace edit with %d operations" (length operations))
    ;; Example: Handle each type of operation (this is simplified for demonstration purposes)
    (dolist (operation operations)
      (let ((op-type (plist-get operation :type)))
        (pcase op-type
          ("create-file" (cody--log "Creating file: %s\nText: %s"
                                    (plist-get operation :uri)
                                    (plist-get operation :textContents)))
          ("rename-file" (cody--log "Renaming file from %s to %s"
                                    (plist-get operation :oldUri)
                                    (plist-get operation :newUri)))
          ("delete-file" (cody--log "Deleting file: %s"
                                    (plist-get operation :uri)))
          ("edit-file" (cody--log "Editing file: %s with %d edits"
                                  (plist-get operation :uri)
                                  (length (plist-get operation :edits)))))))
    t)) ;; Stub implementation returning success

(defun cody--handle-webview-create (params)
  "Handle the 'webview/create' request with PARAMS from the server."
  (let ((id (plist-get params :id))
        (data (plist-get params :data)))
    (cody--log "Creating webview with id: %s\nData: %s" id data))
  nil) ;; TODO: return value

(defun cody--handle-debug-message (params)
  "Handle 'debug/message' notification with PARAMS from the server."
  (let ((message (plist-get params :message)))
    (cody--log "Agent debug: %s" message)))

(defun cody--handle-edit-task-did-update (params)
  "Handle 'editTask/didUpdate' notification with PARAMS from the server."
  (let ((task-id (plist-get params :id))
        (task-state (plist-get params :state)))
    (cody--log "Edit Task Updated: ID=%s, State=%s" task-id task-state)))

(defun cody--handle-edit-task-did-delete (params)
  "Handle 'editTask/didDelete' notification with PARAMS from the server."
  (let ((task-id (plist-get params :id)))
    (cody--log "Edit Task Deleted: ID=%s" task-id)))

(defun cody--handle-code-lenses-display (params)
  "Handle 'codeLenses/display' notification with PARAMS from the server."
  (cody--log "Display Code Lenses: %s" params))

(defun cody--handle-ignore-did-change (params)
  "Handle 'ignore/didChange' notification."
  (cody--log "Ignore settings changed."))

(defun cody--handle-webview-post-message (params)
  "Handle 'webview/postMessage' notification with PARAMS from the server."
  (let ((id (plist-get params :id))
        (message (plist-get params :message)))
    (cody--log "Webview Post Message: ID=%s, Message=%s" id message)))

(defun cody--handle-webview-post-message-string-encoded (params)
  "Handle 'webview/postMessageStringEncoded' notification with PARAMS from the server."
  (let ((id (plist-get params :id))
        (encoded-message (plist-get params :stringEncodedMessage)))
    (cody--log "Webview Post Message String Encoded: ID=%s, Message=%s" id encoded-message)))

(defun cody--handle-progress-start (params)
  "Handle 'progress/start' notification with PARAMS from the server."
  (let ((id (plist-get params :id))
        (title (plist-get params :title)))
    (cody--log "Progress Start: ID=%s, Title=%s" id title)))

(defun cody--handle-progress-report (params)
  "Handle 'progress/report' notification with PARAMS from the server."
  (let ((id (plist-get params :id))
        (percentage (plist-get params :percentage)))
    (cody--log "Progress Report: ID=%s, Percentage=%s" id (or percentage "N/A"))))

(defun cody--handle-progress-end (params)
  "Handle 'progress/end' notification with PARAMS from the server."
  (let ((id (plist-get params :id)))
    (cody--log "Progress End: ID=%s" id)))

(defun cody--handle-remote-repo-did-change (params)
  "Handle 'remoteRepo/didChange' notification."
  (cody--log "Remote repository list changed."))

(defun cody--handle-remote-repo-did-change-state (params)
  "Handle 'remoteRepo/didChangeState' notification with PARAMS from the server."
  (cody--log "Remote repository fetch state changed: %s" params))

(provide 'cody)
;;; cody.el ends here
