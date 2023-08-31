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
  "Path to cody-agent.js.")

(defvar cody--connection nil "")
(defvar cody--message-in-progress nil "")
(defvar cody--access-token nil "")

(defmacro with-buffer (buf form)
  "Executes FORM in buffer BUF.
BUF can be a buffer name or a buffer object.
If the buffer doesn't exist, it's created."
  `(let ((buffer (gentemp)))
    (setq buffer
	  (if (stringp ,buf)
	      (get-buffer-create ,buf)
	    ,buf))
    (save-excursion
      (set-buffer buffer)
      ,form)))

(put 'with-buffer 'lisp-indent-function 1)
(def-edebug-spec with-buffer t)

;; Add to you ~/.authinfo.gpg something that looks like
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
  "Helper to send a cody request for a method with one argument."
  (jsonrpc-request (cody--connection) method params))

(defun cody--alive-p ()
  ""
  (and cody--connection
       (zerop (process-exit-status (jsonrpc--process cody--connection)))))

(defun cody--connection ()
  ""
  ;; TODO check node version and allow overriding location of node
  (unless (cody--alive-p)
    (setq cody--connection
          (make-instance 'jsonrpc-process-connection
                         :name "cody"
                         :events-buffer-scrollback-size nil
                         :notification-dispatcher #'cody--handle-notification
                         :process (make-process :name "cody"
                                                :command (list "node" cody--cody-agent)
                                                :coding 'utf-8-emacs-unix
                                                :connection-type 'pipe
                                                :stderr (get-buffer-create "*cody stderr*")
                                                :noquery t)))

    (message "Cody started.")

    ;; The 'initialize' request must be sent at the start of the connection
    ;; before any other request/notification is sent.
    (jsonrpc-request cody--connection 'initialize
                     (list
                      :name "emacs"
                      :version "0.1"
                      :workspaceRootPath "" ;; TODO
                      :connectionConfiguration (cody--connection-configuration)))

    ;; The 'initalized' notification must be sent after receiving the 'initialize' response.
    (jsonrpc-notify cody--connection 'initialized nil))
  cody--connection)

;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/JSONRPC-Overview.html
;; for a description of the parameters for this jsonrpc notification callback.
(defun cody--handle-notification (_ method params)
  "Handle notifications from the agent, e.g. shutdown."
  (cl-case method
    ('chat/updateMessageInProgress
    ;; The agent sends an update with increasingly long hunks of the response,
    ;; e.g. "Here", "Here is", "Here is an", "Here is an explanation", ...
    ;; This permits a typewriter effect.
     (with-buffer "*cody-chat*"
       (if params
           (let* ((old-text cody--message-in-progress)
                  (new-text (plist-get params :displayText))
                  (tail (if (>= (length new-text) (length old-text))
                            (substring new-text (length old-text))
                          ;; This could happen if the generator "backs up",
                          ;; which I've seen happen with ChatGPT, so we'll
                          ;; likely need to handle this at some point.
                          "")))
             (insert tail)
             (setq cody--message-in-progress new-text))
         (newline))))))
       
(defun cody-shutdown ()
  "Stop the Cody agent process."
  (interactive)
  (when (cody--alive-p)
    (cody--request 'shutdown)
    (kill-process (jsonrpc--process cody--connection))
    (setq cody--connection nil)))

(defun cody ()
  ""
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

(defun cody--log (msg &rest args)
  "Log a message, currently just for debugging"
  ;; TODO: Use a real logging package
  (save-excursion
    (set-buffer (get-buffer-create "*cody-log*"))
    (goto-char (point-max))
    (insert (apply #'format msg args) "\n")))

(defun cody-chat ()
  "Shorthand for the chat recipe.
Output goes into the *cody-chat* buffer."
  (interactive)
  (let ((chatbuf (get-buffer-create "*cody-chat*")))
    (display-buffer chatbuf)
    (with-buffer chatbuf
      (goto-char (point-max)))
    (cody--request 'recipes/execute
                   :id "chat-question"
                   :humanChatInput (read-from-minibuffer "Ask Cody: "))
    (message "Awaiting response from Cody...")))

        
;; (display-buffer (jsonrpc-events-buffer (cody--connection)))
;; (cody--request 'recipes/list)
;; (cody--connection)
;; (cody--alive-p)
;; (cody-shutdown)

;;; emacs-cody.el ends here
