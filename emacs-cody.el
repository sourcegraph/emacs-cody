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

(defun cody--handle-notification (_ method params)
  ""
  (pcase method
    ;; keep the latest message until nil then send it
    ('chat/updateMessageInProgress
     (if params
         (setq cody--message-in-progress (plist-get params :displayText))
       (message "assistant:%s" cody--message-in-progress)))))

(defun cody-shutdown ()
  ""
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
  (message "Cody recipe sent. Will update minibuffer asynchronously."))

;; (display-buffer (jsonrpc-events-buffer (cody--connection)))
;; (cody--request 'recipes/list)
;; (cody--connection)
;; (cody--alive-p)
;; (cody-shutdown)

;;; emacs-cody.el ends here
