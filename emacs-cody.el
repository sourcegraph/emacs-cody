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

(defvar cody---connection nil "")

;; Add to you ~/.authinfo.gpg something that looks like
;;
;;  machine sourcegraph.sourcegraph.com login apikey password sgp_SECRET
(defun cody--access-token ()
  ""
  ;; We are looking for an API key, so look for the first entry where the secret
  ;; starts with sgp_
  (seq-some (lambda (found)
              (let ((token (auth-info-password found)))
                (if (string-prefix-p "sgp_" token) token)))
            (auth-source-search
             :max 10
             :host "sourcegraph.sourcegraph.com"
             :require '(:secret :host))))

(defun cody--connection-configuration ()
  ""
  `(:accessToken ,(cody--access-token)
    :serverEndpoint "https://sourcegraph.sourcegraph.com"
    :codebase "https://github.com/keegancsmith/emacs-cody"))

(cl-defun cody--request (method &rest argument &allow-other-keys)
  "Helper to send a cody request for a method with one argument."
  (jsonrpc-request (cody--connection) method argument))

(defun cody--connection ()
  ""
  ;; TODO check node version and allow overriding location of node
  (unless (and cody---connection
               (zerop (process-exit-status (jsonrpc--process cody---connection))))
    (setq cody---connection
          (make-instance 'jsonrpc-process-connection
                         :name "cody"
                         :events-buffer-scrollback-size 0
                         :notification-dispatcher #'cody--handle-notification
                         :process (make-process :name "cody"
                                                :command (list "node" cody--cody-agent)
                                                :coding 'utf-8-emacs-unix
                                                :connection-type 'pipe
                                                :stderr (get-buffer-create "*cody stderr*")
                                                :noquery t)))
    (message "Cody started.")
    (cody--request 'initialize
               :name "emacs"
               :version "0.1"
               :workspaceRootPath "" ;; TODO
               :connectionConfiguration (cody--connection-configuration)))
  cody---connection)

(defun cody--handle-notification (_ method msg)
  ""
  (message "NOTIF %s %s" method msg))

;;; emacs-cody.el ends here
