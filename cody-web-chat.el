;;; cody-web-chat.el --- Cody Chat in a Web Browser -*- lexical-binding: t; -*-
;; Author: Dominic Cooney <dominic.cooney@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Resources for a chat web client integrated with emacs
;; through an emacs hosted web-server.

;;; Code:

(defun cody--web-rewrite-root-html (html)
  "Rewrites Webview root HTML.
Adds thunks to communicate with the emacs client."
  (let ((api "<head>
<script>
globalThis.acquireVsCodeApi = (function() {
  let acquired = false;
  let state = undefined;

  let socket = new WebSocket(`ws://${window.location.host}/ws${window.location.search}`)
  socket.onmessage = (event) => {
    let message = JSON.parse(event.data);
    switch (message.what) {
    case 'postMessageStringEncoded':
      let synthEvent = new CustomEvent('message');
      synthEvent.data = JSON.parse(message.data);
      window.dispatchEvent(synthEvent);
      break;
    default:
      console.warn('do not know how to handle ws message', message);
      break;
    }
  };
  socket.onclose = (event) => {
    console.warn('socket closed, NYI reconnection', event);
  };
  socket.onerror = (event) => {
    console.warn('socked error, NYI reconnection', event);
  };

  return () => {
    if (acquired) {
      throw new Error('VsCodeApi already acquired');
    }
    acquired = true;
    return Object.freeze({
      postMessage: function(message) {
        socket.send(JSON.stringify({
          what: 'postMessageStringEncoded',
          data: JSON.stringify(message)
        }));
      },
      setState: function(newState) {
        console.log('not yet implemented: setState', newState);
        state = newState;
      },
      getState: function() {
        console.log('not yet implemented: getState', state);
        return state;
      }
    });
  };
})();
delete window.parent;
delete window.top;
delete window.frameElement;
</script>
"))
    (replace-regexp-in-string "<head>" api html t t)))
