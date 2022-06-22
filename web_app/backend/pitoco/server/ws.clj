(ns pitoco.server.ws
  (:require
   [taoensso.sente :as sente]
   [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]))

;; Sente.
(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
      ;; Disable CSRF as this is supposed to be used for localhost.
      (sente/make-channel-socket! (get-sch-adapter) {:csrf-token-fn nil})]

    (def ring-ajax-post                ajax-post-fn)
    (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
    (def ch-chsk                       ch-recv) ; ChannelSocket's receive channel
    (def chsk-send!                    send-fn) ; ChannelSocket's send API fn
    (def connected-uids                connected-uids)) ; Watchable, read-only atom


(defmulti ws-msg-handler :event-type)

(defmethod ws-msg-handler :default
  [_])

(sente/start-chsk-router!
 ch-chsk
 (fn [{:keys [:id]}]
   (when (= id :chsk/uidport-open)
     (ws-msg-handler {:event-type :chsk/uidport-open}))))
