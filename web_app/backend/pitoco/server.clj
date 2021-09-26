(ns pitoco.server
  (:require
   [com.wsscode.transito :as transito]
   [cognitect.transit :as t]
   [com.wsscode.pathom3.connect.operation.transit :as pcot]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [muuntaja.core :as muuntaja]
   [reitit.ring.middleware.muuntaja :as reitit-muuntaja]
   [reitit.ring.middleware.exception :as exception]
   [reitit.ring.middleware.multipart :as multipart]
   [ring.middleware.keyword-params :as ring-keyword-params]
   [ring.middleware.session :as ring-session]
   [reitit.coercion.spec]
   [reitit.ring.coercion :as coercion]
   [ring.middleware.params :as params]
   [muuntaja.middleware :as middleware]
   [pitoco.core :as pit]
   [ring.middleware.cors :as cors]
   [reitit.ring :as ring]
   [clojure.java.io :as io]
   [org.httpkit.server :refer [run-server]]
   [pitoco.server.ws :as ws]))

;; Helper functions.
(defn- tread [s]
  (transito/read-str s {:handlers pcot/read-handlers}))

(defn- twrite [x]
  (transito/write-str x {:handlers pcot/write-handlers
                         :write-meta? false}))

;; Handlers.
(def ^:private pathom (p.eql/boundary-interface pit/pathom-env))

(defn- pathom-handler
  [{:keys [body-params]}]
  (try
    {:status 200
     :body   (twrite (pathom (tread body-params)))}
    (catch Exception e
      (println e e)
      (throw e))))

(def ^:private pitoco-upload-sources-folder
  (io/file "../.pitoco/uploaded-sources"))

(defn- send-available-sources
  []
  (ws/chsk-send! :sente/all-users-without-uid
                 [:pitoco/sources
                  {:sources (->> (file-seq pitoco-upload-sources-folder)
                                 (filter #(.isFile %))
                                 (mapv #(hash-map :id (hash (.getPath %))
                                                  :name (.getName %)
                                                  :filepath (str %))))}]))

(defn- upload-source-handler
  [{:keys [:parameters]}]
  (let [{:keys [:tempfile :filename]} (-> parameters :multipart :file)]
    (.mkdirs pitoco-upload-sources-folder)
    (io/copy tempfile (io/file (str pitoco-upload-sources-folder "/" filename)))
    (send-available-sources)
    {:status 200}))

;; Server.
(def ^:private muuntaja-options
  (update-in
   muuntaja/default-options
   [:formats "application/transit+json"]
   merge {:decoder-opts {:handlers pcot/read-handlers}
          :encoder-opts {:handlers  pcot/write-handlers
                         :transform t/write-meta}}))

(def ^:private routes
  [["/pathom" {:post pathom-handler}]
   ["/upload-source" {:post {:parameters {:multipart {:file multipart/temp-file-part}}
                             :handler upload-source-handler}}]
   ["/chsk" {:get {:handler ws/ring-ajax-get-or-ws-handshake}
             :post {:handler ws/ring-ajax-post}}]
   ["/init" {:get {:handler (fn [_]
                              (send-available-sources)
                              {:status 200})}}]])

(def app
  (-> (ring/ring-handler
       (ring/router
        routes
        {:data {:coercion reitit.coercion.spec/coercion
                :muuntaja muuntaja/instance
                :middleware [ ;; query-params & form-params
                             params/wrap-params

                             ring-keyword-params/wrap-keyword-params
                             ring-session/wrap-session

                             ;; content-negotiation
                             reitit-muuntaja/format-negotiate-middleware
                             ;; encoding response body
                             reitit-muuntaja/format-response-middleware
                             ;; exception handling
                             exception/exception-middleware
                             ;; decoding request body
                             reitit-muuntaja/format-request-middleware
                             ;; coercing response bodys
                             coercion/coerce-response-middleware
                             ;; coercing request parameters
                             coercion/coerce-request-middleware
                             ;; multipart
                             multipart/multipart-middleware]}}))
      (cors/wrap-cors :access-control-allow-origin [#"http://localhost:8000"]
                      :access-control-allow-methods [:get :post])
      (middleware/wrap-format muuntaja-options)))

;; Websockets.
(defmethod ws/ws-msg-handler :chsk/uidport-open
  [_]
  (send-available-sources))

(comment

  (def server
    (run-server app {:port 8081 :join? false}))
  (server)

  ())

(when (resolve `server)
  (eval '(server)))
(def server
  (run-server app {:port 8081
                   :join? false}))

(comment

  ;; TODO:
  ;; - [ ] Cache data so same source is processed once (pcap files
  ;;       can take a while to generate the dat files).

  ())
