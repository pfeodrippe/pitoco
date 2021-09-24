(ns pitoco.events
  (:require
   [ajax.core :as ajax]
   [clojure.string :as str]
   [com.wsscode.pathom3.connect.operation.transit :as pcot]
   [com.wsscode.transito :as transito]
   [re-frame.core :as r]))

(defn tread [s]
  (transito/read-str s {:handlers pcot/read-handlers}))

(defn twrite [x]
  (transito/write-str x {:handlers pcot/write-handlers}))

(r/reg-event-fx ::generic-success
  (fn [] {}))

(defn- clear-schema
  [db]
  (assoc-in db [:current :visualizer] nil))

(r/reg-event-fx ::fetch-source
  (fn [{:keys [:db]} [_ source source-base params]]
    (if (nil? source)
      {:db (clear-schema db)}
      {:db (assoc-in db [:current :loading?] true)
       :http-xhrio
       {:method :post
        :uri "http://localhost:8081/pathom"
        :params (twrite
                 {:pathom/eql
                  (if source-base
                    ;; API schemas With diff.
                    [{`(:>/result ~(merge
                                    (if (str/ends-with? (:filepath source) ".har")
                                      {:pitoco.core/har-path (:filepath source)}
                                      {:pitoco.core/pcap-path (:filepath source)})
                                    {:pitoco.core/base
                                     (if (str/ends-with? (:filepath source-base) ".har")
                                       {:pitoco.core/har-path (:filepath source-base)}
                                       {:pitoco.core/pcap-path (:filepath source-base)})}))
                      [{:pitoco.core/api-schemas
                        ['*
                         :pitoco.core/response-subschemas
                         :pitoco.core/request-subschemas]}
                       {:pitoco.core/base [{:pitoco.core/api-schemas
                                            ['*
                                             :pitoco.core/response-subschemas
                                             :pitoco.core/request-subschemas]}]}
                       {:pitoco.core/api-schemas-diff
                        [:pitoco.api-schema/id
                         '*
                         :response-schema-diff
                         :request-schema-diff]}]}]
                    ;; Just API schemas.
                    [{`(:>/result ~(if (str/ends-with? (:filepath source) ".har")
                                     {:pitoco.core/har-path (:filepath source)}
                                     {:pitoco.core/pcap-path (:filepath source)}))
                      [{:pitoco.core/api-schemas
                        ['*
                         :pitoco.core/response-subschemas
                         :pitoco.core/request-subschemas]}]}])})
        :on-success [::fetch-source-success (assoc params :source source)]
        :on-failure [::fetch-source-failure]
        :format (ajax/transit-request-format)
        :response-format (ajax/text-response-format)
        :timeout 300000}})))

(r/reg-event-fx ::fetch-source-success
  (fn [{:keys [:db]} [_ _ response]]
    {:db (-> db
             (update-in [:current :visualizer] merge
                        {:data (tread response)})
             (assoc-in [:current :loading?] false))}))

(r/reg-event-fx ::fetch-source-failure
  (fn [{:keys [:db]}]
    {:db (assoc-in db [:current :loading?] false)}))

(r/reg-event-fx ::upload-source
  (fn [_ [_ file]]
    {:http-xhrio
     {:method :post
      :uri "http://localhost:8081/upload-source"
      :body (doto (js/FormData.)
              (.append "file" file))
      :on-success [::upload-source-success]
      :on-failure [::upload-source-failure]
      :response-format (ajax/raw-response-format)
      :timeout 20000}}))

(r/reg-event-fx ::upload-source-success
  (fn [] {}))

(r/reg-event-fx ::upload-source-failure
  (fn [_ [_ _response]]
    nil))

(r/reg-event-fx ::init
  (fn []
    {:http-xhrio
     {:method :get
      :uri "http://localhost:8081/init"
      :response-format (ajax/raw-response-format)
      :on-success [::generic-success]
      :timeout 20000}}))

(r/reg-event-fx ::set-api-schema
  (fn [{:keys [:db]} [_ api-schema]]
    {:db (assoc-in db [:current :visualizer :pitoco.api-schema/id] (:pitoco.api-schema/id api-schema))}))

(r/reg-event-fx ::clean-api-schema
  (fn [{:keys [:db]}]
    {:db (update-in db [:current :visualizer] dissoc :pitoco.api-schema/id :api-schema-tab)}))

(r/reg-event-fx ::set-api-schema-tab
  (fn [{:keys [:db]} [_ k]]
    {:db (assoc-in db [:current :visualizer :api-schema-tab] k)}))

(r/reg-event-fx ::reset-sources
  (fn [{:keys [:db]} [_ sources]]
    {:db (assoc-in db [:current :sources] sources)}))

(r/reg-event-fx ::source-select
  (fn [{:keys [:db]} [_ source]]
    {:db (assoc-in db [:current :source-selected] source)
     :dispatch [::fetch-source
                source
                (get-in db [:current :source-base-selected])
                {:old-source (get-in db [:current :source-selected])}]}))

(r/reg-event-fx ::source-base-select
  (fn [{:keys [:db]} [_ source-base]]
    (merge {:db (assoc-in db [:current :source-base-selected] source-base)}
           (when (get-in db [:current :source-selected])
             {:dispatch [::fetch-source
                         (get-in db [:current :source-selected])
                         source-base
                         {:old-source (get-in db [:current :source-selected])}]}))))
