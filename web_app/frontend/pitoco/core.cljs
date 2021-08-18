(ns pitoco.core
  "Quick and dirty page, we should split it into multiple namespaces (and divide
  the components into functions) later."
  (:require
   [clojure.string :as str]
   [day8.re-frame.http-fx]
   [goog.dom :as gdom]
   [pitoco.events :as events]
   [pitoco.subs :as subs]
   [re-frame.core :as r]
   [reagent.dom :as dom]
   [taoensso.sente  :as sente]))

;; Websockets.
(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket-client!
       "/chsk"                          ; Note the same path as before
       nil
       {:type :auto                     ; e/o #{:auto :ajax :ws}
        :port "8081"})]

  (def chsk       chsk)
  (def ch-chsk    ch-recv)           ; ChannelSocket's receive channel
  (def chsk-send! send-fn)           ; ChannelSocket's send API fn
  (def chsk-state state)             ; Watchable, read-only atom
  )

(defmulti ws-msg-handler :event-type)

(defmethod ws-msg-handler :pitoco/sources
  [{:keys [:data]}]
  (r/dispatch [::events/reset-sources (:sources data)]))

(defmethod ws-msg-handler :default
  [_])

(sente/start-chsk-router!
 ch-chsk
 (fn [{:keys [:id :?data]}]
   (when (= id :chsk/recv)
     (ws-msg-handler {:event-type (first ?data)
                      :data (second ?data)}))))

;; Views.
(defn- diff?
  [{:keys [:schema]}]
  (and (map? schema)
       (= (set (keys schema))
          #{:+ :-})))

(defn- grid-template
  [template]
  (->> template
       (map (fn [[positions size]]
              (if (coll? positions)
                (str (->> (map name positions)
                          (str/join " ")
                          pr-str)
                     " "
                     size)
                (str "/ "
                     (->> (map name size)
                          (str/join " "))))))
       (str/join "\n")))

(defn- endpoint
  [api-schema]
  (if (or (:request-schema-diff api-schema)
          (:response-schema-diff api-schema))
    (let [new? (= (:request-schema-diff api-schema) :pitoco.api-schema/new)
          removed? (= (:request-schema-diff api-schema) :pitoco.api-schema/removed)]
      [:div.indicator
       (cond
         new?
         [:div.indicator-item.badge.badge-success.badge-xs
          "New"]

         removed?
         [:div.indicator-item.badge.badge-warning.badge-xs
          "Del"]

         :else
         [:div.indicator-item.badge.badge-secondary.badge-xs
          "- / +"])
       [:span.text-gray-500.text-sm.mr-6.text-xs
        [:a.hover:text-gray-700 {:href "#"
                                 :on-click #(r/dispatch [::events/set-api-schema api-schema])
                                 :class (cond
                                          removed?
                                          :text-yellow-700

                                          new?
                                          :text-green-700

                                          :else
                                          :text-pink-700)}
         (str (:host api-schema) (:path api-schema))]]])
    [:span.text-gray-500.text-sm
     [:a.hover:text-gray-700 {:href "#"
                              :on-click #(r/dispatch [::events/set-api-schema api-schema])}
      (str (:host api-schema) (:path api-schema))]]))

(defn- api-schema-view
  []
  (let [api-schema @(r/subscribe [::subs/api-schema])
        api-schema-tab @(r/subscribe [::subs/api-schema-tab])]
    [:div {:style {:display :grid}}
     [:div
      [:div.bg-gray-50.text-primary-content.p-2.w-auto
       [:div.body.rounded-tl-lg-0 {:style {:display :grid
                                           :grid-template-rows "40px 45px 1fr"}}

        ;; Back button + endpoint.
        [:div.flex.flex-row.inline-flex.items-center
         [:span.p-1
          [:a.button.badge.p-3.mr-5 {:href "#"
                                     :on-click #(r/dispatch [::events/clean-api-schema])}
           "←"]]
         [:span.p-1
          [:div.badge.badge-accent.p-3.text-xs
           (str/upper-case (name (:method api-schema)))]]
         [endpoint api-schema]]

        ;; Tabs.
        (->> [{:text "Request Schema" :identifier :request-schema}
              {:text "Response Schema" :identifier :response-schema}
              (when (and (:request-schema-diff api-schema)
                         ;; TODO: Get rid of these all seqables, we just need
                         ;; a new place to indicate if the schem is new (or removed).
                         (seqable? (:request-schema-diff api-schema)))
                {:text "Request Diff Schema" :identifier :request-diff-schema})
              (when (and (:response-schema-diff api-schema)
                         (seqable? (:response-schema-diff api-schema)))
                {:text "Response Diff Schema" :identifier :response-diff-schema})]
             (remove nil?)
             (mapv (fn [{:keys [:text :identifier]}]
                     [:a.tab.tab-bordered {:class (cond-> []
                                                    (= identifier api-schema-tab)
                                                    (conj :tab-active))
                                           :on-click #(r/dispatch [::events/set-api-schema-tab identifier])}
                      text]))
             (into [:div.tabs.items-baseline]))

        ;; Schema visualization.
        (let [path->response-diff (if (seqable? (:response-schema-diff api-schema))
                                    (->> (:response-schema-diff api-schema)
                                         (mapv second)
                                         (filter diff?)
                                         (group-by :path))
                                    {})
              path->request-diff (if (seqable? (:request-schema-diff api-schema))
                                   (->> (:request-schema-diff api-schema)
                                        (mapv second)
                                        (filter diff?)
                                        (group-by :path))
                                   {})]
          (->> (case api-schema-tab
                 :request-schema (::request-subschemas api-schema)
                 :response-schema (::response-subschemas api-schema)
                 :response-diff-schema (if (seqable? (:response-schema-diff api-schema))
                                         (:response-schema-diff api-schema)
                                         [])
                 :request-diff-schema (if (seqable? (:request-schema-diff api-schema))
                                        (:request-schema-diff api-schema)
                                        []))
               (mapv (fn [[_ row']]
                       (let [{:keys [:path :schema] :as row}
                             (or (and (= api-schema-tab :response-schema)
                                      (first (path->response-diff (:path row'))))
                                 (and (= api-schema-tab :request-schema)
                                      (first (path->request-diff (:path row'))))
                                 row')

                             field (last path)]
                         [:div.text-xs
                          {:style {:margin-left (* (+ 1 (* 3 (count path)))
                                                   8)
                                   :color "#423818"}}
                          [:span.pl-2.pr-2.rounded-l {:class (when-not (or (int? field)
                                                                           (= field :malli.core/in))
                                                               ["bg-yellow-300"])}
                           (cond
                             (= field :malli.core/in) ""
                             (int? field) ""
                             (nil? field) "root"
                             :else (some-> field name))]
                          [:span {:style {:color "#1c3956"}}
                           (if (diff? row)
                             [:<>
                              [:span.bg-red-300.pl-2.pr-2
                               "- " (:- schema)]
                              [:span.bg-green-300.pl-2.pr-2.rounded-r
                               "+ " (:+ schema)]]
                             [:span.bg-blue-300.pl-2.pr-2.rounded-r
                              (cond
                                (coll? schema)
                                (name (first schema))

                                (keyword? schema)
                                (str (symbol schema))

                                :else
                                (str schema))])]])))
               (into [:div])))]]]]))

(defn- schemas-view
  []
  (let [result (-> @(r/subscribe [::subs/data]) :>/result)
        api-schema @(r/subscribe [::subs/api-schema])
        api-schemas @(r/subscribe [::subs/api-schemas])]
    (if api-schema
      [api-schema-view]
      [:div.overflow-x-auto.p-2.bg-gray-50.text-primary-content.w-auto
       (if (::api-schemas result)
         [:table
          (->> api-schemas
               (sort-by (juxt :host :path :method))
               (mapv (fn [{:keys [:method] :as api-schema}]
                       [:tr
                        [:td.p-1
                         [:div.badge.badge-accent.p-3.text-xs
                          (str/upper-case (name method))]]
                        [:td [endpoint api-schema]]]))
               (into [:tbody]))]
         [:span.p-2.text-sm.text-gray-500 "No source selected."])])))

(defn main-view
  []
  [:div.bg-50.p-2
   [:div.navbar.bg-gray-900.shadow-lg.bg-neutral.text-neutral-content {:style {:grid-area :a}}
    [:div.flex-1.px-2.mx-2.text-white.items-center
     [:span "Pitoco"]
     (when @(r/subscribe [::subs/loading?])
       [:div.ml-5.btn.loading])]
    [:button.btn.px-2.text-xs {:on-click #(.click (gdom/getElement "pcap-input"))}
     [:input#pcap-input.hidden {:type :file
                                :on-change (fn [this]
                                             (when-not (= "" (-> this .-target .-value))
                                               (let [^js/File file (-> this .-target .-files (aget 0))]
                                                 (r/dispatch [::events/upload-source file])
                                                 (set! (-> this .-target .-value) ""))))}]
     "Upload File"]]

   [:div.text-xs {:style {:grid-area :b
                          :display :grid
                          :grid-template (grid-template
                                          [[[:a :b] "1fr"]
                                           [:/ ["4fr" "1fr"]]])}}
    [:div {:style {:grid-area :a}}
     [schemas-view]]
    (let [sources @(r/subscribe [::subs/sources])
          source-selected @(r/subscribe [::subs/source-selected])
          source-base-selected @(r/subscribe [::subs/source-base-selected])]
      [:div.place-content-start
       [:div.grid.p-2.bg-gray-50.place-content-end {:style {:grid-area :b
                                                            :align-items :start}}
        [:span.text-lg.folt-extrabold "Available Sources"]
        (into
         [:div]
         (->> sources
              (sort-by :name)
              (map (fn [{:keys [:name] :as source}]
                     [:div.flex
                      [:div {:on-click #(if (= source source-base-selected)
                                          (r/dispatch [::events/source-base-select nil])
                                          (r/dispatch [::events/source-base-select source]))}
                       (if (= source source-base-selected)
                         [:button.grid.w-3.h-10.place-content-center.mt-2.bg-red-500.rounded]
                         [:div.tooltip.tooltip-left {:data-tip "Choose as base"}
                          [:button.grid.w-3.h-10.place-content-center.mt-2.bg-red-300.hover:bg-red-400.rounded]])]
                      [:button.grid.w-36.h-10.place-content-center.mt-2.ml-1
                       {:on-click #(cond
                                     (= source source-selected)
                                     (r/dispatch [::events/source-select nil])

                                     (= source source-base-selected)
                                     (r/dispatch [::events/source-base-select nil])

                                     :else
                                     (r/dispatch [::events/source-select source]))
                        :class (cond
                                 (= source source-selected)
                                 "bg-yellow-200"

                                 (= source source-base-selected)
                                 "bg-red-200"

                                 :else
                                 ["bg-gray-200" "hover:bg-yellow-100"])}
                       [:span.text-gray-800
                        name]]]))))]])]])

(r/dispatch [::events/init])

(comment

  ;; TODO:
  ;; - [x] Show api schemas.
  ;; - [x] Show diff.
  ;; - [x] Upload PCAP file.
  ;; - [x] Upload HAR file.
  ;; - [x] Give option to select current and base sources.
  ;; - [x] Show removed endpoints.
  ;; - [x] Add spinner.
  ;; - [ ] Start/Stop sniffing.
  ;; - [ ] Show path on hover.
  ;; - [ ] Enable users to add regex for new data formats.
  ;; - [ ] Indicate which endpoints had modification when diffing.
  ;; - [x] List available api schemas sources.
  ;; - [ ] Update schemas in real time.
  ;; - [ ] Upload to S3 (test with minio).
  ;; - [ ] Add button to start/stop capturing.
  ;; - [ ] Do things async so we don't have timeout (cache in the backend).

  ())

(defn ^:export refresh
  "During development, shadow-cljs will call this on every hot reload of source. See shadow-cljs.edn"
  []
  ;; re-mounting will cause forced UI refresh, update internals, etc.
  (dom/render [main-view] (.getElementById js/document "app"))
  (js/console.log "Hot reload"))

(defn ^:export main!
  []
  (dom/render [main-view] (.getElementById js/document "app"))
  (js/console.log "Loaded"))
