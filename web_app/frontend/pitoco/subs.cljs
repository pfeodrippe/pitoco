(ns pitoco.subs
  (:require
   [clojure.set :as set]
   [re-frame.core :as r]))

(r/reg-sub ::data
  (fn [db] (-> db :current :visualizer :data)))

(r/reg-sub ::api-schemas
  :<- [::data]
  (fn [data]
    (let [result (-> data :>/result)
          diffs (:pitoco.core/api-schemas-diff result)]
      (if (seq diffs)
        (->> (set/join (:pitoco.core/api-schemas result)
                       diffs
                       {:pitoco.api-schema/id :pitoco.api-schema/id})
             (concat (->> diffs
                          (filter #(-> % :response-schema-diff (= :pitoco.api-schema/removed))))))
        (:pitoco.core/api-schemas result)))))

(r/reg-sub ::api-schema-selected-id
  (fn [db] (-> db :current :visualizer :pitoco.api-schema/id)))

(r/reg-sub ::api-schema
  :<- [::api-schemas]
  :<- [::api-schema-selected-id]
  (fn [[api-schemas api-schema-id]]
    (->> api-schemas
         (filter #(-> % :pitoco.api-schema/id (= api-schema-id)))
         first)))

(r/reg-sub ::api-schema-tab
  (fn [db]
    (or (-> db :current :visualizer :api-schema-tab)
        :request-schema)))

(r/reg-sub ::sources
  (fn [db]
    (-> db :current :sources)))

(r/reg-sub ::source-selected
  (fn [db]
    (-> db :current :source-selected)))

(r/reg-sub ::source-base-selected
  (fn [db]
    (-> db :current :source-base-selected)))

(r/reg-sub ::loading?
  (fn [db]
    (-> db :current :loading?)))
