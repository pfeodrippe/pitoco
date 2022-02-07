(ns pitoco.instrument
  (:require
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [pitoco.core :as pit]
   [clojure.java.io :as io])
  (:import
   (java.time LocalDateTime)))

(defn find-vars
  "Find vars in your project according to one or more of the following queries:
  - `:namespaces` (collection of namespaces in symbol or string format)
  - `:ns-meta` (namespaces which contain this metadata)
  - `:ns-prefix` (namespaces with this prefix)
  - `:vars` (collection of vars in symbol or string format)
  - `:var-meta` (vars which contain this metadata)"
  [{:keys [namespaces ns-meta ns-prefix vars var-meta]}]
  (when (or (seq namespaces) ns-meta (seq ns-prefix) (seq vars) var-meta)
    (let [namespaces-set (set (mapv str namespaces))
          vars-set (set (mapv (comp str symbol) vars))]
      (cond->> (all-ns)
        (seq ns-prefix)  (filter #(str/starts-with? % ns-prefix))
        (seq namespaces) (filter #(contains? namespaces-set (str %)))
        ns-meta          (filter #(-> % meta ns-meta))
        true             (mapv ns-interns)
        true             (mapcat vals)
        (seq vars)       (filter #(contains? vars-set (str (symbol %))))
        var-meta         (filter #(-> % meta var-meta))))))

(defn instrument!
  "Instrument vars of your choice.

  `:mode` can be `:instrument` to instrument or `:unstrument` to remove the
  instrumentation.

  See `pitoco.instrument/find-vars`."
  [vars {:keys [mode]}]
  (doseq [v vars]
    (case mode
      :instrument
      (let [original-fn (or (::original-fn (meta v))
                            (deref v))
            input-output* (atom [])]
        (when (fn? original-fn)
          (alter-meta! v assoc
                       ::original-fn original-fn
                       ::input-output input-output*
                       ::original-meta (meta v))
          (alter-var-root v (constantly (fn [& args]
                                          (let [value (apply original-fn args)]
                                            (swap! input-output* conj {:input args
                                                                       :output value})
                                            value))))))

      :unstrument
      (when-let [original-fn (::original-fn (meta v))]
        (when (:doc (::original-meta (meta v)))
          (alter-meta! v assoc :doc (:doc (::original-meta (meta v)))))
        (alter-meta! v dissoc ::original-fn ::input-output ::original-meta :malli/schema)
        (alter-var-root v (constantly original-fn))))))

(defn- transpose [m]
  (apply mapv vector m))

(defn infer-schemas!
  "Infer schemas after some run (e.g. run some test which call the vars of your
   interest).

  See `pitoco.instrument/find-vars`."
  [vars]
  (->> vars
       (filter #(some-> (meta %) ::input-output deref seq))
       (mapv (fn [v]
               (try
                 (let [input-output (some-> (meta v) ::input-output deref)
                       schema (into [:function {:pitoco/generated-at (str (LocalDateTime/now))}]
                                    (->> (group-by (comp count :input) input-output)
                                         (mapv val)
                                         (mapv (fn [per-arity]
                                                 [:=>
                                                  (->> (transpose (mapv :input per-arity))
                                                       (mapv (fn [arg-values]
                                                               (pit/infer-schema arg-values)))
                                                       (into [:cat]))
                                                  (pit/infer-schema (mapv :output per-arity))]))))]
                   (alter-meta! v assoc :malli/schema schema)
                   (alter-meta! v assoc :doc (str (:doc (::original-meta (meta v)))
                                                  "\n\n### Generated Malli Schema ###\n\n"
                                                  (with-out-str (pprint/pprint schema))))
                   [v :ok])
                 (catch Error e
                   [v e]))))))

(defn store-instrumented-vars!
  "Save generated file inside the `.pitoco` folder at your root with some runtime information
  about the inferred vars schema and some associated data.

  It can be used on your own or in conjunction with the VSCode Pitoco extension, see
  https://marketplace.visualstudio.com/items?itemName=feodrippe.pitoco-extension."
  [vars]
  (let [file-path ".pitoco/generated-files/pitoco-schemas.edn"
        _ (io/make-parents (io/file file-path))
        vars' (filter #(some-> (meta %) ::input-output deref seq) vars)
        namespaces (->> vars'
                        (mapv (comp :ns meta))
                        distinct
                        (mapv (fn [n]
                                [(str n)
                                 {:ns-aliases
                                  (->> (ns-aliases n)
                                       (mapv (fn [[k v]]
                                               [(str k) (str v)]))
                                       (into {}))

                                  :ns-interns
                                  (->> (ns-interns n)
                                       (mapv (fn [[k v]]
                                               [(str k) (str (symbol v))]))
                                       (into {}))

                                  :ns-refers
                                  (->> (ns-refers n)
                                       (mapv (fn [[k v]]
                                               [(str k) (str (symbol v))]))
                                       (remove (comp #(str/starts-with? % "clojure.core/") last))
                                       (into {}))}]))
                        (into {}))]
    (->> {:namespaces namespaces
          :vars (->> vars'
                     (mapv (fn [v]
                             (let [{:keys [:malli/schema
                                           ::input-output
                                           :arglists]}
                                   (meta v)
                                   {:keys [:input :output]} (-> input-output deref first)]
                               [(str (symbol v))
                                {:malli/schema schema
                                 :example {:inputs-str (->> input
                                                            (mapv
                                                             (fn [in]
                                                               (with-out-str
                                                                 (pprint/pprint in)))))
                                           :output-str (with-out-str
                                                         (pprint/pprint output))}
                                 :arity->arglist (->> arglists
                                                      (mapv (juxt count identity))
                                                      (into {}))}])))
                     (into {}))}
         pprint/pprint
         with-out-str
         (spit file-path))))

(comment

  (-> (find-vars {:namespaces ['pitoco.core]})
      (instrument! {:mode :instrument}))

  (infer-schemas! (find-vars {:namespaces ['pitoco.core]}))

  (-> (find-vars {:namespaces ['pitoco.core]})
      (instrument! {:mode :unstrument}))

  ())
