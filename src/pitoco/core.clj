(ns pitoco.core
  (:require
   [babashka.process :as sh]
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test.check.generators :as gen]
   [clojure.walk :as walk]
   [lambdaisland.deep-diff2 :as ddiff]
   [lambdaisland.uri :as uri]
   [malli.core :as m]
   [malli.generator :as mg]
   [malli.registry :as mr]
   [malli.swagger :as swagger]
   [malli.util :as mu]
   [pitoco.api-schema :as api-schema]
   [spec-provider.stats :as stats]
   [tick.core :as t]

   ;; Pathom.
   [com.wsscode.pathom3.cache :as p.cache]
   [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
   [com.wsscode.pathom3.connect.built-in.plugins :as pbip]
   [com.wsscode.pathom3.connect.foreign :as pcf]
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.connect.operation.transit :as pcot]
   [com.wsscode.pathom3.connect.planner :as pcp]
   [com.wsscode.pathom3.connect.runner :as pcr]
   [com.wsscode.pathom3.error :as p.error]
   [com.wsscode.pathom3.format.eql :as pf.eql]
   [com.wsscode.pathom3.interface.async.eql :as p.a.eql]
   [com.wsscode.pathom3.interface.eql :as p.eql]
   [com.wsscode.pathom3.interface.smart-map :as psm]
   [com.wsscode.pathom3.path :as p.path]
   [com.wsscode.pathom3.plugin :as p.plugin])
  (:import
   (java.time.format DateTimeFormatter)
   (java.time LocalDate Instant)
   (rawhttp.core RawHttp)
   (lambdaisland.deep_diff2.diff_impl Mismatch Deletion Insertion)))

(defn uuid-str?
  [s]
  (boolean (try (java.util.UUID/fromString s)
                (catch Exception _))))

(def UUIDStr
  (m/schema
   (m/-simple-schema
    {:type ::UUIDStr
     :pred uuid-str?
     :type-properties {:json-schema/type "string"
                       :json-schema/description ::UUIDStr
                       :gen/gen (gen/fmap str gen/uuid)}})))

(defn iso-date?
  [s]
  (boolean
   (try
     (LocalDate/parse s DateTimeFormatter/ISO_DATE)
     (catch Exception _))))

(def IsoDate
  (m/schema
   (m/-simple-schema
    {:type ::IsoDate
     :pred iso-date?
     :type-properties {:json-schema/type "string"
                       :json-schema/description ::IsoDate
                       :gen/gen (gen/fmap #(str (t/date (t/instant %)))
                                          (mg/generator inst?))}})))

(defn iso-instant?
  [s]
  (boolean
   (try
     (Instant/parse s)
     (catch Exception _))))

(def IsoInstant
  (m/schema
   (m/-simple-schema
    {:type ::IsoInstant
     :pred iso-instant?
     :type-properties {:json-schema/type "string"
                       :json-schema/description ::IsoInstant
                       :gen/gen (gen/fmap #(str (t/instant %))
                                          (mg/generator inst?))}})))

(def default-string-data-formats
  [UUIDStr IsoDate IsoInstant])

(def default-data-formats
  (concat default-string-data-formats []))

(defn- json-parse-string
  "Don't simply use `keyword`, some of the keys can be
  degenerated and really should be kept as string. E.g.
  uuids or names of people."
  [s]
  (json/parse-string
   s
   (fn [k-str]
     ;; If our key is one of the fomatted ones or if it has a space, do not
     ;; convert it to a keyword
     ;; TODO: This appears to have some non-neligible performance hit, let's
     ;; check it later.
     (if (or (str/includes? k-str " ")
             (str/includes? k-str "@")
             (some #(when (m/validate % k-str)
                      k-str)
                   default-string-data-formats))
       k-str
       (keyword k-str)))))

(def default-registry
  (mr/composite-registry
   (m/-registry)
   (->> default-data-formats
        (mapv (fn [df]
                [(m/type df) df]))
        (into {}))))

(defn process-options
  [{:keys [::schemas]}]
  {:registry (if schemas
               (mr/composite-registry
                default-registry
                (->> schemas
                     (mapv (fn [sch]
                             [(m/type sch) sch]))
                     (into {})))
               default-registry)})

(defn generate-sample
  ([schema]
   (generate-sample schema {}))
  ([schema options]
   (mg/generate schema (process-options options))))

(def ^:private original-preds
  stats/preds)

(defn value-schema
  "Create schema for a single value.
  The type is itself."
  [v]
  (m/schema
   (m/-simple-schema {:type v
                      :pred #(= % v)
                      :type-properties {::itself? true
                                        :gen/gen (gen/elements [v])}})))

(defn- new-data-formats
  [schemas]
  (->> default-data-formats
       (concat schemas)
       (reduce (fn [acc data-format]
                 ;; Prioritize first validators if there is a tie, we do this
                 ;; by putting a check in the lower priority validators
                 ;; that it's not one of higher priorities.
                 (->> (m/-simple-schema
                       ;; We only use `:type` and `:pred` and `:type-properties`
                       ;; to infer the schema, so we only care about these here.
                       {:type (m/type data-format)
                        :pred #(boolean
                                (when-not (some (fn [df]
                                                  (m/validate df %))
                                                acc)
                                  (m/validate data-format %)))
                        :type-properties (m/type-properties data-format)})
                      (conj acc)))
               [])))

(defn collect-stats
  [samples {existing-data-formats ::data-formats
            existing-stats ::stats :as options}]
  (binding [stats/preds (concat original-preds (mapv m/validator existing-data-formats))]
    (reduce (fn [stats x]
              (stats/update-stats stats x options))
            existing-stats
            samples)))

;; TODO: Maybe find hash map patterns by storing their hashes while we iterate
;; over `stats`?
(defn infer-schema
  "Infer schema from a collection of samples.

  Set it to `falsy` if you want the output in the form of preds/schemas."
  ([samples]
   (infer-schema samples {}))
  ([samples {:keys [::schemas]
             existing-data-formats ::data-formats
             existing-stats ::stats
             :or {schemas []}
             :as options}]
   ;; We cannot have `existing-stats` set and not have the `existing-data-formats`
   ;; are not serializable and each new evaluation here (from `new-data-formats`)
   ;; would return different values. We will try to deal with it in the future.
   (when (and existing-stats (not existing-data-formats))
     (throw (ex-info (str "`existing-data-formats` should have a value when `existing-stats` "
                          "is not `nil`.")
                     {:existing-stats existing-stats
                      :existing-data-formats existing-data-formats})))
   (let [data-formats' (or existing-data-formats (new-data-formats schemas))
         ;; `data-formats` needs to be exactly the same to match correctly as
         ;; they use the preds for matching, which are just functions which
         ;; are created on the fly (not equal as each evaluation returns a
         ;; different function). We could work in the future to make them
         ;; serializable using `malli` itself (with `sci`).
         stats (collect-stats samples (assoc options ::data-formats data-formats'))
         ;; `itself` data formats are the ones which check to themselves. They
         ;; are used to represent keys in a map so you can have a less generic
         ;; type if all of the map keys are `itself`.
         itself-value? (fn [v]
                         (->> data-formats'
                              (filter #(-> % m/type-properties ::itself?))
                              (mapv #(m/validate % v))
                              (some true?)))
         validators (set (mapv m/validator data-formats'))
         ;; TODO: Let's try to make this as deterministic as possible
         ;; so we can compare schema data directly without having to
         ;; define a powerful form of equivalence.
         schema
         (fn schema [stats]
           (let [;; We remove the preds which have a lower hierarchy (custom ones
                 ;; always are in a higher level).
                 invalid-original-validators (->> (::stats/pred-map stats)
                                                  (remove (comp (conj validators map? set? sequential?) first))
                                                  (filter (fn [[spec-type]]
                                                            (->> (::stats/distinct-values stats)
                                                                 (filter spec-type)
                                                                 (every? #(some (fn [type] (m/validate type %))
                                                                                data-formats')))))
                                                  set)
                 types' (->> (::stats/pred-map stats)
                             (remove invalid-original-validators)
                             (mapv (fn [[spec-type]]
                                     (let [data-format (delay (->> data-formats'
                                                                   (filter (comp #{spec-type} m/validator))
                                                                   first))
                                           res (delay (condp = spec-type
                                                        map? (if (and (or (some-> stats
                                                                                  ::stats/map
                                                                                  ::stats/non-keyword-sample-count
                                                                                  pos?)
                                                                          (some-> stats
                                                                                  ::stats/map
                                                                                  ::stats/mixed-sample-count
                                                                                  pos?))
                                                                      ;; If all preds are part of
                                                                      ;; a schema which checks itself,
                                                                      ;; (ignoring keywords), then
                                                                      ;; we have a one to one mapping
                                                                      ;; and we can use these schemas
                                                                      ;; as keys (no need for `:map-of`.
                                                                      (->> (get-in stats [::stats/map ::stats/keys])
                                                                           keys
                                                                           (remove keyword?)
                                                                           (every? itself-value?)
                                                                           not))
                                                               ;; If we have some non keyword key,
                                                               ;; we move to a more generic map
                                                               ;; using `:map-of`.
                                                               [:map-of
                                                                (infer-schema
                                                                 (->> (get-in stats [::stats/map ::stats/keys])
                                                                      keys
                                                                      ;; Convert any keyword to string
                                                                      ;; to be in sync with the JSON
                                                                      ;; format.
                                                                      (mapv #(if (keyword? %) (name %) %)))
                                                                 ;; We remove `::stats` here as
                                                                 ;; we want a clean slate for these.
                                                                 (dissoc options ::stats))
                                                                (let [map-of-types (some->>
                                                                                    (get-in stats [::stats/map ::stats/keys])
                                                                                    vals
                                                                                    (mapv #(schema %))
                                                                                    set)]
                                                                  (if (> (count map-of-types) 1)
                                                                    ;; Here we could have nested `:or`s
                                                                    ;; which could be simplified, but
                                                                    ;; not a priority now.
                                                                    (into [:or] (sort-by str map-of-types))
                                                                    (first map-of-types)))]
                                                               (->> (get-in stats [::stats/map ::stats/keys])
                                                                    (mapv (fn [[k nested-stats]]
                                                                            ;; If some key has less samples
                                                                            ;; than the count of maps, then
                                                                            ;; this is a optional key.
                                                                            (if (< (::stats/sample-count nested-stats)
                                                                                   (get-in stats [::stats/map ::stats/sample-count]))
                                                                              [k {:optional true} (schema nested-stats)]
                                                                              [k (schema nested-stats)])))
                                                                    (sort-by first)
                                                                    (into [:map])))
                                                        string? :string
                                                        integer? :int
                                                        set? [:set (schema (::stats/elements-set stats))]
                                                        sequential? [:sequential (schema (::stats/elements-coll stats))]
                                                        nil? :nil
                                                        stats/float? 'number?
                                                        stats/double? 'number?
                                                        decimal? 'decimal?
                                                        number? 'number?
                                                        boolean? :boolean
                                                        inst? 'inst?
                                                        symbol? :symbol
                                                        keyword? :keyword
                                                        nil))]
                                       (cond
                                         @res
                                         @res

                                         @data-format
                                         (m/type @data-format)

                                         :else
                                         :any)))))
                 types (remove #{:any :nil} types')]
             (cond
               (zero? (count types'))
               :any

               ;; Convert `:nil` to `:any` as
               ;; it's very likely that a parameter
               ;; is not really only `nil`, it's only
               ;; that we are not testing all the
               ;; possible cases.
               (= (set types') #{:nil})
               :any

               (= (count types') 1)
               (first types')

               (= (set types') #{:any :nil})
               :any

               (some #{:nil} types')
               [:maybe
                (if (= (count types) 1)
                  ;; When there is some `:any` together some other types, we can
                  ;; get rid of the any.
                  (first types)
                  (into [:or] (sort-by str types)))]

               :else
               (if (= (count types) 1)
                 (first types)
                 (into [:or] (sort-by str types))))))]
     (schema stats))))

;; TODO: Remove this.
(defn pprint
  "See https://docs.cider.mx/cider/usage/pretty_printing.html.

  Used for cider output so it does not hang emacs when trying to print a long
  line, should be used for debug only."
  [value writer options]
  (apply clojure.pprint/write
         (try (walk/prewalk (fn [v]
                              (if (and (string? v)
                                       (> (count v) 97))
                                (str (subs v 0 97) "...")
                                v))
                            value)
              (catch Exception _
                value))
         (mapcat identity (assoc options :stream writer))))

(def default-regexes
  {#"(?<=\/)([0-9a-fA-F]{8}(?:-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12})(?=/|\z)"
   "PITOCO_UUID"

   #"(?<=\/)(\d+)(?=/|\z)"
   "PITOCO_INT"})

(defn normalize-api-call
  "Normalize a API call path to a regex format.

  E.g. if the path is `/api/resource/12`, we will have the pat converted to
  `/api/resource/PITOCO_INT` (check `default-regexes`) "
  ([api-call]
   (normalize-api-call api-call default-regexes))
  ([api-call regexes]
   (update-in api-call [:request :path]
              #(loop [s %
                      [[regex identifier] & rest-regexes] regexes]
                 (if regex
                   (recur (str/replace s regex (str identifier)) rest-regexes)
                   s)))))

(defn dat-files-from-pcap-path
  [pcap-path]
  (let [folder-path (str (java.nio.file.Files/createTempDirectory
                          "pitoco" (into-array java.nio.file.attribute.FileAttribute [])))]
    ;; Run `tcptrace` to generate the dat files.
    (-> (conj '[sh -c] (str "tcptrace -el " (.getAbsolutePath (io/file pcap-path))))
        (sh/process {:out :string :err :string :dir folder-path})
        deref
        (doto (as-> % (when (= 127 (:exit %))
                        (throw (ex-info (:err %)
                                 %)))))
        :out
        str/split-lines)
    ;; Return all the generated `.dat` files.
    (->> (io/file folder-path) file-seq (filter #(str/includes? % ".dat")))))

;; TODO: Infer headers;
;; TODO: Infer query params.
(defn api-schemas-from-api-calls
  "Receives a collection of a map with `:request` and `:response` data.

  You can pass a `:pitoco.core/regexes` map from regexes to identifiers
  which will be used to try to identify paths which are in reality the same
  endpoint.

  Example of input (try it by copying it!).
[{:request {:body nil
            :path \"/api/myresource/1\"
            :method :get
            :host \"myexample.com\"}
  :response {:body {:a 10}
             :status 200}}
 {:request {:body {:b \"30\"}
            :path \"/api/other/1\"
            :method :post
            :host \"myexample.com\"}
  :response {:body {:a 10}
             :status 200}}]"
  ([api-calls]
   (api-schemas-from-api-calls api-calls {}))
  ([api-calls {:keys [::regexes ::api-schemas ::data-formats ::schemas] :as options}]
   (let [ ;; Assoc `::data-formats` so we "initialize" it here.
         options (if data-formats
                   options
                   (assoc options ::data-formats (or (-> (meta api-schemas) ::data-formats)
                                                     (new-data-formats schemas))))
         group->api-schema (->> api-schemas
                                (mapv (juxt #(select-keys % [:path :method :host]) identity))
                                (into {}))]
     (with-meta
       (merge
        (->> api-calls
             ;; Infer path template.
             (mapv #(normalize-api-call % (or regexes default-regexes)))
             ;; Only success for now, it may change in the future.
             (filter #(when-let [status (some-> % :response :status)]
                        (<= 200 status 299)))
             (group-by #(select-keys (:request %) [:path :method :host]))
             (mapv (fn [[k samples]]
                     [k
                      (let [api-schema (group->api-schema k)
                            request-samples (mapv #(-> % :request :body) samples)
                            request-stats (collect-stats request-samples
                                                         (assoc options ::stats
                                                                (-> api-schema meta ::request-schema-stats)))
                            response-samples (mapv #(-> % :response :body) samples)
                            response-stats (collect-stats response-samples
                                                          (assoc options ::stats
                                                                 (-> api-schema meta ::response-schema-stats)))]
                        (-> (merge k
                                   ;; Assoc `::stats` into metadata.
                                   ;; TODO: We probably could make this a record so we
                                   ;; can store things more explicitly.
                                   {::api-schema/id (hash k)
                                    :request-schema (infer-schema request-samples
                                                                  (assoc options ::stats request-stats))
                                    :response-schema (infer-schema response-samples
                                                                   (assoc options ::stats response-stats))})
                            (with-meta {::api-calls samples
                                        ::request-schema-stats request-stats
                                        ::response-schema-stats response-stats})))]))
             (into {})
             ;; Merge existing api schemas, if any.
             (merge group->api-schema)
             (mapv last)
             (sort-by (juxt :host :path :method))
             doall))
       {::data-formats (::data-formats options)}))))

(def ^:private string-method->keyword-method
  {"GET" :get
   "POST" :post
   "PUT" :put
   "HEAD" :head
   "DELETE" :delete
   "PATCH" :patch})

(defn- json-content-type?
  [api-call]
  (or (:json-content-type? (:request api-call)) (:json-content-type? (:response api-call))))

(defn api-calls-from-har-path
  [har-file-path']
  (let [har-file-path (.getAbsolutePath (io/file har-file-path'))
        har-map (json/parse-string (slurp har-file-path) keyword)]
    (->> (get-in har-map [:log :entries])
         (mapv (fn [entry]
                 (let [{:keys [:host :path] :as uri} (uri/uri (-> entry :request :url))
                       request-headers (->> (-> entry :request :headers)
                                            (group-by :name)
                                            (mapv (fn [[name headers]]
                                                    [(-> name str/lower-case keyword)
                                                     (mapv :value headers)]))
                                            (into {}))
                       response-headers (->> (-> entry :response :headers)
                                             (group-by :name)
                                             (mapv (fn [[name headers]]
                                                     [(-> name str/lower-case keyword)
                                                      (mapv :value headers)]))
                                             (into {}))]
                   {:request {:file-origin har-file-path
                              :body (try (-> entry :request :postData :text json-parse-string)
                                         (catch Exception _
                                           (-> entry :request :postData :text)))
                              :json-content-type? (some #(str/includes? % "json")
                                                        (:content-type request-headers))
                              :headers request-headers
                              :path path
                              :method (string-method->keyword-method (-> entry :request :method))
                              :host host
                              :uri uri}
                    :response {:file-origin har-file-path
                               :body (try (-> entry :response :content :text json-parse-string)
                                          (catch Exception _
                                            (-> entry :response :content :text)))
                               :json-content-type? (some #(str/includes? % "json")
                                                         (:content-type response-headers))
                               :headers response-headers
                               :status (-> entry :response :status)}})))
         (filter json-content-type?))))

(defn api-calls-from-dat-files
  "`dat-files` are generated using `tcptrace`."
  [dat-files]
  (->> dat-files
       (group-by #(let [file-name (.getName %)
                        identifier (-> file-name (str/split #"_") first)
                        [client server] (str/split identifier #"2")]
                    (sort
                     [(io/file (str (.getParent %) "/" file-name))
                      (io/file (str (.getParent %) "/" (format "%s2%s_contents.dat" server client)))])))
       keys
       (map-indexed vector)
       (pmap
        (fn [[idx [file-1 file-2]]]
          (let [read-stream
                (fn read-stream [file]
                  ;; Return `nil` if file does not exist.
                  (when (.exists file)
                    (let [
                          ;; Try to read requests
                          requests
                          (with-open [is (io/input-stream file)]
                            (try
                              (let [raw-http (RawHttp.)]
                                (loop [acc []]
                                  (if (pos? (.available is))
                                    (let [request (.eagerly (.parseRequest raw-http is))
                                          headers (->> (.getHeaders request)
                                                       .asMap
                                                       (mapv (fn [entry]
                                                               [(-> (key entry) str/lower-case keyword) (val entry)]))
                                                       (into {}))
                                          content-types (:content-type headers)
                                          ;; `body` is of type Optional<EagerBodyReader>
                                          body (.getBody request)
                                          json? (some #(str/includes? % "json") content-types)
                                          uri (uri/uri (.getUri request))]
                                      (->> {:file-origin (str file)
                                            :json-content-type? json?
                                            :body (cond
                                                    (not (.isPresent body))
                                                    nil

                                                    (and (seq content-types)
                                                         (some #(str/includes? % "json") content-types))
                                                    (try (json-parse-string (str (.get body)))
                                                         (catch Exception _
                                                           ""))

                                                    :else
                                                    "")
                                            :headers headers
                                            :method (string-method->keyword-method (.getMethod request))
                                            :path (:path uri)
                                            :host (if (seq (:port uri))
                                                    (str (:host uri) ":" (:port uri))
                                                    (:host uri))
                                            :uri uri}
                                           (conj acc)
                                           recur))
                                    acc)))
                              ;; If some exception happened, then it means that
                              ;; this is a response.
                              (catch Exception _ nil)))

                          ;; Try to read response if `requests` is `nil`.
                          responses
                          (when-not requests
                            ;; There are times where the response is incomplete,
                            ;; so we will just ignore these cases.
                            (try
                              (with-open [is (io/input-stream file)]
                                (let [raw-http (RawHttp.)]
                                  (loop [acc []]
                                    (if (pos? (.available is))
                                      (let [response (.eagerly (.parseResponse raw-http is))
                                            headers (->> (.getHeaders response)
                                                         .asMap
                                                         (mapv (fn [entry]
                                                                 [(-> (key entry) str/lower-case keyword) (val entry)]))
                                                         (into {}))
                                            content-types (:content-type headers)
                                            ;; `body` is of type Optional<EagerBodyReader>
                                            body (.getBody response)
                                            json? (some #(str/includes? % "json") content-types)]
                                        (->> {:file-origin (str file)
                                              :json-content-type? json?
                                              :body (cond
                                                      (not (.isPresent body))
                                                      nil

                                                      (and (seq content-types)
                                                           (some #(str/includes? % "json") content-types))
                                                      (try (json-parse-string (str (.get body)))
                                                           (catch Exception _
                                                             ""))

                                                      :else
                                                      "")
                                              :headers headers
                                              :status (.getStatusCode response)}
                                             (conj acc)
                                             recur))
                                      acc))))
                              (catch Exception _ nil)))]
                      (or requests responses))))

                file-1-results (read-stream file-1)
                file-2-results (read-stream file-2)]
            (when (zero? (mod idx 500))
              (println :done (str idx "/" (count dat-files)) :file-path (str file-1)))
            ;; Find which is the request file.
            (if (:host (first file-1-results))
              (mapv (fn [result-1 result-2] {:request result-1 :response result-2})
                    file-1-results
                    file-2-results)
              (mapv (fn [result-1 result-2] {:request result-2 :response result-1})
                    file-1-results
                    file-2-results)))))
       (apply concat)
       (filter json-content-type?)
       doall))

(defn- adapt-swagger
  [swg]
  (let [swg' (walk/prewalk (fn [obj]
                             (if (and (map-entry? obj)
                                      (= (key obj) :$ref))
                               (let [[_ v] (str/split (val obj) #"#/definitions/")]
                                 [(key obj) (str "#/definitions/"
                                                 (-> v
                                                     (str/replace #"/" ".")
                                                     (str/replace #":" "")))])
                               obj))
                           swg)]
    (cond-> swg'
      (seq (:definitions swg'))
      (update :definitions
              (fn [definitions]
                (->> definitions
                     (mapv (fn [[k v]]
                             [(-> (str k)
                                  (str/replace #"/" ".")
                                  (str/replace #":" ""))
                              v]))
                     (into {})))))))

;; TODO: Make default registry a dynamic variable (?).
(defn openapi-3-from-api-schemas
  ([api-schemas]
   (openapi-3-from-api-schemas api-schemas {}))
  ([api-schemas {:keys [::regexes]
                 :or {regexes default-regexes}
                 :as options}]
   {:openapi "3.0.1"
    :info {:version "1.0.0"
           :title "My Schemas"}
    :servers [{:url "/"}]
    :paths
    (->> api-schemas
         (group-by #(select-keys % [:host :path]))
         (mapv (fn [[{:keys [:path]} schemas]]
                 (let [pattern (->> regexes vals (str/join "|") re-pattern)
                       splitted-str (str/split path pattern)
                       path-params (->> (re-seq pattern path)
                                        (map-indexed (fn [idx v]
                                                       {:in :path
                                                        :name (format "arg%s" idx)
                                                        :schema {:type :string}
                                                        :description v
                                                        :required true})))
                       path' (or (some->> path-params
                                          seq
                                          (mapv #(format "{%s}" (:name %)))
                                          (interleave splitted-str)
                                          (str/join ""))
                                 path)
                       path' (if (and (> (count splitted-str) 1)
                                      (< (count path-params)
                                         (count splitted-str)))
                               ;; Add last element of splitted str so we
                               ;; don't have a bougs path.
                               (str path' (last splitted-str))
                               path')]
                   {path'
                    (->> schemas
                         (mapv (fn [{:keys [:method :request-schema :response-schema]}]
                                 {method
                                  (merge
                                   {:parameters path-params
                                    :responses {200 {:content
                                                     {"*/*"
                                                      {:schema (adapt-swagger
                                                                (swagger/transform
                                                                 response-schema
                                                                 (process-options options)))}}
                                                     :description "Responses."}}}
                                   (when-not (contains? #{:nil :any} request-schema)
                                     {:requestBody {:content
                                                    {"*/*"
                                                     {:schema (adapt-swagger
                                                               (swagger/transform
                                                                request-schema
                                                                (process-options options)))}}
                                                    :required true}}))}))
                         (apply merge))})))
         (apply merge))}))

(defn- contains-diff?
  [v]
  ;; If the key or the value contains a diff
  ;; instance, keep it.
  (or (instance? Mismatch v)
      (instance? Deletion v)
      (instance? Insertion v)
      (let [result-atom (atom false)]
        (walk/prewalk (fn [form]
                        (when (or (instance? Mismatch form)
                                  (instance? Deletion form)
                                  (instance? Insertion form))
                          (reset! result-atom true))
                        form)
                      v)
        @result-atom)))

(defn- get-subschemas
  [schema options]
  (->> (mu/subschemas schema (process-options options))
       (mu/distinct-by :id)
       pr-str
       edn/read-string
       (mapv #(select-keys % [:path :schema]))
       (mapv (juxt first identity))))

(defn api-request-subschemas-from-api-schema
  [api-schema options]
  (get-subschemas (:request-schema api-schema) options))

(defn api-response-subschemas-from-api-schema
  [api-schema options]
  (get-subschemas (:response-schema api-schema) options))

(defn diff-api-schemas
  ([base-api-schemas new-api-schemas]
   (diff-api-schemas base-api-schemas new-api-schemas {}))
  ([base-api-schemas new-api-schemas options]
   (let [removed-api-schemas
         (->> base-api-schemas
              (mapv (fn [original-api-schema]
                      (when-not (->> new-api-schemas
                                     (filter #(= (select-keys % [:path :method :host])
                                                 (select-keys original-api-schema [:path :method :host])))
                                     first)
                        (-> original-api-schema
                            (dissoc :response-schema :request-schema)
                            (merge {:response-schema-diff ::api-schema/removed
                                    :request-schema-diff ::api-schema/removed})))))
              (remove nil?))]
     (->> new-api-schemas
          (mapv (fn [new-api-schema]
                  (if-let [original-api-schema (->> base-api-schemas
                                                    (filter #(= (select-keys % [:path :method :host])
                                                                (select-keys new-api-schema [:path :method :host])))
                                                    first)]
                    (-> new-api-schema
                        (dissoc :response-schema :request-schema)
                        (merge {:response-schema-diff (when-not (= (:response-schema original-api-schema)
                                                                   (:response-schema new-api-schema))
                                                        (ddiff/diff
                                                         (mu/to-map-syntax (:response-schema original-api-schema)
                                                                           (process-options options))
                                                         (mu/to-map-syntax (:response-schema new-api-schema)
                                                                           (process-options options))))
                                :request-schema-diff (when-not (= (:request-schema original-api-schema)
                                                                  (:request-schema new-api-schema))
                                                       (ddiff/diff
                                                        (mu/to-map-syntax (:request-schema original-api-schema)
                                                                          (process-options options))
                                                        (mu/to-map-syntax (:request-schema new-api-schema)
                                                                          (process-options options))))}))
                    (-> new-api-schema
                        (dissoc :response-schema :request-schema)
                        (merge {:response-schema-diff ::api-schema/new
                                :request-schema-diff ::api-schema/new})))))
          (concat removed-api-schemas)
          (sort-by (juxt :host :path :method))))))

;;;;;;;;;;;;;;;;;;;;;;;;;; PATHOM ;;;;;;;;;;;;;;;;;;;;;;;;;
(pco/defresolver pcap-path->dat-files [{:keys [::pcap-path]}]
  {::dat-files (dat-files-from-pcap-path pcap-path)})

(pco/defresolver har-path->api-calls [{:keys [::har-path]}]
  {::api-calls (api-calls-from-har-path har-path)})

(pco/defresolver dat-files->api-calls [{:keys [::dat-files]}]
  {::api-calls (api-calls-from-dat-files dat-files)})

;; TODO: Make these options come from the main query so we don't have to
;; repeat them everywhere!
(pco/defresolver api-calls->api-schemas [env {:keys [::api-calls]}]
  {::api-schemas (api-schemas-from-api-calls api-calls (pco/params env))})

(pco/defresolver api-schema->api-calls [api-schema]
  {::pco/input [::api-schema/id]}
  {::api-calls (::api-calls (meta api-schema))})

(pco/defresolver api-schemas->openapi-3 [env {:keys [::api-schemas]}]
  {::openapi-3 (openapi-3-from-api-schemas api-schemas (pco/params env))})

(pco/defresolver api-schemas-diff-resolver [env {:keys [::base ::api-schemas]}]
  {::pco/input [{::base [::api-schemas]}
                ::api-schemas]}
  {::api-schemas-diff (diff-api-schemas (::api-schemas base) api-schemas (pco/params env))})

(pco/defresolver api-schema->api-request-subschemas [env api-schema]
  {::pco/input [::api-schema/id
                :request-schema]}
  {::request-subschemas (api-request-subschemas-from-api-schema api-schema (pco/params env))})

(pco/defresolver api-schema->api-response-subschemas [env api-schema]
  {::pco/input [::api-schema/id
                :response-schema]}
  {::response-subschemas (api-response-subschemas-from-api-schema api-schema (pco/params env))})

(pco/defresolver api-schema->request-schema-map [env api-schema]
  {::pco/input [::api-schema/id
                :request-schema]}
  {::request-schema-map (mu/to-map-syntax (:request-schema api-schema) (process-options (pco/params env)))})

(pco/defresolver api-schema->response-schema-map [env api-schema]
  {::pco/input [::api-schema/id
                :response-schema]}
  {::response-schema-map (mu/to-map-syntax (:response-schema api-schema) (process-options (pco/params env)))})

(def pathom-env
  (pci/register [pcap-path->dat-files
                 har-path->api-calls
                 dat-files->api-calls
                 api-calls->api-schemas
                 api-schemas->openapi-3
                 api-schema->api-calls
                 api-schemas-diff-resolver
                 api-schema->api-request-subschemas
                 api-schema->api-response-subschemas
                 api-schema->response-schema-map
                 api-schema->request-schema-map]))

(defn process
  ([tx]
   (p.eql/process pathom-env tx))
  ([entity tx]
   (p.eql/process pathom-env entity tx)))

(comment

  ;; We can get the schemas from a pcap path.
  (process {::pcap-path "resources-test/pcap-files/dump.pcap"}
           [::api-schemas])

  (process [{[::pcap-path "resources-test/pcap-files/dump2.pcap"]
             [{::api-schemas
               ['*
                ::request-subschemas
                ::response-subschemas]}]}])

  ;; But we also can get other info derived from a pcap path (e.g. openapi-3
  ;; schema).
  (process {::pcap-path "resources-test/pcap-files/dump.pcap"}
           [::api-schemas
            ::openapi-3])

  ;; Here we fetch the api schemas, then the api-calls and
  ;; open api 3 related to each api schema. Also we get open api 3
  ;; for everyone. So we can mix and match things freely.
  (process {::pcap-path "resources-test/pcap-files/dump.pcap"}
           [{::api-schemas
             ['* ::api-calls ::openapi-3]}
            ::openapi-3])

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Diff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (process {::pcap-path "resources-test/pcap-files/dump2.pcap"
            ::base {::pcap-path "resources-test/pcap-files/dump.pcap"}}
           [::api-schemas-diff])

  (process [{'(:>/a {::pcap-path "resources-test/pcap-files/dump2.pcap"
                     ::base {::pcap-path "resources-test/pcap-files/dump.pcap"}})
             [::api-schemas-diff]}])

  ())

(comment

  (process {::har-path "/Users/paulo.feodrippe/Downloads/www.notion.so.har"}
           [::api-schemas])

  (def github-schemas
    (::api-schemas
     (process {::har-path "/Users/paulo.feodrippe/Downloads/github.com.har"}
              [::api-schemas])))

  (->> (-> (process {::api-schemas github-schemas}
                    [::openapi-3])
           ::openapi-3
           (json/generate-string {:pretty true}))
       (spit "github.json"))

  ())

(comment

  ;; TODO
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;; CURRENT (in order!) ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - [ ] Start web app.
  ;; - [ ] Give option to upload two files at FE.
  ;; - [ ] Show diffs at FE.

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;; TBD or DONE ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - [x] Group same method/path so it's added as a sample for the same
  ;;       `mp/provide` call.
  ;; - [x] Improve grouping of API calls (EDIT: Let the user pass any regex).
  ;; - [ ] Separate query params from path and schematize it as well.
  ;; - [x] Accept HAR files. See https://support.zendesk.com/hc/en-us/articles/204410413-Generating-a-HAR-file-for-troubleshooting.
  ;; - [x] Generate OpenAPI 3 spec from REST endpoints.
  ;; - [x] Solve pipelined http requests.
  ;; - [x] Some data formats: ISO date format and UUID.
  ;; - [ ] See someway to do trace between multiple services.
  ;; - [ ] Parallelize inference(?).
  ;; - [x] Run `tcpdump` and `tcptrace` from Clojure. For `tcpdump`, maybe we
  ;;       can even use some Java wrapper for `libpcap`.
  ;; - [ ] Use `libpcap` so we don't need `tcpdump` installed (?).
  ;; - [x] Group paths.
  ;; - [ ] Validate data (?).
  ;; - [ ] Create mock for `http-client-fake` so it can generate HAR files.
  ;; - [ ] Make it extensible?
  ;; - [ ] This is very much like  integration tests + unit tests, while you
  ;;       can unit test a method/function, the "real" usage (integr.) can create
  ;;       a schema which can be used to check the data passed to unit tests.
  ;; - [ ] Create front end (not pretty, but also not ugly, please).
  ;; - [ ] Check what to show to the user.
  ;; - [ ] See other ways to output this data to the user (CLI, excel to an
  ;;       email etc) (?).
  ;; - [x] Convert from clojure.spec to Malli (for schema generation with
  ;;       `spec-provider`.
  ;; - [ ] Use https://github.com/FudanSELab/train-ticket for testing.
  ;; - [x] Use some real HTTP parser (or converter to HAR).
  ;; - [ ] Check if it's feasible to generate enums.
  ;; - [ ] Optimize some code.
  ;; - [ ] Add tests.
  ;; - [ ] Add zip code data format as if we were a user of the library.
  ;; - [ ] Recognize patterns (subschemas) and maybe add them dynamically
  ;;       as new schemas. With it we can match patterns with other data and see
  ;;       where they are used.
  ;; - [ ] Add timestamps to the request/response pairs (can we use
  ;;       tcpdump/libpcap for it?).
  ;; - [ ] Refactor code of the schema parser in functions.
  ;; - [ ] Maybe add schema to our functions?
  ;; - [ ] Add some way to generate samples from our UI.
  ;; - [ ] Give options to store generated schema somewhere (S3 first?).
  ;; - [ ] Maybe show some possible values (`enum`) using a percentage over the
  ;;       distinct samples.
  ;; - [ ] Show similar specs (e.g. one is a subset of another or how much this
  ;;       this is like one another one).
  ;; - [ ] Maybe change the format of api-calls so it has more information and
  ;;       it's more like the HAR file.
  ;; - [ ] Show schemas appearing in realtime in the FE. We can leverage the fact
  ;;       that `stats/collect` returns a unique map which we can keep in some atom
  ;;       and update it whenever some new file appears. We will keep track of the
  ;;       .dat files created and create `api-calls` from it which will be "collected".
  ;; - [x] Give option to use existing api schemas stats.
  ;; - [ ] Accept `regal` so we can have automatic generators.
  ;; - [ ] Document public functions.
  ;; - [ ] Stream schemas?
  ;; - [ ] Create a library out of it only for the schemas (we have to be more
  ;;       general about the types and NOT assume that the data is coming from
  ;;       JSON.
  ;; - [x] Diff between schemas.
  ;; - [ ] Infer query params.
  ;; - [ ] Infer headers.
  ;; - [ ] Add origin so it's easy to know who called who. But there is no good
  ;;       way to know it AFAIK with `tcptrace`.
  ;; - [x] Validate Cypress fixtures. Make it read schema from some source.
  ;;       Better yet, we can generate HAR files using a Cypress plugin.
  ;; - [x] Generate Swagger API (low hanging fruit). Check if it's possible to
  ;;       add metadata for custom formats.
  ;; - [ ] Add options to pass `options` using a dynamic var.
  ;; - [ ] Show diffs between two API schemas.
  ;; - [ ] Start FE. Show and search APIs (also in real time (?)).
  ;; - [ ] Show better validation error for Cypress fixtures (easier to understand).
  ;; - [ ] Suggest fix for Cypress fixtures (using generated data)?
  ;; - [ ] Measure other things like latency.
  ;; - [ ] Separate schemas per status code. Identify which ones are marked
  ;;       as json and are not returning parseable json.
  ;; - [ ] User can create views with their own queries.
  ;; - [x] Call this library `Pitoco`.
  ;; - [x] Add Pathom so we don't need to create a lot of `-from` functions and
  ;;       the data requested by the user can be just what he/she needs.
  ;; - [x] Create resolver to generate dat files from a pcap file.
  ;; - [ ] Add docker environment with non-usual dependencies (e.g. `tcptrace`
  ;;       and `tcpdump`.
  ;; - [ ] Add possibilities of filtering data. E.g. get all with status from
  ;;       400 to 499, whoch one had schema changes (this is between two schemas),
  ;;       by method, why is this content-type, but it's not parseable. Should
  ;;       we query using `datascript`?
  ;; - [ ] Group api calls by status.
  ;; - [ ] Read file from S3.
  ;; - [ ] How can this tool be used for contract testing?

  ;; TODO - OTHERS:
  ;; - [ ] For `tla-edn`, make it convert empty domains to edn maps and empty
  ;;       edn maps to empty domains.
  ;; - [ ] Add metadata to views so we can easily find the code for it.
  ;; - [ ] Make REPL for TLA+ with nRepl integration.

  ())

;; Examples (to be used for testing).
