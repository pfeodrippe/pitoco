(ns pitoco.core-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [clojure.test.check.generators :as gen]
   [clojure.walk :as walk]
   [malli.core :as m]
   [pitoco.core :as pit]
   matcher-combinators.test)
  (:import
   (lambdaisland.deep_diff2.diff_impl Mismatch Deletion Insertion)))

(defn- bigx?
  [v]
  (when (int? v)
    (<= 10 v 19)))

(def ^:private Bigx
  (m/schema
   (m/-simple-schema
    {:type `Bigx
     :pred bigx?
     :type-properties {:json-schema/type "string"
                       :gen/gen (gen/large-integer* {:min 10 :max 19})}})))

(defn ^:private my-schemas
  ([]
   (my-schemas []))
  ([schemas]
   (concat [Bigx] schemas)))

(defn- infer-schema
  ([samples]
   (infer-schema samples {}))
  ([samples {:keys [::pit/schemas] :as options}]
   (pit/infer-schema samples (merge options {::pit/schemas (my-schemas schemas)}))))

(defn- generate-sample
  ([schema]
   (generate-sample schema {}))
  ([schema {:keys [::pit/schemas] :as options}]
   (pit/generate-sample schema (merge options {::pit/schemas (my-schemas schemas)}))))

(deftest infer-schema-test
  (testing "inferring schema"
    (let [sch (infer-schema
               [{:a 4 :b 65
                 :c true :d 10.5
                 :e (float 10.2) :f nil
                 :g 10.5M :h :hey
                 :i #{1 4} :j 'a
                 :k #inst "2020-10-10" :l [1 #{10}]
                 :m "123e4567-e89b-12d3-a456-426614174000"}
                {:m "2020-10-31"}])]
      (is (some? (generate-sample sch)))
      (is (= '[:map
               [:a {:optional true} int?]
               [:b {:optional true} int?]
               [:c {:optional true} boolean?]
               [:d {:optional true} number?]
               [:e {:optional true} number?]
               [:f {:optional true} any?]
               [:g {:optional true} decimal?]
               [:h {:optional true} keyword?]
               [:i {:optional true} [:set int?]]
               [:j {:optional true} symbol?]
               [:k {:optional true} inst?]
               [:l {:optional true} [:sequential [:or [:set pitoco.core-test/Bigx] int?]]]
               [:m [:or pitoco.core/IsoDate pitoco.core/UUIDStr]]]
             sch))))

  (testing "empty map"
    (let [sch (infer-schema
               [{:a nil}
                {:a {:b 10 :c "30" :d "2021-04-09T01:07:22.418Z"}}
                {:a {:b 30}}
                {}])]
      (is (some? (generate-sample sch)))
      (is (= '[:map
               [:a
                {:optional true}
                [:maybe
                 [:map
                  [:b [:or int? pitoco.core-test/Bigx]]
                  [:c {:optional true} string?]
                  [:d {:optional true} pitoco.core/IsoInstant]]]]]
             sch))))

  (testing "sequential"
    (let [sch (infer-schema [{:a []}
                             {:a []}])]
      (is (some? (generate-sample sch)))
      (is (= '[:map
               [:a [:sequential any?]]]
             sch))))

  (testing "with a string as a key, you have a more generic schema"
    (let [sch (infer-schema [{"az" 3}
                             {:b 3}])]
      (is (some? (generate-sample sch)))
      (is (= '[:map-of string? int?]
             sch)))

    (let [sch (infer-schema [{:board:Completed 9
                              "board:In progress" 8}])]
      (is (some? (generate-sample sch)))
      (is (= '[:map-of string? int?]
             sch))))

  (testing "uuid"
    (let [sch (infer-schema [{"0ba576b1-deda-4011-ab04-baf1af42d75c" 3}
                             {"0ba576b1-deda-4011-ab04-baf1af42d75c" 3}])]
      (is (some? (generate-sample sch)))
      (is (= '[:map-of pitoco.core/UUIDStr int?]
             sch))))

  (testing "`map-of`"
    (let [sch (infer-schema [{"az" 3}
                             {:b 3}
                             {:b "3"}
                             {:b "3"}
                             {:b 3}])]
      (is (some? (generate-sample sch)))
      (is (= '[:map-of string? [:or [:or int? string?] int?]]
             sch))))

  (testing "`or` with custom schema"
    (let [sch (infer-schema
               [{"0d19acf5-ab6d-4e7c-929a-9fa182fd594e"
                 {:spaceId "29f9ed68-33ea-4487-81e6-1e2c7f614967"}

                 "0d19acf5-aa6d-4e7c-929a-9fa182fd594e"
                 {:spaceId 312}}])]
      (is (some? (generate-sample sch)))
      (is (= '[:map-of
               pitoco.core/UUIDStr
               [:or
                [:map [:spaceId int?]]
                [:map [:spaceId pitoco.core/UUIDStr]]]]
             sch))))

  (testing "key as string or custom schema"
    (let [sch (infer-schema [{:board:Completed 9
                              "d0452d8f-f04f-493f-9efd-c774a237d698" 8}])]
      (is (some? (generate-sample sch)))
      (is (= '[:map-of [:or pitoco.core/UUIDStr string?] int?]
             sch))))

  (testing "order for the passed schemas matters"
    (let [options {::pit/schemas [(pit/value-schema "abc")
                                  (pit/value-schema "bc")
                                  (m/-simple-schema {:type :abc/regex
                                                     :pred #(when (string? %)
                                                              (boolean (re-matches #"abc.*" %)))
                                                     :type-properties {:gen/gen (gen/elements ["LOOK"])}})]}
          sch (infer-schema [{"abc" 10}
                             {"bc" 30
                              "abc" "20"}
                             {"abc" "abc"}]
                            options)]
      (is (some? (generate-sample sch options)))
      (is (= '[:map
               ["abc" [:or "abc" pitoco.core-test/Bigx string?]]
               ["bc" {:optional true} int?]]
             sch)))

    (let [options {::pit/schemas [(m/-simple-schema {:type :abc/regex
                                                     :pred #(when (string? %)
                                                              (boolean (re-matches #"abc.*" %)))
                                                     :type-properties {:gen/gen (gen/elements ["LOOK"])}})
                                  (pit/value-schema "abc")
                                  (pit/value-schema "bc")]}
          sch (infer-schema [{"abc" 10}
                             {"bc" 30
                              "abc" "20"}
                             {"abc" "abc"}]
                            options)]
      (is (some? (generate-sample sch options)))
      (is (= '[:map-of
               [:or :abc/regex "bc"]
               [:or [:or :abc/regex pitoco.core-test/Bigx string?] int?]]
             sch)))

    (let [options {::pit/schemas [(pit/value-schema "abc")
                                  (m/-simple-schema {:type :abc/regex
                                                     :pred #(when (string? %)
                                                              (boolean (re-matches #"abc.*" %)))})]}
          sch (infer-schema [{"abc" 10}
                             {"bc" 30
                              "abc" "20"}
                             {"abc" "abc"}]
                            options)]
      (is (some? (generate-sample sch options)))
      (is (= '[:map-of
               [:or "abc" string?]
               [:or [:or "abc" pitoco.core-test/Bigx string?] int?]]
             sch)))))

(deftest process
  (testing "api-calls->api-schemas - ::regexes"
    (let [options {::pit/schemas (my-schemas)
                   ::pit/regexes (merge pit/default-regexes
                                        {#"(?<=\/.{1,50}\/)([a-z|A-Z|0-9]{12})(?=/|\z)"
                                         "SCHEMA_EXTERNAL_ID"

                                         #"(?<=\/)(\d{4}-\d{2}-\d{2})(?=/|\z)"
                                         "SCHEMA_ISO_DATE"})}
          pull-query (->> [::pit/api-schemas ::pit/openapi-3]
                          (mapv #(list % options)))]
      (let [{:keys [::pit/api-schemas]} (pit/process {::pit/api-calls [{:request {:body nil
                                                                                  :path "/api/myresource/1"
                                                                                  :method :get
                                                                                  :host "myexample.com"}
                                                                        :response {:body {:a 10}
                                                                                   :status 200}}
                                                                       {:request {:body {:b "30"}
                                                                                  :path "/api/other/1"
                                                                                  :method :post
                                                                                  :host "myexample.com"}
                                                                        :response {:body {:a 10}
                                                                                   :status 200}}]}
                                                     pull-query)]
        (is (match?
             [{:pitoco.api-schema/id int?
               :path "/api/myresource/PITOCO_INT"
               :method :get
               :host "myexample.com"
               :request-schema 'any?
               :response-schema [:map [:a `Bigx]]}
              {:pitoco.api-schema/id int?
               :path "/api/other/PITOCO_INT"
               :method :post
               :host "myexample.com"
               :request-schema [:map [:b 'string?]]
               :response-schema [:map [:a `Bigx]]}]
             api-schemas)))

      ;; TODO: Add a test which adds a schema using a custom schema.

      (testing "custom regexes for path and openapi-3"
        (let [{:keys [::pit/api-schemas
                      ::pit/openapi-3]} (pit/process
                                         {::pit/api-calls [{:request {:body nil
                                                                      :path "/api/myresource/10/c8259thu9842"
                                                                      :method :get
                                                                      :host "myexample.com"}
                                                            :response {:body {:a 10}
                                                                       :status 200}}
                                                           {:request {:body nil
                                                                      :path "/api/myresource/20/fj9140jf1jf0"
                                                                      :method :get
                                                                      :host "myexample.com"}
                                                            :response {:body {:a 10}
                                                                       :status 200}}
                                                           {:request {:body {:b "30"}
                                                                      :path "/api/other/2000-01-01"
                                                                      :method :post
                                                                      :host "myexample.com"}
                                                            :response {:body {:a 10}
                                                                       :status 200}}
                                                           {:request {:body {:b "30"}
                                                                      :path "/api/other/1990-10-10/abc"
                                                                      :method :post
                                                                      :host "myexample.com"}
                                                            :response {:body {:a 10}
                                                                       :status 200}}
                                                           {:request {:body {:b "30"
                                                                             :c 10}
                                                                      :path "/api/other/2020-10-10/abc"
                                                                      :method :post
                                                                      :host "myexample.com"}
                                                            :response {:body {:a 10}
                                                                       :status 200}}]}
                                         pull-query)]
          (is (match?
               [{:path "/api/myresource/PITOCO_INT/SCHEMA_EXTERNAL_ID"
                 :method :get
                 :host "myexample.com"
                 :request-schema 'any?
                 :response-schema [:map [:a `Bigx]]}
                {:path "/api/other/SCHEMA_ISO_DATE"
                 :method :post
                 :host "myexample.com"
                 :request-schema [:map [:b 'string?]]
                 :response-schema [:map [:a `Bigx]]}
                {:path "/api/other/SCHEMA_ISO_DATE/abc"
                 :method :post
                 :host "myexample.com"
                 :request-schema [:map
                                  [:b 'string?]
                                  [:c {:optional true} `Bigx]]
                 :response-schema [:map [:a `Bigx]]}]
               api-schemas))
          (is (match?
               {:openapi "3.0.1"
                :info {:version "1.0.0" :title "My Schemas"}
                :servers [{:url "/"}]
                :paths {"/api/myresource/{arg0}/{arg1}"
                        {:get {:parameters [{:in :path
                                             :name "arg0"
                                             :schema {:type :string}
                                             :description "PITOCO_INT"
                                             :required true}
                                            {:in :path
                                             :name "arg1"
                                             :schema {:type :string}
                                             :description "SCHEMA_EXTERNAL_ID"
                                             :required true}]
                               :responses {200
                                           {:content
                                            {"*/*"
                                             {:schema
                                              {:type "object"
                                               :properties {:a {:type "string"}}
                                               :required [:a]}}}
                                            :description "Responses."}}}}

                        "/api/other/{arg0}"
                        {:post {:parameters [{:in :path
                                              :name "arg0"
                                              :schema {:type :string}
                                              :description "SCHEMA_ISO_DATE"
                                              :required true}]
                                :responses {200
                                            {:content
                                             {"*/*"
                                              {:schema
                                               {:type "object"
                                                :properties {:a {:type "string"}}
                                                :required [:a]}}}
                                             :description "Responses."}}
                                :requestBody {:content
                                              {"*/*"
                                               {:schema
                                                {:type "object"
                                                 :properties {:b {:type "string"}}
                                                 :required [:b]}}}
                                              :required true}}}

                        "/api/other/{arg0}/abc"
                        {:post {:parameters [{:in :path
                                              :name "arg0"
                                              :schema {:type :string}
                                              :description "SCHEMA_ISO_DATE"
                                              :required true}]
                                :responses {200
                                            {:content
                                             {"*/*"
                                              {:schema
                                               {:type "object"
                                                :properties {:a {:type "string"}}
                                                :required [:a]}}}
                                             :description "Responses."}}
                                :requestBody {:content
                                              {"*/*"
                                               {:schema
                                                {:type "object"
                                                 :properties
                                                 {:b {:type "string"} :c {:type "string"}}
                                                 :required [:b]}}}
                                              :required true}}}}}
               openapi-3))))

      (testing "existing schemas"
        (let [api-calls-1 [{:request {:body nil
                                      :path "/api/myresource/c8259thu9842"
                                      :method :get
                                      :host "myexample.com"}
                            :response {:body {:a 10}
                                       :status 200}}
                           {:request {:body nil
                                      :path "/api/myresource/fj9140jf1jf0"
                                      :method :get
                                      :host "myexample.com"}
                            :response {:body {:a 10}
                                       :status 200}}]
              api-calls-2 [{:request {:body {:b "30"}
                                      :path "/api/other/2000-01-01"
                                      :method :post
                                      :host "myexample.com"}
                            :response {:body {:a 10}
                                       :status 200}}
                           {:request {:body {:b "30"}
                                      :path "/api/other/1990-10-10/abc"
                                      :method :post
                                      :host "myexample.com"}
                            :response {:body {:a 10}
                                       :status 200}}]
              api-calls-3 [{:request {:body {:b "30"
                                             :c 10}
                                      :path "/api/other/2020-10-10/abc"
                                      :method :post
                                      :host "myexample.com"}
                            :response {:body {:a 10}
                                       :status 200}}]
              {schemas-1 ::pit/api-schemas} (pit/process {::pit/api-calls api-calls-1}
                                                         [`(::pit/api-schemas ~options)])
              {schemas-2 ::pit/api-schemas} (pit/process {::pit/api-calls api-calls-2}
                                                         [`(::pit/api-schemas ~(assoc options ::pit/api-schemas schemas-1))])
              ;; It uses existing schemas to update the stats.
              {schemas-3 ::pit/api-schemas} (pit/process {::pit/api-calls api-calls-3}
                                                         [`(::pit/api-schemas ~(assoc options ::pit/api-schemas schemas-2))])]
          (is (match?
               [{:path "/api/myresource/SCHEMA_EXTERNAL_ID"
                 :method :get
                 :host "myexample.com"
                 :request-schema 'any?
                 :response-schema [:map [:a `Bigx]]}
                {:path "/api/other/SCHEMA_ISO_DATE"
                 :method :post
                 :host "myexample.com"
                 :request-schema [:map [:b 'string?]]
                 :response-schema [:map [:a `Bigx]]}
                {:path "/api/other/SCHEMA_ISO_DATE/abc"
                 :method :post
                 :host "myexample.com"
                 :request-schema
                 [:map
                  [:b 'string?]
                  [:c {:optional true} `Bigx]]
                 :response-schema [:map [:a `Bigx]]}]
               schemas-3)))))))

(defn- convert-ddiff
  [ddiff]
  (walk/prewalk (fn [form]
                  (cond
                    (instance? Mismatch form) {:+ (:+ form) :- (:- form)}
                    (instance? Deletion form) {:- (:- form)}
                    (instance? Insertion form) {:+ (:+ form)}
                    :else form))
                ddiff))

(deftest from-file-test
  (testing "pcap"
    (is (match?
         {::pit/api-schemas '[{:path "/get"
                               :method :get
                               :host "httpbin.org"
                               :request-schema any?
                               :response-schema
                               [:map
                                [:args [:map]]
                                [:headers
                                 [:map
                                  [:Accept string?]
                                  [:Host string?]
                                  [:User-Agent string?]
                                  [:X-Amzn-Trace-Id string?]]]
                                [:origin string?]
                                [:url string?]]}]}
         (pit/process {::pit/pcap-path "resources-test/pcap-files/dump.pcap"}
                      [::pit/api-schemas]))))

  (testing "diff"
    (testing "pcap files"
      (is (match?
           {::pit/api-schemas-diff
            [{:path "/get"
              :method :get
              :host "httpbin.org"
              :response-schema-diff
              '{:type :map,
                :children
                [[:args nil {:type :map}]
                 [:headers
                  nil
                  {:type :map,
                   :children
                   [[:Accept nil {:type string?}]
                    [:Host nil {:type string?}]
                    [:User-Agent nil {:type string?}]
                    [:X-Amzn-Trace-Id nil {:type string?}]]}]
                 [:origin nil {:type {:+ int?, :- string?}}]
                 [:url nil {:type string?}]]}
              :request-schema-diff nil}]}
           (convert-ddiff
            (pit/process {::pit/pcap-path "resources-test/pcap-files/dump2.pcap"
                          ::pit/base {::pit/pcap-path "resources-test/pcap-files/dump.pcap"}}
                         [::pit/api-schemas-diff])))))

    (testing "pcap and api schemas"
      (is (match?
           {::pit/api-schemas-diff
            [{:path "/get"
              :method :get
              :host "httpbin.org"
              :response-schema-diff nil
              :request-schema-diff nil}
             {:path "/olha"
              :method :get
              :host "httpbin.org"
              :response-schema-diff :pitoco.api-schema/removed
              :request-schema-diff :pitoco.api-schema/removed}]}
           (convert-ddiff
            (pit/process {::pit/pcap-path "resources-test/pcap-files/dump2.pcap"
                          ::pit/base {::pit/api-schemas [{:path "/get"
                                                          :method :get
                                                          :host "httpbin.org"
                                                          :request-schema 'any?
                                                          :response-schema '[:map
                                                                             [:args [:map]]
                                                                             [:headers
                                                                              [:map
                                                                               [:Accept string?]
                                                                               [:Host string?]
                                                                               [:User-Agent string?]
                                                                               [:X-Amzn-Trace-Id string?]]]
                                                                             [:origin int?]
                                                                             [:url string?]]}
                                                         ;; We add some bogus one two.
                                                         {:path "/olha"
                                                          :method :get
                                                          :host "httpbin.org"
                                                          :request-schema 'any?
                                                          :response-schema '[:map
                                                                             [:args [:map]]
                                                                             [:headers
                                                                              [:map
                                                                               [:Accept string?]
                                                                               [:Host string?]
                                                                               [:User-Agent string?]
                                                                               [:X-Amzn-Trace-Id string?]]]
                                                                             [:url string?]]}]}}
                         [::pit/api-schemas-diff])))))

    (testing "empty"
      (is (match?
           {::pit/api-schemas-diff
            [{:path "/get"
              :method :get
              :host "httpbin.org"
              :response-schema-diff :pitoco.api-schema/new
              :request-schema-diff :pitoco.api-schema/new}]}
           (convert-ddiff
            (pit/process {::pit/pcap-path "resources-test/pcap-files/dump2.pcap"
                          ::pit/base {::pit/api-schemas []}}
                         [::pit/api-schemas-diff])))))))
