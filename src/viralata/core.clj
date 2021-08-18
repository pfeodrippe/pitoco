(ns viralata.core
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [babashka.process :as sh]
   [malli.provider :as mp]))

(comment

  #_(#"tcpdump -i lo0 'port 8005' -w lo.pcap")
  #_(#"tcptrace -el lo.pcap")
  #_(#"rm *.dat")

  (def dat-files
    (->> (-> '[sh -c "grep --include=\\*.dat /api/ * -lR | xargs grep -l 'Host:'"]
             (sh/process {:out :string :err :string :dir "/Users/paulo.feodrippe/dump"})
             :out
             deref
             str/split-lines)
         (mapv #(let [identifier (subs % 0 5)
                      [client server] (str/split identifier #"2")]
                  (hash-map :request (str "/Users/paulo.feodrippe/dump/" %)
                            :response (str "/Users/paulo.feodrippe/dump/"
                                           (format "%s2%s_contents.dat" server client)))))))

  (def api-calls
    (for [{:keys [:request :response]} dat-files]
      (let [read-stream
            (fn [file]
              (let [keeper (atom [])]
                (with-open [in (io/reader (io/input-stream file))]
                  (loop []
                    (let [[req-line & headers] (loop [headers []]
                                                 (let [line (.readLine in)]
                                                   (if (str/blank? line)
                                                     headers
                                                     (recur (conj headers line)))))]
                      (when-not (nil? req-line)
                        (let [[method-or-http-version path-or-status _] (str/split req-line #" ")
                              headers-map (->> headers
                                               (map #(update (str/split % #":" 2) 0 str/lower-case))
                                               (into {}))
                              content-length (some-> (get headers-map "content-length") str/trim Integer/parseInt)
                              host (some-> (get headers-map "host") str/trim)]
                          (swap! keeper conj
                                 (merge
                                  {:body (-> (if (some-> content-length pos?)
                                               (loop [buf ""
                                                      count 1]
                                                 (let [c (.read in)]
                                                   (if (>= count content-length)
                                                     (str buf (char c))
                                                     (recur (str buf (char c)) (inc count)))))
                                               ;; No content-length measn that we are going to read
                                               ;; the whole line (also connection is closing).
                                               (.readLine in))
                                             (json/parse-string keyword))}
                                  (if (str/starts-with? path-or-status "/")
                                    {:path path-or-status}
                                    {:status (Integer/parseInt path-or-status)})
                                  (when-let [method ({"GET" :get
                                                      "POST" :post
                                                      "PUT" :put}
                                                     method-or-http-version)]
                                    {:method method})
                                  (when host
                                    {:host host})))
                          (recur))))))
                (first @keeper)))]
        {:request (read-stream request)
         :response (read-stream response)})))

  (->> api-calls
       (filter #(-> % :response :status (<= 299)))
       (filter #(-> % :request :host (= "admin.local.gravie.us")))
       (take 4)
       (mapv (fn [{:keys [:request :response]}]
               {(select-keys request [:method :path] )
                {:request/schema (mp/provide [(:body request)])
                 :response/schema (mp/provide [(:body response)])}})))

  ())

(comment

  ;; TODO
  ;; - [ ] Group same method/path so it's added as a sample to `mp/provide`.

  ())
