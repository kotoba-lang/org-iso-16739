(ns ifc.corpus-runner
  "Fetch and verify pinned, large external IFC conformance fixtures."
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [ifc.core :as ifc])
  (:import [java.net URI]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest]))

(def manifest-path "test/fixtures/external/manifest.edn")

(defn- hex [bytes]
  (apply str (map #(format "%02x" (bit-and 0xff %)) bytes)))

(defn- sha256 [bytes]
  (hex (.digest (doto (MessageDigest/getInstance "SHA-256") (.update bytes)))))

(defn- encoded-path [path]
  (string/join "/" (map #(string/replace
                           (java.net.URLEncoder/encode % "UTF-8") "+" "%20")
                         (string/split path #"/"))))

(defn- fixture-url [manifest fixture]
  (str "https://raw.githubusercontent.com/buildingSMART/Sample-Test-Files/"
       (:source/commit manifest) "/" (encoded-path (:source/path fixture))))

(defn- verify-fixture [manifest fixture]
  (let [url (fixture-url manifest fixture)
        bytes (with-open [stream (.openStream (.toURL (URI. url)))] (.readAllBytes stream))
        digest (sha256 bytes)
        _ (when-not (= (:sha256 fixture) digest)
            (throw (ex-info "external IFC fixture checksum mismatch"
                            {:name (:name fixture) :expected (:sha256 fixture)
                             :actual digest :url url})))
        report (ifc/roundtrip-report (String. bytes StandardCharsets/UTF_8))
        result {:name (:name fixture) :schema (:roundtrip/input-schema report)
                :products (:roundtrip/input-elements report)
                :lossless? (:roundtrip/lossless? report)}]
    (when-not (and (= (:expected/schema fixture) (:schema result))
                   (= (:expected/products fixture) (:products result))
                   (:lossless? result))
      (throw (ex-info "external IFC conformance failed" result)))
    result))

(defn -main [& _]
  (let [manifest (edn/read-string (slurp manifest-path))
        results (mapv #(verify-fixture manifest %) (:remote-fixtures manifest))]
    (prn {:corpus/source (:source/repository manifest)
          :corpus/commit (:source/commit manifest)
          :corpus/files results
          :corpus/products (reduce + (map :products results))
          :corpus/lossless? (every? :lossless? results)})))
