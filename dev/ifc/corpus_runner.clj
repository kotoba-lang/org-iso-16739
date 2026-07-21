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
  (str (or (:source/raw-base fixture)
           "https://raw.githubusercontent.com/buildingSMART/Sample-Test-Files/")
       (or (:source/commit fixture) (:source/commit manifest)) "/"
       (encoded-path (:source/path fixture))))

(defn- near? [expected actual]
  (<= (Math/abs (- (double expected) (double actual)))
      (* 1.0e-10 (max 1.0 (Math/abs (double expected))))))

(defn- verify-fixture [manifest fixture]
  (let [url (fixture-url manifest fixture)
        bytes (with-open [stream (.openStream (.toURL (URI. url)))] (.readAllBytes stream))
        digest (sha256 bytes)
        _ (when-not (= (:sha256 fixture) digest)
            (throw (ex-info "external IFC fixture checksum mismatch"
                            {:name (:name fixture) :expected (:sha256 fixture)
                             :actual digest :url url})))
        text (String. bytes StandardCharsets/UTF_8)
        document (ifc/read-document text)
        report (ifc/roundtrip-report text)
        edited-name (str "Kotoba round-trip — " (:name fixture))
        edit (fn [document]
               (if (seq (:ifc/elements document))
                 (assoc-in document [:ifc/elements 0 :name] edited-name)
                 document))
        hybrid (ifc/hybrid-roundtrip-report text edit)
        edited-document (ifc/read-document (:roundtrip/output hybrid))
        expected-georef (:expected/georeference fixture)
        georef (:ifc/georeference document)
        projected (when expected-georef
                    (ifc/model-to-map-coordinate georef (:model-point expected-georef)))
        restored (when expected-georef
                   (ifc/map-to-model-coordinate georef projected))
        result {:name (:name fixture) :schema (:roundtrip/input-schema report)
                :products (:roundtrip/input-elements report)
                :lossless? (:roundtrip/lossless? report)
                :hybrid-semantic-lossless? (:roundtrip/semantic-lossless? hybrid)
                :hybrid-opaque-lossless? (:roundtrip/opaque-lossless? hybrid)
                :hybrid-edit-observed?
                (= edited-name (get-in edited-document [:ifc/elements 0 :name]))
                :opaque-entities (:roundtrip/opaque-input-count hybrid)
                :georeference?
                (when expected-georef
                  (and (= (:crs expected-georef) (get-in georef [:projected-crs :name]))
                       (every? true? (map near? (:map-point expected-georef) projected))
                       (every? true? (map near? (:model-point expected-georef) restored))))}]
    (when-not (and (= (:expected/schema fixture) (:schema result))
                   (= (:expected/products fixture) (:products result))
                   (not= false (:georeference? result))
                   (:lossless? result)
                   (:hybrid-semantic-lossless? result)
                   (:hybrid-opaque-lossless? result)
                   (:hybrid-edit-observed? result))
      (throw (ex-info "external IFC conformance failed" result)))
    result))

(defn -main [& _]
  (let [manifest (edn/read-string (slurp manifest-path))
        results (mapv (fn [fixture]
                        (try
                          (verify-fixture manifest fixture)
                          (catch Exception error
                            (throw (ex-info "external IFC fixture failed"
                                            {:name (:name fixture)
                                             :source/path (:source/path fixture)}
                                            error)))))
                      (:remote-fixtures manifest))]
    (prn {:corpus/source (:source/repository manifest)
          :corpus/commit (:source/commit manifest)
          :corpus/files results
          :corpus/products (reduce + (map :products results))
          :corpus/lossless? (every? :lossless? results)
          :corpus/hybrid-edit-lossless?
          (every? #(and (:hybrid-semantic-lossless? %)
                        (:hybrid-opaque-lossless? %)
                        (:hybrid-edit-observed? %)) results)})))
