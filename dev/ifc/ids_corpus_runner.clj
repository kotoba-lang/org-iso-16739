(ns ifc.ids-corpus-runner
  "Run the pinned buildingSMART IDS 1.0 implementer corpus against IFC data."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [ifc.core :as ifc]
            [ifc.ids :as ids]
            [ifc.ids.xml :as xml]))

(def expected-cases 287)

(defn- ids-case? [file]
  (and (.isFile file)
       (string/ends-with? (.getName file) ".ids")
       (or (string/starts-with? (.getName file) "pass-")
           (string/starts-with? (.getName file) "fail-"))))

(defn- paired-cases [root]
  (for [ids-file (file-seq root)
        :when (ids-case? ids-file)
        :let [path (.getPath ids-file)
              ifc-file (io/file (str (subs path 0 (- (count path) 4)) ".ifc"))]
        :when (.exists ifc-file)]
    [ids-file ifc-file]))

(defn- verify-case [[ids-file ifc-file]]
  (let [expected (string/starts-with? (.getName ids-file) "pass-")]
    (try
      (let [actual (:ids.report/pass?
                    (ids/validate (ifc/read-document (slurp ifc-file))
                                  (xml/read-xml (slurp ids-file))))]
        {:file (.getPath ids-file) :expected expected :actual actual
         :pass? (= expected actual)})
      (catch Throwable error
        {:file (.getPath ids-file) :expected expected
         :error (.getMessage error) :pass? false}))))

(defn -main [& args]
  (let [path (or (first (remove #{"--"} args))
                 (System/getenv "BUILDINGSMART_IDS_TEST_CASES"))
        _ (when-not path
            (throw (ex-info "IDS corpus path is required"
                            {:usage "clojure -M:ids-corpus -- /path/to/TestCases"})))
        root (io/file path)
        _ (when-not (.isDirectory root)
            (throw (ex-info "IDS corpus path is not a directory" {:path path})))
        results (mapv verify-case (paired-cases root))
        failed (filterv (complement :pass?) results)
        report {:ids-corpus/cases (count results)
                :ids-corpus/passed (- (count results) (count failed))
                :ids-corpus/failed (count failed)
                :ids-corpus/failures failed}]
    (prn report)
    (when-not (and (= expected-cases (count results)) (empty? failed))
      (throw (ex-info "buildingSMART IDS 1.0 corpus failed" report)))))
