(ns ifc.core-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [ifc.core :as ifc]))

(deftest spf-round-trip
  (let [model {:id "tower" :name "Tower" :sites []}
        document (ifc/exchange-document
                  {:project {:id "tower" :name "Tower" :model model}
                   :elements [{:id 10 :kind :wall :name "Wall 10"}]})
        text (ifc/write-spf document)]
    (is (string/starts-with? text "ISO-10303-21;"))
    (is (string/includes? text "IFCWALL"))
    (is (= model (ifc/read-spf text)))))
