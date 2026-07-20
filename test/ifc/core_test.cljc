(ns ifc.core-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.java.io :as io])
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

(deftest reads-external-spatial-hierarchy-and-extrusion
  (let [text #?(:clj (slurp (io/file "test/fixtures/revit-wall.ifc")) :cljs "")
        document (ifc/read-document text)
        project (:ifc/project document)
        wall (first (filter #(= 100 (:id %)) (:ifc/elements document)))
        mapped (first (filter #(= 130 (:id %)) (:ifc/elements document)))]
    (is (= :external-spf (:ifc/source document)))
    (is (= [:ifcsite :ifcbuilding :ifcbuildingstorey]
           [(get-in project [:children 0 :type])
            (get-in project [:children 0 :children 0 :type])
            (get-in project [:children 0 :children 0 :children 0 :type])]))
    (is (= [10.0 20.0 0.0] (get-in wall [:placement :location])))
    (is (= :rectangle (get-in wall [:geometry :profile :kind])))
    (is (= 8.0 (get-in wall [:geometry :profile :x-dim])))
    (is (= 0.25 (get-in wall [:geometry :profile :y-dim])))
    (is (= 3.2 (get-in wall [:geometry :depth])))
    (is (= 4 (:container-id wall)))
    (is (= :metre (get-in document [:ifc/units :lengthunit :name])))
    (is (true? (get-in wall [:property-sets "Pset_WallCommon" :properties "IsExternal" :value])))
    (is (= "2 HR" (get-in wall [:property-sets "Pset_WallCommon" :properties "FireRating" :value])))
    (is (= :opening (get-in wall [:openings 0 :kind])))
    (is (= [12.0 20.0 0.0] (get-in wall [:openings 0 :placement :location])))
    (is (= 120 (get-in wall [:openings 0 :filled-by])))
    (is (= :proxy (:kind mapped)))
    (is (= :mapped-item (get-in mapped [:geometry :kind])))
    (is (= [5.0 6.0 0.0] (get-in mapped [:geometry :transform :origin])))
    (is (= 2.0 (get-in mapped [:geometry :transform :scale])))
    (is (= :arbitrary-closed (get-in mapped [:geometry :source :profile :kind])))
    (is (= [[0.0 0.0] [2.0 0.0] [2.0 1.0] [0.0 1.0] [0.0 0.0]]
           (get-in mapped [:geometry :source :profile :points])))))
