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

(deftest standard-ifc-geometry-round-trip
  (let [document (ifc/exchange-document
                  {:project {:global-id "project-standard" :name "Standard Tower"}
                   :elements
                   [{:id 10 :global-id "wall-standard" :kind :wall :name "Standard Wall"
                     :placement {:location [4.0 5.0 0.0]}
                     :geometry {:kind :extruded-area-solid
                                :profile {:kind :arbitrary-closed :name "Wall Footprint"
                                          :points [[0.0 0.0] [3.0 0.0] [3.0 0.2]
                                                   [0.0 0.2] [0.0 0.0]]}
                                :direction [0.0 0.0 1.0] :depth 2.8}}
                    {:id 11 :global-id "brep-standard" :kind :proxy :name "Tetrahedron"
                     :geometry {:kind :faceted-brep
                                :faces [{:bounds [{:kind :outer :orientation true
                                                  :points [[0 0 0] [0 1 0] [1 0 0]]}]}
                                        {:bounds [{:kind :outer :orientation true
                                                  :points [[0 0 0] [1 0 0] [0 0 1]]}]}]}}
                    {:id 12 :global-id "tess-standard" :kind :proxy :name "Tessellated"
                     :geometry {:kind :triangulated-face-set :closed true
                                :coordinates [[0 0 0] [1 0 0] [0 1 0]]
                                :coord-indices [[1 2 3]]}}]})
        text (ifc/write-spf document)
        imported (ifc/read-document text)
        by-name (into {} (map (juxt :name identity) (:ifc/elements imported)))]
    (is (string/includes? text "IFCEXTRUDEDAREASOLID"))
    (is (string/includes? text "IFCFACETEDBREP"))
    (is (string/includes? text "IFCTRIANGULATEDFACESET"))
    (is (= :external-spf (:ifc/source imported)))
    (is (= "Standard Tower" (get-in imported [:ifc/project :name])))
    (is (= [4.0 5.0 0.0] (get-in by-name ["Standard Wall" :placement :location])))
    (is (= :arbitrary-closed
           (get-in by-name ["Standard Wall" :geometry :profile :kind])))
    (is (= 2.8 (get-in by-name ["Standard Wall" :geometry :depth])))
    (is (= :faceted-brep (get-in by-name ["Tetrahedron" :geometry :kind])))
    (is (= [[1 2 3]] (get-in by-name ["Tessellated" :geometry :coord-indices])))))

(deftest reads-external-spatial-hierarchy-and-extrusion
  (let [text #?(:clj (slurp (io/file "test/fixtures/revit-wall.ifc")) :cljs "")
        document (ifc/read-document text)
        project (:ifc/project document)
        wall (first (filter #(= 100 (:id %)) (:ifc/elements document)))
        mapped (first (filter #(= 130 (:id %)) (:ifc/elements document)))
        clipped (first (filter #(= 140 (:id %)) (:ifc/elements document)))
        brep (first (filter #(= 150 (:id %)) (:ifc/elements document)))
        tessellated (first (filter #(= 160 (:id %)) (:ifc/elements document)))
        polygonal (first (filter #(= 170 (:id %)) (:ifc/elements document)))]
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
           (get-in mapped [:geometry :source :profile :points])))
    (is (= :collection (get-in clipped [:geometry :kind])))
    (is (= 2 (count (get-in clipped [:geometry :items]))))
    (is (= :boolean-result (get-in clipped [:geometry :items 0 :kind])))
    (is (= :difference (get-in clipped [:geometry :items 0 :operator])))
    (is (= :half-space-solid
           (get-in clipped [:geometry :items 0 :second-operand :kind])))
    (is (= [10.0 20.0 1.5]
           (get-in clipped [:geometry :items 0 :second-operand :base-surface :position :location])))
    (is (= :faceted-brep (get-in brep [:geometry :kind])))
    (is (= 4 (count (get-in brep [:geometry :faces]))))
    (is (= :outer (get-in brep [:geometry :faces 0 :bounds 0 :kind])))
    (is (true? (get-in brep [:geometry :faces 0 :bounds 0 :orientation])))
    (is (= [[0.0 0.0 0.0] [0.0 2.0 0.0] [2.0 0.0 0.0]]
           (get-in brep [:geometry :faces 0 :bounds 0 :points])))
    (is (= :triangulated-face-set (get-in tessellated [:geometry :kind])))
    (is (true? (get-in tessellated [:geometry :closed])))
    (is (= [[1 2 3] [1 4 2] [2 4 3] [3 4 1]]
           (get-in tessellated [:geometry :coord-indices])))
    (is (= :polygonal-face-set (get-in polygonal [:geometry :kind])))
    (is (= [1 2 3] (get-in polygonal [:geometry :faces 0 :outer])))))
