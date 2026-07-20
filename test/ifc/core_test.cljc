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
                  {:project {:global-id "project-standard" :name "Standard Tower"
                             :children [{:id 20 :global-id "site-standard" :name "Tokyo Site"
                                         :type :ifcsite :children
                                         [{:id 21 :global-id "building-standard" :name "Main Building"
                                           :type :ifcbuilding :children
                                           [{:id 22 :global-id "storey-standard" :name "Level 01"
                                             :type :ifcbuildingstorey :elevation 4.2
                                             :children []}]}]}]}
                   :elements
                   [{:id 10 :global-id "wall-standard" :kind :wall :name "Standard Wall"
                     :container-id 22
                     :type-object {:id "wall-type-200" :global-id "wall-type-standard"
                                   :name "Exterior 200mm" :element-type "Basic Wall"
                                   :predefined-type :standard}
                     :placement {:location [4.0 5.0 0.0]}
                     :property-sets {"Pset_WallCommon"
                                     {:global-id "pset-wall-standard" :name "Pset_WallCommon"
                                      :properties {"IsExternal" {:value true :value-type :ifcboolean}
                                                   "FireRating" {:value "90 min" :value-type :ifclabel}}}}
                     :openings [{:id 30 :global-id "opening-standard" :name "Door Opening"
                                 :filled-by 13 :placement {:location [5.0 5.0 0.0]}
                                 :geometry {:kind :extruded-area-solid
                                            :profile {:kind :rectangle :x-dim 0.9 :y-dim 0.2}
                                            :direction [0 0 1] :depth 2.1}}]
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
                                :coord-indices [[1 2 3]]}}
                    {:id 13 :global-id "door-standard" :kind :door :name "Door"}
                    {:id 14 :global-id "pipe-standard" :kind :proxy :name "Pipe"
                     :geometry {:kind :swept-disk-solid
                                :directrix [[0 0 0] [3 0 0] [3 2 0]] :radius 0.08}}
                    {:id 15 :global-id "round-standard" :kind :column :name "Round Column"
                     :geometry {:kind :extruded-area-solid
                                :profile {:kind :circle :radius 0.3}
                                :direction [0 0 1] :depth 3.5}}]})
        text (ifc/write-spf document)
        imported (ifc/read-document text)
        by-name (into {} (map (juxt :name identity) (:ifc/elements imported)))]
    (is (string/includes? text "IFCEXTRUDEDAREASOLID"))
    (is (string/includes? text "IFCFACETEDBREP"))
    (is (string/includes? text "IFCTRIANGULATEDFACESET"))
    (is (string/includes? text "IFCSWEPTDISKSOLID"))
    (is (string/includes? text "IFCCIRCLEPROFILEDEF"))
    (is (= :external-spf (:ifc/source imported)))
    (is (= "Standard Tower" (get-in imported [:ifc/project :name])))
    (is (= "Tokyo Site" (get-in imported [:ifc/project :children 0 :name])))
    (is (= "Main Building" (get-in imported [:ifc/project :children 0 :children 0 :name])))
    (is (= "Level 01" (get-in imported [:ifc/project :children 0 :children 0 :children 0 :name])))
    (is (= 4.2 (get-in imported [:ifc/project :children 0 :children 0 :children 0 :placement :location 2])))
    (is (= [4.0 5.0 0.0] (get-in by-name ["Standard Wall" :placement :location])))
    (is (= "wall-type-standard"
           (get-in by-name ["Standard Wall" :type-object :global-id])))
    (is (= "Exterior 200mm" (get-in by-name ["Standard Wall" :type-object :name])))
    (is (true? (get-in by-name ["Standard Wall" :property-sets
                                "Pset_WallCommon" :properties "IsExternal" :value])))
    (is (= "90 min" (get-in by-name ["Standard Wall" :property-sets
                                     "Pset_WallCommon" :properties "FireRating" :value])))
    (is (= :opening (get-in by-name ["Standard Wall" :openings 0 :kind])))
    (is (= "door-standard"
           (get-in by-name ["Standard Wall" :openings 0 :filled-by-global-id])))
    (is (= :arbitrary-closed
           (get-in by-name ["Standard Wall" :geometry :profile :kind])))
    (is (= 2.8 (get-in by-name ["Standard Wall" :geometry :depth])))
    (is (= :faceted-brep (get-in by-name ["Tetrahedron" :geometry :kind])))
    (is (= [[1 2 3]] (get-in by-name ["Tessellated" :geometry :coord-indices])))
    (is (= :swept-disk-solid (get-in by-name ["Pipe" :geometry :kind])))
    (is (= :circle (get-in by-name ["Round Column" :geometry :profile :kind])))))

(deftest reads-external-spatial-hierarchy-and-extrusion
  (let [text #?(:clj (slurp (io/file "test/fixtures/revit-wall.ifc")) :cljs "")
        document (ifc/read-document text)
        project (:ifc/project document)
        wall (first (filter #(= 100 (:id %)) (:ifc/elements document)))
        mapped (first (filter #(= 130 (:id %)) (:ifc/elements document)))
        clipped (first (filter #(= 140 (:id %)) (:ifc/elements document)))
        brep (first (filter #(= 150 (:id %)) (:ifc/elements document)))
        tessellated (first (filter #(= 160 (:id %)) (:ifc/elements document)))
        polygonal (first (filter #(= 170 (:id %)) (:ifc/elements document)))
        swept (first (filter #(= 180 (:id %)) (:ifc/elements document)))
        round-column (first (filter #(= 190 (:id %)) (:ifc/elements document)))]
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
    (is (= [1 2 3] (get-in polygonal [:geometry :faces 0 :outer])))
    (is (= :swept-disk-solid (get-in swept [:geometry :kind])))
    (is (= [[0.0 0.0 0.0] [4.0 0.0 0.0] [4.0 3.0 0.0]]
           (get-in swept [:geometry :directrix])))
    (is (= 0.1 (get-in swept [:geometry :radius])))
    (is (= :circle (get-in round-column [:geometry :profile :kind])))
    (is (= 0.25 (get-in round-column [:geometry :profile :radius])))))
