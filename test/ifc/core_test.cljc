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
                             :georeference {:projected-crs {:name "EPSG:6677"
                                                            :geodetic-datum "JGD2011"
                                                            :map-projection "Japan Plane Rectangular CS IX"}
                                            :world-origin [100.0 200.0 0.0]
                                            :true-north [0.0 1.0]
                                            :eastings 500000.0 :northings 3950000.0
                                            :orthogonal-height 42.5
                                            :x-axis-abscissa 0.8660254038
                                            :x-axis-ordinate 0.5 :scale 1.0}
                             :children [{:id 20 :global-id "site-standard" :name "Tokyo Site"
                                         :type :ifcsite
                                         :latitude [35 40 0 0] :longitude [139 45 0 0]
                                         :elevation 42.5 :children
                                         [{:id 21 :global-id "building-standard" :name "Main Building"
                                           :type :ifcbuilding :children
                                           [{:id 22 :global-id "storey-standard" :name "Level 01"
                                             :type :ifcbuildingstorey :elevation 4.2
                                             :children
                                             [{:id 23 :global-id "space-standard"
                                               :name "Office 101" :long-name "Open Office"
                                               :type :ifcspace :predefined-type :internal
                                               :elevation-with-flooring 4.25
                                               :children []}]}]}]}]}
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
                                :direction [0 0 1] :depth 3.5}}
                    {:id 16 :global-id "advanced-standard" :kind :proxy :name "Advanced Body"
                     :geometry {:kind :advanced-brep
                                :faces [{:same-sense true
                                         :surface {:kind :plane
                                                   :position {:location [0 0 0]}}
                                         :bounds [{:kind :outer :orientation true
                                                   :points [[0 0 0] [3 0 0] [3 2 0] [0 2 0]]}]}
                                        {:same-sense true
                                         :surface {:kind :cylinder :radius 1.5
                                                   :position {:location [0 0 0]
                                                              :axis [0 0 1]}}
                                         :bounds [{:kind :outer :orientation true
                                                   :points [[1.5 0 0] [1.5 0 2]
                                                            [-1.5 0 2] [-1.5 0 0]]}]}
                                        {:same-sense true
                                         :surface {:kind :b-spline-surface
                                                   :u-degree 1 :v-degree 1
                                                   :control-points [[[0 0 0] [0 2 0]]
                                                                    [[2 0 0] [2 2 1]]]
                                                   :surface-form :unspecified
                                                   :u-closed false :v-closed false
                                                   :self-intersect false
                                                   :u-multiplicities [2 2]
                                                   :v-multiplicities [2 2]
                                                   :u-knots [0.0 1.0] :v-knots [0.0 1.0]
                                                   :knot-spec :piecewise-bezier-knots
                                                   :weights [[1.0 1.0] [1.0 0.75]]}
                                         :bounds [{:kind :outer :orientation true
                                                   :points [[0 0 0] [2 0 0] [2 2 1] [0 2 0]]}]}]}}]})
        text (ifc/write-spf document)
        imported (ifc/read-document text)
        by-name (into {} (map (juxt :name identity) (:ifc/elements imported)))]
    (is (string/includes? text "IFCEXTRUDEDAREASOLID"))
    (is (string/includes? text "IFCFACETEDBREP"))
    (is (string/includes? text "IFCTRIANGULATEDFACESET"))
    (is (string/includes? text "IFCSWEPTDISKSOLID"))
    (is (string/includes? text "IFCCIRCLEPROFILEDEF"))
    (is (string/includes? text "IFCADVANCEDBREP"))
    (is (string/includes? text "IFCCYLINDRICALSURFACE"))
    (is (string/includes? text "IFCRATIONALBSPLINESURFACEWITHKNOTS"))
    (is (= :external-spf (:ifc/source imported)))
    (is (= "Standard Tower" (get-in imported [:ifc/project :name])))
    (is (= "Tokyo Site" (get-in imported [:ifc/project :children 0 :name])))
    (is (= [35.0 40.0 0.0 0.0]
           (get-in imported [:ifc/project :children 0 :latitude])))
    (is (= [139.0 45.0 0.0 0.0]
           (get-in imported [:ifc/project :children 0 :longitude])))
    (is (= 42.5 (get-in imported [:ifc/project :children 0 :elevation])))
    (is (= "EPSG:6677" (get-in imported [:ifc/georeference :projected-crs :name])))
    (is (= "JGD2011"
           (get-in imported [:ifc/georeference :projected-crs :geodetic-datum])))
    (is (= [100.0 200.0 0.0] (get-in imported [:ifc/georeference :world-origin])))
    (is (= [0.0 1.0] (get-in imported [:ifc/georeference :true-north])))
    (is (= 500000.0 (get-in imported [:ifc/georeference :eastings])))
    (is (= 3950000.0 (get-in imported [:ifc/georeference :northings])))
    (is (= 42.5 (get-in imported [:ifc/georeference :orthogonal-height])))
    (is (= "Main Building" (get-in imported [:ifc/project :children 0 :children 0 :name])))
    (is (= "Level 01" (get-in imported [:ifc/project :children 0 :children 0 :children 0 :name])))
    (is (= 4.2 (get-in imported [:ifc/project :children 0 :children 0 :children 0 :placement :location 2])))
    (is (= "Open Office"
           (get-in imported [:ifc/project :children 0 :children 0 :children 0
                             :children 0 :long-name])))
    (is (= :internal
           (get-in imported [:ifc/project :children 0 :children 0 :children 0
                             :children 0 :predefined-type])))
    (is (= 4.25
           (get-in imported [:ifc/project :children 0 :children 0 :children 0
                             :children 0 :elevation-with-flooring])))
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
    (is (= :circle (get-in by-name ["Round Column" :geometry :profile :kind])))
    (is (= :advanced-brep (get-in by-name ["Advanced Body" :geometry :kind])))
    (is (= :b-spline-surface
           (get-in by-name ["Advanced Body" :geometry :faces 2 :surface :kind])))
    (is (= [[1.0 1.0] [1.0 0.75]]
           (get-in by-name ["Advanced Body" :geometry :faces 2 :surface :weights])))
    (is (= :plane (get-in by-name ["Advanced Body" :geometry :faces 0 :surface :kind])))
    (is (= :cylinder (get-in by-name ["Advanced Body" :geometry :faces 1 :surface :kind])))
    (is (= 1.5 (get-in by-name ["Advanced Body" :geometry :faces 1 :surface :radius])))))

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
        round-column (first (filter #(= 190 (:id %)) (:ifc/elements document)))
        edge-loop (first (filter #(= 210 (:id %)) (:ifc/elements document)))
        circular-edge (first (filter #(= 260 (:id %)) (:ifc/elements document)))
        nurbs-edge (first (filter #(= 270 (:id %)) (:ifc/elements document)))]
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
    (is (= 0.25 (get-in round-column [:geometry :profile :radius])))
    (is (= :advanced-brep (get-in edge-loop [:geometry :kind])))
    (is (= :edge-loop (get-in edge-loop [:geometry :faces 0 :bounds 0 :loop-kind])))
    (is (= 4 (count (get-in edge-loop [:geometry :faces 0 :bounds 0 :edges]))))
    (is (= [[0.0 0.0 0.0] [3.0 0.0 0.0] [3.0 2.0 0.0] [0.0 2.0 0.0]]
           (get-in edge-loop [:geometry :faces 0 :bounds 0 :points])))
    (is (= :polyline
           (get-in edge-loop [:geometry :faces 0 :bounds 0 :edges 0 :curve :kind])))
    (is (= :circle
           (get-in circular-edge [:geometry :faces 0 :bounds 0 :edges 0 :curve :kind])))
    (is (= 24 (count (get-in circular-edge [:geometry :faces 0 :bounds 0 :points]))))
    (is (= [2.0 0.0 0.0]
           (first (get-in circular-edge [:geometry :faces 0 :bounds 0 :points]))))
    (is (= :b-spline-curve
           (get-in nurbs-edge [:geometry :faces 0 :bounds 0 :edges 0 :curve :kind])))
    (is (= [1.0 0.5 1.0]
           (get-in nurbs-edge [:geometry :faces 0 :bounds 0 :edges 0 :curve :weights])))
    (is (= 8 (count (get-in nurbs-edge [:geometry :faces 0 :bounds 0 :points]))))
    (is (= [0.0 0.0 0.0]
           (first (get-in nurbs-edge [:geometry :faces 0 :bounds 0 :points]))))))

(deftest external-revit-fixture-is-semantically-lossless-after-standard-ifc-rewrite
  #?(:clj
     (let [text (slurp (io/file "test/fixtures/revit-wall.ifc"))
           report (ifc/roundtrip-report text)
           rewritten (:roundtrip/output report)]
       (is (= "IFC4X3_ADD2" (:roundtrip/input-schema report)))
       (is (= "IFC4X3_ADD2" (:roundtrip/output-schema report)))
       (is (= 12 (:roundtrip/input-elements report)))
       (is (= 12 (:roundtrip/output-elements report)))
       (is (string/includes? rewritten "IFCMAPPEDITEM"))
       (is (string/includes? rewritten "IFCBOOLEANRESULT"))
       (is (string/includes? rewritten "IFCPOLYGONALFACESET"))
       (is (string/includes? rewritten "IFCEDGELOOP"))
       (is (string/includes? rewritten "IFCRATIONALBSPLINECURVEWITHKNOTS"))
       (is (:roundtrip/lossless? report)
           (pr-str {:expected (:roundtrip/expected report)
                    :actual (:roundtrip/actual report)})))
     :cljs (is true)))

(deftest corpus-report-aggregates-external-roundtrip-evidence
  #?(:clj
     (let [text (slurp (io/file "test/fixtures/revit-wall.ifc"))
           report (ifc/corpus-report {"revit-wall" text})]
       (is (:corpus/lossless? report))
       (is (= 1 (:corpus/file-count report)))
       (is (= 12 (:corpus/input-elements report)))
       (is (= (:corpus/input-elements report) (:corpus/output-elements report))))
     :cljs (is true)))

(deftest external-schema-and-unknown-entities-use-byte-exact-passthrough
  (let [text (str "ISO-10303-21;\nHEADER;\n"
                  "FILE_DESCRIPTION(('CoordinationView'),'2;1');\n"
                  "FILE_NAME('legacy.ifc','',('External'),('Vendor'),'','','');\n"
                  "FILE_SCHEMA(('IFC2X3'));\nENDSEC;\nDATA;\n"
                  "#1=IFCPROJECT('legacy-project',$,'Legacy Project',$,$,$,$,$,$);\n"
                  "#2=IFCANNOTATION('unknown-object',$,'Vendor Annotation',$,$,$,$);\n"
                  "ENDSEC;\nEND-ISO-10303-21;\n")
        document (ifc/read-document text)]
    (is (= "IFC2X3" (:ifc/schema document)))
    (is (= 2 (:ifc/raw-entity-count document)))
    (is (= text (ifc/write-spf document))
        "an unedited external file, including unknown entities and headers, is byte exact")
    (let [edited (assoc-in document [:ifc/project :name] "Renamed Project")
          output (ifc/write-spf edited)]
      (is (string/includes? output "FILE_SCHEMA(('IFC2X3'))"))
      (is (string/includes? output "'Renamed Project'"))
      (is (string/includes? output "IFCANNOTATION"))
      (is (string/includes? output "'Vendor Annotation'"))
      (is (= 2 (:ifc/raw-entity-count (ifc/read-document output)))
          "hybrid export retains the unknown entity graph while applying the edit"))))

(deftest forced-standard-rewrite-retains-ifc4-schema
  (let [document (assoc (ifc/exchange-document
                         {:project {:global-id "p" :name "IFC4 Project"}
                          :elements []})
                        :ifc/schema "IFC4")
        output (ifc/rewrite-spf document)]
    (is (string/includes? output "FILE_SCHEMA(('IFC4'))"))
    (is (= "IFC4" (:ifc/schema (ifc/read-document output))))))

(deftest hybrid-export-splices-edited-geometry-without-dropping-unknown-entities
  #?(:clj
     (let [fixture (slurp (io/file "test/fixtures/revit-wall.ifc"))
           input (string/replace
                  fixture "\nENDSEC;\nEND-ISO-10303-21;"
                  "\n#999=IFCANNOTATION('vendor-extension',$,'Keep Me',$,$,$,$);\nENDSEC;\nEND-ISO-10303-21;")
           document (ifc/read-document input)
           edited (update document :ifc/elements
                          (fn [elements]
                            (mapv (fn [element]
                                    (if (= "wall-guid-000000000001" (:global-id element))
                                      (assoc-in element [:geometry :depth] 4.4)
                                      element))
                                  elements)))
           output (ifc/write-spf edited)
           reimported (ifc/read-document output)
           wall (first (filter #(= "wall-guid-000000000001" (:global-id %))
                               (:ifc/elements reimported)))]
       (is (string/includes? output "IFCANNOTATION"))
       (is (string/includes? output "'Keep Me'"))
       (is (= 4.4 (get-in wall [:geometry :depth])))
       (is (> (:ifc/raw-entity-count reimported) (:ifc/raw-entity-count document))))
     :cljs (is true)))
