(ns ifc.ids-test
  (:require [clojure.test :refer [deftest is]]
            [ifc.core :as ifc]
            [ifc.ids :as ids]))

(def model
  (ifc/exchange-document
   {:project {:global-id "project" :name "IDS Tower"}
    :elements
    [{:id 10 :global-id "wall-ok" :kind :wall :name "Exterior Wall"
      :property-sets
      {"Pset_WallCommon"
       {:properties {"FireRating" {:value "90 min" :value-type :ifclabel}}}}
      :quantity-sets
      {"Qto_WallBaseQuantities"
       {:quantities {"Length" {:kind :length :value 8.0}}}}
      :material {:kind :layer-set-usage
                 :layer-set {:layers [{:material {:name "Concrete"}
                                       :thickness 0.2}]}}
      :classifications [{:identification "Ss_25_10_20"
                         :source {:name "Uniclass 2015"}}]}
     {:id 11 :global-id "wall-fail" :kind :wall :name "Internal Wall"
      :property-sets {"Pset_WallCommon" {:properties {}}}
      :classifications []}]}))

(def requirements
  (ids/document
   {:title "Wall handover requirements" :author "Owner"
    :specifications
    [{:name "Every wall is classified, fire-rated, and materialized"
      :ifc-versions #{"IFC4X3_ADD2"}
      :applicability [{:type :entity :name "IFCWALL"}]
      :requirements
      [{:type :attribute :name "Name" :value {:pattern ".+ Wall"}}
       {:type :property :property-set "Pset_WallCommon" :name "FireRating"
       :value {:pattern "[0-9]+ min"}}
       {:type :property :property-set "Qto_WallBaseQuantities" :name "Length"
        :data-type :ifclengthmeasure :value {:min-inclusive 0.1}}
       {:type :classification :system "Uniclass 2015"
        :value {:pattern "Ss_[0-9_]+"}}
       {:type :material :value {:values #{"Concrete" "Masonry"}}}]}
     {:name "No proxies"
      :applicability [{:type :entity :name "IFCBUILDINGELEMENTPROXY"}]
      :requirements [] :min-occurs 0 :max-occurs 0}]}))

(deftest validates-ids-facets-cardinality-and-object-provenance
  (let [report (ids/validate model requirements)
        wall-spec (first (:ids.report/specifications report))
        no-proxy-spec (second (:ids.report/specifications report))
        issue (first (:ids.report/issues report))]
    (is (false? (:ids.report/pass? report)))
    (is (= 2 (:ids.report/specification-count report)))
    (is (= 2 (:ids.specification/applicable-count wall-spec)))
    (is (false? (:ids.specification/pass? wall-spec)))
    (is (:ids.specification/pass? no-proxy-spec))
    (is (= "wall-fail" (:ids.issue/global-id issue)))
    (is (= #{:property :classification :material}
           (set (map :ids.requirement/type
                     (:ids.issue/failed-requirements issue)))))))

(deftest supports-optional-and-prohibited-requirement-cardinality
  (let [optional
        (ids/document
         {:title "Optional asset data"
          :specifications
          [{:name "Optional tag must use W prefix"
            :applicability [{:type :entity :name "IFCWALL"}]
            :requirements [{:type :attribute :name "Tag"
                            :value {:pattern "W-.*"} :cardinality :optional}]}]})
        prohibited
        (ids/document
         {:title "No temporary status"
          :specifications
          [{:name "Walls cannot be temporary"
            :applicability [{:type :entity :name "IFCWALL"}]
            :requirements [{:type :property :property-set "Pset_WallCommon"
                            :name "Status" :value "Temporary"
                            :cardinality :prohibited}]}]})]
    (is (:ids.report/pass? (ids/validate model optional)))
    (is (:ids.report/pass? (ids/validate model prohibited)))))
