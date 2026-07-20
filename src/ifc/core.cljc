(ns ifc.core
  "IFC 4.3 exchange over the shared kotoba-lang/step serializer."
  (:require [clojure.string :as string]
            [iso-10303.part21 :as part21]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])))

(def schema "IFC4X3_ADD2")
(def contract-version 1)

(def entity-types
  {:wall :ifcwall :slab :ifcslab :column :ifccolumn :beam :ifcbeam
   :door :ifcdoor :window :ifcwindow :roof :ifcroof :stair :ifcstair
   :railing :ifcrailing :mep-segment :ifcdistributionflowelement})

(defn exchange-document [{:keys [project elements]}]
  {:ifc/schema schema :ifc/contract-version contract-version
   :ifc/project project :ifc/elements (vec elements)})

(defn write-spf [document]
  (let [project (:ifc/project document)
        elements (:ifc/elements document)
        entities (concat
                  [[1 :ifcproject (or (:global-id project) "KOTOBA_PROJECT") :$
                    (:name project) :$ :$ :$ :$ :$ :$]]
                  (map-indexed
                   (fn [index element]
                     [(+ 100 index) (get entity-types (:kind element) :ifcbuildingelementproxy)
                      (or (:global-id element) (str (:id element))) :$ (:name element)
                      :$ :$ :$ :$ :$])
                   elements)
                  [[900 :ifcpropertysinglevalue "KOTOBA_MODEL_EDN" :$
                    [:typed :ifctext (pr-str (:model project))] :$]])]
    (apply part21/file {:description "ViewDefinition [DesignTransferView]"
                      :name "building.ifc" :schema schema
                      :author "KAMI" :org "kotoba-lang"}
           entities)))

(defn read-spf [text]
  (when-not (string/includes? text (str "FILE_SCHEMA(('" schema "'))"))
    (throw (ex-info "unsupported or missing IFC schema" {:expected schema})))
  (let [prefix "IFCPROPERTYSINGLEVALUE('KOTOBA_MODEL_EDN', $, IFCTEXT('"
        start (string/index-of text prefix)]
    (when-not start
      (throw (ex-info "IFC has no lossless Kotoba model payload" {})))
    (let [from (+ start (count prefix))
          to (string/index-of text "'), $);" from)]
      (when-not to (throw (ex-info "truncated Kotoba model payload" {})))
      (edn/read-string (string/replace (subs text from to) "''" "'")))))

(defn- ref-id [value]
  (when (and (vector? value) (= :ref (first value))) (second value)))
(defn- list-values [value]
  (when (and (vector? value) (= :list (first value))) (vec (rest value))))

(defn- referenced [table value]
  (get table (ref-id value)))

(defn- coordinates [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifccartesianpoint (:type entity))
      (list-values (first (:args entity))))))

(defn- direction [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifcdirection (:type entity))
      (list-values (first (:args entity))))))

(defn- axis-placement [table ref]
  (when-let [entity (referenced table ref)]
    (case (:type entity)
      :ifcaxis2placement3d
      {:location (or (coordinates table (get-in entity [:args 0])) [0.0 0.0 0.0])
       :axis (or (direction table (get-in entity [:args 1])) [0.0 0.0 1.0])
       :ref-direction (or (direction table (get-in entity [:args 2])) [1.0 0.0 0.0])}
      :ifcaxis2placement2d
      {:location (or (coordinates table (get-in entity [:args 0])) [0.0 0.0])
       :ref-direction (or (direction table (get-in entity [:args 1])) [1.0 0.0])}
      nil)))

(defn- v+ [a b]
  (mapv + (concat a (repeat (- 3 (count a)) 0.0))
          (concat b (repeat (- 3 (count b)) 0.0))))

(defn local-placement [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifclocalplacement (:type entity))
      (let [parent (some->> (get-in entity [:args 0]) (local-placement table))
            relative (axis-placement table (get-in entity [:args 1]))]
        (assoc relative :location (v+ (or (:location parent) [0.0 0.0 0.0])
                                      (or (:location relative) [0.0 0.0 0.0])))))))

(defn- rectangle-profile [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifcrectangleprofiledef (:type entity))
      {:kind :rectangle :profile-type (get-in entity [:args 0])
       :name (get-in entity [:args 1])
       :position (axis-placement table (get-in entity [:args 2]))
       :x-dim (get-in entity [:args 3]) :y-dim (get-in entity [:args 4])})))

(defn- representation-items [table representation-ref]
  (when-let [product-shape (referenced table representation-ref)]
    (when (= :ifcproductdefinitionshape (:type product-shape))
      (mapcat (fn [shape-ref]
                (let [shape (referenced table shape-ref)]
                  (when (= :ifcshaperepresentation (:type shape))
                    (list-values (get-in shape [:args 3])))))
              (list-values (get-in product-shape [:args 2]))))))

(defn product-geometry [table representation-ref]
  (some (fn [item-ref]
          (let [item (referenced table item-ref)]
            (when (= :ifcextrudedareasolid (:type item))
              {:kind :extruded-area-solid
               :profile (rectangle-profile table (get-in item [:args 0]))
               :position (axis-placement table (get-in item [:args 1]))
               :direction (direction table (get-in item [:args 2]))
               :depth (get-in item [:args 3])})))
        (representation-items table representation-ref)))

(def product-types (set (vals entity-types)))

(defn- product [table entity]
  {:id (:id entity) :global-id (get-in entity [:args 0])
   :name (or (get-in entity [:args 2]) (name (:type entity)))
   :kind (or (some (fn [[kind type]] (when (= type (:type entity)) kind)) entity-types) :other)
   :placement (local-placement table (get-in entity [:args 5]))
   :geometry (product-geometry table (get-in entity [:args 6]))})

(defn- aggregates [entities]
  (into {} (map (fn [entity]
                  [(ref-id (get-in entity [:args 4]))
                   (mapv ref-id (list-values (get-in entity [:args 5])))])
                (filter #(= :ifcrelaggregates (:type %)) entities))))

(defn- containment [entities]
  (into {} (mapcat (fn [entity]
                     (let [container (ref-id (get-in entity [:args 5]))]
                       (map (fn [item] [(ref-id item) container])
                            (list-values (get-in entity [:args 4])))))
                   (filter #(= :ifcrelcontainedinspatialstructure (:type %)) entities))))

(defn- spatial-node [table children id]
  (let [entity (get table id)]
    {:id id :global-id (get-in entity [:args 0]) :name (get-in entity [:args 2])
     :type (:type entity) :placement (local-placement table (get-in entity [:args 5]))
     :children (mapv #(spatial-node table children %) (get children id))}))

(defn read-external-spf [text]
  (let [parsed (part21/parse-file text)
        entities (:part21/entities parsed)
        table (:part21/entity-by-id parsed)
        children (aggregates entities)
        contained-by (containment entities)
        project-entity (first (filter #(= :ifcproject (:type %)) entities))
        products (mapv #(assoc (product table %) :container-id (get contained-by (:id %)))
                       (filter #(contains? product-types (:type %)) entities))]
    {:ifc/schema (:part21/schema parsed) :ifc/contract-version contract-version
     :ifc/project (when project-entity (spatial-node table children (:id project-entity)))
     :ifc/elements products :ifc/source :external-spf}))

(defn read-document [text]
  (try
    (let [model (read-spf text)]
      (exchange-document {:project {:id (:id model) :name (:name model) :model model}
                          :elements []}))
    (catch #?(:clj Exception :cljs :default) _
      (read-external-spf text))))
