(ns ifc.ids
  "Portable buildingSMART IDS 1.0 validation over an `ifc.core` exchange
  document. XML transport is isolated in `ifc.ids.xml`; this namespace owns
  deterministic facet evaluation and reports."
  (:require [clojure.string :as string]))

(def contract-version 1)
(def facet-types #{:entity :attribute :property :classification :material :part-of})
(def cardinalities #{:required :optional :prohibited})

(defn restriction
  "Create an IDS value restriction. Supported keys are `:value`, `:values`,
  `:pattern`, inclusive/exclusive numeric bounds, and string length bounds."
  [value-or-map]
  (if (map? value-or-map) value-or-map {:value value-or-map}))

(defn facet [{:keys [type cardinality] :as value}]
  (when-not (contains? facet-types type)
    (throw (ex-info "unsupported IDS facet" {:facet value})))
  (when (and cardinality (not (contains? cardinalities cardinality)))
    (throw (ex-info "invalid IDS facet cardinality" {:facet value})))
  (update value :cardinality #(or % :required)))

(defn specification
  [{:keys [name ifc-versions applicability requirements min-occurs max-occurs]
    :as value}]
  (when-not (and (string? name) (some? applicability))
    (throw (ex-info "invalid IDS specification" {:specification value})))
  {:ids.specification/name name
   :ids.specification/ifc-versions (set (or ifc-versions ["IFC4" "IFC4X3_ADD2"]))
   :ids.specification/min-occurs (if (nil? min-occurs) 1 min-occurs)
   :ids.specification/max-occurs (or max-occurs :unbounded)
   :ids.specification/applicability (mapv facet applicability)
   :ids.specification/requirements (mapv facet requirements)})

(defn document [{:keys [title version author specifications] :as value}]
  (when-not (and (string? title) (seq specifications))
    (throw (ex-info "invalid IDS document" {:document value})))
  {:ids/version (or version "1.0") :ids/contract-version contract-version
   :ids/title title :ids/author author
   :ids/specifications (mapv specification specifications)})

(defn- numeric-bound? [operator actual expected]
  (and (number? actual) (number? expected) (operator actual expected)))

(defn matches-restriction?
  [actual value-restriction]
  (if (nil? value-restriction)
    true
    (let [{:keys [value values pattern min-inclusive max-inclusive
                  min-exclusive max-exclusive min-length max-length]}
          (restriction value-restriction)
          text (when (some? actual) (str actual))]
      (and (if (contains? (restriction value-restriction) :value) (= value actual) true)
           (if (seq values) (contains? (set values) actual) true)
           (if pattern (and text (boolean (re-matches (re-pattern pattern) text))) true)
           (if (some? min-inclusive) (numeric-bound? >= actual min-inclusive) true)
           (if (some? max-inclusive) (numeric-bound? <= actual max-inclusive) true)
           (if (some? min-exclusive) (numeric-bound? > actual min-exclusive) true)
           (if (some? max-exclusive) (numeric-bound? < actual max-exclusive) true)
           (if (some? min-length) (and text (>= (count text) min-length)) true)
           (if (some? max-length) (and text (<= (count text) max-length)) true)))))

(def quantity-data-types
  {:length :ifclengthmeasure :area :ifcareameasure :volume :ifcvolumemeasure
   :count :ifccountmeasure :weight :ifcmassmeasure :time :ifctimemeasure
   :number :ifcnumericmeasure})

(defn- property-values [element]
  (concat
   (for [[set-name property-set] (:property-sets element)
         [property-name property] (:properties property-set)]
     {:set set-name :name property-name
      :value (if (contains? property :value) (:value property)
                 (or (:values property) (:set-point property)))
      :data-type (:value-type property) :source :property})
   (for [[set-name quantity-set] (:quantity-sets element)
         [quantity-name quantity] (:quantities quantity-set)]
     {:set set-name :name quantity-name :value (:value quantity)
      :data-type (get quantity-data-types (:kind quantity) (:kind quantity))
      :source :quantity})))

(defn- material-values [element]
  (let [assignment (:material element)]
    (if (= :layer-set-usage (:kind assignment))
      (mapcat (fn [layer]
                (remove nil? [(:name layer) (:category layer)
                              (get-in layer [:material :name])
                              (get-in layer [:material :category])]))
              (get-in assignment [:layer-set :layers]))
      (remove nil? [(:name assignment) (:category assignment)]))))

(defn- classification-values [element]
  (mapcat (fn [classification]
            [{:system (get-in classification [:source :name])
              :value (:identification classification)
              :name (:name classification)}])
          (:classifications element)))

(defn- spatial-index [project]
  (into {}
        (map (juxt :id identity))
        (tree-seq #(seq (:children %)) :children project)))

(defn- entity-name [element]
  (let [value (-> (or (:ifc/entity-type element) (:type element) (:kind element))
                  name string/upper-case)]
    (if (string/starts-with? value "IFC") value (str "IFC" value))))

(defn- attribute-value [element attribute-name]
  (case (string/upper-case (str attribute-name))
    "NAME" (:name element)
    "GLOBALID" (:global-id element)
    "TAG" (:tag element)
    "PREDEFINEDTYPE" (:predefined-type element)
    (get element (keyword (string/lower-case (str attribute-name))))))

(declare facet-matches?)

(defn- base-facet-matches? [element spatial facet]
  (case (:type facet)
    :entity
    (and (matches-restriction? (entity-name element) (:name facet))
         (if (:predefined-type facet)
           (matches-restriction? (some-> (:predefined-type element) name string/upper-case)
                                 (:predefined-type facet))
           true))

    :attribute
    (let [actual (attribute-value element (:name facet))]
      (and (some? actual) (matches-restriction? actual (:value facet))))

    :property
    (boolean
     (some (fn [property]
             (and (matches-restriction? (:set property) (:property-set facet))
                  (matches-restriction? (:name property) (:name facet))
                  (if (:data-type facet)
                    (= (:data-type facet) (:data-type property)) true)
                  (if (:value facet)
                    (let [value (:value property)]
                      (if (sequential? value)
                        (some #(matches-restriction? % (:value facet)) value)
                        (matches-restriction? value (:value facet))))
                    true)))
           (property-values element)))

    :classification
    (boolean
     (some #(and (matches-restriction? (:system %) (:system facet))
                 (matches-restriction? (:value %) (:value facet))
                 (if (:name facet) (matches-restriction? (:name %) (:name facet)) true))
           (classification-values element)))

    :material
    (boolean (some #(matches-restriction? % (:value facet))
                   (material-values element)))

    :part-of
    (let [container (get spatial (:container-id element))]
      (and container
           (if-let [entity (:entity facet)]
             (facet-matches? container spatial (assoc entity :type :entity)) true)))
    false))

(defn facet-matches? [element spatial facet]
  (let [matches? (base-facet-matches? element spatial facet)]
    (if (= :prohibited (:cardinality facet)) (not matches?) matches?)))

(defn- facet-present? [element spatial facet]
  (case (:type facet)
    :entity true
    :attribute (some? (attribute-value element (:name facet)))
    :property
    (boolean
     (some #(and (matches-restriction? (:set %) (:property-set facet))
                 (matches-restriction? (:name %) (:name facet)))
           (property-values element)))
    :classification
    (boolean
     (some #(matches-restriction? (:system %) (:system facet))
           (classification-values element)))
    :material (seq (material-values element))
    :part-of (contains? spatial (:container-id element))
    false))

(defn- requirement-result [element spatial requirement]
  (let [base-match? (base-facet-matches? element spatial requirement)
        cardinality (:cardinality requirement)
        pass? (case cardinality
                :prohibited (not base-match?)
                :optional (or (not (facet-present? element spatial requirement)) base-match?)
                :required base-match?)]
    {:ids.requirement/type (:type requirement)
     :ids.requirement/cardinality cardinality
     :ids.requirement/pass? pass?
     :ids.requirement/facet requirement}))

(defn- specification-result [ifc-document spatial spec]
  (let [schema (:ifc/schema ifc-document)
        schema-supported? (contains? (:ids.specification/ifc-versions spec) schema)
        applicable (if schema-supported?
                     (filterv #(every? (partial facet-matches? % spatial)
                                      (:ids.specification/applicability spec))
                              (:ifc/elements ifc-document)) [])
        count-applicable (count applicable)
        min-occurs (:ids.specification/min-occurs spec)
        max-occurs (:ids.specification/max-occurs spec)
        occurrence-pass? (and (>= count-applicable min-occurs)
                              (or (= :unbounded max-occurs)
                                  (<= count-applicable max-occurs)))
        objects
        (mapv (fn [element]
                (let [requirements
                      (mapv #(requirement-result element spatial %)
                            (:ids.specification/requirements spec))]
                  {:ids.object/global-id (:global-id element)
                   :ids.object/name (:name element)
                   :ids.object/entity (entity-name element)
                   :ids.object/pass? (every? :ids.requirement/pass? requirements)
                   :ids.object/requirements requirements}))
              applicable)
        pass? (and schema-supported? occurrence-pass?
                   (every? :ids.object/pass? objects))]
    {:ids.specification/name (:ids.specification/name spec)
     :ids.specification/pass? pass?
     :ids.specification/schema-supported? schema-supported?
     :ids.specification/applicable-count count-applicable
     :ids.specification/occurrence-pass? occurrence-pass?
     :ids.specification/objects objects}))

(defn validate
  "Validate an IFC exchange document against a normalized IDS document."
  [ifc-document ids-document]
  (let [spatial (spatial-index (:ifc/project ifc-document))
        specifications
        (mapv #(specification-result ifc-document spatial %)
              (:ids/specifications ids-document))
        issues
        (vec
         (mapcat (fn [spec]
                   (if-not (:ids.specification/pass? spec)
                     (let [failed (filterv (complement :ids.object/pass?)
                                           (:ids.specification/objects spec))]
                       (if (seq failed)
                         (mapv (fn [object]
                                 {:ids.issue/specification (:ids.specification/name spec)
                                  :ids.issue/global-id (:ids.object/global-id object)
                                  :ids.issue/entity (:ids.object/entity object)
                                  :ids.issue/failed-requirements
                                  (filterv (complement :ids.requirement/pass?)
                                           (:ids.object/requirements object))})
                               failed)
                         [{:ids.issue/specification (:ids.specification/name spec)
                           :ids.issue/reason
                           (cond
                             (not (:ids.specification/schema-supported? spec)) :schema
                             (not (:ids.specification/occurrence-pass? spec)) :occurrence
                             :else :unknown)}]))
                     []))
                 specifications))]
    {:ids.report/version contract-version
     :ids.report/title (:ids/title ids-document)
     :ids.report/pass? (every? :ids.specification/pass? specifications)
     :ids.report/specification-count (count specifications)
     :ids.report/applicable-object-count
     (reduce + (map :ids.specification/applicable-count specifications))
     :ids.report/specifications specifications
     :ids.report/issues issues}))
