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
   :railing :ifcrailing :mep-segment :ifcdistributionflowelement
   :opening :ifcopeningelement :proxy :ifcbuildingelementproxy})

(def type-entity-types
  {:wall :ifcwalltype :slab :ifcslabtype :column :ifccolumntype :beam :ifcbeamtype
   :door :ifcdoortype :window :ifcwindowtype :roof :ifcrooftype :stair :ifcstairtype
   :railing :ifcrailingtype :proxy :ifcbuildingelementproxytype})

(defn exchange-document [{:keys [project elements]}]
  {:ifc/schema schema :ifc/contract-version contract-version
   :ifc/project project :ifc/elements (vec elements)})

(defn- standard-entities [document]
  (let [next-id (atom 0) entities (atom [])
        emit! (fn [type & args]
                (let [id (swap! next-id inc)]
                  (swap! entities conj (into [id type] args))
                  [:ref id]))
        list* (fn [values] (into [:list] values))
        point! (fn [coordinates] (emit! :ifccartesianpoint (list* coordinates)))
        direction! (fn [ratios] (emit! :ifcdirection (list* ratios)))
        axis! (fn [placement]
                (emit! :ifcaxis2placement3d
                       (point! (or (:location placement) [0.0 0.0 0.0]))
                       (direction! (or (:axis placement) [0.0 0.0 1.0]))
                       (direction! (or (:ref-direction placement) [1.0 0.0 0.0]))))
        local! (fn [placement] (emit! :ifclocalplacement :$ (axis! placement)))
        geometry! (fn geometry! [geometry]
                    (case (:kind geometry)
                      :extruded-area-solid
                      (let [profile (:profile geometry)
                            profile-ref
                            (case (:kind profile)
                              :rectangle (emit! :ifcrectangleprofiledef :area (or (:name profile) "Profile")
                                                :$ (:x-dim profile) (:y-dim profile))
                              :circle (emit! :ifccircleprofiledef :area (or (:name profile) "Profile")
                                             :$ (:radius profile))
                              :i-shape (emit! :ifcishapeprofiledef :area (or (:name profile) "Profile") :$
                                              (:overall-width profile) (:overall-depth profile)
                                              (:web-thickness profile) (:flange-thickness profile)
                                              (or (:fillet-radius profile) :$)
                                              (or (:flange-edge-radius profile) :$)
                                              (or (:flange-slope profile) :$))
                              :arbitrary-closed
                              (let [points (mapv point! (:points profile))
                                    curve (emit! :ifcpolyline (list* points))]
                                (emit! :ifcarbitraryclosedprofiledef :area
                                       (or (:name profile) "Profile") curve))
                              nil)]
                        (when profile-ref
                          (emit! :ifcextrudedareasolid profile-ref (axis! (:position geometry))
                                 (direction! (or (:direction geometry) [0.0 0.0 1.0]))
                                 (:depth geometry))))
                      :swept-disk-solid
                      (let [directrix (emit! :ifcpolyline
                                             (list* (mapv point! (:directrix geometry))))]
                        (emit! :ifcsweptdisksolid directrix (:radius geometry)
                               (or (:inner-radius geometry) :$)
                               (or (:start-param geometry) :$)
                               (or (:end-param geometry) :$)))
                      :faceted-brep
                      (let [faces
                            (mapv (fn [face]
                                    (let [bounds
                                          (mapv (fn [bound]
                                                  (let [loop-ref (emit! :ifcpolyloop
                                                                        (list* (mapv point! (:points bound))))]
                                                    (emit! (if (= :outer (:kind bound))
                                                             :ifcfaceouterbound :ifcfacebound)
                                                           loop-ref (not (false? (:orientation bound))))))
                                                (:bounds face))]
                                      (emit! :ifcface (list* bounds))))
                                  (:faces geometry))
                            shell (emit! :ifcclosedshell (list* faces))]
                        (emit! :ifcfacetedbrep shell))
                      :triangulated-face-set
                      (let [coordinates (emit! :ifccartesianpointlist3d
                                               (list* (mapv list* (:coordinates geometry))))]
                        (emit! :ifctriangulatedfaceset coordinates
                               (if (seq (:normals geometry))
                                 (list* (mapv list* (:normals geometry))) :$)
                               (boolean (:closed geometry))
                               (list* (mapv list* (:coord-indices geometry))) :$))
                      :collection (first (keep geometry! (:items geometry)))
                      nil))
        product-shape! (fn [geometry]
                         (when-let [item (geometry! geometry)]
                           (let [shape (emit! :ifcshaperepresentation :$ "Body" "Body" (list* [item]))]
                             (emit! :ifcproductdefinitionshape :$ :$ (list* [shape])))))
        container-refs (atom {})
        spatial! (fn spatial! [node]
                   (let [type (:type node)
                         ref (case type
                               :ifcsite (emit! type (or (:global-id node) (str "SITE_" (:id node))) :$
                                               (:name node) :$ :$ (local! (:placement node)) :$ :$
                                               :element
                                               (if (seq (:latitude node)) (list* (:latitude node)) :$)
                                               (if (seq (:longitude node)) (list* (:longitude node)) :$)
                                               (or (:elevation node) :$) :$ :$)
                               :ifcbuilding (emit! type (or (:global-id node) (str "BUILDING_" (:id node))) :$
                                                   (:name node) :$ :$ (local! (:placement node)) :$ :$
                                                   :element :$ :$ :$)
                               :ifcbuildingstorey
                               (emit! type (or (:global-id node) (str "STOREY_" (:id node))) :$
                                      (:name node) :$ :$
                                      (local! (or (:placement node)
                                                  {:location [0.0 0.0 (or (:elevation node) 0.0)]})) :$ :$
                                      :element (or (:elevation node)
                                                   (get-in node [:placement :location 2]) 0.0))
                               nil)
                         children (vec (keep spatial! (:children node)))]
                     (when ref
                       (swap! container-refs assoc (:id node) ref)
                       (when (= :ifcbuildingstorey type)
                         (swap! container-refs assoc :default ref))
                       (when (seq children)
                         (emit! :ifcrelaggregates (str "REL_AGG_" (:id node)) :$ :$ :$
                                ref (list* children))))
                     ref))
        property-value (fn [property]
                         (let [value (:value property)
                               value-type (or (:value-type property)
                                              (cond (boolean? value) :ifcboolean
                                                    (integer? value) :ifcinteger
                                                    (number? value) :ifcreal
                                                    :else :ifclabel))]
                           [:typed value-type
                            (if (= :ifcboolean value-type) (if value :t :f) value)]))
        psets! (fn [product-ref element]
                 (doseq [[name pset] (:property-sets element)]
                   (let [properties (mapv (fn [[property-name property]]
                                            (emit! :ifcpropertysinglevalue property-name
                                                   (or (:description property) :$)
                                                   (property-value property) :$))
                                          (:properties pset))
                         pset-ref (emit! :ifcpropertyset
                                         (or (:global-id pset) (str "PSET_" (:id element) "_" name))
                                         :$ name :$ (list* properties))]
                     (emit! :ifcreldefinesbyproperties
                            (str "REL_PSET_" (:id element) "_" name) :$ :$ :$
                            (list* [product-ref]) pset-ref))))
        product! (fn [element type]
                   (emit! type (or (:global-id element) (str (:id element))) :$ (:name element) :$ :$
                          (local! (:placement element)) (or (product-shape! (:geometry element)) :$)
                          (or (:tag element) :$) :$))
        type! (fn [product-ref element]
                (when-let [type-object (:type-object element)]
                  (let [type-ref (emit! (get type-entity-types (:kind element)
                                             :ifcbuildingelementproxytype)
                                        (or (:global-id type-object)
                                            (str "TYPE_" (:id type-object)))
                                        :$ (:name type-object) :$ :$ :$ :$ :$
                                        (or (:element-type type-object) :$)
                                        (or (:predefined-type type-object) :notdefined))]
                    (emit! :ifcreldefinesbytype
                           (str "REL_TYPE_" (:id element)) :$ :$ :$
                           (list* [product-ref]) type-ref))))
        project (:ifc/project document)
        georeference (:georeference project)
        elements (:ifc/elements document)
        units (emit! :ifcunitassignment
                     (list* [(emit! :ifcsiunit :$ :lengthunit :$ :metre)]))
        world-axis (axis! {:location (or (:world-origin georeference) [0.0 0.0 0.0])})
        true-north (when-let [direction (:true-north georeference)] (direction! direction))
        context (emit! :ifcgeometricrepresentationcontext :$ "Model" 3 1.0e-5
                       world-axis (or true-north :$))
        projected-crs (when georeference
                        (let [crs (:projected-crs georeference)]
                          (emit! :ifcprojectedcrs (or (:name crs) "Undefined CRS")
                                 (or (:description crs) :$) (or (:geodetic-datum crs) :$)
                                 (or (:vertical-datum crs) :$) (or (:map-projection crs) :$)
                                 (or (:map-zone crs) :$) :$)))
        _map-conversion (when projected-crs
                          (emit! :ifcmapconversion context projected-crs
                                 (or (:eastings georeference) 0.0)
                                 (or (:northings georeference) 0.0)
                                 (or (:orthogonal-height georeference) 0.0)
                                 (or (:x-axis-abscissa georeference) 1.0)
                                 (or (:x-axis-ordinate georeference) 0.0)
                                 (or (:scale georeference) 1.0)))
        project-ref (emit! :ifcproject (or (:global-id project) "KOTOBA_PROJECT") :$
                           (or (:name project) "Project") :$ :$ :$ (list* [context]) :$ units)
        spatial-roots (or (seq (:children project))
                          [{:id :site :type :ifcsite :name "Site" :children
                            [{:id :building :type :ifcbuilding :name "Building" :children
                              [{:id :storey :type :ifcbuildingstorey :name "Storey" :children []}]}]}])
        spatial-refs (mapv spatial! spatial-roots)
        _project-spatial (emit! :ifcrelaggregates "KOTOBA_REL_PROJECT_SPATIAL" :$ :$ :$
                                project-ref (list* spatial-refs))
        product-by-source (atom {})
        _product-refs
        (mapv (fn [element]
                (let [ref (product! element (get entity-types (:kind element)
                                                 :ifcbuildingelementproxy))]
                  (swap! product-by-source assoc (:id element) ref)
                  (psets! ref element)
                  (type! ref element)
                  ref))
              elements)
        default-container (get @container-refs :default :$)
        _containment
        (doseq [[container-id grouped] (group-by #(or (:container-id %) :default) elements)]
          (let [refs (mapv #(get @product-by-source (:id %)) grouped)
                container (if (= :default container-id) default-container
                              (get @container-refs container-id default-container))]
            (when (and (seq refs) (not= :$ container))
              (emit! :ifcrelcontainedinspatialstructure (str "REL_CONTAIN_" container-id)
                     :$ :$ :$ (list* refs) container))))
        _openings
        (doseq [host elements opening (:openings host)]
          (let [opening-ref (product! opening :ifcopeningelement)
                host-ref (get @product-by-source (:id host))]
            (psets! opening-ref opening)
            (emit! :ifcrelvoidselement (str "REL_VOID_" (:id host) "_" (:id opening))
                   :$ :$ :$ host-ref opening-ref)
            (when-let [fill-ref (get @product-by-source (:filled-by opening))]
              (emit! :ifcrelfillselement (str "REL_FILL_" (:id opening))
                     :$ :$ :$ opening-ref fill-ref))))
        _payload (when (some? (:model project))
            (emit! :ifcpropertysinglevalue "KOTOBA_MODEL_EDN" :$
                   [:typed :ifctext (pr-str (:model project))] :$))]
    @entities))

(defn write-spf [document]
  (apply part21/file {:description "ViewDefinition [DesignTransferView]"
                      :name "building.ifc" :schema schema
                      :author "KAMI" :org "kotoba-lang"}
         (standard-entities document)))

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

(defn- polyline [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifcpolyline (:type entity))
      (mapv #(coordinates table %) (list-values (get-in entity [:args 0]))))))

(defn- profile [table ref]
  (when-let [entity (referenced table ref)]
    (case (:type entity)
      :ifcrectangleprofiledef
      {:kind :rectangle :profile-type (get-in entity [:args 0])
       :name (get-in entity [:args 1])
       :position (axis-placement table (get-in entity [:args 2]))
       :x-dim (get-in entity [:args 3]) :y-dim (get-in entity [:args 4])}
      :ifccircleprofiledef
      {:kind :circle :profile-type (get-in entity [:args 0])
       :name (get-in entity [:args 1])
       :position (axis-placement table (get-in entity [:args 2]))
       :radius (get-in entity [:args 3])}
      :ifcishapeprofiledef
      {:kind :i-shape :profile-type (get-in entity [:args 0])
       :name (get-in entity [:args 1])
       :position (axis-placement table (get-in entity [:args 2]))
       :overall-width (get-in entity [:args 3]) :overall-depth (get-in entity [:args 4])
       :web-thickness (get-in entity [:args 5]) :flange-thickness (get-in entity [:args 6])
       :fillet-radius (get-in entity [:args 7])
       :flange-edge-radius (get-in entity [:args 8]) :flange-slope (get-in entity [:args 9])}
      :ifcarbitraryclosedprofiledef
      {:kind :arbitrary-closed :profile-type (get-in entity [:args 0])
       :name (get-in entity [:args 1])
       :points (polyline table (get-in entity [:args 2]))}
      nil)))

(defn- shape-items [table shape-ref]
  (let [shape (referenced table shape-ref)]
    (when (= :ifcshaperepresentation (:type shape))
      (list-values (get-in shape [:args 3])))))

(defn- representation-items [table representation-ref]
  (when-let [product-shape (referenced table representation-ref)]
    (when (= :ifcproductdefinitionshape (:type product-shape))
      (mapcat (fn [shape-ref] (shape-items table shape-ref))
              (list-values (get-in product-shape [:args 2]))))))

(defn- transformation [table ref]
  (when-let [entity (referenced table ref)]
    (when (#{:ifccartesiantransformationoperator3d
             :ifccartesiantransformationoperator3dnonuniform} (:type entity))
      {:axis1 (or (direction table (get-in entity [:args 0])) [1.0 0.0 0.0])
       :axis2 (or (direction table (get-in entity [:args 1])) [0.0 1.0 0.0])
       :origin (or (coordinates table (get-in entity [:args 2])) [0.0 0.0 0.0])
       :scale (let [value (get-in entity [:args 3])] (if (= :$ value) 1.0 value))
       :axis3 (or (direction table (get-in entity [:args 4])) [0.0 0.0 1.0])
       :scale2 (let [value (get-in entity [:args 5])] (if (or (nil? value) (= :$ value)) 1.0 value))
       :scale3 (let [value (get-in entity [:args 6])] (if (or (nil? value) (= :$ value)) 1.0 value))})))

(declare geometry-item geometry-items)
(defn- mapped-geometry [table item]
  (let [source (referenced table (get-in item [:args 0]))
        source-items (when (= :ifcrepresentationmap (:type source))
                       (shape-items table (get-in source [:args 1])))]
    {:kind :mapped-item
     :mapping-origin (axis-placement table (get-in source [:args 0]))
     :transform (transformation table (get-in item [:args 1]))
     :source (geometry-items table source-items)}))

(defn- half-space [table item]
  (let [surface (referenced table (get-in item [:args 0]))]
    {:kind :half-space-solid
     :agreement-flag (get-in item [:args 1])
     :base-surface (when (= :ifcplane (:type surface))
                     {:kind :plane :position (axis-placement table (get-in surface [:args 0]))})
     :boundary (when (= :ifcpolygonalboundedhalfspace (:type item))
                 (polyline table (get-in item [:args 3])))
     :position (when (= :ifcpolygonalboundedhalfspace (:type item))
                 (axis-placement table (get-in item [:args 2])))}))

(defn- face-bound [table ref]
  (let [entity (referenced table ref)
        loop-entity (referenced table (get-in entity [:args 0]))]
    (when (and (#{:ifcfacebound :ifcfaceouterbound} (:type entity))
               (= :ifcpolyloop (:type loop-entity)))
      {:kind (if (= :ifcfaceouterbound (:type entity)) :outer :inner)
       :orientation (get-in entity [:args 1])
       :points (mapv #(coordinates table %) (list-values (get-in loop-entity [:args 0])))})))

(defn- faceted-brep [table item]
  (let [shell (referenced table (get-in item [:args 0]))]
    (when (= :ifcclosedshell (:type shell))
      {:kind :faceted-brep
       :faces (mapv (fn [face-ref]
                      (let [face (referenced table face-ref)]
                        {:bounds (vec (keep #(face-bound table %)
                                            (list-values (get-in face [:args 0]))))}))
                    (list-values (get-in shell [:args 0])))})))

(defn- point-list [table ref]
  (let [entity (referenced table ref)]
    (when (#{:ifccartesianpointlist2d :ifccartesianpointlist3d} (:type entity))
      (mapv list-values (list-values (get-in entity [:args 0]))))))

(defn- index-list [value]
  (mapv long (list-values value)))

(defn- indexed-face [table ref]
  (let [entity (referenced table ref)]
    (case (:type entity)
      :ifcindexedpolygonalface
      {:outer (index-list (get-in entity [:args 0])) :inners []}
      :ifcindexedpolygonalfacewithvoids
      {:outer (index-list (get-in entity [:args 0]))
       :inners (mapv index-list (list-values (get-in entity [:args 1])))}
      nil)))

(defn- tessellated-face-set [table item]
  (case (:type item)
    :ifctriangulatedfaceset
    {:kind :triangulated-face-set
     :coordinates (point-list table (get-in item [:args 0]))
     :normals (when-not (= :$ (get-in item [:args 1]))
                (mapv list-values (list-values (get-in item [:args 1]))))
     :closed (get-in item [:args 2])
     :coord-indices (mapv index-list (list-values (get-in item [:args 3])))
     :normal-indices (when-not (= :$ (get-in item [:args 4]))
                       (mapv index-list (list-values (get-in item [:args 4]))))}
    :ifcpolygonalfaceset
    {:kind :polygonal-face-set
     :coordinates (point-list table (get-in item [:args 0]))
     :closed (get-in item [:args 1])
     :faces (vec (keep #(indexed-face table %)
                       (list-values (get-in item [:args 2]))))
     :pn-index (when-not (= :$ (get-in item [:args 3]))
                 (index-list (get-in item [:args 3])))}
    nil))

(defn- geometry-item [table item-ref]
  (let [item (referenced table item-ref)]
    (case (:type item)
      :ifcextrudedareasolid
      {:kind :extruded-area-solid
       :profile (profile table (get-in item [:args 0]))
       :position (axis-placement table (get-in item [:args 1]))
       :direction (direction table (get-in item [:args 2]))
       :depth (get-in item [:args 3])}
      :ifcsweptdisksolid
      {:kind :swept-disk-solid
       :directrix (polyline table (get-in item [:args 0]))
       :radius (get-in item [:args 1])
       :inner-radius (get-in item [:args 2])
       :start-param (get-in item [:args 3]) :end-param (get-in item [:args 4])}
      :ifcmappeditem (mapped-geometry table item)
      (:ifcbooleanclippingresult :ifcbooleanresult)
      {:kind :boolean-result :operator (get-in item [:args 0])
       :first-operand (geometry-item table (get-in item [:args 1]))
       :second-operand (geometry-item table (get-in item [:args 2]))}
      (:ifchalfspacesolid :ifcpolygonalboundedhalfspace) (half-space table item)
      :ifcfacetedbrep (faceted-brep table item)
      (:ifctriangulatedfaceset :ifcpolygonalfaceset) (tessellated-face-set table item)
      nil)))

(defn- geometry-items [table item-refs]
  (let [items (vec (keep #(geometry-item table %) item-refs))]
    (case (count items)
      0 nil
      1 (first items)
      {:kind :collection :items items})))

(defn product-geometry [table representation-ref]
  (geometry-items table (representation-items table representation-ref)))

(def product-types (set (vals entity-types)))

(defn- typed-value [value]
  (if (and (vector? value) (= :typed (first value))) (nth value 2) value))

(def unit-prefix-scale
  {:exa 1.0e18 :peta 1.0e15 :tera 1.0e12 :giga 1.0e9 :mega 1.0e6
   :kilo 1.0e3 :hecto 1.0e2 :deca 1.0e1 :deci 1.0e-1 :centi 1.0e-2
   :milli 1.0e-3 :micro 1.0e-6 :nano 1.0e-9 :pico 1.0e-12
   :femto 1.0e-15 :atto 1.0e-18})

(defn- unit [table ref]
  (let [entity (referenced table ref)]
    (case (:type entity)
      :ifcsiunit
      (let [unit-type (get-in entity [:args 1]) prefix (get-in entity [:args 2])]
        {:kind :si :type unit-type :prefix (when-not (= :$ prefix) prefix)
         :name (get-in entity [:args 3]) :scale (get unit-prefix-scale prefix 1.0)})
      :ifcconversionbasedunit
      {:kind :conversion-based :type (get-in entity [:args 1])
       :name (get-in entity [:args 2]) :conversion-factor (get-in entity [:args 3])}
      nil)))

(defn- project-units [table project-entity]
  (when-let [assignment (referenced table (get-in project-entity [:args 8]))]
    (when (= :ifcunitassignment (:type assignment))
      (into {} (keep (fn [ref] (when-let [u (unit table ref)] [(:type u) u])))
            (list-values (get-in assignment [:args 0]))))))

(defn- property-single-value [entity]
  (let [nominal (get-in entity [:args 2])]
    {:name (get-in entity [:args 0]) :description (get-in entity [:args 1])
     :value (typed-value nominal)
     :value-type (when (and (vector? nominal) (= :typed (first nominal))) (second nominal))
     :unit (get-in entity [:args 3])}))

(defn- property-sets [table entities]
  (let [sets (into {}
                   (map (fn [entity]
                          [(:id entity)
                           {:id (:id entity) :global-id (get-in entity [:args 0])
                            :name (get-in entity [:args 2])
                            :properties
                            (into {} (keep (fn [ref]
                                             (let [property (referenced table ref)]
                                               (when (= :ifcpropertysinglevalue (:type property))
                                                 (let [p (property-single-value property)]
                                                   [(:name p) p])))))
                                  (list-values (get-in entity [:args 4])))}])
                        (filter #(= :ifcpropertyset (:type %)) entities)))]
    (reduce (fn [by-object relation]
              (let [pset (get sets (ref-id (get-in relation [:args 5])))]
                (reduce #(assoc-in %1 [(ref-id %2) (:name pset)] pset)
                        by-object (list-values (get-in relation [:args 4])))))
            {} (filter #(= :ifcreldefinesbyproperties (:type %)) entities))))

(defn- opening-relations [entities]
  {:voids (into {} (map (fn [relation]
                          [(ref-id (get-in relation [:args 5]))
                           (ref-id (get-in relation [:args 4]))])
                        (filter #(= :ifcrelvoidselement (:type %)) entities)))
   :fills (into {} (map (fn [relation]
                          [(ref-id (get-in relation [:args 4]))
                           (ref-id (get-in relation [:args 5]))])
                        (filter #(= :ifcrelfillselement (:type %)) entities)))})

(defn- type-relations [table entities]
  (reduce (fn [by-object relation]
            (let [type-entity (referenced table (get-in relation [:args 5]))
                  type-object {:id (:id type-entity) :ifc/type (:type type-entity)
                               :global-id (get-in type-entity [:args 0])
                               :name (get-in type-entity [:args 2])
                               :element-type (get-in type-entity [:args 8])
                               :predefined-type (get-in type-entity [:args 9])}]
              (reduce #(assoc %1 (ref-id %2) type-object) by-object
                      (list-values (get-in relation [:args 4])))))
          {} (filter #(= :ifcreldefinesbytype (:type %)) entities)))

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

(defn- georeference [table entities]
  (when-let [conversion (first (filter #(#{:ifcmapconversion :ifcmapconversionscaled}
                                           (:type %)) entities))]
    (let [context (referenced table (get-in conversion [:args 0]))
          crs (referenced table (get-in conversion [:args 1]))]
      {:projected-crs {:name (get-in crs [:args 0]) :description (get-in crs [:args 1])
                       :geodetic-datum (get-in crs [:args 2])
                       :vertical-datum (get-in crs [:args 3])
                       :map-projection (get-in crs [:args 4]) :map-zone (get-in crs [:args 5])}
       :world-origin (get-in (axis-placement table (get-in context [:args 4])) [:location])
       :true-north (direction table (get-in context [:args 5]))
       :eastings (get-in conversion [:args 2]) :northings (get-in conversion [:args 3])
       :orthogonal-height (get-in conversion [:args 4])
       :x-axis-abscissa (get-in conversion [:args 5])
       :x-axis-ordinate (get-in conversion [:args 6]) :scale (get-in conversion [:args 7])})))

(defn- spatial-node [table children id]
  (let [entity (get table id)]
    (cond->
     {:id id :global-id (get-in entity [:args 0]) :name (get-in entity [:args 2])
      :type (:type entity) :placement (local-placement table (get-in entity [:args 5]))
      :children (mapv #(spatial-node table children %) (get children id))}
      (= :ifcsite (:type entity))
      (assoc :latitude (when-not (= :$ (get-in entity [:args 9]))
                         (list-values (get-in entity [:args 9])))
             :longitude (when-not (= :$ (get-in entity [:args 10]))
                          (list-values (get-in entity [:args 10])))
             :elevation (when-not (= :$ (get-in entity [:args 11]))
                          (get-in entity [:args 11]))))))

(defn read-external-spf [text]
  (let [parsed (part21/parse-file text)
        entities (:part21/entities parsed)
        table (:part21/entity-by-id parsed)
        children (aggregates entities)
        contained-by (containment entities)
        project-entity (first (filter #(= :ifcproject (:type %)) entities))
        psets-by-object (property-sets table entities)
        types-by-object (type-relations table entities)
        {:keys [voids fills]} (opening-relations entities)
        raw-products (into {} (map (fn [entity] [(:id entity) (product table entity)]))
                           (filter #(contains? product-types (:type %)) entities))
        openings-by-host (group-by voids (keys voids))
        products (->> raw-products
                      (remove (fn [[_ value]] (= :opening (:kind value))))
                      (mapv (fn [[id value]]
                              (assoc value :container-id (get contained-by id)
                                     :type-object (get types-by-object id)
                                     :property-sets (get psets-by-object id {})
                                     :openings
                                     (mapv (fn [opening-id]
                                             (assoc (get raw-products opening-id)
                                                    :filled-by (get fills opening-id)
                                                    :filled-by-global-id
                                                    (:global-id (get raw-products (get fills opening-id)))
                                                    :property-sets (get psets-by-object opening-id {})))
                                           (get openings-by-host id))))))]
    {:ifc/schema (:part21/schema parsed) :ifc/contract-version contract-version
     :ifc/project (when project-entity (spatial-node table children (:id project-entity)))
     :ifc/units (project-units table project-entity)
     :ifc/georeference (georeference table entities)
     :ifc/elements products :ifc/source :external-spf}))

(defn read-document [text]
  (try
    (let [model (read-spf text)]
      (exchange-document {:project {:id (:id model) :name (:name model) :model model}
                          :elements []}))
    (catch #?(:clj Exception :cljs :default) _
      (read-external-spf text))))
