(ns ifc.core
  "IFC 4.3 exchange over the shared kotoba-lang/step serializer."
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [brep.spline :as spline]
            [iso-10303.part21 :as part21]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])))

(def schema "IFC4X3_ADD2")
(def contract-version 1)
(def supported-schemas #{"IFC2X3" "IFC4" "IFC4X3" "IFC4X3_ADD2"})

(declare semantic-fingerprint product-types ref-id)

(def entity-types
  {:wall :ifcwall :slab :ifcslab :column :ifccolumn :beam :ifcbeam
   :door :ifcdoor :window :ifcwindow :roof :ifcroof :stair :ifcstair
   :railing :ifcrailing :curtain :ifccurtainwall
   :mep-segment :ifcdistributionflowelement
   :duct-segment :ifcductsegment :pipe-segment :ifcpipesegment
   :air-terminal :ifcairterminal :sanitary-terminal :ifcsanitaryterminal
   :flow-fitting :ifcflowfitting
   :flow-controller :ifcflowcontroller :flow-moving-device :ifcflowmovingdevice
   :footing :ifcfooting :pile :ifcpile :member :ifcmember :plate :ifcplate
   :opening :ifcopeningelement :proxy :ifcbuildingelementproxy})

(def legacy-entity-kinds
  "IFC2x3 and IFC4 standard-case product aliases accepted by the reader. The
  writer continues to emit the canonical entity in `entity-types`."
  {:ifcwallstandardcase :wall
   :ifcslabstandardcase :slab
   :ifccolumnstandardcase :column
   :ifcbeamstandardcase :beam
   :ifcmemberstandardcase :member
   :ifcplatestandardcase :plate
   :ifcdoorstandardcase :door
   :ifcwindowstandardcase :window
   :ifcflowsegment :mep-segment})

(def entity-kind-by-type
  (merge (into {} (map (fn [[kind type]] [type kind])) entity-types)
         legacy-entity-kinds))

(def type-entity-types
  {:wall :ifcwalltype :slab :ifcslabtype :column :ifccolumntype :beam :ifcbeamtype
   :door :ifcdoortype :window :ifcwindowtype :roof :ifcrooftype :stair :ifcstairtype
   :railing :ifcrailingtype :curtain :ifccurtainwalltype
   :duct-segment :ifcductsegmenttype
   :pipe-segment :ifcpipesegmenttype :air-terminal :ifcairterminaltype
   :sanitary-terminal :ifcsanitaryterminaltype
   :footing :ifcfootingtype :pile :ifcpiletype :member :ifcmembertype
   :plate :ifcplatetype :proxy :ifcbuildingelementproxytype})

(def group-types #{:ifcgroup :ifcsystem :ifcdistributionsystem :ifczone})

(defn exchange-document [{:keys [project elements]}]
  {:ifc/schema schema :ifc/contract-version contract-version
   :ifc/project project :ifc/elements (vec elements)})

(defn- standard-entities [document]
  (let [next-id (atom 0) entities (atom [])
        emit! (fn [type & args]
                (let [id (swap! next-id inc)]
                  (swap! entities conj (into [id type] args))
                  [:ref id]))
        unit! (fn unit! [unit]
                (case (:kind unit)
                  :si
                  (emit! :ifcsiunit :$ (:type unit) (or (:prefix unit) :$)
                         (:name unit))

                  (:conversion-based :conversion-based-with-offset)
                  (let [dimensions (or (:dimensions unit) [1 0 0 0 0 0 0])
                        dimension-ref (apply emit! :ifcdimensionalexponents dimensions)
                        factor (:conversion-factor unit)
                        base-ref (unit! (:unit factor))
                        measure-ref (emit! :ifcmeasurewithunit
                                           [:typed (or (:value-type factor) :ifcreal)
                                            (:value factor)]
                                           base-ref)
                        arguments [dimension-ref (:type unit) (:name unit) measure-ref]]
                    (apply emit!
                           (if (= :conversion-based-with-offset (:kind unit))
                             :ifcconversionbasedunitwithoffset
                             :ifcconversionbasedunit)
                           (cond-> arguments
                             (= :conversion-based-with-offset (:kind unit))
                             (conj (:conversion-offset unit)))))

                  :derived
                  (let [elements
                        (mapv (fn [{:keys [unit exponent]}]
                                (emit! :ifcderivedunitelement (unit! unit) exponent))
                              (:elements unit))]
                    (emit! :ifcderivedunit (into [:list] elements) (:type unit)
                           (or (:user-defined-type unit) :$)))

                  :monetary
                  (emit! :ifcmonetaryunit (:currency unit))

                  :context-dependent
                  (let [dimensions (or (:dimensions unit) [0 0 0 0 0 0 0])]
                    (emit! :ifccontextdependentunit
                           (apply emit! :ifcdimensionalexponents dimensions)
                           (:type unit) (:name unit)))

                  (throw (ex-info "unsupported IFC unit" {:unit unit}))))
        list* (fn [values] (into [:list] values))
        logical (fn [value] (if (#{:$ :* :u :unknown} value) value (boolean value)))
        logical-true (fn [value] (if (nil? value) true (logical value)))
        point! (fn [coordinates] (emit! :ifccartesianpoint (list* coordinates)))
        direction! (fn [ratios] (emit! :ifcdirection (list* ratios)))
        axis! (fn [placement]
                (emit! :ifcaxis2placement3d
                       (point! (or (:location placement) [0.0 0.0 0.0]))
                       (direction! (or (:axis placement) [0.0 0.0 1.0]))
                       (direction! (or (:ref-direction placement) [1.0 0.0 0.0]))))
        axis2! (fn [placement]
                 (emit! :ifcaxis2placement2d
                        (point! (or (:location placement) [0.0 0.0]))
                        (direction! (or (:ref-direction placement) [1.0 0.0]))))
        axis1! (fn [placement]
                 (emit! :ifcaxis1placement
                        (point! (or (:location placement) [0.0 0.0 0.0]))
                        (direction! (or (:axis placement) [0.0 0.0 1.0]))))
        local! (fn [placement] (emit! :ifclocalplacement :$ (axis! placement)))
        curve! (fn curve! [curve]
                 (case (:kind curve)
                   :polyline (emit! :ifcpolyline (list* (mapv point! (:points curve))))
                   :indexed-polycurve
                   (let [coordinates (:points curve)
                         dimension (count (first coordinates))
                         point-list (emit! (if (= 2 dimension)
                                             :ifccartesianpointlist2d
                                             :ifccartesianpointlist3d)
                                           (list* (mapv list* coordinates)))
                         segments
                         (if (seq (:segments curve))
                           (list* (mapv (fn [{:keys [kind indices]}]
                                         [:typed (if (= :arc kind)
                                                   :ifcarcindex :ifclineindex)
                                          (list* indices)])
                                       (:segments curve)))
                           :$)]
                     (emit! :ifcindexedpolycurve point-list segments
                            (if (nil? (:self-intersect curve)) :$
                                (logical (:self-intersect curve)))))
                   :composite-curve
                   (let [segments
                         (mapv (fn [segment]
                                 (emit! :ifccompositecurvesegment
                                        (or (:transition segment) :continuous)
                                        (logical-true (:same-sense segment))
                                        (curve! (:parent-curve segment))))
                               (:segments curve))]
                     (emit! :ifccompositecurve (list* segments)
                            (logical (:self-intersect curve))))
                   :line (let [orientation (emit! :ifcvector
                                                   (direction! (:direction curve))
                                                   (or (:magnitude curve) 1.0))]
                           (emit! :ifcline (point! (:origin curve)) orientation))
                   :circle (emit! :ifccircle (axis! (:position curve)) (:radius curve))
                   :ellipse (emit! :ifcellipse (axis! (:position curve))
                                   (:semi-axis1 curve) (:semi-axis2 curve))
                   :b-spline-curve
                   (let [points (list* (mapv point! (:control-points curve)))
                         args [(:degree curve) points (or (:curve-form curve) :unspecified)
                               (logical (:closed curve)) (logical (:self-intersect curve))]]
                     (if (seq (:knots curve))
                       (let [knot-args (concat args
                                               [(list* (:multiplicities curve))
                                                (list* (:knots curve))
                                                (or (:knot-spec curve) :unspecified)])]
                         (if (seq (:weights curve))
                           (apply emit! :ifcrationalbsplinecurvewithknots
                                  (concat knot-args [(list* (:weights curve))]))
                           (apply emit! :ifcbsplinecurvewithknots knot-args)))
                       (apply emit! :ifcbsplinecurve args)))
                   nil))
        profile! (fn [profile]
                   (case (:kind profile)
                     :rectangle (emit! :ifcrectangleprofiledef
                                       (or (:profile-type profile) :area)
                                       (or (:name profile) "Profile")
                                       (if-let [position (:position profile)]
                                         (axis2! position) :$)
                                       (:x-dim profile) (:y-dim profile))
                     :circle (emit! :ifccircleprofiledef
                                    (or (:profile-type profile) :area)
                                    (or (:name profile) "Profile")
                                    (if-let [position (:position profile)]
                                      (axis2! position) :$)
                                    (:radius profile))
                     :i-shape (emit! :ifcishapeprofiledef
                                     (or (:profile-type profile) :area)
                                     (or (:name profile) "Profile")
                                     (if-let [position (:position profile)]
                                       (axis2! position) :$)
                                     (:overall-width profile) (:overall-depth profile)
                                     (:web-thickness profile) (:flange-thickness profile)
                                     (or (:fillet-radius profile) :$)
                                     (or (:flange-edge-radius profile) :$)
                                     (or (:flange-slope profile) :$))
                     :arbitrary-closed
                     (emit! :ifcarbitraryclosedprofiledef :area
                            (or (:name profile) "Profile")
                            (curve! (or (:curve profile)
                                        {:kind :polyline :points (:points profile)})))
                     :arbitrary-with-voids
                     (emit! :ifcarbitraryprofiledefwithvoids
                            (or (:profile-type profile) :area)
                            (or (:name profile) "Profile")
                            (curve! (:outer-curve profile))
                            (list* (mapv curve! (:inner-curves profile))))
                     nil))
        loop! (fn [bound]
                (if (seq (:edges bound))
                  (let [edges
                        (mapv (fn [edge]
                                (let [orientation (if (nil? (:orientation edge))
                                                    true (:orientation edge))
                                      [edge-start edge-end] (if (false? orientation)
                                                              [(:end edge) (:start edge)]
                                                              [(:start edge) (:end edge)])
                                      start (emit! :ifcvertexpoint (point! edge-start))
                                      end (emit! :ifcvertexpoint (point! edge-end))
                                      edge-curve (emit! :ifcedgecurve start end
                                                        (curve! (:curve edge))
                                                        (logical-true (:same-sense edge)))]
                                  (emit! :ifcorientededge :$ :$ edge-curve
                                         (logical orientation))))
                              (:edges bound))]
                    (emit! :ifcedgeloop (list* edges)))
                  (emit! :ifcpolyloop (list* (mapv point! (:points bound))))))
        surface! (fn [surface]
                   (case (:kind surface)
                     :plane (emit! :ifcplane (axis! (:position surface)))
                     :cylinder (emit! :ifccylindricalsurface (axis! (:position surface))
                                      (:radius surface))
                     :sphere (emit! :ifcsphericalsurface (axis! (:position surface))
                                    (:radius surface))
                     :torus (emit! :ifctoroidalsurface (axis! (:position surface))
                                   (:major-radius surface) (:minor-radius surface))
                     :b-spline-surface
                     (let [points (list* (mapv (fn [row] (list* (mapv point! row)))
                                                (:control-points surface)))]
                       (if (seq (:u-knots surface))
                         (let [args [(:u-degree surface) (:v-degree surface) points
                                     (or (:surface-form surface) :unspecified)
                                     (logical (:u-closed surface)) (logical (:v-closed surface))
                                     (logical (:self-intersect surface))
                                     (list* (:u-multiplicities surface))
                                     (list* (:v-multiplicities surface))
                                     (list* (:u-knots surface)) (list* (:v-knots surface))
                                     (or (:knot-spec surface) :unspecified)]]
                           (if (seq (:weights surface))
                             (apply emit! :ifcrationalbsplinesurfacewithknots
                                    (conj args (list* (mapv list* (:weights surface)))))
                             (apply emit! :ifcbsplinesurfacewithknots args)))
                         (emit! :ifcbsplinesurface
                                (:u-degree surface) (:v-degree surface) points
                                (or (:surface-form surface) :unspecified)
                                (logical (:u-closed surface)) (logical (:v-closed surface))
                                (logical (:self-intersect surface)))))
                     nil))
        geometry! (fn geometry! [geometry]
                    (case (:kind geometry)
                      :extruded-area-solid
                      (let [profile-ref (profile! (:profile geometry))]
                        (when profile-ref
                          (emit! :ifcextrudedareasolid profile-ref (axis! (:position geometry))
                                 (direction! (or (:direction geometry) [0.0 0.0 1.0]))
                                 (:depth geometry))))
                      :revolved-area-solid
                      (emit! :ifcrevolvedareasolid
                             (profile! (:profile geometry))
                             (axis! (:position geometry))
                             (axis1! (:axis geometry))
                             (:angle geometry))
                      :fixed-reference-swept-area-solid
                      (emit! :ifcfixedreferencesweptareasolid
                             (profile! (:profile geometry))
                             (axis! (:position geometry))
                             (curve! (:directrix geometry))
                             (or (:start-param geometry) :$)
                             (or (:end-param geometry) :$)
                             (direction! (:fixed-reference geometry)))
                      :surface-curve-swept-area-solid
                      (emit! :ifcsurfacecurvesweptareasolid
                             (profile! (:profile geometry))
                             (axis! (:position geometry))
                             (curve! (:directrix geometry))
                             (or (:start-param geometry) :$)
                             (or (:end-param geometry) :$)
                             (surface! (:reference-surface geometry)))
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
                                                  (let [loop-ref (loop! bound)]
                                                    (emit! (if (= :outer (:kind bound))
                                                             :ifcfaceouterbound :ifcfacebound)
                                                           loop-ref (logical-true (:orientation bound)))))
                                                (:bounds face))]
                                      (emit! :ifcface (list* bounds))))
                                  (:faces geometry))
                            shell (emit! :ifcclosedshell (list* faces))]
                        (emit! :ifcfacetedbrep shell))
                      :advanced-brep
                      (let [faces
                            (mapv (fn [face]
                                    (let [bounds
                                          (mapv (fn [bound]
                                                  (let [loop-ref (loop! bound)]
                                                    (emit! (if (= :outer (:kind bound))
                                                             :ifcfaceouterbound :ifcfacebound)
                                                           loop-ref (logical-true (:orientation bound)))))
                                                (:bounds face))]
                                      (emit! :ifcadvancedface (list* bounds)
                                             (surface! (:surface face))
                                             (logical-true (:same-sense face)))))
                                  (:faces geometry))
                            shell (emit! :ifcclosedshell (list* faces))]
                        (emit! :ifcadvancedbrep shell))
                      :triangulated-face-set
                      (let [coordinates (emit! :ifccartesianpointlist3d
                                               (list* (mapv list* (:coordinates geometry))))]
                        (emit! :ifctriangulatedfaceset coordinates
                               (if (seq (:normals geometry))
                                 (list* (mapv list* (:normals geometry))) :$)
                               (logical (:closed geometry))
                               (list* (mapv list* (:coord-indices geometry))) :$))
                      :polygonal-face-set
                      (let [coordinates (emit! :ifccartesianpointlist3d
                                               (list* (mapv list* (:coordinates geometry))))
                            faces (mapv (fn [{:keys [outer inners]}]
                                          (if (seq inners)
                                            (emit! :ifcindexedpolygonalfacewithvoids
                                                   (list* outer) (list* (mapv list* inners)))
                                            (emit! :ifcindexedpolygonalface (list* outer))))
                                        (:faces geometry))]
                        (emit! :ifcpolygonalfaceset coordinates
                               (logical (:closed geometry)) (list* faces)
                               (if (seq (:pn-index geometry))
                                 (list* (:pn-index geometry)) :$)))
                      :half-space-solid
                      (let [plane (surface! (:base-surface geometry))]
                        (if (seq (:boundary geometry))
                          (emit! :ifcpolygonalboundedhalfspace
                                 plane (logical-true (:agreement-flag geometry))
                                 (axis! (:position geometry))
                                 (emit! :ifcpolyline
                                        (list* (mapv point! (:boundary geometry)))))
                          (emit! :ifchalfspacesolid
                                 plane (logical-true (:agreement-flag geometry)))))
                      :boolean-result
                      (emit! :ifcbooleanresult (or (:operator geometry) :difference)
                             (geometry! (:first-operand geometry))
                             (geometry! (:second-operand geometry)))
                      :mapped-item
                      (let [source (:source geometry)
                            source-items (if (= :collection (:kind source))
                                           (vec (keep geometry! (:items source)))
                                           (some-> (geometry! source) vector))
                            source-shape (emit! :ifcshaperepresentation
                                                :$ "Body" "Body" (list* source-items))
                            representation-map (emit! :ifcrepresentationmap
                                                      (axis! (:mapping-origin geometry))
                                                      source-shape)
                            transform (:transform geometry)
                            nonuniform? (or (not= 1.0 (:scale2 transform 1.0))
                                            (not= 1.0 (:scale3 transform 1.0)))
                            transform-ref
                            (apply emit!
                                   (if nonuniform?
                                     :ifccartesiantransformationoperator3dnonuniform
                                     :ifccartesiantransformationoperator3d)
                                   (cond-> [(direction! (:axis1 transform))
                                            (direction! (:axis2 transform))
                                            (point! (:origin transform))
                                            (or (:scale transform) 1.0)
                                            (direction! (:axis3 transform))]
                                     nonuniform?
                                     (conj (or (:scale2 transform) 1.0)
                                           (or (:scale3 transform) 1.0))))]
                        (emit! :ifcmappeditem representation-map transform-ref))
                      :collection nil
                      nil))
        presentation! (fn [items element]
                        (when-let [appearance (:appearance element)]
                          (let [[red green blue] (:surface-color appearance)
                                colour (emit! :ifccolourrgb
                                              (or (:color-name appearance) :$)
                                              red green blue)
                                rendering
                                (emit! :ifcsurfacestylerendering colour
                                       (or (:transparency appearance) 0.0)
                                       :$ :$ :$ :$ :$ :$
                                       (or (:reflectance-method appearance) :notdefined))
                                style (emit! :ifcsurfacestyle
                                             (or (:name appearance) "Surface Style")
                                             (or (:side appearance) :both)
                                             (list* [rendering]))]
                            (doseq [item items]
                              (emit! :ifcstyleditem item (list* [style])
                                     (or (:name appearance) :$)))))
                        (doseq [layer (:presentation-layers element)]
                          (emit! :ifcpresentationlayerassignment
                                 (:name layer) (or (:description layer) :$)
                                 (list* items) (or (:identifier layer) :$))))
        product-shape! (fn [element]
                         (let [geometry (:geometry element)
                               items (if (= :collection (:kind geometry))
                                       (vec (keep geometry! (:items geometry)))
                                       (some-> (geometry! geometry) vector))]
                           (when (seq items)
                             (presentation! items element)
                             (let [shape (emit! :ifcshaperepresentation :$ "Body" "Body" (list* items))]
                               (emit! :ifcproductdefinitionshape :$ :$ (list* [shape]))))))
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
                               :ifcspace
                               (emit! type (or (:global-id node) (str "SPACE_" (:id node))) :$
                                      (:name node) :$ :$ (local! (:placement node)) :$
                                      (or (:long-name node) :$) :element
                                      (or (:predefined-type node) :notdefined)
                                      (or (:elevation-with-flooring node) :$))
                               nil)
                         children (vec (keep spatial! (:children node)))]
                     (when ref
                       (swap! container-refs assoc (:id node) ref (:global-id node) ref)
                       (when (= :ifcbuildingstorey type)
                         (swap! container-refs assoc :default ref))
                       (when (seq children)
                         (emit! :ifcrelaggregates (str "REL_AGG_" (:id node)) :$ :$ :$
                                ref (list* children))))
                     ref))
        property-value (fn [value value-type]
                         (let [value-type (or value-type
                                              (cond (boolean? value) :ifcboolean
                                                    (integer? value) :ifcinteger
                                                    (number? value) :ifcreal
                                                    :else :ifclabel))]
                           [:typed value-type
                            (if (= :ifcboolean value-type) (if value :t :f) value)]))
        property-unit! (fn [unit] (if unit (unit! unit) :$))
        property-set! (fn [owner-id name pset]
                        (let [properties
                         (mapv
                          (fn [[property-name property]]
                            (let [description (or (:description property) :$)
                                  typed #(property-value % (:value-type property))
                                  unit-ref (property-unit! (:unit property))]
                              (case (:kind property)
                                :enumerated
                                (let [enumeration (:enumeration property)
                                      enumeration-ref
                                      (when enumeration
                                        (emit! :ifcpropertyenumeration
                                               (:name enumeration)
                                               (list* (mapv typed (:values enumeration)))
                                               (property-unit! (:unit enumeration))))]
                                  (emit! :ifcpropertyenumeratedvalue
                                         property-name description
                                         (if (seq (:values property))
                                           (list* (mapv typed (:values property))) :$)
                                         (or enumeration-ref :$)))

                                :bounded
                                (emit! :ifcpropertyboundedvalue
                                       property-name description
                                       (if (some? (:upper property))
                                         (typed (:upper property)) :$)
                                       (if (some? (:lower property))
                                         (typed (:lower property)) :$)
                                       unit-ref
                                       (if (some? (:set-point property))
                                         (typed (:set-point property)) :$))

                                :list
                                (emit! :ifcpropertylistvalue
                                       property-name description
                                       (if (seq (:values property))
                                         (list* (mapv typed (:values property))) :$)
                                       unit-ref)

                                (emit! :ifcpropertysinglevalue property-name description
                                       (property-value (:value property)
                                                       (:value-type property))
                                       unit-ref))))
                          (:properties pset))
                         pset-ref (emit! :ifcpropertyset
                                         (or (:global-id pset) (str "PSET_" owner-id "_" name))
                                         :$ name :$ (list* properties))]
                          pset-ref))
        psets! (fn [product-ref element]
                 (doseq [[name pset] (:property-sets element)]
                   (let [pset-ref (property-set! (:id element) name pset)]
                     (emit! :ifcreldefinesbyproperties
                            (str "REL_PSET_" (:id element) "_" name) :$ :$ :$
                            (list* [product-ref]) pset-ref))))
        quantity-types {:length :ifcquantitylength :area :ifcquantityarea
                        :volume :ifcquantityvolume :count :ifcquantitycount
                        :weight :ifcquantityweight :time :ifcquantitytime
                        :number :ifcquantitynumber}
        quantity-unit! (fn [unit] (if unit (unit! unit) :$))
        qsets! (fn [product-ref element]
                 (doseq [[name qset] (:quantity-sets element)]
                   (let [quantities
                         (mapv (fn [[quantity-name quantity]]
                                 (emit! (get quantity-types (:kind quantity)
                                             :ifcquantitynumber)
                                        quantity-name (or (:description quantity) :$)
                                        (quantity-unit! (:unit quantity))
                                        (:value quantity) (or (:formula quantity) :$)))
                               (:quantities qset))
                         qset-ref
                         (emit! :ifcelementquantity
                                (or (:global-id qset)
                                    (str "QSET_" (:id element) "_" name))
                                :$ name (or (:description qset) :$)
                                (or (:method-of-measurement qset) :$)
                                (list* quantities))]
                     (emit! :ifcreldefinesbyproperties
                            (str "REL_QSET_" (:id element) "_" name) :$ :$ :$
                            (list* [product-ref]) qset-ref))))
        material-definition!
        (fn material-definition! [assignment]
          (case (:kind assignment)
            :layer-set
            (let [layers
                  (mapv (fn [layer]
                          (emit! :ifcmateriallayer
                                 (if-let [material (:material layer)]
                                   (material-definition! material) :$)
                                 (:thickness layer)
                                 (if (nil? (:ventilated layer)) :$
                                     (logical (:ventilated layer)))
                                 (or (:name layer) :$)
                                 (or (:description layer) :$)
                                 (or (:category layer) :$)
                                 (or (:priority layer) :$)))
                        (:layers assignment))]
              (emit! :ifcmateriallayerset (list* layers)
                     (or (:name assignment) :$)
                     (or (:description assignment) :$)))

            :layer-set-usage
            (emit! :ifcmateriallayersetusage
                   (material-definition! (assoc (:layer-set assignment) :kind :layer-set))
                   (or (:direction assignment) :axis2)
                   (or (:direction-sense assignment) :positive)
                   (or (:offset assignment) 0.0)
                   (or (:reference-extent assignment) :$))

            (emit! :ifcmaterial (:name assignment)
                   (or (:description assignment) :$)
                   (or (:category assignment) :$))))
        material-association!
        (fn [product-ref element]
          (when-let [assignment (:material element)]
            (let [material-ref (material-definition! assignment)]
              (emit! :ifcrelassociatesmaterial
                     (str "REL_MATERIAL_" (:id element)) :$ :$ :$
                     (list* [product-ref]) material-ref))))
        classification-associations!
        (fn [product-ref element]
          (doseq [[index classification] (map-indexed vector (:classifications element))]
            (let [source (:source classification)
                  source-ref
                  (emit! :ifcclassification
                         (or (:source source) :$) (or (:edition source) :$)
                         (or (:edition-date source) :$) (:name source)
                         (or (:description source) :$) (or (:specification source) :$)
                         (if (seq (:reference-tokens source))
                           (list* (:reference-tokens source)) :$))
                  reference-ref
                  (emit! :ifcclassificationreference
                         (or (:location classification) :$)
                         (or (:identification classification) :$)
                         (or (:name classification) :$) source-ref
                         (or (:description classification) :$)
                         (or (:sort classification) :$))]
              (emit! :ifcrelassociatesclassification
                     (str "REL_CLASSIFICATION_" (:id element) "_" index)
                     :$ :$ :$ (list* [product-ref]) reference-ref))))
        product! (fn [element type]
                   (emit! type (or (:global-id element) (str (:id element))) :$ (:name element) :$ :$
                          (local! (:placement element)) (or (product-shape! element) :$)
                          (or (:tag element) :$) :$))
        type! (fn [product-ref element]
                (when-let [type-object (:type-object element)]
                  (let [type-psets (mapv (fn [[name pset]]
                                           (property-set! (:id type-object) name pset))
                                         (:property-sets type-object))
                        representation-maps
                        (mapv (fn [representation-map]
                                (let [geometry (:geometry representation-map)
                                      items (if (= :collection (:kind geometry))
                                              (vec (keep geometry! (:items geometry)))
                                              (some-> (geometry! geometry) vector))
                                      shape (emit! :ifcshaperepresentation :$
                                                   (or (:identifier representation-map) "Body")
                                                   (or (:representation-type representation-map)
                                                       "Body")
                                                   (list* items))]
                                  (emit! :ifcrepresentationmap
                                         (axis! (:mapping-origin representation-map)) shape)))
                              (:representation-maps type-object))
                        type-ref (emit! (or (:ifc/type type-object)
                                            (get type-entity-types (:kind element)
                                                 :ifcbuildingelementproxytype))
                                        (or (:global-id type-object)
                                            (str "TYPE_" (:id type-object)))
                                        :$ (:name type-object) :$ :$
                                        (if (seq type-psets) (list* type-psets) :$)
                                        (if (seq representation-maps)
                                          (list* representation-maps) :$) :$
                                        (or (:element-type type-object) :$)
                                        (or (:predefined-type type-object) :notdefined))]
                    (material-association! type-ref type-object)
                    (classification-associations! type-ref type-object)
                    (emit! :ifcreldefinesbytype
                           (str "REL_TYPE_" (:id element)) :$ :$ :$
                           (list* [product-ref]) type-ref))))
        project (:ifc/project document)
        georeference (or (:ifc/georeference document) (:georeference project))
        elements (:ifc/elements document)
        source-units (:ifc/units document)
        unit-refs (if (seq source-units)
                    (mapv (fn [[unit-type unit]]
                            (unit! (assoc unit :type unit-type)))
                          (sort-by (comp name key) source-units))
                    [(emit! :ifcsiunit :$ :lengthunit :$ :metre)])
        units (emit! :ifcunitassignment (list* unit-refs))
        world-axis (axis! {:location (or (:world-origin georeference) [0.0 0.0 0.0])})
        true-north (when-let [direction (:true-north georeference)] (direction! direction))
        context (emit! :ifcgeometricrepresentationcontext :$ "Model" 3 1.0e-5
                       world-axis (or true-north :$))
        projected-crs (when georeference
                        (let [crs (:projected-crs georeference)]
                          (emit! :ifcprojectedcrs (or (:name crs) "Undefined CRS")
                                 (or (:description crs) :$) (or (:geodetic-datum crs) :$)
                                 (or (:vertical-datum crs) :$) (or (:map-projection crs) :$)
                                 (or (:map-zone crs) :$)
                                 (if-let [map-unit (:map-unit crs)]
                                   (unit! map-unit) :$))))
        _map-conversion (when projected-crs
                          (let [arguments [context projected-crs
                                           (or (:eastings georeference) 0.0)
                                           (or (:northings georeference) 0.0)
                                           (or (:orthogonal-height georeference) 0.0)
                                           (or (:x-axis-abscissa georeference) 1.0)
                                           (or (:x-axis-ordinate georeference) 0.0)
                                           (or (:scale georeference) 1.0)]
                                scaled? (or (= :scaled (:map-conversion-kind georeference))
                                            (some? (:factor-x georeference))
                                            (some? (:factor-y georeference))
                                            (some? (:factor-z georeference)))]
                            (apply emit!
                                   (if scaled? :ifcmapconversionscaled :ifcmapconversion)
                                   (cond-> arguments
                                     scaled? (into [(or (:factor-x georeference) 1.0)
                                                    (or (:factor-y georeference) 1.0)
                                                    (or (:factor-z georeference) 1.0)])))))
        project-ref (emit! :ifcproject (or (:global-id project) "KOTOBA_PROJECT") :$
                           (or (:name project) "Project") :$ :$ :$ (list* [context]) :$ units)
        spatial-roots (or (seq (:children project))
                          (when-not (= :external-spf (:ifc/source document))
                            [{:id :site :type :ifcsite :name "Site" :children
                              [{:id :building :type :ifcbuilding :name "Building" :children
                                [{:id :storey :type :ifcbuildingstorey :name "Storey"
                                  :children []}]}]}])
                          [])
        spatial-refs (mapv spatial! spatial-roots)
        _project-spatial (emit! :ifcrelaggregates "KOTOBA_REL_PROJECT_SPATIAL" :$ :$ :$
                                project-ref (list* spatial-refs))
        product-by-source (atom {})
        _product-refs
        (mapv (fn [element]
                (let [kind-type (get entity-types (:kind element))
                      imported-type (:ifc/entity-type element)
                      entity-type (if (and imported-type
                                           (or (= :other (:kind element))
                                               (= (:kind element)
                                                  (get entity-kind-by-type imported-type))))
                                    imported-type
                                    (or kind-type :ifcbuildingelementproxy))
                      ref (product! element entity-type)]
                  (swap! product-by-source assoc (:id element) ref (:global-id element) ref)
                  (psets! ref element)
                  (qsets! ref element)
                  (material-association! ref element)
                  (classification-associations! ref element)
                  (type! ref element)
                  ref))
              elements)
        default-container (get @container-refs :default :$)
        _containment
        (doseq [[container-id grouped]
                (group-by #(if (some? (:container-id %))
                             (:container-id %)
                             (when-not (= :external-spf (:ifc/source document)) :default))
                          elements)
                :when (some? container-id)]
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
        port-by-source (atom {})
        _ports
        (doseq [element elements port (:ports element)]
          (let [port-ref
                (emit! :ifcdistributionport
                       (or (:global-id port) (str "PORT_" (:id port))) :$
                       (:name port) (or (:description port) :$)
                       (or (:object-type port) :$) (local! (:placement port)) :$
                       (or (:tag port) :$) (or (:flow-direction port) :notdefined)
                       (or (:predefined-type port) :notdefined)
                       (or (:system-type port) :notdefined))
                element-ref (get @product-by-source (:id element))]
            (swap! port-by-source assoc (:id port) port-ref (:global-id port) port-ref)
            (emit! :ifcrelnests (str "REL_PORT_" (:id element) "_" (:id port))
                   :$ :$ :$ element-ref (list* [port-ref]))))
        group-by-source (atom {})
        _groups
        (doseq [group (:ifc/groups document)]
          (let [group-ref
                (case (:kind group)
                  :zone (emit! :ifczone (or (:global-id group) (str "ZONE_" (:id group)))
                               :$ (:name group) (or (:description group) :$)
                               (or (:object-type group) :$) (or (:long-name group) :$))
                  :distribution-system
                  (emit! :ifcdistributionsystem
                         (or (:global-id group) (str "SYSTEM_" (:id group))) :$
                         (:name group) (or (:description group) :$)
                         (or (:object-type group) :$) (or (:long-name group) :$)
                         (or (:predefined-type group) :notdefined))
                  (emit! :ifcsystem (or (:global-id group) (str "SYSTEM_" (:id group)))
                         :$ (:name group) (or (:description group) :$)
                         (or (:object-type group) :$)))
                members (keep #(or (get @product-by-source %)
                                   (get @container-refs %)
                                   (get @port-by-source %))
                              (concat (:member-ids group) (:member-global-ids group)))]
            (swap! group-by-source assoc (:id group) group-ref (:global-id group) group-ref)
            (when (seq members)
              (emit! :ifcrelassignstogroup (str "REL_GROUP_" (:id group)) :$ :$ :$
                     (list* members) :$ group-ref))))
        _connections
        (doseq [connection (:ifc/connections document)]
          (let [relating (or (get @port-by-source (:relating-port-id connection))
                             (get @port-by-source (:relating-port-global-id connection)))
                related (or (get @port-by-source (:related-port-id connection))
                            (get @port-by-source (:related-port-global-id connection)))
                realizing (or (get @product-by-source (:realizing-element-id connection))
                              (get @product-by-source (:realizing-element-global-id connection)) :$)]
            (when (and relating related)
              (emit! :ifcrelconnectsports
                     (or (:global-id connection) (str "REL_CONNECT_" (:id connection)))
                     :$ (:name connection) (or (:description connection) :$)
                     relating related realizing))))
        _payload (when (some? (:model project))
            (emit! :ifcpropertysinglevalue "KOTOBA_MODEL_EDN" :$
                   [:typed :ifctext (pr-str (:model project))] :$))]
    @entities))

(defn- entity-refs [entity]
  (let [refs (atom #{})]
    (walk/postwalk (fn [value]
                     (when (and (vector? value) (= :ref (first value)))
                       (swap! refs conj (second value)))
                     value)
                   (:args entity))
    @refs))

(defn- dependency-ids [table roots]
  (loop [pending (vec roots) seen #{}]
    (if-let [id (peek pending)]
      (if (contains? seen id)
        (recur (pop pending) seen)
        (recur (into (pop pending) (entity-refs (get table id))) (conj seen id)))
      seen)))

(defn- remap-refs [value id-map]
  (walk/postwalk (fn [node]
                   (if (and (vector? node) (= :ref (first node)))
                     [:ref (get id-map (second node) (second node))]
                     node))
                 value))

(defn- replace-args [entity replacements]
  (update entity :args
          (fn [args]
            (reduce-kv (fn [result index value] (assoc result index value))
                       (vec args) replacements))))

(defn- hybrid-entities
  "Retain vendor entities while reconciling edited, added, removed, and retyped
  products against a regenerated semantic graph."
  [document]
  (let [raw (:ifc/raw-entities document)
        raw-table (into {} (map (juxt :id identity)) raw)
        generated-vectors (standard-entities (dissoc document :ifc/raw-spf
                                                      :ifc/import-fingerprint))
        generated (mapv (fn [[id type & args]] {:id id :type type :args (vec args)})
                        generated-vectors)
        generated-table (into {} (map (juxt :id identity)) generated)
        spatial-types #{:ifcproject :ifcsite :ifcbuilding :ifcbuildingstorey :ifcspace}
        grouped-or-port-types (conj group-types :ifcdistributionport)
        product-or-spatial? #(or (contains? product-types (:type %))
                                 (contains? spatial-types (:type %))
                                 (contains? grouped-or-port-types (:type %)))
        relationship? #(string/starts-with? (name (:type %)) "ifcrel")
        by-global (fn [entities]
                    (into {} (keep (fn [entity]
                                     (when (and (product-or-spatial? entity)
                                                (string? (get-in entity [:args 0])))
                                       [(get-in entity [:args 0]) entity])))
                          entities))
        generated-by-global (by-global generated)
        raw-by-global (by-global raw)
        matched (keep (fn [[global-id original]]
                        (when-let [replacement (get generated-by-global global-id)]
                          [original replacement]))
                      raw-by-global)
        matched-raw-ids (set (map (comp :id first) matched))
        matched-generated-ids (set (map (comp :id second) matched))
        deleted (keep (fn [[global-id entity]]
                        (when (and (or (contains? product-types (:type entity))
                                       (contains? grouped-or-port-types (:type entity)))
                                   (not (contains? generated-by-global global-id)))
                          (:id entity)))
                      raw-by-global)
        deleted-ids (set deleted)
        new-products (keep (fn [[global-id entity]]
                             (when (and (or (contains? product-types (:type entity))
                                            (contains? grouped-or-port-types (:type entity)))
                                        (not (contains? raw-by-global global-id)))
                               (:id entity)))
                           generated-by-global)
        new-product-ids (set new-products)
        generated-to-raw
        (into {} (keep (fn [[global-id generated-entity]]
                         (when-let [raw-entity (get raw-by-global global-id)]
                           [(:id generated-entity) (:id raw-entity)]))
                       generated-by-global))
        geometry-roots
        (mapcat (fn [[original replacement]]
                  (if (= :ifcproject (:type original))
                    []
                    (keep ref-id [(get-in replacement [:args 5])
                                  (get-in replacement [:args 6])])))
                matched)
        raw-geometry-roots
        (mapcat (fn [[original _]]
                  (if (= :ifcproject (:type original)) []
                      (keep ref-id [(get-in original [:args 5])
                                    (get-in original [:args 6])])))
                matched)
        raw-geometry-dependencies (dependency-ids raw-table raw-geometry-roots)
        reconciled-relation-types
        #{:ifcreldefinesbyproperties :ifcreldefinesbytype
          :ifcrelassociatesmaterial :ifcrelassociatesclassification
          :ifcrelassignstogroup :ifcrelnests :ifcrelconnectsports}
        relation-roots
        (keep (fn [entity]
                (when (and (contains? reconciled-relation-types (:type entity))
                           (seq (set/intersection
                                 (set/union matched-generated-ids new-product-ids)
                                 (entity-refs entity))))
                  (:id entity)))
              generated)
        geometry-dependencies (dependency-ids generated-table
                                              (concat geometry-roots new-products))
        presentation-types #{:ifcstyleditem :ifcpresentationlayerassignment}
        presentation-roots
        (keep (fn [entity]
                (when (and (contains? presentation-types (:type entity))
                           (seq (set/intersection geometry-dependencies
                                                  (entity-refs entity))))
                  (:id entity)))
              generated)
        dependencies (dependency-ids generated-table
                                     (concat geometry-roots new-products relation-roots
                                             presentation-roots))
        append-ids (remove #(contains? generated-to-raw %) dependencies)
        max-raw-id (reduce max 0 (map :id raw))
        id-map (into {} (map-indexed (fn [index id] [id (+ max-raw-id index 1)])
                                     (sort append-ids)))
        reference-map (merge id-map generated-to-raw)
        appended
        (mapv (fn [id]
                (let [entity (get generated-table id)]
                  (assoc entity :id (get id-map id)
                         :args (remap-refs (:args entity) reference-map))))
              (sort append-ids))
        replacement-by-raw-id
        (into {}
              (map (fn [[original replacement]]
                     (let [project? (= :ifcproject (:type original))
                           full-replacement?
                           (contains? grouped-or-port-types (:type original))]
                       [(:id original)
                        (if full-replacement?
                          (assoc replacement :id (:id original)
                                 :args (remap-refs (:args replacement) reference-map))
                          (let [replacements
                                (cond-> {2 (get-in replacement [:args 2])}
                                  (not project?)
                                  (assoc 5 (remap-refs (get-in replacement [:args 5]) id-map)
                                         6 (remap-refs (get-in replacement [:args 6]) id-map)))]
                            (assoc (replace-args original replacements)
                                   :type (:type replacement))))])))
              matched)
        removed-relationship? (fn [entity]
                                (and (relationship? entity)
                                     (or (seq (set/intersection deleted-ids
                                                                (entity-refs entity)))
                                         (and (contains? reconciled-relation-types
                                                         (:type entity))
                                              (seq (set/intersection
                                                    matched-raw-ids
                                                    (entity-refs entity)))))))
        removed-presentation? (fn [entity]
                                (and (contains? presentation-types (:type entity))
                                     (seq (set/intersection raw-geometry-dependencies
                                                            (entity-refs entity)))))
        patched (->> raw
                     (remove #(or (contains? deleted-ids (:id %))
                                  (removed-relationship? %)
                                  (removed-presentation? %)))
                     (mapv #(get replacement-by-raw-id (:id %) %)))]
    (mapv (fn [{:keys [id type args]}] (into [id type] args))
          (concat patched appended))))

(defn write-spf [document]
  (let [target-schema (or (:ifc/schema document) schema)
        unchanged-external? (and (:ifc/raw-spf document)
                                 (= (:ifc/import-fingerprint document)
                                    (semantic-fingerprint document)))]
    (when-not (contains? supported-schemas target-schema)
      (throw (ex-info "unsupported IFC export schema"
                      {:schema target-schema :supported supported-schemas})))
    (if unchanged-external?
      (:ifc/raw-spf document)
      (apply part21/file {:description "ViewDefinition [DesignTransferView]"
                          :name "building.ifc" :schema target-schema
                          :author "KAMI" :org "kotoba-lang"}
             (if (seq (:ifc/raw-entities document))
               (hybrid-entities document)
               (standard-entities document))))))

(defn rewrite-spf
  "Force standard-entity regeneration while retaining the imported schema."
  [document]
  (write-spf (dissoc document :ifc/raw-spf :ifc/import-fingerprint
                     :ifc/raw-entities)))

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

(defn- axis1-placement [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifcaxis1placement (:type entity))
      {:location (or (coordinates table (get-in entity [:args 0])) [0.0 0.0 0.0])
       :axis (or (direction table (get-in entity [:args 1])) [0.0 0.0 1.0])})))

(defn- vector3 [values]
  (vec (take 3 (concat values (repeat 0.0)))))

(defn- v+ [a b]
  (mapv + (vector3 a) (vector3 b)))

(defn- scale-vector [value vector]
  (mapv #(* value %) vector))

(defn- cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn- normalize [vector fallback]
  (let [vector (vector3 vector)
        magnitude #?(:clj (Math/sqrt (reduce + (map #(* % %) vector)))
                     :cljs (js/Math.sqrt (reduce + (map #(* % %) vector))))]
    (if (pos? magnitude) (mapv #(/ % magnitude) vector) fallback)))

(defn- placement-basis [placement]
  (let [z (normalize (:axis placement) [0.0 0.0 1.0])
        supplied-x (normalize (:ref-direction placement) [1.0 0.0 0.0])
        y (normalize (cross z supplied-x) [0.0 1.0 0.0])
        x (normalize (cross y z) [1.0 0.0 0.0])]
    [x y z]))

(defn- transform-vector [basis vector]
  (reduce v+ [0.0 0.0 0.0]
          (map scale-vector (vector3 vector) basis)))

(defn- compose-placement [parent relative]
  (if-not parent
    relative
    (let [basis (placement-basis parent)]
      {:location (v+ (:location parent)
                     (transform-vector basis (:location relative)))
       :axis (normalize (transform-vector basis (:axis relative)) [0.0 0.0 1.0])
       :ref-direction
       (normalize (transform-vector basis (:ref-direction relative)) [1.0 0.0 0.0])})))

(defn local-placement [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifclocalplacement (:type entity))
      (let [parent (some->> (get-in entity [:args 0]) (local-placement table))
            relative (axis-placement table (get-in entity [:args 1]))]
        (compose-placement parent relative)))))

(defn- polyline [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifcpolyline (:type entity))
      (mapv #(coordinates table %) (list-values (get-in entity [:args 0]))))))

(declare curve)

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
      (let [profile-curve (curve table (get-in entity [:args 2]))]
        (cond-> {:kind :arbitrary-closed :profile-type (get-in entity [:args 0])
                 :name (get-in entity [:args 1])}
          (= :polyline (:kind profile-curve)) (assoc :points (:points profile-curve))
          (not= :polyline (:kind profile-curve)) (assoc :curve profile-curve)))
      :ifcarbitraryprofiledefwithvoids
      {:kind :arbitrary-with-voids :profile-type (get-in entity [:args 0])
       :name (get-in entity [:args 1])
       :outer-curve (curve table (get-in entity [:args 2]))
       :inner-curves (mapv #(curve table %)
                           (list-values (get-in entity [:args 3])))}
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

(defn- surface-appearance [table style-ref]
  (let [style (referenced table style-ref)
        style (if (= :ifcpresentationstyleassignment (:type style))
                (referenced table (first (list-values (get-in style [:args 0]))))
                style)]
    (when (= :ifcsurfacestyle (:type style))
      (let [rendering (some #(let [candidate (referenced table %)]
                               (when (#{:ifcsurfacestylerendering
                                        :ifcsurfacestyleshading} (:type candidate))
                                 candidate))
                            (list-values (get-in style [:args 2])))
            colour (referenced table (get-in rendering [:args 0]))]
        (when (and rendering (= :ifccolourrgb (:type colour)))
          {:name (get-in style [:args 0]) :side (get-in style [:args 1])
           :color-name (get-in colour [:args 0])
           :surface-color [(get-in colour [:args 1]) (get-in colour [:args 2])
                           (get-in colour [:args 3])]
           :transparency (if (= :ifcsurfacestylerendering (:type rendering))
                           (get-in rendering [:args 1]) 0.0)
           :reflectance-method
           (when (= :ifcsurfacestylerendering (:type rendering))
             (get-in rendering [:args 8]))})))))

(defn- presentation-indexes [table entities]
  (let [appearance
        (into {}
              (keep (fn [styled]
                      (when-let [value
                                 (some #(surface-appearance table %)
                                       (list-values (get-in styled [:args 1])))]
                        [(ref-id (get-in styled [:args 0])) value])))
              (filter #(= :ifcstyleditem (:type %)) entities))
        layers
        (reduce (fn [result layer]
                  (let [value {:name (get-in layer [:args 0])
                               :description (get-in layer [:args 1])
                               :identifier (get-in layer [:args 3])}]
                    (reduce #(update %1 (ref-id %2) (fnil conj []) value)
                            result (list-values (get-in layer [:args 2])))))
                {} (filter #(= :ifcpresentationlayerassignment (:type %)) entities))]
    {:appearance appearance :layers layers}))

(defn- product-presentation [table representation-ref indexes]
  (let [items (representation-items table representation-ref)]
    {:appearance (some #(get-in indexes [:appearance (ref-id %)]) items)
     :presentation-layers
     (vec (distinct (mapcat #(get-in indexes [:layers (ref-id %)] []) items)))}))

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

(defn- vertex-point [table ref]
  (let [entity (referenced table ref)]
    (when (= :ifcvertexpoint (:type entity))
      (coordinates table (get-in entity [:args 0])))))

(declare curve point-list)

(defn- indexed-segment [value]
  (when (and (vector? value) (= :typed (first value)))
    {:kind (if (= :ifcarcindex (second value)) :arc :line)
     :indices (mapv long (list-values (nth value 2)))}))

(defn- composite-segment [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifccompositecurvesegment (:type entity))
      {:transition (get-in entity [:args 0])
       :same-sense (get-in entity [:args 1])
       :parent-curve (curve table (get-in entity [:args 2]))})))

(defn- curve [table ref]
  (let [entity (referenced table ref)]
    (case (:type entity)
      :ifcpolyline {:kind :polyline
                    :points (mapv #(coordinates table %)
                                  (list-values (get-in entity [:args 0])))}
      :ifcindexedpolycurve
      {:kind :indexed-polycurve
       :points (point-list table (get-in entity [:args 0]))
       :segments (when-not (= :$ (get-in entity [:args 1]))
                   (vec (keep indexed-segment
                              (list-values (get-in entity [:args 1])))))
       :self-intersect (get-in entity [:args 2])}
      :ifccompositecurve
      {:kind :composite-curve
       :segments (vec (keep #(composite-segment table %)
                            (list-values (get-in entity [:args 0]))))
       :self-intersect (get-in entity [:args 1])}
      :ifcline (let [vector-entity (referenced table (get-in entity [:args 1]))]
                 {:kind :line :origin (coordinates table (get-in entity [:args 0]))
                  :direction (direction table (get-in vector-entity [:args 0]))
                  :magnitude (get-in vector-entity [:args 1])})
      :ifccircle {:kind :circle :position (axis-placement table (get-in entity [:args 0]))
                  :radius (get-in entity [:args 1])}
      :ifcellipse {:kind :ellipse :position (axis-placement table (get-in entity [:args 0]))
                   :semi-axis1 (get-in entity [:args 1]) :semi-axis2 (get-in entity [:args 2])}
      (:ifcbsplinecurve :ifcbsplinecurvewithknots :ifcrationalbsplinecurvewithknots)
      (cond-> {:kind :b-spline-curve
               :degree (get-in entity [:args 0])
               :control-points (mapv #(coordinates table %)
                                     (list-values (get-in entity [:args 1])))
               :curve-form (get-in entity [:args 2])
               :closed (get-in entity [:args 3])
               :self-intersect (get-in entity [:args 4])}
        (not= :ifcbsplinecurve (:type entity))
        (assoc :multiplicities (mapv long (list-values (get-in entity [:args 5])))
               :knots (vec (list-values (get-in entity [:args 6])))
               :knot-spec (get-in entity [:args 7]))
        (= :ifcrationalbsplinecurvewithknots (:type entity))
        (assoc :weights (vec (list-values (get-in entity [:args 8])))))
      nil)))

(defn- oriented-edge [table ref]
  (let [entity (referenced table ref)
        edge (referenced table (get-in entity [:args 2]))]
    (when (and (= :ifcorientededge (:type entity)) (= :ifcedgecurve (:type edge)))
      (let [orientation (get-in entity [:args 3])
            edge-start (vertex-point table (get-in edge [:args 0]))
            edge-end (vertex-point table (get-in edge [:args 1]))]
        {:kind :edge-curve :orientation orientation
         :start (if (false? orientation) edge-end edge-start)
         :end (if (false? orientation) edge-start edge-end)
         :curve (curve table (get-in edge [:args 2]))
         :same-sense (get-in edge [:args 3])}))))

(defn- dot [a b] (reduce + (map * a b)))
(defn- subtract [a b] (mapv - a b))
(defn- cross3 [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx))])
(defn- length3 [v] (#?(:clj Math/sqrt :cljs js/Math.sqrt) (dot v v)))
(defn- normalize3 [v]
  (let [length (length3 v)] (if (pos? length) (mapv #(/ % length) v) [0.0 0.0 0.0])))

(defn- sampled-edge-points [edge]
  (let [curve (:curve edge)]
    (cond
      (#{:circle :ellipse} (:kind curve))
      (let [position (:position curve) origin (or (:location position) [0.0 0.0 0.0])
            z-axis (normalize3 (or (:axis position) [0.0 0.0 1.0]))
            x-axis (normalize3 (or (:ref-direction position) [1.0 0.0 0.0]))
            y-axis (normalize3 (cross3 z-axis x-axis))
            angle (fn [point]
                    (let [delta (subtract point origin)]
                      (#?(:clj Math/atan2 :cljs js/Math.atan2)
                       (dot delta y-axis) (dot delta x-axis))))
            start-angle (angle (:start edge)) end-angle (angle (:end edge))
            increasing? (= (false? (:orientation edge)) (false? (:same-sense edge)))
            tau (* 2.0 #?(:clj Math/PI :cljs js/Math.PI))
            raw-delta (- end-angle start-angle)
            delta (cond
                    (and increasing? (<= raw-delta 0.0)) (+ raw-delta tau)
                    (and (not increasing?) (>= raw-delta 0.0)) (- raw-delta tau)
                    :else raw-delta)
            delta (if (< (#?(:clj Math/abs :cljs js/Math.abs) delta) 1.0e-9)
                    (if increasing? tau (- tau)) delta)
            segments (max 1 (long (#?(:clj Math/ceil :cljs js/Math.ceil)
                                    (/ (#?(:clj Math/abs :cljs js/Math.abs) delta)
                                       (/ #?(:clj Math/PI :cljs js/Math.PI) 12.0)))))
            rx (if (= :circle (:kind curve)) (:radius curve) (:semi-axis1 curve))
            ry (if (= :circle (:kind curve)) (:radius curve) (:semi-axis2 curve))]
        (mapv (fn [i]
                (let [theta (+ start-angle (* delta (/ i segments)))]
                  (mapv + origin
                        (mapv #(* rx (#?(:clj Math/cos :cljs js/Math.cos) theta) %) x-axis)
                        (mapv #(* ry (#?(:clj Math/sin :cljs js/Math.sin) theta) %) y-axis))))
              (range segments)))

      (= :b-spline-curve (:kind curve))
      (let [control-count (count (:control-points curve))
            knots (spline/expand-knots (:knots curve) (:multiplicities curve)
                                       (:degree curve) control-count)
            curve (assoc curve :knots knots)
            [domain-start domain-end] (spline/parameter-range (:degree curve) knots control-count)
            distance-squared (fn [a b] (reduce + (map (fn [x y] (let [d (- x y)] (* d d))) a b)))
            nearest-parameter
            (fn [point]
              (apply min-key
                     (fn [parameter]
                       (distance-squared point (spline/curve-point curve parameter)))
                     (map #(+ domain-start (* (- domain-end domain-start) (/ % 128.0)))
                          (range 129))))
            start (nearest-parameter (:start edge)) end (nearest-parameter (:end edge))
            increasing? (= (false? (:orientation edge)) (false? (:same-sense edge)))
            [start end] (if increasing? [start end] [end start])
            domain-size (- domain-end domain-start)
            end (if (and (:closed curve) (<= end start)) (+ end domain-size) end)
            segments (max 8 (* 4 (dec control-count)))
            wrap (fn [parameter]
                   (if (> parameter domain-end) (- parameter domain-size) parameter))]
        (mapv (fn [i]
                (spline/curve-point curve
                                    (wrap (+ start (* (- end start) (/ i segments))))))
              (range segments)))

      :else [(:start edge)])))

(defn- loop-data [table loop-entity]
  (case (:type loop-entity)
    :ifcpolyloop
    {:loop-kind :polyloop
     :points (mapv #(coordinates table %) (list-values (get-in loop-entity [:args 0])))}
    :ifcedgeloop
    (let [edges (vec (keep #(oriented-edge table %)
                           (list-values (get-in loop-entity [:args 0]))))]
      {:loop-kind :edge-loop :edges edges
       :points (vec (mapcat sampled-edge-points edges))})
    nil))

(defn- face-bound [table ref]
  (let [entity (referenced table ref)
        loop-entity (referenced table (get-in entity [:args 0]))
        loop (loop-data table loop-entity)]
    (when (and (#{:ifcfacebound :ifcfaceouterbound} (:type entity)) loop)
      (merge {:kind (if (= :ifcfaceouterbound (:type entity)) :outer :inner)
              :orientation (get-in entity [:args 1])}
             loop))))

(defn- faceted-brep [table item]
  (let [shell (referenced table (get-in item [:args 0]))]
    (when (= :ifcclosedshell (:type shell))
      {:kind :faceted-brep
       :faces (mapv (fn [face-ref]
                      (let [face (referenced table face-ref)]
                        {:bounds (vec (keep #(face-bound table %)
                                            (list-values (get-in face [:args 0]))))}))
                    (list-values (get-in shell [:args 0])))})))

(defn- surface [table ref]
  (let [entity (referenced table ref)]
    (case (:type entity)
      :ifcplane {:kind :plane :position (axis-placement table (get-in entity [:args 0]))}
      :ifccylindricalsurface
      {:kind :cylinder :position (axis-placement table (get-in entity [:args 0]))
       :radius (get-in entity [:args 1])}
      :ifcsphericalsurface
      {:kind :sphere :position (axis-placement table (get-in entity [:args 0]))
       :radius (get-in entity [:args 1])}
      :ifctoroidalsurface
      {:kind :torus :position (axis-placement table (get-in entity [:args 0]))
       :major-radius (get-in entity [:args 1]) :minor-radius (get-in entity [:args 2])}
      (:ifcbsplinesurface :ifcbsplinesurfacewithknots :ifcrationalbsplinesurfacewithknots)
      (cond-> {:kind :b-spline-surface
               :u-degree (get-in entity [:args 0]) :v-degree (get-in entity [:args 1])
               :control-points
               (mapv (fn [row]
                       (mapv #(coordinates table %) (list-values row)))
                     (list-values (get-in entity [:args 2])))
               :surface-form (get-in entity [:args 3])
               :u-closed (get-in entity [:args 4]) :v-closed (get-in entity [:args 5])
               :self-intersect (get-in entity [:args 6])}
        (not= :ifcbsplinesurface (:type entity))
        (assoc :u-multiplicities (mapv long (list-values (get-in entity [:args 7])))
               :v-multiplicities (mapv long (list-values (get-in entity [:args 8])))
               :u-knots (vec (list-values (get-in entity [:args 9])))
               :v-knots (vec (list-values (get-in entity [:args 10])))
               :knot-spec (get-in entity [:args 11]))
        (= :ifcrationalbsplinesurfacewithknots (:type entity))
        (assoc :weights (mapv (comp vec list-values)
                              (list-values (get-in entity [:args 12])))))
      nil)))

(defn- advanced-brep [table item]
  (let [shell (referenced table (get-in item [:args 0]))]
    (when (= :ifcclosedshell (:type shell))
      {:kind :advanced-brep
       :faces (mapv (fn [face-ref]
                      (let [face (referenced table face-ref)]
                        {:bounds (vec (keep #(face-bound table %)
                                            (list-values (get-in face [:args 0]))))
                         :surface (surface table (get-in face [:args 1]))
                         :same-sense (get-in face [:args 2])}))
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
      :ifcrevolvedareasolid
      {:kind :revolved-area-solid
       :profile (profile table (get-in item [:args 0]))
       :position (axis-placement table (get-in item [:args 1]))
       :axis (axis1-placement table (get-in item [:args 2]))
       :angle (get-in item [:args 3])}
      :ifcfixedreferencesweptareasolid
      {:kind :fixed-reference-swept-area-solid
       :profile (profile table (get-in item [:args 0]))
       :position (axis-placement table (get-in item [:args 1]))
       :directrix (curve table (get-in item [:args 2]))
       :start-param (get-in item [:args 3]) :end-param (get-in item [:args 4])
       :fixed-reference (direction table (get-in item [:args 5]))}
      :ifcsurfacecurvesweptareasolid
      {:kind :surface-curve-swept-area-solid
       :profile (profile table (get-in item [:args 0]))
       :position (axis-placement table (get-in item [:args 1]))
       :directrix (curve table (get-in item [:args 2]))
       :start-param (get-in item [:args 3]) :end-param (get-in item [:args 4])
       :reference-surface (surface table (get-in item [:args 5]))}
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
      :ifcadvancedbrep (advanced-brep table item)
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

(def product-types (set (keys entity-kind-by-type)))

(defn- typed-value [value]
  (if (and (vector? value) (= :typed (first value))) (nth value 2) value))

(def unit-prefix-scale
  {:exa 1.0e18 :peta 1.0e15 :tera 1.0e12 :giga 1.0e9 :mega 1.0e6
   :kilo 1.0e3 :hecto 1.0e2 :deca 1.0e1 :deci 1.0e-1 :centi 1.0e-2
   :milli 1.0e-3 :micro 1.0e-6 :nano 1.0e-9 :pico 1.0e-12
   :femto 1.0e-15 :atto 1.0e-18})

(declare typed-value-type)

(defn- unit [table ref]
  (let [entity (referenced table ref)]
    (case (:type entity)
      :ifcsiunit
      (let [unit-type (get-in entity [:args 1]) prefix (get-in entity [:args 2])]
        {:kind :si :type unit-type :prefix (when-not (= :$ prefix) prefix)
         :name (get-in entity [:args 3]) :scale (get unit-prefix-scale prefix 1.0)})
      (:ifcconversionbasedunit :ifcconversionbasedunitwithoffset)
      (let [dimensions (referenced table (get-in entity [:args 0]))
            factor (referenced table (get-in entity [:args 3]))
            factor-value (get-in factor [:args 0])]
        (cond->
         {:kind (if (= :ifcconversionbasedunitwithoffset (:type entity))
                  :conversion-based-with-offset :conversion-based)
          :type (get-in entity [:args 1]) :name (get-in entity [:args 2])
          :dimensions (when (= :ifcdimensionalexponents (:type dimensions))
                        (vec (:args dimensions)))
          :conversion-factor
          {:value (typed-value factor-value)
           :value-type (typed-value-type factor-value)
           :unit (unit table (get-in factor [:args 1]))}}
          (= :ifcconversionbasedunitwithoffset (:type entity))
          (assoc :conversion-offset (get-in entity [:args 4]))))

      :ifcderivedunit
      {:kind :derived :type (get-in entity [:args 1])
       :user-defined-type (when-not (= :$ (get-in entity [:args 2]))
                            (get-in entity [:args 2]))
       :elements
       (mapv (fn [element-ref]
               (let [element (referenced table element-ref)]
                 {:unit (unit table (get-in element [:args 0]))
                  :exponent (get-in element [:args 1])}))
             (list-values (get-in entity [:args 0])))}

      :ifcmonetaryunit
      {:kind :monetary :type :monetaryunit :currency (get-in entity [:args 0])}

      :ifccontextdependentunit
      (let [dimensions (referenced table (get-in entity [:args 0]))]
        {:kind :context-dependent :type (get-in entity [:args 1])
         :name (get-in entity [:args 2])
         :dimensions (when (= :ifcdimensionalexponents (:type dimensions))
                       (vec (:args dimensions)))})
      nil)))

(defn- project-units [table project-entity]
  (when-let [assignment (referenced table (get-in project-entity [:args 8]))]
    (when (= :ifcunitassignment (:type assignment))
      (into {} (keep (fn [ref] (when-let [u (unit table ref)] [(:type u) u])))
            (list-values (get-in assignment [:args 0]))))))

(defn- typed-value-type [value]
  (when (and (vector? value) (= :typed (first value))) (second value)))

(defn- property-value [table entity]
  (let [base {:name (get-in entity [:args 0])
              :description (get-in entity [:args 1])}]
    (case (:type entity)
      :ifcpropertysinglevalue
      (let [nominal (get-in entity [:args 2])]
        (assoc base :kind :single :value (typed-value nominal)
               :value-type (typed-value-type nominal)
               :unit (unit table (get-in entity [:args 3]))))

      :ifcpropertyenumeratedvalue
      (let [values (list-values (get-in entity [:args 2]))
            enumeration (referenced table (get-in entity [:args 3]))
            allowed (when enumeration (list-values (get-in enumeration [:args 1])))
            sample (or (first values) (first allowed))]
        (assoc base :kind :enumerated :values (mapv typed-value values)
               :value-type (typed-value-type sample)
               :enumeration
               (when enumeration
                 {:name (get-in enumeration [:args 0])
                  :values (mapv typed-value allowed)
                  :unit (unit table (get-in enumeration [:args 2]))})))

      :ifcpropertyboundedvalue
      (let [upper (get-in entity [:args 2]) lower (get-in entity [:args 3])
            set-point (get-in entity [:args 5])
            sample (first (remove #(= :$ %) [upper lower set-point]))]
        (assoc base :kind :bounded
               :upper (when-not (= :$ upper) (typed-value upper))
               :lower (when-not (= :$ lower) (typed-value lower))
               :set-point (when-not (= :$ set-point) (typed-value set-point))
               :value-type (typed-value-type sample)
               :unit (unit table (get-in entity [:args 4]))))

      :ifcpropertylistvalue
      (let [values (list-values (get-in entity [:args 2]))]
        (assoc base :kind :list :values (mapv typed-value values)
               :value-type (typed-value-type (first values))
               :unit (unit table (get-in entity [:args 3]))))
      nil)))

(defn- property-sets [table entities]
  (let [sets (into {}
                   (map (fn [entity]
                          [(:id entity)
                           {:id (:id entity) :global-id (get-in entity [:args 0])
                            :name (get-in entity [:args 2])
                            :properties
                            (into {} (keep (fn [ref]
                                             (when-let [p (property-value table
                                                                          (referenced table ref))]
                                               [(:name p) p])))
                                  (list-values (get-in entity [:args 4])))}])
                        (filter #(= :ifcpropertyset (:type %)) entities)))]
    (reduce (fn [by-object relation]
              (let [pset (get sets (ref-id (get-in relation [:args 5])))]
                (if (and pset (:name pset))
                  (reduce #(assoc-in %1 [(ref-id %2) (:name pset)] pset)
                          by-object (list-values (get-in relation [:args 4])))
                  by-object)))
            {} (filter #(= :ifcreldefinesbyproperties (:type %)) entities))))

(def quantity-kinds
  {:ifcquantitylength :length :ifcquantityarea :area
   :ifcquantityvolume :volume :ifcquantitycount :count
   :ifcquantityweight :weight :ifcquantitytime :time
   :ifcquantitynumber :number})

(defn- quantity [table ref]
  (let [entity (referenced table ref)]
    (when-let [kind (get quantity-kinds (:type entity))]
      {:name (get-in entity [:args 0])
       :description (get-in entity [:args 1])
       :kind kind
       :unit (unit table (get-in entity [:args 2]))
       :value (get-in entity [:args 3])
       :formula (get-in entity [:args 4])})))

(defn- quantity-sets [table entities]
  (let [sets
        (into {}
              (map (fn [entity]
                     [(:id entity)
                      {:id (:id entity) :global-id (get-in entity [:args 0])
                       :name (get-in entity [:args 2])
                       :description (get-in entity [:args 3])
                       :method-of-measurement (get-in entity [:args 4])
                       :quantities
                       (into {}
                             (keep (fn [ref]
                                     (when-let [value (quantity table ref)]
                                       [(:name value) value])))
                             (list-values (get-in entity [:args 5])))}])
                   (filter #(= :ifcelementquantity (:type %)) entities)))]
    (reduce (fn [by-object relation]
              (let [qset (get sets (ref-id (get-in relation [:args 5])))]
                (if (and qset (:name qset))
                  (reduce #(assoc-in %1 [(ref-id %2) (:name qset)] qset)
                          by-object (list-values (get-in relation [:args 4])))
                  by-object)))
            {} (filter #(= :ifcreldefinesbyproperties (:type %)) entities))))

(defn- material-definition [table ref]
  (when-let [entity (referenced table ref)]
    (case (:type entity)
      :ifcmaterial
      {:kind :material :name (get-in entity [:args 0])
       :description (get-in entity [:args 1]) :category (get-in entity [:args 2])}

      :ifcmateriallayerset
      {:kind :layer-set :name (get-in entity [:args 1])
       :description (get-in entity [:args 2])
       :layers
       (mapv (fn [layer-ref]
               (let [layer (referenced table layer-ref)]
                 {:material (material-definition table (get-in layer [:args 0]))
                  :thickness (get-in layer [:args 1])
                  :ventilated (let [value (get-in layer [:args 2])]
                                (when-not (= :$ value) value))
                  :name (get-in layer [:args 3])
                  :description (get-in layer [:args 4])
                  :category (get-in layer [:args 5])
                  :priority (get-in layer [:args 6])}))
             (list-values (get-in entity [:args 0])))}

      :ifcmateriallayersetusage
      {:kind :layer-set-usage
       :layer-set (material-definition table (get-in entity [:args 0]))
       :direction (get-in entity [:args 1])
       :direction-sense (get-in entity [:args 2])
       :offset (get-in entity [:args 3])
       :reference-extent (get-in entity [:args 4])}
      nil)))

(defn- materials-by-object [table entities]
  (reduce (fn [by-object relation]
            (let [material (material-definition table (get-in relation [:args 5]))]
              (reduce #(assoc %1 (ref-id %2) material) by-object
                      (list-values (get-in relation [:args 4])))))
          {} (filter #(= :ifcrelassociatesmaterial (:type %)) entities)))

(defn- classification-source [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifcclassification (:type entity))
      {:source (get-in entity [:args 0]) :edition (get-in entity [:args 1])
       :edition-date (get-in entity [:args 2]) :name (get-in entity [:args 3])
       :description (get-in entity [:args 4]) :specification (get-in entity [:args 5])
       :reference-tokens (when-not (= :$ (get-in entity [:args 6]))
                           (vec (list-values (get-in entity [:args 6]))))})))

(defn- classification-reference [table ref]
  (when-let [entity (referenced table ref)]
    (when (= :ifcclassificationreference (:type entity))
      {:location (get-in entity [:args 0])
       :identification (get-in entity [:args 1])
       :name (get-in entity [:args 2])
       :source (classification-source table (get-in entity [:args 3]))
       :description (get-in entity [:args 4])
       :sort (get-in entity [:args 5])})))

(defn- classifications-by-object [table entities]
  (reduce (fn [by-object relation]
            (if-let [classification
                     (classification-reference table (get-in relation [:args 5]))]
              (reduce #(update %1 (ref-id %2) (fnil conj []) classification)
                      by-object (list-values (get-in relation [:args 4])))
              by-object))
          {} (filter #(= :ifcrelassociatesclassification (:type %)) entities)))

(defn- opening-relations [entities]
  {:voids (into {} (map (fn [relation]
                          [(ref-id (get-in relation [:args 5]))
                           (ref-id (get-in relation [:args 4]))])
                        (filter #(= :ifcrelvoidselement (:type %)) entities)))
   :fills (into {} (map (fn [relation]
                          [(ref-id (get-in relation [:args 4]))
                           (ref-id (get-in relation [:args 5]))])
                        (filter #(= :ifcrelfillselement (:type %)) entities)))})

(defn- type-relations [table entities psets-by-id materials classifications]
  (reduce (fn [by-object relation]
            (let [type-entity (referenced table (get-in relation [:args 5]))
                  type-object {:id (:id type-entity) :ifc/type (:type type-entity)
                               :global-id (get-in type-entity [:args 0])
                               :name (get-in type-entity [:args 2])
                               :property-sets
                               (into {}
                                     (keep (fn [ref]
                                             (when-let [pset (get psets-by-id (ref-id ref))]
                                               [(:name pset) pset])))
                                     (list-values (get-in type-entity [:args 5])))
                               :material (get materials (:id type-entity))
                               :classifications (get classifications (:id type-entity) [])
                               :representation-maps
                               (mapv (fn [map-ref]
                                       (let [representation-map (referenced table map-ref)
                                             shape-ref (get-in representation-map [:args 1])
                                             shape (referenced table shape-ref)]
                                         {:mapping-origin
                                          (axis-placement table
                                                          (get-in representation-map [:args 0]))
                                          :identifier (get-in shape [:args 1])
                                          :representation-type (get-in shape [:args 2])
                                          :geometry
                                          (geometry-items table (shape-items table shape-ref))}))
                                     (list-values (get-in type-entity [:args 6])))
                               :element-type (get-in type-entity [:args 8])
                               :predefined-type (get-in type-entity [:args 9])}]
              (reduce #(assoc %1 (ref-id %2) type-object) by-object
                      (list-values (get-in relation [:args 4])))))
          {} (filter #(= :ifcreldefinesbytype (:type %)) entities)))

(defn- product [table entity presentation]
  (merge
   {:id (:id entity) :global-id (get-in entity [:args 0])
    :ifc/entity-type (:type entity)
    :name (or (get-in entity [:args 2]) (name (:type entity)))
    :kind (get entity-kind-by-type (:type entity) :other)
    :placement (local-placement table (get-in entity [:args 5]))
    :geometry (product-geometry table (get-in entity [:args 6]))}
   (product-presentation table (get-in entity [:args 6]) presentation)))

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

(defn- groups [table entities]
  (let [members
        (reduce (fn [result relation]
                  (let [group-id (ref-id (get-in relation [:args 6]))]
                    (assoc result group-id
                           (mapv ref-id (list-values (get-in relation [:args 4]))))))
                {} (filter #(= :ifcrelassignstogroup (:type %)) entities))]
    (mapv (fn [entity]
            (let [member-entities (keep table (get members (:id entity)))]
              (cond->
               {:id (:id entity) :global-id (get-in entity [:args 0])
                :kind (case (:type entity)
                        :ifczone :zone :ifcdistributionsystem :distribution-system :system)
                :ifc/type (:type entity) :name (get-in entity [:args 2])
                :description (get-in entity [:args 3])
                :object-type (get-in entity [:args 4])
                :member-global-ids (mapv #(get-in % [:args 0]) member-entities)}
                (#{:ifczone :ifcdistributionsystem} (:type entity))
                (assoc :long-name (get-in entity [:args 5]))
                (= :ifcdistributionsystem (:type entity))
                (assoc :predefined-type (get-in entity [:args 6])))))
          (filter #(contains? group-types (:type %)) entities))))

(defn- ports [table entities]
  (let [owner-by-port
        (into {} (mapcat (fn [relation]
                           (let [owner (ref-id (get-in relation [:args 4]))]
                             (map (fn [port] [(ref-id port) owner])
                                  (list-values (get-in relation [:args 5])))))
                         (filter #(= :ifcrelnests (:type %)) entities)))]
    {:by-owner
     (group-by #(get owner-by-port (:id %))
               (mapv (fn [entity]
                       {:id (:id entity) :global-id (get-in entity [:args 0])
                        :name (get-in entity [:args 2])
                        :description (get-in entity [:args 3])
                        :object-type (get-in entity [:args 4])
                        :placement (local-placement table (get-in entity [:args 5]))
                        :tag (get-in entity [:args 7])
                        :flow-direction (get-in entity [:args 8])
                        :predefined-type (get-in entity [:args 9])
                        :system-type (get-in entity [:args 10])})
                     (filter #(= :ifcdistributionport (:type %)) entities)))
     :by-id (into {} (map (juxt :id identity))
                  (map (fn [entity]
                         {:id (:id entity) :global-id (get-in entity [:args 0])})
                       (filter #(= :ifcdistributionport (:type %)) entities)))}))

(defn- port-connections [table entities port-by-id]
  (mapv (fn [relation]
          (let [relating (get port-by-id (ref-id (get-in relation [:args 4])))
                related (get port-by-id (ref-id (get-in relation [:args 5])))
                realizing (referenced table (get-in relation [:args 6]))]
            {:id (:id relation) :global-id (get-in relation [:args 0])
             :name (get-in relation [:args 2]) :description (get-in relation [:args 3])
             :relating-port-global-id (:global-id relating)
             :related-port-global-id (:global-id related)
             :realizing-element-global-id (get-in realizing [:args 0])}))
        (filter #(= :ifcrelconnectsports (:type %)) entities)))

(defn- georeference [table entities]
  (when-let [conversion (first (filter #(#{:ifcmapconversion :ifcmapconversionscaled}
                                           (:type %)) entities))]
    (let [context (referenced table (get-in conversion [:args 0]))
          crs (referenced table (get-in conversion [:args 1]))]
      (cond->
       {:projected-crs {:name (get-in crs [:args 0]) :description (get-in crs [:args 1])
                        :geodetic-datum (get-in crs [:args 2])
                        :vertical-datum (get-in crs [:args 3])
                        :map-projection (get-in crs [:args 4]) :map-zone (get-in crs [:args 5])
                        :map-unit (when-not (= :$ (get-in crs [:args 6]))
                                    (unit table (get-in crs [:args 6])))}
       :world-origin (get-in (axis-placement table (get-in context [:args 4])) [:location])
       :true-north (direction table (get-in context [:args 5]))
       :eastings (get-in conversion [:args 2]) :northings (get-in conversion [:args 3])
       :orthogonal-height (get-in conversion [:args 4])
       :x-axis-abscissa (get-in conversion [:args 5])
        :x-axis-ordinate (get-in conversion [:args 6]) :scale (get-in conversion [:args 7])}
        (= :ifcmapconversionscaled (:type conversion))
        (assoc :map-conversion-kind :scaled
               :factor-x (get-in conversion [:args 8])
               :factor-y (get-in conversion [:args 9])
               :factor-z (get-in conversion [:args 10]))))))

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
                          (get-in entity [:args 11])))

      (= :ifcspace (:type entity))
      (assoc :long-name (when-not (= :$ (get-in entity [:args 7]))
                          (get-in entity [:args 7]))
             :composition-type (get-in entity [:args 8])
             :predefined-type (get-in entity [:args 9])
             :elevation-with-flooring
             (when-not (= :$ (get-in entity [:args 10]))
               (get-in entity [:args 10]))))))

(defn read-external-spf [text]
  (let [parsed (part21/parse-file text)
        entities (:part21/entities parsed)
        table (:part21/entity-by-id parsed)
        children (aggregates entities)
        contained-by (containment entities)
        project-entity (first (filter #(= :ifcproject (:type %)) entities))
        psets-by-object (property-sets table entities)
        qsets-by-object (quantity-sets table entities)
        materials (materials-by-object table entities)
        classifications (classifications-by-object table entities)
        psets-by-id
        (into {}
              (map (juxt :id identity)
                   (map (fn [entity]
                          (let [properties
                                (into {}
                                      (keep (fn [ref]
                                              (when-let [p
                                                         (property-value
                                                          table (referenced table ref))]
                                                [(:name p) p])))
                                      (list-values (get-in entity [:args 4])))]
                            {:id (:id entity) :global-id (get-in entity [:args 0])
                             :name (get-in entity [:args 2])
                             :description (get-in entity [:args 3])
                             :properties properties}))
                        (filter #(= :ifcpropertyset (:type %)) entities))))
        types-by-object (type-relations table entities psets-by-id
                                        materials classifications)
        presentation (presentation-indexes table entities)
        port-data (ports table entities)
        {:keys [voids fills]} (opening-relations entities)
        raw-products (into {} (map (fn [entity] [(:id entity) (product table entity presentation)]))
                           (filter #(contains? product-types (:type %)) entities))
        openings-by-host (group-by voids (keys voids))
        products (->> raw-products
                      (remove (fn [[_ value]] (= :opening (:kind value))))
                      (mapv (fn [[id value]]
                              (assoc value :container-id (get contained-by id)
                                     :type-object (get types-by-object id)
                                     :property-sets (get psets-by-object id {})
                                     :quantity-sets (get qsets-by-object id {})
                                     :material (get materials id)
                                     :classifications (get classifications id [])
                                     :ports (vec (get-in port-data [:by-owner id] []))
                                     :openings
                                     (mapv (fn [opening-id]
                                             (assoc (get raw-products opening-id)
                                                    :filled-by (get fills opening-id)
                                                    :filled-by-global-id
                                                    (:global-id (get raw-products (get fills opening-id)))
                                                    :property-sets (get psets-by-object opening-id {})
                                                    :quantity-sets (get qsets-by-object opening-id {})
                                                    :material (get materials opening-id)
                                                    :classifications
                                                    (get classifications opening-id [])))
                                           (get openings-by-host id))))))
        document
        {:ifc/schema (:part21/schema parsed) :ifc/contract-version contract-version
         :ifc/project (when project-entity (spatial-node table children (:id project-entity)))
         :ifc/units (project-units table project-entity)
         :ifc/georeference (georeference table entities)
         :ifc/elements products
         :ifc/groups (groups table entities)
         :ifc/connections (port-connections table entities (:by-id port-data))
         :ifc/source :external-spf
         :ifc/raw-spf text
         :ifc/raw-entities entities
         :ifc/raw-entity-count (count entities)
         :ifc/raw-type-frequencies (frequencies (map :type entities))}]
    (assoc document :ifc/import-fingerprint (semantic-fingerprint document))))

(defn read-document [text]
  (try
    (let [model (read-spf text)]
      (exchange-document {:project {:id (:id model) :name (:name model) :model model}
                          :elements []}))
    (catch #?(:clj Exception :cljs :default) _
      (read-external-spf text))))

(defn- spatial-container-index [project]
  (letfn [(visit [node]
            (when node
              (into {(:id node) (:global-id node)}
                    (mapcat visit (:children node)))))]
    (or (visit project) {})))

(defn- portable-semantic-value [value]
  (walk/postwalk
   (fn [node]
     (cond
       (and (map? node)
            (seq node)
            (set/subset? (set (keys node)) #{:location :axis :ref-direction})
            (every? #(zero? (double %)) (:location node))
            (or (nil? (:axis node)) (= [0.0 0.0 1.0] (:axis node)))
            (or (nil? (:ref-direction node))
                (= (if (= 2 (count (:location node))) [1.0 0.0] [1.0 0.0 0.0])
                   (:ref-direction node)))) nil
       (and (map? node) (contains? #{:single :enumerated :bounded :list} (:kind node))
            (nil? (:value-type node)))
       (let [sample (or (:value node) (first (:values node)) (:lower node)
                        (:upper node) (:set-point node))]
         (assoc node :value-type
                (cond (boolean? sample) :ifcboolean
                      (integer? sample) :ifcinteger
                      (number? sample) :ifcreal
                      :else :ifclabel)))
       (map? node) (dissoc node :id :container-id :filled-by)
       (= :$ node) nil
       (= :notdefined node) nil
       :else node))
   value))

(defn semantic-fingerprint
  "Stable external-exchange meaning, excluding regenerated STEP entity ids.

  GlobalIds, hierarchy, placement, geometry, types, properties, openings,
  units, and georeferencing remain part of the comparison."
  [document]
  (let [containers (spatial-container-index (:ifc/project document))
        elements (mapv (fn [element]
                         (portable-semantic-value
                          (-> element
                              (update :property-sets #(or % {}))
                              (update :openings #(vec (sort-by :global-id (or % []))))
                              (assoc :container-global-id
                                     (get containers (:container-id element))))))
                       (:ifc/elements document))]
    {:ifc/schema (:ifc/schema document)
     :ifc/project (portable-semantic-value (:ifc/project document))
     :ifc/units (portable-semantic-value (:ifc/units document))
     :ifc/georeference (portable-semantic-value (:ifc/georeference document))
     :ifc/groups (vec (sort-by :global-id
                               (map portable-semantic-value (:ifc/groups document))))
     :ifc/connections (vec (sort-by :global-id
                                    (map portable-semantic-value
                                         (:ifc/connections document))))
     :ifc/elements (vec (sort-by (juxt :global-id :name) elements))}))

(defn roundtrip-report
  "Parse an external SPF, rewrite standard IFC, and compare exchange meaning."
  [text]
  (let [before (read-document text)
        passthrough (write-spf before)
        output (rewrite-spf before)
        after (read-document output)
        expected (semantic-fingerprint before)
        actual (semantic-fingerprint after)]
    {:roundtrip/lossless? (= expected actual)
     :roundtrip/passthrough-byte-identical? (= text passthrough)
     :roundtrip/export-mode :standard-rewrite
     :roundtrip/input-schema (:ifc/schema before)
     :roundtrip/output-schema (:ifc/schema after)
     :roundtrip/input-elements (count (:ifc/elements before))
     :roundtrip/output-elements (count (:ifc/elements after))
     :roundtrip/expected expected :roundtrip/actual actual
     :roundtrip/output output}))

(defn- generated-entity-types [document]
  (set (map second (standard-entities (dissoc document :ifc/raw-spf
                                               :ifc/import-fingerprint
                                               :ifc/raw-entities)))))

(defn- opaque-entity-index [document managed-types]
  (into (sorted-map)
        (keep (fn [{:keys [id type args]}]
                (when-not (contains? managed-types type)
                  [id {:type type :args args}])))
        (:ifc/raw-entities document)))

(defn hybrid-roundtrip-report
  "Apply an exchange-document edit, use provenance-aware hybrid export, and
  verify both mapped IFC meaning and untouched opaque/vendor STEP entities.

  `edit` is a pure document -> document function, suitable for real-file
  differential corpus tests. Entity ids and arguments of opaque entities must
  survive exactly; generated graph entities are compared semantically."
  [text edit]
  (let [before (read-document text)
        edited (edit before)
        managed-types (set/union (generated-entity-types before)
                                 (generated-entity-types edited))
        expected-opaque (opaque-entity-index before managed-types)
        output (write-spf edited)
        after (read-document output)
        actual-opaque (select-keys (opaque-entity-index after managed-types)
                                   (keys expected-opaque))
        expected (semantic-fingerprint edited)
        actual (semantic-fingerprint after)
        semantic-lossless? (= expected actual)
        opaque-lossless? (= expected-opaque actual-opaque)]
    {:roundtrip/lossless? (and semantic-lossless? opaque-lossless?)
     :roundtrip/semantic-lossless? semantic-lossless?
     :roundtrip/opaque-lossless? opaque-lossless?
     :roundtrip/export-mode :hybrid-edit
     :roundtrip/input-schema (:ifc/schema before)
     :roundtrip/output-schema (:ifc/schema after)
     :roundtrip/input-elements (count (:ifc/elements before))
     :roundtrip/output-elements (count (:ifc/elements after))
     :roundtrip/opaque-input-count (count expected-opaque)
     :roundtrip/opaque-output-count (count actual-opaque)
     :roundtrip/opaque-missing-ids
     (vec (remove (set (keys actual-opaque)) (keys expected-opaque)))
     :roundtrip/expected expected :roundtrip/actual actual
     :roundtrip/expected-opaque expected-opaque
     :roundtrip/actual-opaque actual-opaque
     :roundtrip/output output}))

(defn corpus-report
  "Run round-trip verification for `{label spf-text}` corpus entries."
  [entries]
  (let [files (into (sorted-map)
                    (map (fn [[label text]] [label (roundtrip-report text)]))
                    entries)]
    {:corpus/files files
     :corpus/file-count (count files)
     :corpus/input-elements (reduce + (map :roundtrip/input-elements (vals files)))
     :corpus/output-elements (reduce + (map :roundtrip/output-elements (vals files)))
     :corpus/lossless? (every? :roundtrip/lossless? (vals files))}))

(defn hybrid-corpus-report
  "Verify edited external IFC corpus entries.

  Each value is `{:text spf-text :edit document->document}`."
  [entries]
  (let [files (into (sorted-map)
                    (map (fn [[label {:keys [text edit]}]]
                           [label (hybrid-roundtrip-report text edit)]))
                    entries)]
    {:corpus/files files
     :corpus/file-count (count files)
     :corpus/input-elements (reduce + (map :roundtrip/input-elements (vals files)))
     :corpus/output-elements (reduce + (map :roundtrip/output-elements (vals files)))
     :corpus/opaque-input-count
     (reduce + (map :roundtrip/opaque-input-count (vals files)))
     :corpus/opaque-output-count
     (reduce + (map :roundtrip/opaque-output-count (vals files)))
     :corpus/semantic-lossless?
     (every? :roundtrip/semantic-lossless? (vals files))
     :corpus/opaque-lossless? (every? :roundtrip/opaque-lossless? (vals files))
     :corpus/lossless? (every? :roundtrip/lossless? (vals files))}))
