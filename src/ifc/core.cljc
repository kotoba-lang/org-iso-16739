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
