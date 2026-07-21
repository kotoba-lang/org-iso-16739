(ns ifc.external-validator-runner
  "Generate representative IFC and run pinned IfcOpenShell schema/WHERE rules."
  (:require [ifc.core :as ifc])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def official-fixtures
  ["test/fixtures/external/buildingSMART-basin-tessellation.ifc"
   "test/fixtures/external/buildingSMART-column-tessellation.ifc"
   "test/fixtures/external/buildingSMART-tessellated-item.ifc"
   "test/fixtures/external/buildingSMART-tessellation-colors.ifc"
   "test/fixtures/external/buildingSMART-wall-opening-window.ifc"])

(def generated-document
  (ifc/exchange-document
   {:project
    {:id 1 :global-id "1hqIFTRjfV6AWq_bMtnZwI" :name "Validation Project"
     :children
     [{:id 2 :global-id "0eA6m4fELI9QBIhP3wiLAp" :name "Site" :type :ifcsite
       :children
       [{:id 3 :global-id "05rScmOVzMoQXOfbYdtLYj" :name "Building"
         :type :ifcbuilding
         :children
         [{:id 4 :global-id "2nJrDaLQfJ1QPhdJR0o97J" :name "Level 01"
           :type :ifcbuildingstorey :children []}]}]}]}
    :elements
    [{:id 10 :global-id "3b0AoFivPN6RDJO6UL_GfZ" :kind :wall
      :name "Validated Wall" :container-id 4}]}))

(defn -main [& _]
  (let [path (Files/createTempFile "kotoba-ifc-validation-" ".ifc"
                                   (make-array FileAttribute 0))]
    (try
      (spit (.toFile path) (ifc/write-spf generated-document))
      (let [command (into ["uv" "run" "--isolated"
                           "--with" "ifcopenshell==0.8.5"
                           "--with" "pytest"
                           "python" "dev/ifc/ifcopenshell_validate.py"]
                          (conj official-fixtures (str path)))
            process (-> (ProcessBuilder. command) .inheritIO .start)
            status (.waitFor process)]
        (when-not (zero? status)
          (throw (ex-info "IfcOpenShell validation failed" {:exit status}))))
      (finally (Files/deleteIfExists path)))))
