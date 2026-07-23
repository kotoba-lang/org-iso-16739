(ns ifc.ids-xml-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [ifc.ids :as ids]
            [ifc.ids-test :as fixture]
            [ifc.ids.xml :as ids-xml]))

(deftest writes-and-reads-buildingsmart-ids-1-xml
  (let [source (assoc fixture/requirements :ids/author "owner@example.com")
        xml (ids-xml/write-xml source)
        parsed (ids-xml/read-xml xml)
        report (ids/validate fixture/model parsed)]
    (is (string/includes? xml "http://standards.buildingsmart.org/IDS"))
    (is (string/includes? xml "<propertySet>"))
    (is (string/includes? xml "<xs:pattern"))
    (is (= "Wall handover requirements" (:ids/title parsed)))
    (is (= "owner@example.com" (:ids/author parsed)))
    (is (= 2 (count (:ids/specifications parsed))))
    (is (false? (:ids.report/pass? report)))
    (is (= "wall-fail" (get-in report [:ids.report/issues 0 :ids.issue/global-id])))))

(deftest rejects-doctype-and-external-entity-input
  (is (thrown? Exception
               (ids-xml/read-xml
                "<?xml version='1.0'?><!DOCTYPE ids [<!ENTITY xxe SYSTEM 'file:///etc/passwd'>]><ids>&xxe;</ids>"))))

(deftest writes-and-reads-multiple-or-combined-patterns
  (let [source
        (ids/document
         {:title "Name matches either case pattern"
          :specifications
          [{:name "Name is AB12-style or ab12-style"
            :applicability [{:type :entity :name "IFCWALL"}]
            :requirements
            [{:type :attribute :name "Name"
              :value {:patterns ["[A-Z]{2}[0-9]{2}" "[a-z]{2}[0-9]{2}"]}}]}]})
        xml (ids-xml/write-xml source)
        parsed (ids-xml/read-xml xml)
        restriction (get-in parsed [:ids/specifications 0 :ids.specification/requirements
                                    0 :value])]
    (is (= 2 (count (re-seq #"<xs:pattern" xml))))
    (is (true? (ids/matches-restriction? "AB12" restriction)))
    (is (true? (ids/matches-restriction? "ab12" restriction)))
    (is (false? (ids/matches-restriction? "no-match" restriction)))))
