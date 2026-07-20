(ns kotoba.ifc
  (:require [ifc.core :as impl]))

(def schema impl/schema)
(def contract-version impl/contract-version)
(def entity-types impl/entity-types)
(def exchange-document impl/exchange-document)
(def write-spf impl/write-spf)
(def read-spf impl/read-spf)
