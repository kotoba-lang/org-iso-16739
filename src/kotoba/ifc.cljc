(ns kotoba.ifc
  (:require [ifc.core :as impl]))

(def schema impl/schema)
(def contract-version impl/contract-version)
(def entity-types impl/entity-types)
(def exchange-document impl/exchange-document)
(def write-spf impl/write-spf)
(def rewrite-spf impl/rewrite-spf)
(def semantic-fingerprint impl/semantic-fingerprint)
(def roundtrip-report impl/roundtrip-report)
(def corpus-report impl/corpus-report)
(def read-spf impl/read-spf)
