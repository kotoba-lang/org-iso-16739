# kotoba-lang/org-iso-16739

Portable IFC 4.3 exchange primitives shared by BIM, CAD, facility-management,
and cloud applications. The library owns IFC schema naming, entity mapping,
IFC-SPF transport, and lossless Kotoba model envelopes. Generic ISO 10303-21
serialization lives in `kotoba-lang/org-iso-10303`; the legacy
`kotoba-lang/step` package is now a compatibility facade over that authority.

`ifc.core/write-spf` accepts a neutral exchange document:

```clojure
{:ifc/project {:id "p" :name "Tower" :model portable-model}
 :ifc/elements [{:id 10 :global-id "g10" :kind :wall :name "Wall"}]}
```

The emitted standard entities are available to IFC readers. `:model` is also
stored in a `KOTOBA_MODEL_EDN` property for lossless round-trip between Kotoba
applications while full external IFC geometry/profile import is developed.

External exchange can be verified with `ifc.core/roundtrip-report` for one SPF
or `ifc.core/corpus-report` for a `{label spf-text}` collection. The semantic
gate compares GlobalIds, spatial hierarchy, placements, units, georeferencing,
types, property sets, openings, and supported geometry while ignoring only
regenerated STEP entity numbers. The writer currently covers IFC4.3 products
used by the buildingSMART PCERT architecture, HVAC, and structural samples,
including mapped geometry, boolean/half-space collections, tessellated and
polygonal face sets, swept disks, and advanced BREP edge curves.
