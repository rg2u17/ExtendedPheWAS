# ExtendedPheWAS

An R package that extends the [PheWAS](https://github.com/PheWAS/PheWAS) framework to support seven clinical coding vocabularies: ICD-9, ICD-10, OPCS-3, OPCS-4, Read v2, Read v3 (CTV3), and SNOMED-CT.

OPCS procedure codes are mapped to phecodes using a three-tier NLP matching strategy: condition-specific operations link to the relevant condition's phecode, anatomical matches link to body-system phecodes, and remaining procedures receive new phecodes in the 1000+ range.
