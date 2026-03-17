# ExtendedPheWAS

An R package that extends the [PheWAS](https://github.com/PheWAS/PheWAS) framework to support seven clinical coding vocabularies: ICD-9, ICD-10, OPCS-3, OPCS-4, Read v2, Read v3 (CTV3), and SNOMED-CT.

OPCS procedure codes are mapped to phecodes using a three-tier NLP matching strategy: condition-specific operations link to the relevant condition's phecode, anatomical matches link to body-system phecodes, and remaining procedures receive new phecodes in the 1000+ range.

## Installation

```r
# Install from GitHub (automatically installs the PheWAS dependency)
# install.packages("remotes")  # if not already installed
remotes::install_github("rg2u17/ExtendedPheWAS")
```

### Dependencies

- **Required:** R (>= 4.0.0), [PheWAS](https://github.com/PheWAS/PheWAS)
- **Imports:** ggplot2 (>= 3.0.0), grDevices, rlang, stats, utils
- **Suggests:** testthat (>= 3.0.0), knitr, rmarkdown, ggrepel, DT

The `Remotes:` field in DESCRIPTION ensures that `remotes::install_github()` automatically installs PheWAS from GitHub.

### Building the mapping data

After installation, you need to build the mapping tables from your source vocabulary files before using the analysis functions:

```r
# Point the build script at your source data directory
# and run it from the package root:
source(system.file("scripts", "build_all_data.R", package = "ExtendedPheWAS"))
```

See [Input data formats](#input-data-formats) below for what files you need.

## Quick start

```r
library(ExtendedPheWAS)

# 1. Prepare your data as a data frame with id, vocabulary_id, and code columns
patient_codes <- data.frame(
  id = c(1, 1, 2, 2, 3),
  vocabulary_id = c("ICD10CM", "OPCS4", "SNOMED-CT", "READv2", "ICD9"),
  code = c("N20.0", "M091", "90708001", "K10..", "592"),
  stringsAsFactors = FALSE
)

# 2. Map codes to phecodes
mapped <- mapCodesToPhecodes(patient_codes)

# 3. Create phenotype matrix
phenotypes <- createPhenotypes(patient_codes, min.code.count = 2)

# 4. Run PheWAS
results <- phewas(phenotypes = phenotypes, genotypes = exposure_variable)

# 5. Visualise
phewasManhattan(results)
```

## Supported vocabularies

The `vocabulary_id` column accepts any of the aliases below (case-insensitive). They are all normalised internally.

| Canonical ID | Accepted aliases |
|---|---|
| `ICD9CM` | ICD9, ICD-9, ICD-9-CM, ICD9-CM |
| `ICD10CM` | ICD10, ICD-10, ICD-10-CM, ICD10-CM |
| `OPCS4` | OPCS-4, OPCS 4, OPCS-IV |
| `OPCS3` | OPCS-3, OPCS 3, OPCS-III |
| `READv2` | Read v2, READ-V2, READ2 |
| `READv3` | READ, Read v3, CTV3, READ-V3, READCODE |
| `SNOMEDCT` | SNOMED, SNOMED-CT, SNOMED CT, SCT |

## Input data formats

Each vocabulary has a dedicated parser that reads the official source files and produces a standardised data frame. Below is what each parser expects.

---

### ICD-10-CM (`parse_icd10`)

**Source:** [CMS ICD-10-CM files](https://www.cms.gov/medicare/coding-billing/icd-10-codes)

**File format:** Plain text, one code per line, tab-separated or fixed-width.

| Format | Structure |
|---|---|
| Tab-separated | `code<TAB>description` |
| Fixed-width | Code in columns 1-7, description from column 8 onward |

An optional order/hierarchy file can also be supplied (fixed-width: 5-digit sequence, code at positions 7-13, hierarchy flag at position 15, then description).

```r
icd10 <- parse_icd10(
  filepath_codes = "icd10cm_codes_2024.txt",
  filepath_order = "icd10cm_order_2024.txt"  # optional
)
```

**Output columns:** `code`, `description`, `hierarchy_level` (if order file provided)

---

### ICD-9-CM (`parse_icd9_diagnostics`, `parse_icd9_procedures`)

**Source:** ICD-9-CM reference files (typically available as RTF documents from hospital coding departments or online archives).

**File format:** RTF. Codes are extracted by pattern matching from the document text.

- **Diagnostics:** Expects lines matching 3-5 digit codes (e.g. `001`, `001.0`, `V01.0`, `E800.0`) followed by a description.
- **Procedures:** Expects lines matching 2-4 digit codes (e.g. `00`, `00.0`, `00.01`) followed by a description.

```r
icd9_dx  <- parse_icd9_diagnostics("ICD 9 Diagnostics.rtf")
icd9_proc <- parse_icd9_procedures("ICD9 Operative Codes.RTF")
```

**Output columns:** `code`, `description`

---

### OPCS-4 (`parse_opcs4`)

**Source:** [NHS TRUD - OPCS-4](https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/10) or hospital coding department.

**File format:** RTF containing tab-separated rows with 5 fields:

| Column | Description | Example |
|---|---|---|
| 1 | Code | `A01.1` |
| 2 | Meaning (description, usually prefixed by the code) | `A01.1 Hemispherectomy` |
| 3 | Node ID (integer) | `12345` |
| 4 | Parent ID (integer) | `12340` |
| 5 | Selectable flag | `Y` or `N` |

The first few lines of the RTF are header/metadata and are automatically skipped.

```r
opcs4 <- parse_opcs4("OPCS 4.rtf")
```

**Output columns:** `code`, `description`, `node_id`, `parent_id`, `selectable`, `chapter`

---

### OPCS-3 (`parse_opcs3`)

**Source:** Same as OPCS-4 (NHS TRUD or hospital coding department).

**File format:** Identical structure to OPCS-4 (RTF with 5 tab-separated columns). Codes are numeric (e.g. `001`, `010.1`).

```r
opcs3 <- parse_opcs3("OPCS 3.rtf")
```

**Output columns:** `code`, `description`, `node_id`, `parent_id`, `selectable`

---

### Read v3 / CTV3 (`parse_read_v3`)

**Source:** [NHS TRUD - Read Codes](https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9) (now retired but archived).

**File format:** RTF containing tab-separated rows with 2 fields:

| Column | Description | Example |
|---|---|---|
| 1 | Code (5-character alphanumeric) | `H33..` |
| 2 | Meaning (description or cross-reference) | `Asthma` or `See X100.` |

Cross-references (lines containing "See XXXX") are automatically resolved by following chains up to depth 5.

```r
read_v3 <- parse_read_v3(
  "read_code_v3.rtf",
  resolve_crossrefs = TRUE,   # follow "See" references
  include_occupations = FALSE  # exclude occupation codes (0-6 prefix)
)
```

**Output columns:** `code`, `description`, `code_type` (one of: `diagnostic`, `procedure`, `extended`, `admin`, `occupation`)

---

### Read v2 (`parse_read_v2`)

**Source:** [NHS TRUD - Read Codes Version 2](https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/9) (retired April 2018, archived releases available).

**File format:** The parser auto-detects format from the file extension or input type:

| Format | Extension / Type | Structure |
|---|---|---|
| Tab-separated | `.txt`, `.tsv` | Header row, then `code<TAB>description` (plus optional extra columns) |
| CSV | `.csv` | Header row, then `code,description` (plus optional extra columns) |
| RTF | `.rtf` | Same structure as Read v3 (tab-separated after RTF stripping) |
| NHS TRUD release | Directory | Expects a `Corev2.all` or similar file inside `Unified/V2/` subdirectories. Fixed-width (code in first 7 chars), pipe-delimited (`code\|description\|term_type`), or tab-delimited formats are all supported. |

Read v2 codes are 5-character alphanumeric (e.g. `H33..`, `K190.`, `G30..`).

```r
# From a text file
read_v2 <- parse_read_v2("read_v2_codes.txt")

# From an NHS TRUD release directory
read_v2 <- parse_read_v2("nhs_read_v2/")

# Override column detection
read_v2 <- parse_read_v2("custom_file.csv", format = "csv",
                          code_col = "ReadCode", desc_col = "Term")
```

**Output columns:** `code`, `description`, `code_type`, `chapter`

---

### SNOMED-CT (`parse_snomed`)

**Source:**
- UK: [NHS TRUD](https://isd.digital.nhs.uk/trud/)
- US: [NLM UMLS](https://www.nlm.nih.gov/healthit/snomedct/)
- International: [SNOMED International MLDS](https://mlds.ihtsdotools.org/)

**File format:** The parser auto-detects format:

| Format | Input | Required contents |
|---|---|---|
| RF2 release directory | Directory path | Must contain `Snapshot/`, `Full/`, or `Delta/` subdirectories with `sct2_Concept_*.txt` and `sct2_Description_*.txt` files (standard RF2 tab-separated format) |
| Flat file | `.txt` or `.csv` | Tab- or comma-separated file with at minimum a concept ID column and a description/term column. Column names auto-detected from common patterns (`conceptId`, `concept_id`, `sctid`, `id`, `term`, `description`, `FSN`, `preferredTerm`) |
| Mapping file | `.txt` | SNOMED Extended Map reference set (`der2_iisssccRefset_ExtendedMap*.txt`) for SNOMED-to-ICD-10 cross-mapping. Pass `format = "mapping"` to read this. |

```r
# From an RF2 release directory
snomed <- parse_snomed("SnomedCT_InternationalRF2_PRODUCTION/")

# From a flat export
snomed <- parse_snomed("snomed_concepts.txt")

# Load the SNOMED->ICD-10 cross-map
snomed_icd <- parse_snomed("SnomedCT_Release/", format = "mapping")
```

**Output columns:** `concept_id`, `description`, `semantic_tag`, `hierarchy`, `active`

For mapping files: `concept_id`, `icd10_code`, `map_group`, `map_priority`

---

## Core workflow

### Step 1: Parse source files

Use the parsers above to read your raw vocabulary files into R data frames. This only needs to be done once.

### Step 2: Map codes to phecodes

`mapCodesToPhecodes()` accepts a data frame with `vocabulary_id` and `code` columns. Any additional columns (e.g. patient `id`, dates) are preserved.

```r
input <- data.frame(
  id = c(1, 1, 2),
  vocabulary_id = c("ICD10CM", "OPCS4", "SNOMED-CT"),
  code = c("N20.0", "M091", "90708001"),
  stringsAsFactors = FALSE
)
mapped <- mapCodesToPhecodes(input)
```

### Step 3: Create phenotypes

`createPhenotypes()` takes the same input format and produces a wide-format binary phenotype matrix (rows = individuals, columns = phecodes).

```r
phenotypes <- createPhenotypes(
  id.vocab.code.index = patient_codes,
  min.code.count = 2,             # minimum code occurrences to be a case
  add.phecode.exclusions = TRUE,  # apply phecode exclusion ranges
  id.sex = sex_data               # optional: data frame with id, sex columns
)
```

### Step 4: Run PheWAS

```r
results <- phewas(
  phenotypes = phenotypes,
  genotypes = exposure_vector,
  covariates = c("age", "sex"),  # optional covariate column names
  min.cases = 20                 # minimum cases per phecode
)
```

### Step 5: Visualise

```r
phewasManhattan(results)

# Or use phenotypePlot for more control
phenotypePlot(results, title = "My PheWAS")
```

## OPCS procedure mapping strategy

OPCS codes are mapped to phecodes via a three-tier NLP approach:

1. **Tier 1 -- Condition match (high confidence):** Clinical keywords are extracted from the OPCS description (after removing procedure stopwords such as "operation", "excision", "therapeutic") and matched against phecode descriptions filtered by body-system group. For example, OPCS code M09 "Therapeutic endoscopic operations on calculus of kidney" extracts "calculus of kidney" and matches to phecode 594.

2. **Tier 2 -- Anatomical match (medium confidence):** When no specific condition is found, anatomical keywords (kidney, brain, heart, liver, etc.) are used to match to phecode ranges for the relevant body system.

3. **Tier 3 -- New procedure phecode (fallback):** OPCS codes that cannot be matched to existing phecodes receive new procedure phecodes in the 1000+ range, organised by OPCS chapter (e.g. Chapter A 1000-1099, Chapter M 2100-2199).

All mapping decisions are logged in an audit trail accessible via `exportMappingAudit()`.

## Building package data

To rebuild the internal mapping tables from source files, run:

```r
source("data-raw/build_all_data.R")
```

This parses all source vocabulary files, builds mappings, and saves `.rda` files to the `data/` directory. Read v2 and SNOMED-CT source files are optional -- the build will skip them if not found.

## Utility functions

| Function | Description |
|---|---|
| `addPhecodeInfo(phecodes)` | Look up description, group, and colour for phecodes |
| `restrictPhecodesBySex(phecodes, sex)` | Filter phecodes by sex-specific restrictions |
| `lookupCode(code, vocabulary_id)` | Look up a single code's phecode mapping |
| `mappingSummary(map)` | Summarise mapping coverage by vocabulary |
| `exportMappingAudit(audit)` | Export NLP mapping audit trail for review |

## License

GPL-3
