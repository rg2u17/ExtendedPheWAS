test_that("parse_opcs4 returns expected structure", {
  # Skip if source file not available
  filepath <- file.path(dirname(dirname(dirname(getwd()))), "OPCS 4.rtf")
  skip_if_not(file.exists(filepath), "OPCS 4.rtf not found")

  result <- parse_opcs4(filepath)

  expect_true(is.data.frame(result))
  expect_true(all(c("code", "description", "node_id", "parent_id",
                     "selectable", "chapter") %in% names(result)))
  expect_gt(nrow(result), 1000)
  expect_true("M" %in% result$chapter) # Urinary chapter
  expect_true(any(grepl("kidney", result$description, ignore.case = TRUE)))
})

test_that("parse_opcs3 returns expected structure", {
  filepath <- file.path(dirname(dirname(dirname(getwd()))), "OPCS 3.rtf")
  skip_if_not(file.exists(filepath), "OPCS 3.rtf not found")

  result <- parse_opcs3(filepath)

  expect_true(is.data.frame(result))
  expect_true(all(c("code", "description", "node_id", "parent_id",
                     "selectable") %in% names(result)))
  expect_gt(nrow(result), 100)
})

test_that("parse_icd10 returns expected structure", {
  filepath <- file.path(dirname(dirname(dirname(getwd()))), "icd10cm_codes_2024.txt")
  skip_if_not(file.exists(filepath), "ICD-10 codes file not found")

  result <- parse_icd10(filepath)

  expect_true(is.data.frame(result))
  expect_true(all(c("code", "description") %in% names(result)))
  expect_gt(nrow(result), 70000)
  expect_true("A000" %in% result$code) # Cholera
  expect_true(any(grepl("N20", result$code))) # Kidney calculus
})

test_that("normalize_vocab_id handles all variants", {
  expect_equal(normalize_vocab_id("ICD-10"), "ICD10CM")
  expect_equal(normalize_vocab_id("ICD10CM"), "ICD10CM")
  expect_equal(normalize_vocab_id("icd-10-cm"), "ICD10CM")
  expect_equal(normalize_vocab_id("OPCS-4"), "OPCS4")
  expect_equal(normalize_vocab_id("OPCS 4"), "OPCS4")
  expect_equal(normalize_vocab_id("Read v3"), "READv3")
  expect_equal(normalize_vocab_id("CTV3"), "READv3")
  expect_equal(normalize_vocab_id("ICD9"), "ICD9CM")
  # Read v2 variants
  expect_equal(normalize_vocab_id("Read v2"), "READv2")
  expect_equal(normalize_vocab_id("Read2"), "READv2")
  expect_equal(normalize_vocab_id("READV2"), "READv2")
  # SNOMED-CT variants
  expect_equal(normalize_vocab_id("SNOMED"), "SNOMEDCT")
  expect_equal(normalize_vocab_id("SNOMED-CT"), "SNOMEDCT")
  expect_equal(normalize_vocab_id("SNOMED CT"), "SNOMEDCT")
  expect_equal(normalize_vocab_id("SCT"), "SNOMEDCT")
  expect_equal(normalize_vocab_id("SNOMEDCT"), "SNOMEDCT")
})

test_that("score_substring_overlap works correctly", {
  # Exact substring
  expect_equal(score_substring_overlap("calculus of kidney",
                                        "calculus of kidney"), 1.0)

  # Partial overlap
  score <- score_substring_overlap("calculus kidney",
                                    "calculus of urinary tract")
  expect_gt(score, 0.3)

  # No overlap
  expect_equal(score_substring_overlap("brain tumor",
                                        "kidney stone"), 0)
})

test_that("chapter_to_phecode_base maps correctly", {
  expect_equal(chapter_to_phecode_base("A"), 1000L)
  expect_equal(chapter_to_phecode_base("M"), 2100L)
  expect_equal(chapter_to_phecode_base("K"), 1900L)
  expect_true(is.na(chapter_to_phecode_base("Z")))
})

test_that("assign_procedure_phecode generates valid codes", {
  expect_equal(assign_procedure_phecode("A01", "A"), "1001")
  expect_equal(assign_procedure_phecode("A011", "A"), "1001.1")
  expect_equal(assign_procedure_phecode("M09", "M"), "2109")
  expect_equal(assign_procedure_phecode("M091", "M"), "2109.1")
})

test_that("parse_read_v2 returns expected structure from RTF", {
  # Check for Read v2 file in common locations
  base_dir <- file.path(dirname(dirname(dirname(getwd()))))
  candidates <- c("read_code_v2.rtf", "Read_v2.rtf", "read_v2.txt")
  filepath <- NULL
  for (f in candidates) {
    p <- file.path(base_dir, f)
    if (file.exists(p)) { filepath <- p; break }
  }
  skip_if(is.null(filepath), "Read v2 source file not found")

  result <- parse_read_v2(filepath)

  expect_true(is.data.frame(result))
  expect_true(all(c("code", "description", "code_type") %in% names(result)))
  expect_gt(nrow(result), 100)
  # Read v2 codes are 5-byte alphanumeric
  expect_true(all(nchar(result$code) <= 7))
})

test_that("parse_snomed returns expected structure", {
  base_dir <- file.path(dirname(dirname(dirname(getwd()))))
  # Check for RF2 directory or flat file
  snomed_dir <- file.path(base_dir, "SnomedCT_Release")
  snomed_flat <- file.path(base_dir, "snomed_concepts.txt")
  filepath <- NULL
  if (dir.exists(snomed_dir)) filepath <- snomed_dir
  if (is.null(filepath) && file.exists(snomed_flat)) filepath <- snomed_flat
  skip_if(is.null(filepath), "SNOMED-CT source files not found")

  result <- parse_snomed(filepath)

  expect_true(is.data.frame(result))
  expect_true(all(c("concept_id", "description", "semantic_tag",
                     "hierarchy") %in% names(result)))
  expect_gt(nrow(result), 100)
})

test_that("build_snomed_phecode_map handles synthetic data", {
  # Create minimal synthetic SNOMED data
  snomed_df <- data.frame(
    concept_id = c("431855005", "90708001", "38341003"),
    description = c("Chronic kidney disease", "Kidney stone",
                     "Hypertensive disorder"),
    semantic_tag = c("disorder", "disorder", "disorder"),
    hierarchy = c("disorder", "disorder", "disorder"),
    stringsAsFactors = FALSE
  )

  # Create minimal synthetic ICD map
  snomed_icd_map <- data.frame(
    concept_id = c("431855005", "90708001"),
    icd10_code = c("N18.9", "N20.0"),
    map_group = c(1L, 1L),
    stringsAsFactors = FALSE
  )

  # Create minimal phecode map
  phecode_map <- data.frame(
    vocabulary_id = c("ICD10CM", "ICD10CM"),
    code = c("N189", "N200"),
    phecode = c("585", "594"),
    stringsAsFactors = FALSE
  )

  # Create minimal pheinfo
  pheinfo <- data.frame(
    phecode = c("585", "594", "401"),
    description = c("Renal failure", "Calculus of kidney",
                     "Hypertension"),
    groupnum = c(13L, 13L, 8L),
    group = c("genitourinary", "genitourinary", "circulatory system"),
    color = c("#FF0000", "#FF0000", "#0000FF"),
    stringsAsFactors = FALSE
  )

  result <- build_snomed_phecode_map(snomed_df, snomed_icd_map,
                                      phecode_map, pheinfo)

  expect_true(is.list(result))
  expect_true(all(c("map", "audit") %in% names(result)))
  expect_true(is.data.frame(result$map))
  expect_true(all(c("vocabulary_id", "code", "phecode") %in% names(result$map)))
  expect_true(all(result$map$vocabulary_id == "SNOMEDCT"))
  # Should map at least the two concepts with ICD cross-map
  expect_gte(nrow(result$map), 2)
})
