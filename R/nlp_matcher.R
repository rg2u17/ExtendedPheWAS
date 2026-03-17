#' NLP Matching Engine for OPCS to Phecode Mapping
#'
#' Three-tier matching system that maps OPCS procedure codes to phecodes:
#' - Tier 1: Condition-specific keyword matching (high confidence)
#' - Tier 2: Anatomical/body-system matching (medium confidence)
#' - Tier 3: New procedure phecode assignment (fallback)

#' Procedure stopwords to remove from OPCS descriptions
#' @keywords internal
PROCEDURE_STOPWORDS <- c(
 "operation", "operations", "therapeutic", "endoscopic", "open",
  "percutaneous", "laparoscopic", "arthroscopic", "transluminal",
  "excision", "removal", "repair", "drainage", "biopsy", "exploration",
  "revision", "insertion", "implantation", "replacement", "extraction",
  "fragmentation", "destruction", "division", "closure", "suture",
  "incision", "resection", "ablation", "aspiration", "injection",
  "manipulation", "reduction", "fixation", "reconstruction",
  "transplantation", "anastomosis", "bypass", "catheterisation",
  "catheterization", "dilation", "dilatation", "ligation",
  "other specified", "unspecified", "NEC", "NOS", "NOC",
  "primary", "total", "partial", "radical", "simple", "complex",
  "major", "minor", "bilateral", "unilateral", "elective", "emergency",
  "of", "on", "from", "in", "to", "the", "and", "with", "without",
  "for", "by", "using", "through", "into", "other", "not elsewhere classified"
)

#' Anatomical keyword to phecode range mapping
#' @keywords internal
ANATOMICAL_MAP <- list(
  # Nervous system
  brain = list(groups = "neurological", phecodes = c("290", "291", "292", "293",
    "294", "295", "296", "330", "331", "332", "333", "334", "335", "336", "337",
    "340", "341", "342", "343", "344", "345")),
  cerebral = list(groups = "neurological", phecodes = c("430", "431", "432",
    "433", "434", "435", "436", "437", "438")),
  spinal = list(groups = "neurological;musculoskeletal", phecodes = c("720", "721",
    "722", "723", "724")),
  nerve = list(groups = "neurological", phecodes = c("350", "351", "352", "353",
    "354", "355", "356", "357")),

  # Eye
  eye = list(groups = "sense organs", phecodes = c("360", "361", "362", "363",
    "364", "365", "366", "367", "368", "369", "370", "371", "372", "373", "374",
    "375", "376", "377", "378", "379")),
  retina = list(groups = "sense organs", phecodes = c("361", "362")),
  lens = list(groups = "sense organs", phecodes = c("366")),
  cornea = list(groups = "sense organs", phecodes = c("370", "371")),
  glaucoma = list(groups = "sense organs", phecodes = c("365")),
  cataract = list(groups = "sense organs", phecodes = c("366")),

  # Ear
  ear = list(groups = "sense organs", phecodes = c("380", "381", "382", "383",
    "384", "385", "386", "387", "388", "389")),

  # Digestive
  oesophag = list(groups = "digestive", phecodes = c("530")),
  esophag = list(groups = "digestive", phecodes = c("530")),
  stomach = list(groups = "digestive", phecodes = c("531", "535", "536", "537")),
  gastric = list(groups = "digestive", phecodes = c("531", "535", "536", "537")),
  duoden = list(groups = "digestive", phecodes = c("532")),
  intestin = list(groups = "digestive", phecodes = c("550", "555", "556", "557",
    "558", "560", "562", "564", "569")),
  colon = list(groups = "digestive", phecodes = c("153", "555", "556", "562", "564")),
  rectum = list(groups = "digestive", phecodes = c("154", "562", "565", "566", "569")),
  rectal = list(groups = "digestive", phecodes = c("154", "562", "565", "566", "569")),
  appendix = list(groups = "digestive", phecodes = c("540", "541", "542", "543")),
  hernia = list(groups = "digestive", phecodes = c("550", "551", "552", "553")),
  liver = list(groups = "hepatobiliary", phecodes = c("570", "571", "572", "573")),
  hepatic = list(groups = "hepatobiliary", phecodes = c("570", "571", "572", "573")),
  gallbladder = list(groups = "hepatobiliary", phecodes = c("574", "575", "576")),
  biliary = list(groups = "hepatobiliary", phecodes = c("574", "575", "576", "577")),
  pancrea = list(groups = "hepatobiliary", phecodes = c("577")),
  spleen = list(groups = "hematopoietic", phecodes = c("289")),

  # Cardiovascular
  heart = list(groups = "circulatory system", phecodes = c("390", "391", "392",
    "393", "394", "395", "396", "410", "411", "412", "413", "414", "415",
    "416", "420", "421", "422", "423", "424", "425", "426", "427", "428", "429")),
  cardiac = list(groups = "circulatory system", phecodes = c("410", "411", "412",
    "413", "414", "420", "421", "422", "423", "424", "425", "426", "427", "428")),
  coronary = list(groups = "circulatory system", phecodes = c("410", "411", "412",
    "413", "414")),
  aort = list(groups = "circulatory system", phecodes = c("440", "441", "442", "443")),
  artery = list(groups = "circulatory system", phecodes = c("440", "441", "442",
    "443", "444", "447")),
  arter = list(groups = "circulatory system", phecodes = c("440", "441", "442",
    "443", "444", "447")),
  vein = list(groups = "circulatory system", phecodes = c("451", "452", "453",
    "454", "455", "456")),
  varicose = list(groups = "circulatory system", phecodes = c("454")),
  haemorrhoid = list(groups = "circulatory system", phecodes = c("455")),
  hemorrhoid = list(groups = "circulatory system", phecodes = c("455")),

  # Respiratory
  lung = list(groups = "respiratory", phecodes = c("480", "481", "482", "483",
    "484", "485", "486", "490", "491", "492", "493", "494", "495", "496",
    "500", "501", "502", "503", "504", "505", "506", "507", "508", "510",
    "511", "512", "513", "514", "515", "516", "517", "518", "519")),
  bronch = list(groups = "respiratory", phecodes = c("490", "491", "494")),
  laryn = list(groups = "respiratory", phecodes = c("464", "476", "478")),
  trache = list(groups = "respiratory", phecodes = c("464", "478", "519")),
  nasal = list(groups = "respiratory", phecodes = c("470", "471", "472", "473",
    "474", "475", "476", "477", "478")),
  sinus = list(groups = "respiratory", phecodes = c("461", "473")),
  pleura = list(groups = "respiratory", phecodes = c("510", "511", "512")),
  thorax = list(groups = "respiratory", phecodes = c("510", "511", "512", "513")),

  # Genitourinary
  kidney = list(groups = "genitourinary", phecodes = c("580", "581", "582", "583",
    "584", "585", "586", "587", "588", "589", "590", "591", "592", "593", "594")),
  renal = list(groups = "genitourinary", phecodes = c("580", "581", "582", "583",
    "584", "585", "586", "587", "588", "589", "590", "591", "592", "593", "594")),
  calculus = list(groups = "genitourinary", phecodes = c("574", "592", "594")),
  ureter = list(groups = "genitourinary", phecodes = c("592", "593", "594")),
  bladder = list(groups = "genitourinary", phecodes = c("595", "596", "188")),
  urethr = list(groups = "genitourinary", phecodes = c("597", "598", "599")),
  prostat = list(groups = "genitourinary", phecodes = c("185", "600", "601", "602")),
  testis = list(groups = "genitourinary", phecodes = c("186", "603", "604", "605",
    "606", "607", "608")),
  testicl = list(groups = "genitourinary", phecodes = c("186", "603", "604", "605",
    "606", "607", "608")),
  penis = list(groups = "genitourinary", phecodes = c("607")),
  uterus = list(groups = "genitourinary", phecodes = c("179", "180", "610", "614",
    "615", "617", "618", "619", "620", "621", "622", "623", "624", "625",
    "626", "627", "628", "629")),
  uteri = list(groups = "genitourinary", phecodes = c("179", "180", "610", "614",
    "615", "617", "618", "619", "620", "621", "622", "623", "624", "625")),
  ovary = list(groups = "genitourinary", phecodes = c("183", "620")),
  ovari = list(groups = "genitourinary", phecodes = c("183", "620")),
  vagin = list(groups = "genitourinary", phecodes = c("616", "622", "623")),
  cervix = list(groups = "genitourinary", phecodes = c("180", "616", "622")),
  cervic = list(groups = "genitourinary", phecodes = c("180", "616", "622", "723")),
  fallopian = list(groups = "genitourinary", phecodes = c("614", "620", "628")),
  breast = list(groups = "neoplasms;genitourinary", phecodes = c("174", "175",
    "610", "611", "612")),

  # Skin
  skin = list(groups = "dermatologic", phecodes = c("680", "681", "682", "684",
    "685", "686", "690", "691", "692", "693", "694", "695", "696", "697",
    "698", "700", "701", "702", "703", "704", "706", "707", "708", "709")),

  # Musculoskeletal
  bone = list(groups = "musculoskeletal", phecodes = c("170", "730", "731", "732",
    "733")),
  joint = list(groups = "musculoskeletal", phecodes = c("710", "711", "712", "714",
    "715", "716", "717", "718", "719")),
  hip = list(groups = "musculoskeletal", phecodes = c("715", "718", "719", "820")),
  knee = list(groups = "musculoskeletal", phecodes = c("715", "717", "718", "719")),
  shoulder = list(groups = "musculoskeletal", phecodes = c("718", "719", "726")),
  fracture = list(groups = "injuries & poisonings", phecodes = c("800", "801", "802",
    "803", "804", "805", "806", "807", "808", "809", "810", "811", "812",
    "813", "814", "815", "816", "817", "818", "819", "820", "821", "822",
    "823", "824", "825", "826", "827", "828", "829")),
  spine = list(groups = "musculoskeletal", phecodes = c("720", "721", "722", "723",
    "724", "805", "806")),
  tendon = list(groups = "musculoskeletal", phecodes = c("726", "727")),
  muscle = list(groups = "musculoskeletal", phecodes = c("728", "729")),

  # Endocrine
  thyroid = list(groups = "endocrine/metabolic", phecodes = c("193", "240", "241",
    "242", "243", "244", "245", "246")),
  adrenal = list(groups = "endocrine/metabolic", phecodes = c("194", "255")),
  pituitary = list(groups = "endocrine/metabolic", phecodes = c("194", "253")),

  # Mouth/throat
  tonsil = list(groups = "respiratory;digestive", phecodes = c("474", "475")),
  tongue = list(groups = "digestive", phecodes = c("141", "528", "529")),
  tooth = list(groups = "digestive", phecodes = c("520", "521", "522", "523",
    "524", "525")),
  dental = list(groups = "digestive", phecodes = c("520", "521", "522", "523",
    "524", "525")),
  palate = list(groups = "digestive", phecodes = c("526", "528", "749")),
  salivary = list(groups = "digestive", phecodes = c("527"))
)


#' Match OPCS Code to Phecode
#'
#' Three-tier matching for a single OPCS code description.
#'
#' @param opcs_code The OPCS code
#' @param opcs_desc The OPCS description
#' @param pheinfo Data frame of phecode info (phecode, description, group)
#' @param chapter_groups Character vector of phecode groups for this OPCS chapter
#' @return List with: phecode, confidence, tier, match_method
#' @keywords internal
match_opcs_to_phecode <- function(opcs_code, opcs_desc, pheinfo, chapter_groups) {

  # Tier 1: Condition-specific keyword matching
  tier1_result <- match_tier1(opcs_desc, pheinfo, chapter_groups)
  if (!is.null(tier1_result)) {
    return(list(
      phecode = tier1_result$phecode,
      phecode_description = tier1_result$description,
      confidence = tier1_result$score,
      tier = 1L,
      match_method = "keyword_substring"
    ))
  }

  # Tier 2: Anatomical/body-system matching
  tier2_result <- match_tier2(opcs_desc, pheinfo)
  if (!is.null(tier2_result)) {
    return(list(
      phecode = tier2_result$phecode,
      phecode_description = tier2_result$description,
      confidence = tier2_result$score,
      tier = 2L,
      match_method = "anatomical_keyword"
    ))
  }

  # Tier 3: No match found - will need new procedure phecode
  return(list(
    phecode = NA_character_,
    phecode_description = NA_character_,
    confidence = 0,
    tier = 3L,
    match_method = "new_procedure_phecode"
  ))
}


#' Tier 1: Condition-Specific Keyword Match
#'
#' Extracts clinical condition keywords from OPCS description and matches
#' against phecode descriptions.
#'
#' @param opcs_desc OPCS description string
#' @param pheinfo Phecode info data frame
#' @param chapter_groups Phecode groups to search within
#' @return List with phecode, description, score or NULL if no match
#' @keywords internal
match_tier1 <- function(opcs_desc, pheinfo, chapter_groups) {
  desc_lower <- tolower(opcs_desc)

  # Remove procedure stopwords
  clinical_terms <- desc_lower
  for (sw in tolower(PROCEDURE_STOPWORDS)) {
    clinical_terms <- gsub(paste0("\\b", sw, "\\b"), " ", clinical_terms)
  }
  clinical_terms <- gsub("\\s+", " ", trimws(clinical_terms))

  # Skip if no meaningful clinical terms remain
  if (nchar(clinical_terms) < 3) return(NULL)

  # Split chapter_groups on semicolons
  groups_vec <- unlist(strsplit(chapter_groups, ";"))
  groups_vec <- trimws(groups_vec)

  # Filter pheinfo to relevant groups
  candidates <- pheinfo[tolower(pheinfo$group) %in% tolower(groups_vec), ]

  # Also search all pheinfo if chapter filter is too restrictive
  if (nrow(candidates) < 10) {
    candidates <- pheinfo
  }

  # Score each candidate by substring overlap
  candidates$score <- vapply(tolower(candidates$description), function(phecode_desc) {
    score_substring_overlap(clinical_terms, phecode_desc)
  }, numeric(1))

  # Filter to minimum threshold
  good_matches <- candidates[candidates$score >= 0.5, ]

  if (nrow(good_matches) == 0) return(NULL)

  # Return best match
  best <- good_matches[which.max(good_matches$score), ]
  return(list(
    phecode = as.character(best$phecode),
    description = best$description,
    score = best$score
  ))
}


#' Tier 2: Anatomical Keyword Match
#'
#' Uses anatomical keyword dictionary to find related phecodes.
#'
#' @param opcs_desc OPCS description string
#' @param pheinfo Phecode info data frame
#' @return List with phecode, description, score or NULL if no match
#' @keywords internal
match_tier2 <- function(opcs_desc, pheinfo) {
  desc_lower <- tolower(opcs_desc)

  # Search for anatomical keywords in the description
  matched_phecodes <- character(0)
  matched_keyword <- NA_character_

  for (keyword in names(ANATOMICAL_MAP)) {
    if (grepl(keyword, desc_lower, fixed = TRUE)) {
      matched_phecodes <- c(matched_phecodes, ANATOMICAL_MAP[[keyword]]$phecodes)
      if (is.na(matched_keyword)) matched_keyword <- keyword
    }
  }

  if (length(matched_phecodes) == 0) return(NULL)

  # Find the most specific matching phecode
  matched_phecodes <- unique(matched_phecodes)
  candidates <- pheinfo[pheinfo$phecode %in% matched_phecodes, ]

  if (nrow(candidates) == 0) return(NULL)

  # Score by description similarity
  clinical_terms <- tolower(opcs_desc)
  for (sw in tolower(PROCEDURE_STOPWORDS)) {
    clinical_terms <- gsub(paste0("\\b", sw, "\\b"), " ", clinical_terms)
  }
  clinical_terms <- gsub("\\s+", " ", trimws(clinical_terms))

  candidates$score <- vapply(tolower(candidates$description), function(phecode_desc) {
    score_substring_overlap(clinical_terms, phecode_desc)
  }, numeric(1))

  best <- candidates[which.max(candidates$score), ]

  return(list(
    phecode = as.character(best$phecode),
    description = best$description,
    score = max(0.3, best$score * 0.7) # Discount tier 2 scores
  ))
}


#' Score Substring Overlap Between Two Descriptions
#'
#' Calculates a similarity score based on shared meaningful words.
#'
#' @param clinical_terms Cleaned OPCS clinical terms
#' @param phecode_desc Phecode description
#' @return Numeric score between 0 and 1
#' @keywords internal
score_substring_overlap <- function(clinical_terms, phecode_desc) {
  # Check for exact substring match first (highest score)
  if (nchar(clinical_terms) >= 5 && grepl(clinical_terms, phecode_desc, fixed = TRUE)) {
    return(1.0)
  }
  if (nchar(phecode_desc) >= 5 && grepl(phecode_desc, clinical_terms, fixed = TRUE)) {
    return(0.95)
  }

  # Word-level overlap
  words_clinical <- unique(unlist(strsplit(clinical_terms, "\\s+")))
  words_phecode <- unique(unlist(strsplit(phecode_desc, "\\s+")))

  # Remove very short/common words
  words_clinical <- words_clinical[nchar(words_clinical) >= 3]
  words_phecode <- words_phecode[nchar(words_phecode) >= 3]

  if (length(words_clinical) == 0 || length(words_phecode) == 0) return(0)

  shared <- intersect(words_clinical, words_phecode)

  if (length(shared) == 0) return(0)

  # Jaccard-like score, weighted by number of shared words
  score <- length(shared) / min(length(words_clinical), length(words_phecode))

  return(score)
}


#' OPCS Chapter to Procedure Phecode Base Number
#'
#' Maps OPCS-4 chapter letters to the base phecode number in the 1000+ range.
#'
#' @param chapter Single character A-X
#' @return Integer base phecode number
#' @keywords internal
chapter_to_phecode_base <- function(chapter) {
  bases <- c(
    A = 1000L, B = 1100L, C = 1200L, D = 1300L, E = 1400L,
    F = 1500L, G = 1600L, H = 1700L, J = 1800L, K = 1900L,
    L = 2000L, M = 2100L, N = 2200L, P = 2300L, Q = 2400L,
    R = 2500L, S = 2600L, T = 2700L, U = 2800L, V = 2900L,
    W = 3000L, X = 3100L
  )
  base <- bases[chapter]
  if (is.na(base)) return(NA_integer_)
  return(base)
}


#' Assign New Procedure Phecode
#'
#' For OPCS codes that don't map to existing condition phecodes, assign a new
#' procedure phecode in the 1000+ range.
#'
#' @param opcs_code The OPCS code (e.g., "A01")
#' @param chapter The OPCS chapter letter
#' @return Character string phecode
#' @keywords internal
assign_procedure_phecode <- function(opcs_code, chapter) {
  base <- chapter_to_phecode_base(chapter)
  if (is.na(base)) return(NA_character_)

  # Extract the numeric part of the OPCS code after the chapter letter
  # A01 -> 01, A011 -> 01.1, M091 -> 09.1
  code_part <- substring(opcs_code, 2) # Remove chapter letter

  if (nchar(code_part) <= 2) {
    # Parent code (e.g., A01 -> 1000 + 1 = 1001)
    num <- as.integer(code_part)
    if (is.na(num)) return(NA_character_)
    phecode <- sprintf("%d", base + num)
  } else {
    # Child code (e.g., A011 -> 1001.1)
    parent_num <- as.integer(substr(code_part, 1, 2))
    child_num <- substr(code_part, 3, nchar(code_part))
    if (is.na(parent_num)) return(NA_character_)
    phecode <- sprintf("%d.%s", base + parent_num, child_num)
  }

  return(phecode)
}
