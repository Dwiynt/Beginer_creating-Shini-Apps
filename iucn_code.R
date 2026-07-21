# Set argument for the functions
library(tidyverse)
library(rredlist)
library(iucnredlist)
library(rcites)

# Use environment variables when available.
# Avoid hard-coding API tokens in source files that may be shared.
if (Sys.getenv("CITES_TOKEN") != "") {
  rcites::set_token(Sys.getenv("CITES_TOKEN"))
}

if (Sys.getenv("IUCN_REDLIST_KEY") == "" && exists("api") && is.character(api)) {
  Sys.setenv(IUCN_REDLIST_KEY = api)
}

empty_iucn_row <- function(species_name, status = "Not found") {
  tibble(
    Species = species_name,
    Class = NA_character_,
    Order = NA_character_,
    Family = NA_character_,
    Status = status,
    `Common name` = NA_character_,
    Trend = "Unknown"
  )
}

pick_col <- function(df, candidates, default = NA_character_) {
  found <- intersect(candidates, names(df))
  if (length(found) == 0) {
    rep(default, nrow(df))
  } else {
    df[[found[1]]]
  }
}

# IUCN
# Supports both calls:
#   retrieve_IUCN_data(Species_df)
#   retrieve_IUCN_data(api, Species_df)
retrieve_IUCN_data <- function(api_or_species_df, species_df = NULL, wait_time = 0.5) {
  if (is.null(species_df)) {
    species_df <- api_or_species_df
    api <- Sys.getenv("IUCN_REDLIST_KEY")
    if (identical(api, "")) api <- NULL
  } else {
    api <- api_or_species_df
    if (is.character(api) && length(api) == 1 && !identical(api, "")) {
      Sys.setenv(IUCN_REDLIST_KEY = api)
    } else if (is.null(api) || (is.character(api) && length(api) == 1 && identical(api, ""))) {
      api <- Sys.getenv("IUCN_REDLIST_KEY")
      if (identical(api, "")) api <- NULL
    }
  }
  
  if (!is.data.frame(species_df)) {
    stop("retrieve_IUCN_data() needs a species data frame with Species/genus/species columns.")
  }
  
  if ("Species" %in% names(species_df) && !all(c("genus", "species") %in% names(species_df))) {
    species_df <- species_df %>%
      mutate(
        genus = stringr::word(Species, 1),
        species = stringr::word(Species, 2, -1)
      )
  }
  
  if (!all(c("genus", "species") %in% names(species_df))) {
    stop("Species data must contain either Species or both genus and species columns.")
  }
  
  species_df <- species_df %>%
    mutate(
      genus = as.character(genus),
      species = as.character(species),
      Species = if ("Species" %in% names(.)) as.character(Species) else paste(genus, species)
    ) %>%
    filter(!is.na(genus), genus != "", !is.na(species), species != "") %>%
    distinct(Species, genus, species, .keep_all = TRUE)
  
  if (nrow(species_df) == 0) {
    return(tibble(
      Species = character(),
      Class = character(),
      Order = character(),
      Family = character(),
      Status = character(),
      `Common name` = character(),
      Trend = character()
    ))
  }
  
  safe_assessments <- purrr::safely(function(genus, species) {
    assessments_by_name(api = api, genus = genus, species = species)
  }, otherwise = NULL)
  
  iucn_as <- purrr::pmap_dfr(
    list(species_df$genus, species_df$species, species_df$Species),
    function(genus, species, species_name) {
      Sys.sleep(wait_time)
      result <- safe_assessments(genus, species)
      
      if (is.null(result$result) || !is.data.frame(result$result) || nrow(result$result) == 0) {
        return(tibble(
          Species = species_name,
          genus = genus,
          species = species,
          assessment_id = NA_character_,
          latest = NA,
          scopes_code = NA,
          lookup_status = "Not found"
        ))
      }
      
      res_df <- result$result
      if (!"assessment_id" %in% names(res_df)) res_df$assessment_id <- NA_character_
      if (!"latest" %in% names(res_df)) res_df$latest <- NA
      if (!"scopes_code" %in% names(res_df)) res_df$scopes_code <- NA
      
      res_df %>%
        mutate(
          assessment_id = as.character(assessment_id),
          latest = as.logical(latest),
          scopes_code = suppressWarnings(as.integer(scopes_code)),
          Species = species_name,
          genus = genus,
          species = species,
          lookup_status = "Found"
        )
    }
  )
  
  iucn_as_latest <- iucn_as %>%
    filter(!is.na(assessment_id))
  
  if ("latest" %in% names(iucn_as_latest)) {
    iucn_as_latest <- iucn_as_latest %>%
      filter(latest %in% TRUE | is.na(latest))
  }
  
  if ("scopes_code" %in% names(iucn_as_latest)) {
    iucn_as_latest <- iucn_as_latest %>%
      filter(scopes_code == 1 | is.na(scopes_code))
  }
  
  if (nrow(iucn_as_latest) == 0) {
    return(species_df %>%
             transmute(
               Species,
               Class = NA_character_,
               Order = NA_character_,
               Family = NA_character_,
               Status = "Not found",
               `Common name` = NA_character_,
               Trend = "Unknown"
             ))
  }
  
  safe_assessment_data <- purrr::safely(assessment_data_many, otherwise = NULL)
  valid_ids <- unique(na.omit(iucn_as_latest$assessment_id))
  full_data_result <- safe_assessment_data(api, valid_ids, wait_time = wait_time)
  full_data <- full_data_result$result
  
  if (is.null(full_data) || length(full_data) == 0) {
    found_as_not_found <- iucn_as_latest %>%
      distinct(Species) %>%
      mutate(
        Class = NA_character_,
        Order = NA_character_,
        Family = NA_character_,
        Status = "Not found",
        `Common name` = NA_character_,
        Trend = "Unknown"
      )
    return(found_as_not_found)
  }
  
  full_taxon <- tryCatch(extract_element(full_data, "taxon"), error = function(e) NULL)
  full_rlc <- tryCatch(extract_element(full_data, "red_list_category"), error = function(e) NULL)
  
  common_name <- tryCatch({
    df_cm <- extract_element(full_data, "taxon_common_names")
    if (is.null(df_cm) || !is.data.frame(df_cm) || nrow(df_cm) == 0) {
      return(NULL)
    }
    if ("main" %in% names(df_cm)) df_cm <- df_cm %>% filter(main == TRUE | is.na(main))
    if ("language" %in% names(df_cm)) df_cm <- df_cm %>% filter(language == "eng" | is.na(language))
    df_cm
  }, error = function(e) NULL)
  
  if (!is.null(full_taxon) && !is.null(full_rlc) &&
      is.data.frame(full_taxon) && is.data.frame(full_rlc) &&
      nrow(full_taxon) > 0 && nrow(full_rlc) > 0 &&
      "assessment_id" %in% names(full_taxon) && "assessment_id" %in% names(full_rlc)) {
    
    final_raw <- full_taxon %>%
      inner_join(full_rlc, by = "assessment_id")
    
    if (!is.null(common_name) && "assessment_id" %in% names(common_name)) {
      final_raw <- final_raw %>%
        left_join(common_name, by = "assessment_id", suffix = c("", "_common"))
    }
    
    final_species_df <- tibble(
      Species = pick_col(final_raw, c("scientific_name", "Species")),
      Class = pick_col(final_raw, c("class_name", "Class")),
      Order = pick_col(final_raw, c("order_name", "Order")),
      Family = pick_col(final_raw, c("family_name", "Family")),
      Status = pick_col(final_raw, c("code", "Status")),
      `Common name` = pick_col(final_raw, c("name", "Common name", "name_common")),
      Trend = pick_col(final_raw, c("trend", "Trend"), default = "Unknown")
    ) %>%
      mutate(
        Species = if_else(is.na(Species) | Species == "", NA_character_, Species),
        Trend = replace_na(as.character(Trend), "Unknown")
      ) %>%
      filter(!is.na(Species), Species != "")
  } else {
    final_species_df <- tibble(
      Species = character(),
      Class = character(),
      Order = character(),
      Family = character(),
      Status = character(),
      `Common name` = character(),
      Trend = character()
    )
  }
  
  not_found_df <- species_df %>%
    anti_join(final_species_df %>% distinct(Species), by = "Species") %>%
    transmute(
      Species,
      Class = NA_character_,
      Order = NA_character_,
      Family = NA_character_,
      Status = "Not found",
      `Common name` = NA_character_,
      Trend = "Unknown"
    )
  
  bind_rows(final_species_df, not_found_df) %>%
    distinct(Species, .keep_all = TRUE)
}

# CITES
retrieve_CITES_data <- function(speciesList, wait_time = 1.0) {
  get_cites_status <- function(sp) {
    Sys.sleep(wait_time)
    
    res <- tryCatch({
      rcites::spp_taxonconcept(query_taxon = sp, raw = TRUE)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(res) || length(res) == 0 || is.null(res[[1]]) || is.null(res[[1]]$cites_listing)) {
      message(sp, " ----- SKIPPED/ERROR (not found, API error, or rate limit)")
      return(tibble(
        Species = sp,
        taxon_id = NA_character_,
        CITES_Appendix = NA_character_
      ))
    }
    
    tibble(
      Species = ifelse(is.null(res[[1]]$full_name), sp, res[[1]]$full_name),
      taxon_id = as.character(res[[1]]$id),
      CITES_Appendix = paste(unique(res[[1]]$cites_listing), collapse = "; ")
    )
  }
  
  purrr::map_dfr(speciesList, get_cites_status) %>%
    distinct(Species, .keep_all = TRUE)
}

# Load database for Indonesian protected species.
urlfile <- "https://raw.githubusercontent.com/ryanavri/GetTaxonCS/main/PSG_v3.csv" #see in the https://github.com/ryanavri/GetTaxonCS
db <- tryCatch(
  read.csv(urlfile),
  error = function(e) {
    warning("Could not load protected species database: ", conditionMessage(e))
    tibble(Species = character())
  }
)
