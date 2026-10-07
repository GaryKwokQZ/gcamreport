# Build the internal data objects required by the GCAM 9.1 reporting profile.
#
# GCAM 9.1 retains most GCAM 8.2 common-definition mappings. This script
# starts from the vetted v8.2 package data and replaces the mappings and
# constants that changed in GCAM 9.1. The resulting objects are stored in
# R/sysdata.rda so they are available to package functions without exporting
# a second copy of every mapping as a user-facing dataset.

library(magrittr)

raw_data_folder <- here::here()
data_folder <- file.path(raw_data_folder, "data")
mapping_folder <- file.path(
  raw_data_folder,
  "inst", "extdata", "mappings", "GCAM9.1"
)

gather_mapping <- function(data) {
  untouched_columns <- names(data)[!grepl("var", names(data))]

  data %>%
    tidyr::gather(identifier, var, -dplyr::all_of(untouched_columns)) %>%
    dplyr::select(-identifier) %>%
    dplyr::filter(!is.na(var), var != "")
}

load_single_object <- function(path) {
  object_environment <- new.env(parent = emptyenv())
  object_names <- load(path, envir = object_environment)

  if (length(object_names) != 1) {
    stop("Expected exactly one object in ", path)
  }

  list(
    name = object_names[[1]],
    value = object_environment[[object_names[[1]]]]
  )
}

version_objects <- new.env(parent = emptyenv())
v82_files <- list.files(
  data_folder,
  pattern = "_v8\\.2\\.rda$",
  full.names = TRUE
)

for (data_file in v82_files) {
  source_object <- load_single_object(data_file)
  target_name <- sub("_v8\\.2$", "_v9.1", source_object$name)
  version_objects[[target_name]] <- source_object$value
}

# Model label and historical calibration boundary.
version_objects[["template_v9.1"]]$Model <- "GCAM 9.1"
version_objects[["last_historical_year_v9.1"]] <- 2021

# GCAM 9.1 technology mappings carried by the cloud dev_commdef_9p1 branch.
# EV investment is deliberately not added here: it is a separate derived
# reporting extension, not one of the compatibility mappings migrated by this
# script.
version_objects[["co2_tech_map_v9.1"]] <- readr::read_csv(
  file.path(mapping_folder, "CO2_tech_map.csv"),
  comment = "#",
  na = "",
  show_col_types = FALSE
) %>% gather_mapping()

capacity_mapping <- readr::read_csv(
  file.path(mapping_folder, "capacity_map.csv"),
  comment = "#",
  show_col_types = FALSE
) %>% dplyr::filter(!grepl("cogen", technology))

version_objects[["capacity_map_v9.1"]] <- gather_mapping(capacity_mapping)
version_objects[["secondary_energy_map_v9.1"]] <- gather_mapping(capacity_mapping)

version_objects[["final_energy_map_v9.1"]] <- readr::read_csv(
  file.path(mapping_folder, "final_energy_map.csv"),
  comment = "#",
  show_col_types = FALSE
) %>% gather_mapping()

version_objects[["kyoto_sector_map_v9.1"]] <- readr::read_csv(
  file.path(mapping_folder, "kyotogas_sector.csv"),
  comment = "#",
  na = "",
  show_col_types = FALSE
) %>% gather_mapping()

version_objects[["nonco2_emis_sector_map_v9.1"]] <- readr::read_csv(
  file.path(mapping_folder, "nonCO2_emissions_sector_map.csv"),
  comment = "#",
  na = "",
  show_col_types = FALSE
) %>% gather_mapping()

version_objects[["transport_final_en_map_v9.1"]] <- readr::read_csv(
  file.path(mapping_folder, "transport_final_en_map.csv"),
  comment = "#",
  na = "",
  show_col_types = FALSE
) %>% gather_mapping()

# A policy-market input can appear in the ModelInterface transport-final-energy
# query even though it is not a physical fuel. Mark the exact flow observed in
# the GCAM 9.1 cloud workflow as NoReported so that the strict mapping check is
# retained without double-counting it as energy. Other user-created policy
# inputs remain controlled by generate_report(ignore = ...).
version_objects[["transport_final_en_map_v9.1"]] <- dplyr::bind_rows(
  version_objects[["transport_final_en_map_v9.1"]],
  tibble::tibble(
    input = "BEV-International-Aviation-Service-Floor",
    sector = "trn_aviation_intl",
    mode = "International Aviation",
    unit_conv = 1,
    var = "NoReported"
  )
) %>%
  dplyr::distinct(sector, input, mode, var, .keep_all = TRUE)

# Complete GCAM 9.1 electricity capital costs, including the 2021 model year,
# SMR, and large reactor technologies.
capital_gcam_v9.1 <- readr::read_csv(
  file.path(mapping_folder, "capital_gcam91_electricity.csv"),
  show_col_types = FALSE
) %>%
  dplyr::select(
    sector,
    subsector,
    technology,
    year,
    capital.overnight
  ) %>%
  dplyr::arrange(technology, year)

if (nrow(capital_gcam_v9.1) != 572 ||
    dplyr::n_distinct(capital_gcam_v9.1$technology) != 26) {
  stop("The GCAM 9.1 electricity capital mapping is incomplete.")
}

version_objects[["capital_gcam_v9.1"]] <- capital_gcam_v9.1

# Nuclear capacity factors for technologies introduced by GCAM 9.1 use the
# established nuclear capacity-factor assumption.
cf_gcam_v9.1 <- version_objects[["cf_gcam_v9.1"]]
nuclear_cf_template <- cf_gcam_v9.1 %>%
  dplyr::filter(technology == "Gen_II_LWR")

cf_gcam_v9.1 <- dplyr::bind_rows(
  cf_gcam_v9.1,
  nuclear_cf_template %>% dplyr::mutate(technology = "SMR"),
  nuclear_cf_template %>% dplyr::mutate(technology = "large reactor")
) %>%
  dplyr::distinct(technology, .keep_all = TRUE)

version_objects[["cf_gcam_v9.1"]] <- cf_gcam_v9.1

# Core GCAM 9.1 markets that are not IAMC final-energy price variables are
# explicitly marked NoReported. Policy-specific markets remain controlled by
# generate_report(ignore = ...), rather than being embedded in the core map.
core_unreported_markets <- c(
  "ag food service", "ag service", "Capital_Ag", "capital-ag",
  "capital-energy", "comm cooking", "comm hot water", "comm lighting",
  "comm non-building", "comm office", "comm other", "comm refrigeration",
  "comm ventilation", "elec_large reactor",
  "elec_large reactor-fixed-output", "elec_SMR", "elec_SMR-fixed-output",
  "H2 LDV", "H2 MHDV", "Labor_Ag", "Labor_Materials", "Labor_Total", "LH2"
)

residential_services <- c(
  "clothes dryers", "clothes washers", "computers", "cooking",
  "dishwashers", "freezers", "furnace fans", "hot water", "lighting",
  "other", "refrigerators", "televisions"
)

core_unreported_markets <- c(
  core_unreported_markets,
  as.vector(outer(
    residential_services,
    1:10,
    function(service, decile) {
      paste0("resid ", service, " modern_d", decile)
    }
  ))
)

energy_price_map_v9.1 <- version_objects[["energy_price_map_v9.1"]]
missing_energy_markets <- setdiff(
  core_unreported_markets,
  energy_price_map_v9.1$market
)

energy_price_map_v9.1 <- dplyr::bind_rows(
  energy_price_map_v9.1,
  tibble::tibble(
    market = missing_energy_markets,
    unit_conv = 1,
    var = "NoReported"
  )
) %>% dplyr::distinct(market, .keep_all = TRUE)

version_objects[["energy_price_map_v9.1"]] <- energy_price_map_v9.1

# Water mappings that were required by GCAM 9.1 cloud runs. Crop and biomass
# additions inherit the unambiguous sector mapping; hydrogen and nuclear use
# explicit sector classifications.
water_map_v9.1 <- version_objects[["water_map_v9.1"]]

inherited_water_keys <- tibble::tribble(
  ~sector, ~subsector,
  "Fruits", "FruitsTree_Mackenzie",
  "Legumes", "Legumes_MagdalenaR",
  "MiscCrop", "MiscCropTree_MagdalenaR",
  "OilCrop", "OilCropTree_GangesR",
  "OilCrop", "OilCropTree_Hainan",
  "OilCrop", "OilCropTree_Hong",
  "OilCrop", "OilCropTree_IrrawaddyR",
  "OilCrop", "OilCropTree_LBalkash",
  "OilCrop", "OilCropTree_Mekong",
  "OilCrop", "OilCropTree_Salween",
  "OtherGrain", "OtherGrainC4_CaspianSW",
  "SugarCrop", "SugarCropC4_Gironde",
  "biomass", "biomassTree_AusInt"
)

inherited_water_rows <- lapply(
  seq_len(nrow(inherited_water_keys)),
  function(index) {
    key <- inherited_water_keys[index, ]
    sector_mapping <- water_map_v9.1 %>%
      dplyr::filter(sector == key$sector) %>%
      dplyr::distinct(unit_conv, var)

    if (nrow(sector_mapping) == 0) {
      stop("No water mapping template for GCAM 9.1 sector: ", key$sector)
    }

    sector_mapping %>%
      dplyr::mutate(
        sector = key$sector,
        subsector = key$subsector,
        .before = 1
      )
  }
) %>% dplyr::bind_rows()

explicit_water_rows <- dplyr::bind_rows(
  tidyr::crossing(
    tibble::tribble(
      ~sector, ~subsector,
      "H2 LDV", "onsite production",
      "H2 MHDV", "onsite production",
      "H2 central production", "hybrid",
      "LH2", "onsite production"
    ),
    tibble::tribble(
      ~unit_conv, ~var,
      1, "Water XX",
      1, "Water XX|Industrial Water"
    )
  ),
  tidyr::crossing(
    tibble::tribble(
      ~sector, ~subsector,
      "elec_SMR", "SMR",
      "elec_large reactor", "large reactor"
    ),
    tibble::tribble(
      ~unit_conv, ~var,
      1, "Water XX",
      1, "Water XX|Electricity",
      1, "Water XX|Electricity|Nuclear"
    )
  )
)

water_map_v9.1 <- dplyr::bind_rows(
  water_map_v9.1,
  inherited_water_rows,
  explicit_water_rows
) %>%
  dplyr::distinct(sector, subsector, unit_conv, var)

version_objects[["water_map_v9.1"]] <- water_map_v9.1

# Persist all version-specific objects as package-internal data.
version_object_names <- ls(version_objects, all.names = TRUE)
for (object_name in version_object_names) {
  assign(object_name, version_objects[[object_name]])
}

save(
  list = version_object_names,
  file = file.path(raw_data_folder, "R", "sysdata.rda"),
  compress = "xz"
)

# Register v9.1 as a supported, decile-enabled GCAM version.
available_object <- load_single_object(
  file.path(data_folder, "available_GCAM_versions.rda")
)
available_GCAM_versions <- unique(c(available_object$value, "v9.1"))
save(
  available_GCAM_versions,
  file = file.path(data_folder, "available_GCAM_versions.rda"),
  compress = "xz"
)

deciles_object <- load_single_object(
  file.path(data_folder, "deciles_GCAM_versions.rda")
)
deciles_GCAM_versions <- unique(c(deciles_object$value, "v9.1"))
save(
  deciles_GCAM_versions,
  file = file.path(data_folder, "deciles_GCAM_versions.rda"),
  compress = "xz"
)

message(
  "Saved ", length(version_object_names),
  " GCAM 9.1 internal data objects and registered v9.1."
)
