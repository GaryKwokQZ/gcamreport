test_that("GCAM 9.1 is registered as a native reporting profile", {
  expect_true("v9.1" %in% available_GCAM_versions)
  expect_true("v9.1" %in% deciles_GCAM_versions)

  template <- getFromNamespace("template_v9.1", "gcamreport")
  expect_true(all(template$Model == "GCAM 9.1"))
  expect_equal(getFromNamespace("last_historical_year_v9.1", "gcamreport"), 2021)
})

test_that("GCAM 9.1 electricity capital data are complete", {
  capital <- getFromNamespace("capital_gcam_v9.1", "gcamreport")

  expect_equal(nrow(capital), 572)
  expect_equal(dplyr::n_distinct(capital$technology), 26)
  expect_equal(sum(capital$year == 2021), 26)
  expect_true(all(c("Gen_II_LWR", "SMR", "large reactor") %in% capital$technology))
})

test_that("GCAM 9.1 compatibility mappings contain cloud fixes", {
  energy_prices <- getFromNamespace("energy_price_map_v9.1", "gcamreport")
  water <- getFromNamespace("water_map_v9.1", "gcamreport")
  transport <- getFromNamespace("transport_final_en_map_v9.1", "gcamreport")

  expect_true(all(c("elec_SMR", "elec_large reactor") %in% energy_prices$market))

  expect_true(any(
    water$sector == "elec_SMR" &
      water$subsector == "SMR" &
      water$var == "Water XX|Electricity|Nuclear"
  ))
  expect_true(any(
    water$sector == "elec_large reactor" &
      water$subsector == "large reactor" &
      water$var == "Water XX|Electricity|Nuclear"
  ))

  expect_true(any(
    transport$sector == "trn_aviation_intl" &
      transport$input == "BEV-International-Aviation-Service-Floor" &
      transport$mode == "International Aviation" &
      transport$var == "NoReported"
  ))
})
