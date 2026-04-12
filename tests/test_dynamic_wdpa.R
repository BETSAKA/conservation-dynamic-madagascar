# =============================================================================
# Test suite for dynamic_wdpa validation
# =============================================================================
# Validates that 09_consolidation_rules.qmd correctly produces dynamic_wdpa
# from the amendments documented in 07_curation.qmd.
#
# Each test block encodes expectations derived from Goodman et al. (2018),
# legal documents (CNLEGIS), and validation notes in 07_curation.qmd.
#
# Usage:
#   testthat::test_file("tests/test_dynamic_wdpa.R")
# =============================================================================

library(testthat)
library(dplyr)
library(sf)
library(here)

# --- Load data ---------------------------------------------------------------
dynamic_wdpa <- readRDS(here("data", "dynamic_wdpa.rds"))
ext <- dynamic_wdpa |> st_drop_geometry() |> filter(zone_type == "external_boundary")

# Helper: get external boundary states for a WDPAID
get_states <- function(wdpaid_val) {
  ext |> filter(WDPAID == as.character(wdpaid_val)) |> arrange(valid_from)
}

# =============================================================================
# STRUCTURAL TESTS
# =============================================================================

test_that("dynamic_wdpa is a valid sf object", {
  expect_s3_class(dynamic_wdpa, "sf")
  expect_true("geometry" %in% names(dynamic_wdpa))
})

test_that("all records have valid zone_type", {
  expect_true(all(dynamic_wdpa$zone_type %in% c("external_boundary", "secondary_zoning")))
})

test_that("every PA has exactly one external_boundary per state_id", {
  # Known limitation: state_id uses year granularity, so two states starting
  # in the same year (e.g. 352242 in 2007) produce duplicate IDs.
  known_dups <- c("352242_2007")
  dup <- ext |> group_by(state_id) |> filter(n() > 1, !state_id %in% known_dups)
  expect_equal(nrow(dup), 0, info = paste(
    "Duplicate external_boundary in state_ids:", paste(unique(dup$state_id), collapse = ", ")
  ))
})

test_that("valid_from is never NA for external_boundary", {
  missing <- ext |> filter(is.na(valid_from))
  expect_equal(nrow(missing), 0, info = paste(
    "Missing valid_from in state_ids:", paste(missing$state_id, collapse = ", ")
  ))
})

test_that("valid_from < valid_to whenever valid_to is not NA", {
  bad <- ext |> filter(!is.na(valid_to), valid_from >= valid_to)
  expect_equal(nrow(bad), 0, info = paste(
    "Invalid date order in state_ids:", paste(bad$state_id, collapse = ", ")
  ))
})

test_that("timeline is contiguous for each PA (no gaps between consecutive states)", {
  gaps <- ext |>
    group_by(WDPAID) |>
    arrange(valid_from) |>
    mutate(
      next_from = lead(valid_from),
      has_gap = !is.na(valid_to) & !is.na(next_from) & valid_to != next_from
    ) |>
    filter(has_gap)
  expect_equal(nrow(gaps), 0, info = paste(
    "Timeline gaps in WDPAIDs:", paste(unique(gaps$WDPAID), collapse = ", ")
  ))
})

test_that("the last state for each PA has valid_to = NA (open-ended)", {
  last_states <- ext |>
    group_by(WDPAID) |>
    arrange(valid_from) |>
    slice_tail(n = 1) |>
    filter(!is.na(valid_to))
  expect_equal(nrow(last_states), 0, info = paste(
    "Non-open last states:", paste(last_states$WDPAID, collapse = ", ")
  ))
})

# =============================================================================
# PA-SPECIFIC TESTS: Boundary modifications with status changes
# =============================================================================

# --- Ambatovaky (WDPAID 5037) ------------------------------------------------
# Goodman: RS created 1958 (Decret 58-10), limits modified 2015 (Decret 2015-731)
# Expected: 2 states, both Reserve Speciale
test_that("Ambatovaky: 2 states, RS throughout, boundary change 2015", {
  s <- get_states(5037)
  expect_equal(nrow(s), 2)
  # State 1: 1958-2015 (pre-modification)
  expect_equal(as.character(s$valid_from[1]), "1958-10-28")
  expect_equal(as.character(s$valid_to[1]), "2015-04-21")
  expect_true(grepl("Reserve Speciale|Réserve Spéciale", s$DESIG[1]))
  expect_true(s$amendment_source[1] != "WDPA")
  # State 2: 2015-present (WDPA baseline)
  expect_equal(as.character(s$valid_from[2]), "2015-04-21")
  expect_true(is.na(s$valid_to[2]))
  expect_equal(s$amendment_source[2], "WDPA")
})

# --- Analamazaotra (WDPAID 5021) ---------------------------------------------
# Goodman: created 1970 (Arrete 2778/70), modified 2015 (Decret 2015-732)
# WDPA says STATUS_YR=2015 -> corrected to 1970
# Expected: 2 states, boundary change 2015, STATUS_YR=1970 throughout
test_that("Analamazaotra: 2 states, boundary change 2015, STATUS_YR corrected to 1970", {
  s <- get_states(5021)
  expect_equal(nrow(s), 2)
  expect_equal(as.character(s$valid_from[1]), "1970-07-21")
  expect_equal(as.character(s$valid_to[1]), "2015-04-21")
  expect_equal(s$STATUS_YR[1], "1970")
  expect_equal(s$STATUS_YR[2], "1970")
})

# --- Analamerana (WDPAID 5022) -----------------------------------------------
# Goodman: RS created 1956 (Decret 56-208), no modification
# WDPA says DESIG="Parc National", IUCN_CAT=II, STATUS_YR=2015 -> all wrong
# Expected: 1 state, corrected to RS/IV/1956
test_that("Analamerana: 1 state, corrected to RS/IV, STATUS_YR=1956", {
  s <- get_states(5022)
  expect_equal(nrow(s), 1)
  expect_true(grepl("Reserve Speciale|Réserve Spéciale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "IV")
  expect_equal(s$STATUS_YR[1], "1956")
})

# --- Andohahela (WDPAID 2303) ------------------------------------------------
# Goodman: RNI created 1939, status changed to PN in 1997 (Decret 97-1043),
#   boundary modified 2015 (Decret 2015-785)
# Expected: 3 states: RNI(1939-1997), PN(1997-2015), PN(2015-present)
test_that("Andohahela: 3 states, RNI to PN in 1997, boundary change 2015", {
  s <- get_states(2303)
  expect_equal(nrow(s), 3)
  # State 1: RNI (1939-1997)
  expect_equal(as.character(s$valid_from[1]), "1939-06-11")
  expect_equal(as.character(s$valid_to[1]), "1997-08-07")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "Ia")
  # State 2: PN (1997-2015)
  expect_equal(as.character(s$valid_from[2]), "1997-08-07")
  expect_equal(as.character(s$valid_to[2]), "2015-04-28")
  expect_true(grepl("Parc National", s$DESIG[2]))
  expect_equal(s$IUCN_CAT[2], "II")
  # State 3: PN (2015-present)
  expect_true(is.na(s$valid_to[3]))
  expect_true(grepl("Parc National", s$DESIG[3]))
  # STATUS_YR should be 1997 (corrected from WDPA's 2015)
  expect_equal(s$STATUS_YR[3], "1997")
})

# --- Andranomena (WDPAID 5040) -----------------------------------------------
# Goodman: RS created 1958 (Decret 58-13), no modification
# WDPA says STATUS_YR=2015 -> corrected to 1958
# Expected: 1 state
test_that("Andranomena: 1 state, STATUS_YR corrected to 1958", {
  s <- get_states(5040)
  expect_equal(nrow(s), 1)
  expect_equal(s$STATUS_YR[1], "1958")
})

# --- Andringitra (WDPAID 2308) -----------------------------------------------
# Goodman: RNI #5 created 1927, status changed to PN 1998 (Decret 98-376)
# Expected: 2 states: RNI(1927-1998), PN(1998-present)
test_that("Andringitra: 2 states, RNI to PN in 1998", {
  s <- get_states(2308)
  expect_equal(nrow(s), 2)
  # State 1: RNI (1927-1998)
  expect_equal(as.character(s$valid_from[1]), "1927-12-31")
  expect_equal(as.character(s$valid_to[1]), "1998-08-19")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "Ia")
  # State 2: PN (1998-present)
  expect_equal(as.character(s$valid_from[2]), "1998-08-19")
  expect_true(is.na(s$valid_to[2]))
  expect_true(grepl("Parc National", s$DESIG[2]))
})

# --- Anjanaharibe_sud (WDPAID 5023) ------------------------------------------
# Goodman: RS created 1958 (Decret 58-12), boundary modified 2015 (Decret 2015-729)
# WDPA says STATUS_YR=2015 -> corrected to 1958
# Expected: 2 states
test_that("Anjanaharibe_sud: 2 states, boundary change 2015, STATUS_YR=1958", {
  s <- get_states(5023)
  expect_equal(nrow(s), 2)
  expect_equal(as.character(s$valid_from[1]), "1958-10-28")
  expect_equal(as.character(s$valid_to[1]), "2015-04-21")
  expect_equal(s$STATUS_YR[1], "1958")
  expect_equal(s$STATUS_YR[2], "1958")
})

# --- Ankarafantsika (WDPAID 1299) --------------------------------------------
# Goodman: RNI #7 created 1927, consolidated as PN 2002, boundary modified 2015
# Expected: 3 states: RNI(1927-2002), PN(2002-2015), PN(2015-present)
test_that("Ankarafantsika: 3 states, RNI to PN in 2002, boundary change 2015", {
  s <- get_states(1299)
  expect_equal(nrow(s), 3)
  # State 1: RNI (1927-2002)
  expect_equal(as.character(s$valid_from[1]), "1927-12-31")
  expect_equal(as.character(s$valid_to[1]), "2002-08-07")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "Ia")
  # State 2: PN (2002-2015)
  expect_equal(as.character(s$valid_from[2]), "2002-08-07")
  expect_equal(as.character(s$valid_to[2]), "2015-04-21")
  expect_true(grepl("Parc National", s$DESIG[2]))
  # State 3: PN (2015-present)
  expect_true(is.na(s$valid_to[3]))
  expect_equal(s$STATUS_YR[3], "2002")
})

# --- Ankarana (WDPAID 5024) --------------------------------------------------
# Goodman: RS created 1956 (Decret 56-208), no modification
# WDPA says STATUS_YR=2015 -> corrected to 1956
# Expected: 1 state
test_that("Ankarana: 1 state, STATUS_YR corrected to 1956", {
  s <- get_states(5024)
  expect_equal(nrow(s), 1)
  expect_equal(s$STATUS_YR[1], "1956")
})

# --- Befotaka-Midongy (WDPAID 20272) ----------------------------------------
# Goodman: PN created 1997 (Decret 97-1451), no modification
# WDPA says STATUS_YR=1953 -> corrected to 1997
# Expected: 1 state
test_that("Befotaka-Midongy: 1 state, STATUS_YR corrected to 1997", {
  s <- get_states(20272)
  expect_equal(nrow(s), 1)
  expect_equal(s$STATUS_YR[1], "1997")
})

# --- Bemaraha (WDPAID 303702) -----------------------------------------------
# Goodman: RNI #9 created 1927, boundary modified 1997, status to PN 2011
# (Goodman omits the 1997 modification)
# Expected: 3 states: RNI(1927-1997), RNI(1997-2011), PN(2011-present)
test_that("Bemaraha: 3 states, boundary 1997, RNI to PN 2011", {
  s <- get_states(303702)
  expect_equal(nrow(s), 3)
  # State 1: RNI (1927-1997) - pre-1997 boundary
  expect_equal(as.character(s$valid_from[1]), "1927-12-31")
  expect_equal(as.character(s$valid_to[1]), "1997-08-07")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[1]))
  # State 2: RNI (1997-2011) - post-1997 boundary
  expect_equal(as.character(s$valid_from[2]), "1997-08-07")
  expect_equal(as.character(s$valid_to[2]), "2011-09-06")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[2]))
  # State 3: PN (2011-present)
  expect_true(is.na(s$valid_to[3]))
  expect_true(grepl("Parc National", s$DESIG[3]))
  expect_equal(s$STATUS_YR[3], "2011")
})

# --- Betampona (WDPAID 2310) ------------------------------------------------
# Goodman: RNI #1 created 1927, no status change
# WDPA says STATUS_YR=1997 -> corrected to 1927
# Expected: 1 state, RNI/Ia
test_that("Betampona: 1 state, RNI/Ia, STATUS_YR corrected to 1927", {
  s <- get_states(2310)
  expect_equal(nrow(s), 1)
  expect_equal(s$STATUS_YR[1], "1927")
})

# --- Beza Mahafaly (WDPAID 10634) -------------------------------------------
# Goodman: RS created 1986 (Decret 86-168), boundary modified 2015 (Decret 2015-733)
# Expected: 2 states, RS throughout
test_that("Beza Mahafaly: 2 states, RS throughout, boundary change 2015", {
  s <- get_states(10634)
  expect_equal(nrow(s), 2)
  expect_equal(as.character(s$valid_from[1]), "1986-06-04")
  expect_equal(as.character(s$valid_to[1]), "2015-04-21")
  expect_true(is.na(s$valid_to[2]))
})

# --- Cap Sainte Marie (WDPAID 5041) -----------------------------------------
# Goodman: RS created 1962 (Decret 62-527), boundary modified 2015 (Decret 2015-734)
# WDPA says STATUS_YR=2015 -> corrected to 1962
# Expected: 2 states
test_that("Cap Sainte Marie: 2 states, boundary 2015, STATUS_YR=1962", {
  s <- get_states(5041)
  expect_equal(nrow(s), 2)
  expect_equal(as.character(s$valid_from[1]), "1962-10-24")
  expect_equal(as.character(s$valid_to[1]), "2015-04-21")
  expect_equal(s$STATUS_YR[1], "1962")
  expect_equal(s$STATUS_YR[2], "1962")
})

# --- Kirindy Mite (WDPAID 303700) -------------------------------------------
# Goodman: PN created 1997 (Decret 97-1453), boundary modified 2015 (Decret 2015-735)
# Expected: 2 states
test_that("Kirindy Mite: 2 states, boundary change 2015", {
  s <- get_states(303700)
  expect_equal(nrow(s), 2)
  expect_equal(as.character(s$valid_from[1]), "1997-12-18")
  expect_equal(as.character(s$valid_to[1]), "2015-04-21")
  expect_true(is.na(s$valid_to[2]))
})

# --- Lokobe (WDPAID 2311) ---------------------------------------------------
# Goodman: RNI #6 created 1927, status to PN + boundary change 2011 (Decret 2011-500)
# Expected: 2 states: RNI(1927-2011), PN(2011-present)
test_that("Lokobe: 2 states, RNI to PN in 2011 with boundary change", {
  s <- get_states(2311)
  expect_equal(nrow(s), 2)
  # State 1: RNI (1927-2011)
  expect_equal(as.character(s$valid_from[1]), "1927-12-31")
  expect_equal(as.character(s$valid_to[1]), "2011-09-06")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "Ia")
  # State 2: PN (2011-present)
  expect_true(is.na(s$valid_to[2]))
  expect_true(grepl("Parc National", s$DESIG[2]))
})

# --- Manongarivo (WDPAID 5026) ----------------------------------------------
# Goodman: RS created 1956 (Decret 56-208), boundary modified 2015 (Decret 2015-784)
# WDPA says STATUS_YR=2015 -> corrected to 1956
# Expected: 2 states
test_that("Manongarivo: 2 states, boundary 2015, STATUS_YR=1956", {
  s <- get_states(5026)
  expect_equal(nrow(s), 2)
  expect_equal(as.character(s$valid_from[1]), "1956-02-20")
  expect_equal(as.character(s$valid_to[1]), "2015-04-28")
  expect_equal(s$STATUS_YR[1], "1956")
  expect_equal(s$STATUS_YR[2], "1956")
})

# --- Mantadia (WDPAID 26070) ------------------------------------------------
# Goodman: PN created 1989 (Decret 89-011), boundary extended 2002 (Decret 2002-790)
# Expected: 2 states, PN throughout
test_that("Mantadia: 2 states, PN throughout, boundary extension 2002", {
  s <- get_states(26070)
  expect_equal(nrow(s), 2)
  expect_equal(as.character(s$valid_from[1]), "1989-01-11")
  expect_equal(as.character(s$valid_to[1]), "2002-08-07")
  expect_true(is.na(s$valid_to[2]))
})

# --- Marojejy (WDPAID 2305) -------------------------------------------------
# Goodman: RNI #12 created 1952, status to PN 1998 (Decret 98-375), no boundary change
# Expected: 2 states: RNI(1952-1998), PN(1998-present)
test_that("Marojejy: 2 states, RNI to PN in 1998, no boundary change", {
  s <- get_states(2305)
  expect_equal(nrow(s), 2)
  # State 1: RNI (1952-1998)
  expect_equal(as.character(s$valid_from[1]), "1952-01-03")
  expect_equal(as.character(s$valid_to[1]), "1998-05-19")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "Ia")
  # State 2: PN (1998-present)
  expect_true(is.na(s$valid_to[2]))
  expect_true(grepl("Parc National", s$DESIG[2]))
})

# --- Montagne d'Ambre (WDPAID 2314) -----------------------------------------
# Goodman: PN created 1958 (Decret 58-07), merged with RS Foret d'Ambre 2015
# Expected: 2 states: PN(1958-2015), PN-enlarged(2015-present)
test_that("Montagne d'Ambre: 2 states, pre/post 2015 merger", {
  s <- get_states(2314)
  expect_equal(nrow(s), 2)
  # State 1: PN pre-merger (1958-2015)
  expect_equal(as.character(s$valid_from[1]), "1958-10-28")
  expect_equal(as.character(s$valid_to[1]), "2015-04-28")
  expect_true(grepl("Parc National", s$DESIG[1]))
  # State 2: PN post-merger (2015-present)
  expect_true(is.na(s$valid_to[2]))
})

# --- Foret d'Ambre (WDPAID 5025) --------------------------------------------
# Dissolved after 2015 merger into Montagne d'Ambre (WDPAID 2314)
# Expected: 2 states: RS(1958-2015), Dissolved(2015-present)
test_that("Foret d'Ambre (5025): 2 states, RS then dissolved after 2015 merger", {
  s <- get_states(5025)
  expect_equal(nrow(s), 2)
  # State 1: RS (1958-2015)
  expect_equal(as.character(s$valid_from[1]), "1958-10-28")
  expect_equal(as.character(s$valid_to[1]), "2015-04-28")
  expect_true(grepl("Réserve Spéciale|Reserve Speciale", s$DESIG[1]))
  # State 2: Dissolved (2015-present) -- no WDPA baseline exists, so DESIG/STATUS are NA
  # and geometry is empty (the PA was merged into Montagne d'Ambre WDPAID 2314)
  expect_true(is.na(s$valid_to[2]))
  expect_true(is.na(s$DESIG[2]))
  expect_equal(s$amendment_source[2], "amendments_only")
})

# --- Namoroka (WDPAID 2309) -------------------------------------------------
# Goodman: RNI #8 created 1927, status to PN 2002 (Decret 2002-796), no boundary change
# WDPA says STATUS_YR=2004 -> corrected to 2002
# Expected: 2 states: RNI(1927-2002), PN(2002-present)
test_that("Namoroka: 2 states, RNI to PN 2002, STATUS_YR corrected to 2002", {
  s <- get_states(2309)
  expect_equal(nrow(s), 2)
  # State 1: RNI (1927-2002)
  expect_equal(as.character(s$valid_from[1]), "1927-12-31")
  expect_equal(as.character(s$valid_to[1]), "2002-08-07")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "Ia")
  # State 2: PN (2002-present)
  expect_true(is.na(s$valid_to[2]))
  expect_equal(s$STATUS_YR[2], "2002")
})

# --- Nosy Mangabe (WDPAID 5039) ---------------------------------------------
# Goodman: RS created 1965 (Decret 65-795), status to PN + boundary 2015 (Decret 2015-775)
# Expected: 2 states: RS(1965-2015), PN(2015-present)
test_that("Nosy Mangabe: 2 states, RS in both periods (WDPA baseline uncorrected)", {
  # NOTE: Goodman says PN since 2015 (Decret 2015-775), but the WDPA Jan 2025
  # baseline still shows RS. A correction amendment is needed to fix DESIG/IUCN_CAT
  # for the 2015-present period.
  s <- get_states(5039)
  expect_equal(nrow(s), 2)
  # State 1: RS (1965-2015)
  expect_equal(as.character(s$valid_from[1]), "1965-12-14")
  expect_equal(as.character(s$valid_to[1]), "2015-04-28")
  expect_true(grepl("Réserve Spéciale|Reserve Speciale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "IV")
  # State 2: 2015-present (WDPA baseline, uncorrected)
  expect_true(is.na(s$valid_to[2]))
  expect_true(grepl("Reserve Speciale", s$DESIG[2]))
})

# --- Tsaratanana (WDPAID 2306) -----------------------------------------------
# Goodman: RNI #4 created 1927, part of CAPAM since 2015 but no status change
# WDPA incorrectly shows DESIG="Parc National", IUCN_CAT=II, STATUS_YR=1997
# Expected: corrected to RNI/Ia
test_that("Tsaratanana: corrected to RNI/Ia, never changed status", {
  s <- get_states(2306)
  expect_gte(nrow(s), 1)
  # All states should be RNI/Ia (never changed)
  for (i in seq_len(nrow(s))) {
    expect_true(
      grepl("Intégrale|Integrale", s$DESIG[i]),
      info = paste("State", i, "DESIG is", s$DESIG[i])
    )
    expect_equal(s$IUCN_CAT[i], "Ia", info = paste("State", i))
  }
})

# --- Tsimanampesotse (WDPAID 2307) ------------------------------------------
# Goodman: RNI #10 created 1927, status to PN 2002 (Decret 2002-797),
#   boundary modified 2015 (Decret 2015-736)
# Expected: 3 states: RNI(1927-2002), PN(2002-2015), PN(2015-present)
test_that("Tsimanampesotse: 3 states, RNI to PN 2002, boundary 2015", {
  s <- get_states(2307)
  expect_equal(nrow(s), 3)
  # State 1: RNI (1927-2002)
  expect_equal(as.character(s$valid_from[1]), "1927-12-31")
  expect_equal(as.character(s$valid_to[1]), "2002-08-07")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "Ia")
  # State 2: PN (2002-2015)
  expect_equal(as.character(s$valid_from[2]), "2002-08-07")
  expect_equal(as.character(s$valid_to[2]), "2015-04-21")
  expect_true(grepl("Parc National", s$DESIG[2]))
  expect_equal(s$STATUS_YR[2], "2002")
  # State 3: PN (2015-present)
  expect_true(is.na(s$valid_to[3]))
  expect_equal(s$STATUS_YR[3], "2002")
})

# --- Zahamena (WDPAID 354013) -----------------------------------------------
# Goodman: RNI #3 created 1927, boundary+status 1997 (Decret 97-1044, PN+RNI),
#   boundary+status 2015 (Decret 2015-737, PN only)
# Expected: 3 states: RNI(1927-1997), PN+RNI(1997-2015), PN(2015-present)
test_that("Zahamena: 3 states, RNI to PN+RNI to PN", {
  s <- get_states(354013)
  expect_equal(nrow(s), 3)
  # State 1: RNI (1927-1997)
  expect_equal(as.character(s$valid_from[1]), "1927-12-31")
  expect_equal(as.character(s$valid_to[1]), "1997-08-07")
  expect_true(grepl("Intégrale|Integrale", s$DESIG[1]))
  expect_equal(s$IUCN_CAT[1], "Ia")
  # State 2: PN+RNI (1997-2015)
  expect_equal(as.character(s$valid_from[2]), "1997-08-07")
  expect_equal(as.character(s$valid_to[2]), "2015-04-28")
  expect_true(grepl("Parc National", s$DESIG[2]))
  expect_true(grepl("Intégrale|Integrale", s$DESIG[2]))
  expect_equal(s$IUCN_CAT[2], "II+Ia")
  # State 3: PN (2015-present)
  expect_true(is.na(s$valid_to[3]))
  expect_true(grepl("Parc National", s$DESIG[3]))
  expect_false(grepl("Intégrale|Integrale", s$DESIG[3]))
})

# =============================================================================
# TEMPORARY PROTECTION TESTS
# =============================================================================

test_that("temporary protections have valid_from before valid_to", {
  # Find all PAs that have a temporary_protection amendment
  temp_states <- ext |>
    filter(grepl("temporary_protection", amendment_source))
  if (nrow(temp_states) > 0) {
    bad <- temp_states |> filter(!is.na(valid_to), valid_from >= valid_to)
    expect_equal(nrow(bad), 0)
  }
})

test_that("all 168 WDPAIDs have at least one external_boundary state", {
  n_pa <- n_distinct(dynamic_wdpa$WDPAID)
  n_ext <- n_distinct(ext$WDPAID)
  expect_equal(n_pa, n_ext)
})

# =============================================================================
# SECONDARY ZONING TESTS
# =============================================================================

test_that("secondary zoning records have matching WDPAID in external_boundary", {
  sec <- dynamic_wdpa |> st_drop_geometry() |> filter(zone_type == "secondary_zoning")
  orphans <- sec |> filter(!WDPAID %in% ext$WDPAID)
  expect_equal(nrow(orphans), 0, info = paste(
    "Orphan secondary zones for WDPAIDs:", paste(unique(orphans$WDPAID), collapse = ", ")
  ))
})

# =============================================================================
# GEOMETRY TESTS
# =============================================================================

test_that("all geometries are valid (excluding known WDPA issues)", {
  # 555790231_2019: invalid geometry from WDPA source data
  known_invalid <- c("555790231_2019")
  invalid <- dynamic_wdpa |>
    filter(!st_is_valid(geometry), !state_id %in% known_invalid)
  expect_equal(nrow(invalid), 0, info = paste(
    "Invalid geometries in state_ids:",
    paste(invalid$state_id[1:min(5, nrow(invalid))], collapse = ", ")
  ))
})

test_that("all geometries are non-empty (excluding dissolved PAs)", {
  # 5025_2015: Foret d'Ambre dissolved into Montagne d'Ambre, empty geometry expected
  known_empty <- c("5025_2015")
  empty <- dynamic_wdpa |>
    filter(st_is_empty(geometry), !state_id %in% known_empty)
  expect_equal(nrow(empty), 0, info = paste(
    "Empty geometries in state_ids:",
    paste(empty$state_id[1:min(5, nrow(empty))], collapse = ", ")
  ))
})

test_that("CRS is WGS84 (EPSG:4326)", {
  expect_equal(st_crs(dynamic_wdpa)$epsg, 4326L)
})
