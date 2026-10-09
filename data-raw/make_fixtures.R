# Converts the reference outputs in inst/validation/ into the .rds fixtures
# read by the tests. Run from the package root after regenerating any JSON:
#   Rscript data-raw/make_fixtures.R
# Requires jsonlite (development only; not a package dependency).

fixtures <- c(
  hand_calc_output = "hand_calc_output.json",
  python_chainladder_reference = "python_chainladder_reference.json",
  python_mack_nperiods = "python_chainladder_mack_nperiods.json"
)

for (nm in names(fixtures)) {
  ref <- jsonlite::fromJSON(file.path("inst", "validation", fixtures[[nm]]))
  saveRDS(ref, file.path("tests", "testthat", "fixtures", paste0(nm, ".rds")))
}
