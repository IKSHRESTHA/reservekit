# Small validation triangle; reference values in fixtures/hand_calc_output.rds
# were computed with exact rational arithmetic by inst/validation/hand_calc.py.
small_matrix <- function() {
  matrix(c(100, 150, 180, 190,
           110, 165, 200,  NA,
           120, 186,  NA,  NA,
           130,  NA,  NA,  NA), 4, byrow = TRUE)
}

small_tri <- function() {
  as_triangle(small_matrix(), type = "cumulative", measure = "paid",
              origin_labels = 2021:2024, dev_ages = c(12, 24, 36, 48))
}

hand <- function() readRDS(testthat::test_path("fixtures", "hand_calc_output.rds"))
pyref <- function() readRDS(testthat::test_path("fixtures", "python_chainladder_reference.rds"))

long_from_matrix <- function(m, origins = seq_len(nrow(m)), ages = seq_len(ncol(m))) {
  d <- data.frame(origin = origins[row(m)], age = ages[col(m)], value = as.vector(m))
  d[!is.na(d$value), ]
}
