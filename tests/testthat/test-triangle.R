test_that("matrix input builds a triangle with metadata", {
  tri <- small_tri()
  expect_s3_class(tri, "triangle")
  expect_true(is_triangle(tri))
  expect_identical(triangle_type(tri), "cumulative")
  expect_identical(dev_ages(tri), c(12, 24, 36, 48))
  expect_identical(origins(tri), as.character(2021:2024))
  obs <- observed_mask(tri)
  expect_identical(sum(obs), 10L)
  expect_false(obs[4, 2])
})

test_that("type is required and never guessed", {
  expect_error(as_triangle(small_matrix()), "`type` must be supplied")
  d <- long_from_matrix(small_matrix())
  expect_error(as_triangle(d, "origin", "age", "value"), "`type` must be supplied")
})

test_that("long data and matrix input agree", {
  m <- small_matrix()
  d <- long_from_matrix(m, origins = 2021:2024, ages = c(12, 24, 36, 48))
  a <- as_triangle(d, origin = "origin", development = "age", value = "value",
                   type = "cumulative", measure = "paid")
  b <- small_tri()
  expect_equal(as.matrix(a), as.matrix(b))
  expect_identical(observed_mask(a), observed_mask(b))
})

test_that("rows are ordered regardless of input order", {
  d <- long_from_matrix(small_matrix(), 2021:2024, c(12, 24, 36, 48))
  d <- d[rev(seq_len(nrow(d))), ]
  a <- as_triangle(d, "origin", "age", "value", type = "cumulative")
  expect_equal(unname(as.matrix(a)), unname(small_matrix()))
})

test_that("genuine missing values are distinguished from structural NAs", {
  m <- small_matrix()
  m[2, 2] <- NA
  tri <- as_triangle(m, type = "cumulative")
  expect_true(observed_mask(tri)[2, 2])
  expect_output(print(tri), "1 genuine missing value")
  expect_error(fit_chain_ladder(tri), "genuine missing value")
  v <- validate_triangle(tri)
  expect_true("genuine_missing" %in% v$check)
  expect_identical(as.character(v$severity[v$check == "genuine_missing"]), "error")
})

test_that("missing cumulative rows in long data become genuine NA, not zero", {
  d <- long_from_matrix(small_matrix())
  d <- d[!(d$origin == 2 & d$age == 2), ]
  tri <- as_triangle(d, "origin", "age", "value", type = "cumulative")
  expect_true(is.na(as.matrix(tri)[2, 2]))
  expect_true(observed_mask(tri)[2, 2])
})

test_that("incremental transactions are summed and empty cells recorded as filled zeros", {
  d <- data.frame(ay = c(2020, 2020, 2020, 2021, 2022),
                  age = c(1, 1, 3, 1, 1),
                  amt = c(10, 5, 2, 7, 3))
  # Ages 1 and 3 only: the inferred step is 2, so age 2 is only known to
  # exist when dev_step is given (documented limitation of inference).
  expect_warning(tri <- as_triangle(d, "ay", "age", "amt", type = "incremental", dev_step = 1),
                 "1 of 3 cell\\(s\\) on the latest diagonal")
  m <- as.matrix(tri)
  expect_equal(m[1, ], c(15, 0, 2), ignore_attr = TRUE)
  expect_true(attr(tri, "filled")[1, 2])
  expect_true(attr(tri, "filled")[2, 2])
  expect_output(print(tri), "set to 0")
  expect_equal(as.matrix(to_cumulative(tri))[1, ], c(15, 15, 17), ignore_attr = TRUE)
})

test_that("NA values in incremental input are kept as NA", {
  d <- data.frame(ay = c(1, 1, 2), age = c(1, 2, 1), amt = c(10, NA, 3))
  tri <- as_triangle(d, "ay", "age", "amt", type = "incremental")
  expect_true(is.na(as.matrix(tri)[1, 2]))
  expect_false(attr(tri, "filled")[1, 2])
})

test_that("duplicated cumulative rows are refused unless aggregate = 'sum'", {
  d <- data.frame(ay = c(1, 1, 1, 2), age = c(1, 1, 2, 1), v = c(5, 6, 20, 4))
  expect_error(as_triangle(d, "ay", "age", "v", type = "cumulative"), "more than one row")
  tri <- as_triangle(d, "ay", "age", "v", type = "cumulative", aggregate = "sum")
  expect_equal(as.matrix(tri)[1, 1], 11)
  expect_error(as_triangle(d, "ay", "age", "v", type = "incremental", aggregate = "error"),
               "more than one row")
})

test_that("irregular development ages and origin gaps are handled explicitly", {
  d <- data.frame(ay = c(1, 1, 1), age = c(12, 24, 30), v = 1:3)
  expect_error(as_triangle(d, "ay", "age", "v", type = "incremental"), "regular grid")
  expect_error(as_triangle(d, "ay", "age", "v", type = "incremental", dev_step = 12),
               "regular grid")
  # An empty age inside the grid is an error unless the step is explicit.
  d2 <- data.frame(ay = c(1, 1, 1, 2), age = c(12, 24, 48, 12), v = c(1, 2, 3, 4))
  expect_error(as_triangle(d2, "ay", "age", "v", type = "incremental"), "36 with no data")
  # With the step given, the empty 36 column leaves a whole diagonal empty:
  # refused by default, accepted when the sparsity is declared genuine.
  expect_error(as_triangle(d2, "ay", "age", "v", type = "incremental", dev_step = 12),
               "check_latest")
  expect_warning(tri <- as_triangle(d2, "ay", "age", "v", type = "incremental",
                                    dev_step = 12, check_latest = FALSE), "no rows")
  expect_identical(dev_ages(tri), c(12, 24, 36, 48))
  # Same rule for numeric origins.
  d3 <- data.frame(ay = c(2017, 2017, 2018, 2018, 2020), age = c(1, 2, 1, 2, 1),
                   v = c(1, 2, 3, 4, 5))
  expect_error(as_triangle(d3, "ay", "age", "v", type = "incremental"), "origin_step")
  expect_warning(tri3 <- as_triangle(d3, "ay", "age", "v", type = "incremental",
                                     origin_step = 1), "latest diagonal")
  expect_identical(origins(tri3), c("2017", "2018", "2019", "2020"))
  expect_true(attr(tri3, "filled")[3, 1])
  expect_warning(tri3c <- as_triangle(d3, "ay", "age", "v", type = "cumulative",
                                      origin_step = 1), "latest diagonal")
  expect_true(is.na(as.matrix(tri3c)[3, 1]))
  # Numeric-like character origins are treated as numbers (gap-checked).
  expect_error(as_triangle(transform(d3, ay = as.character(ay)), "ay", "age", "v",
                           type = "incremental"), "origin_step")
  expect_error(as_triangle(transform(d3, ay = paste0("AY", ay)), "ay", "age", "v",
                           type = "incremental", origin_step = 1), "numeric origins")
})

test_that("origins beyond the valuation are rejected", {
  m <- rbind(small_matrix(), NA)
  expect_error(as_triangle(m, type = "cumulative"), "no observed development")
})

test_that("invalid inputs give informative errors", {
  d <- long_from_matrix(small_matrix())
  expect_error(as_triangle(d, "nope", "age", "value", type = "cumulative"), "not found")
  expect_error(as_triangle(d, "origin", "age", "value", type = "cumulative", measure = "x"),
               "`measure` must be one of")
  d$value <- as.character(d$value)
  expect_error(as_triangle(d, "origin", "age", "value", type = "cumulative"), "must be numeric")
  expect_error(as_triangle(d[0, ], "origin", "age", "value", type = "cumulative"), "no rows")
  expect_error(as_triangle(list(1)), "Cannot create a triangle")
  expect_error(as_triangle(matrix(NA_real_, 2, 2), type = "cumulative"), "no observed")
  expect_error(as_triangle(matrix(c(1, Inf), 1), type = "cumulative"), "infinite")
  expect_error(as_triangle(small_matrix(), type = "cumulative", dev_ages = c(1, 1, 2, 3)),
               "strictly increasing")
  expect_error(as_triangle(small_matrix(), type = "cumulative", origin_labels = 1:3),
               "one label per row")
})

test_that("factor, character and Date origins are supported", {
  d <- data.frame(o = factor(c("b", "b", "a"), levels = c("b", "a")), age = c(1, 2, 1), v = 1:3)
  tri <- as_triangle(d, "o", "age", "v", type = "incremental")
  expect_identical(origins(tri), c("b", "a"))
  d$o <- as.Date(c("2020-01-01", "2020-01-01", "2021-01-01"))
  tri <- as_triangle(d, "o", "age", "v", type = "incremental")
  expect_identical(origins(tri), c("2020-01-01", "2021-01-01"))
  d$o <- c("x", "x", "y")
  expect_identical(origins(as_triangle(d, "o", "age", "v", type = "incremental")), c("x", "y"))
})

test_that("non-square triangles are supported", {
  # More origins than ages: older origins fully developed.
  m <- matrix(c(10, 20, 11, 22, 12, 24, 13, NA), 4, byrow = TRUE)
  tri <- as_triangle(m, type = "cumulative")
  expect_identical(sum(observed_mask(tri)), 7L)
  expect_equal(latest_diagonal(tri)$dev_index, c(2, 2, 2, 1))
  # Trapezoid: more ages than origins.
  m2 <- matrix(c(10, 20, 25, 26, 11, 22, 27, NA), 2, byrow = TRUE)
  tri2 <- as_triangle(m2, type = "cumulative")
  expect_equal(latest_diagonal(tri2)$dev_index, c(4, 3))
})

test_that("cumulative/incremental round trip is exact", {
  inc <- to_incremental(raa)
  expect_identical(triangle_type(inc), "incremental")
  expect_equal(to_cumulative(inc), raa)
  expect_identical(to_cumulative(raa), raa)
  expect_identical(to_incremental(inc), inc)
  expect_equal(as.matrix(inc)[2, 7], 15496 - 15599)
})

test_that("a genuine missing increment propagates to later cumulative values", {
  m <- matrix(c(1, NA, 1, 1, 1, NA, 1, NA, NA), 3, byrow = TRUE)
  inc <- as_triangle(m, type = "incremental")
  cum <- as.matrix(to_cumulative(inc))
  expect_true(is.na(cum[1, 2]) && is.na(cum[1, 3]))
  expect_equal(cum[2, 2], 2)
})

test_that("coercion, printing and subsetting behave", {
  df <- as.data.frame(raa)
  expect_identical(nrow(df), 55L)
  expect_named(df, c("origin", "dev_age", "value", "observed", "filled"))
  expect_identical(nrow(as.data.frame(raa, future = TRUE)), 100L)
  expect_output(print(raa), "cumulative incurred: 10 origin x 10")
  sub <- raa[1:2, ]
  expect_false(inherits(sub, "triangle"))
  expect_equal(dim(sub), c(2L, 10L))
  expect_identical(as_triangle(raa), raa)
  expect_equal(raa[5], as.matrix(raa)[5])  # vector-style indexing
  expect_equal(raa[2, 3], 5396)
  expect_equal(raa[, 1], as.matrix(raa)[, 1])
  expect_error(triangle_type(1), "must be a `triangle`")
})

test_that("single-column triangle works but has no link ratios", {
  tri <- as_triangle(matrix(c(1, 2, 3), 3), type = "cumulative")
  expect_error(link_ratios(tri), "single development age")
  expect_true("single_development" %in% validate_triangle(tri)$check)
})

test_that("edge-case inputs are validated", {
  d <- long_from_matrix(small_matrix())
  expect_error(as_triangle(d, c("origin", "age"), "age", "value", type = "cumulative"),
               "single column name")
  d2 <- d
  d2$value[1] <- Inf
  expect_error(as_triangle(d2, "origin", "age", "value", type = "cumulative"), "infinite")
  expect_error(as_triangle(matrix("a"), type = "cumulative"), "must be numeric")
  expect_error(as_triangle(matrix(numeric(), 0, 0), type = "cumulative"), "at least one row")
  expect_error(as_triangle(small_matrix(), type = "cumulative", origin_labels = c(1, 1, 2, 3)),
               "unique")
  one_age <- data.frame(ay = 1:3, age = 12, v = c(5, 6, 7))
  tri <- as_triangle(one_age, "ay", "age", "v", type = "incremental")
  expect_identical(dim(tri), c(3L, 1L))
  expect_equal(as.matrix(to_cumulative(tri))[, 1], c(5, 6, 7), ignore_attr = TRUE)
  expect_error(dev_factors(tri), "single development age")
  expect_error(calendar_weights(tri, 0.5), "single development age")
  one_origin <- data.frame(ay = 2020, age = 1:3, v = c(5, 6, 7))
  expect_identical(origins(as_triangle(one_origin, "ay", "age", "v", type = "incremental")),
                   "2020")
})

test_that("measure labels cover every measure", {
  for (m in c("reported_count", "closed_count", "other")) {
    tri <- as_triangle(as.matrix(raa), type = "cumulative", measure = m)
    expect_output(print(fit_chain_ladder(tri)), "Total")
  }
})

test_that("stray rows that move the inferred valuation are refused (review finding 1)", {
  d2 <- rbind(claims_synthetic,
              data.frame(claim_id = 99999, accident_year = 2024, dev_age = 2, paid = 10,
                         incurred = 0))
  expect_error(as_triangle(d2, "accident_year", "dev_age", "paid", type = "incremental",
                           dev_step = 1), "stray row")
})

test_that("origin ordering and gaps are checked for every origin type", {
  # Numeric-like strings sort numerically, not alphabetically.
  d <- data.frame(o = as.character(1:10), age = 1, v = 1)
  expect_identical(origins(as_triangle(d, "o", "age", "v", type = "incremental")),
                   as.character(1:10))
  # Unused factor levels are refused rather than silently dropped.
  f <- data.frame(o = factor(c("a", "c"), levels = c("a", "b", "c")), age = 1, v = 1)
  expect_error(as_triangle(f, "o", "age", "v", type = "incremental"), "have no data")
  # A gap in Date origins is refused.
  dd <- data.frame(o = as.Date(c("2019-01-01", "2020-01-01", "2022-01-01")), age = 1, v = 1)
  expect_error(as_triangle(dd, "o", "age", "v", type = "incremental"), "not evenly spaced")
  # Month-length variation is fine.
  dm <- data.frame(o = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01", "2024-04-01")),
                   age = 1, v = 1)
  expect_identical(nrow(as_triangle(dm, "o", "age", "v", type = "incremental")), 4L)
})

test_that("future cells exported as 0 trigger a warning", {
  m <- as.matrix(to_incremental(raa))
  m[is.na(m)] <- 0
  expect_warning(as_triangle(m, type = "incremental"), "replace them with NA")
  d <- as.data.frame(suppressWarnings(as_triangle(m, type = "incremental")))
  expect_warning(as_triangle(d, "origin", "dev_age", "value", type = "incremental"),
                 "replace them with NA")
})
