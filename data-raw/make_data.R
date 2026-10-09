# Builds the package datasets. Run from the package root:
#   Rscript data-raw/make_data.R
# Requires the package source to be loadable (uses as_triangle()).

pkgload_ok <- requireNamespace("pkgload", quietly = TRUE)
if (pkgload_ok) pkgload::load_all(".", export_all = TRUE, quiet = TRUE) else library(reservekit)

read_tri <- function(path, origin_labels) {
  d <- utils::read.csv(path, check.names = FALSE)
  m <- as.matrix(d[, -1])
  as_triangle(unname(m), type = "cumulative", measure = "incurred",
              origin_labels = origin_labels, dev_ages = 1:10)
}

# Published triangles (public domain data from academic literature).
raa <- read_tri("data-raw/raa.csv", 1981:1990)
taylor_ashe <- read_tri("data-raw/genins.csv", 1:10)

# Synthetic claim-level data. NOT real insurer experience.
set.seed(20261008)
years <- 2015:2024
valuation_year <- 2024
premium <- round(seq(10e6, 14.5e6, length.out = length(years)) * exp(rnorm(length(years), 0, 0.03)))
pay_pattern <- c(0.18, 0.27, 0.19, 0.12, 0.08, 0.06, 0.04, 0.03, 0.02, 0.01)
rows <- list()
id <- 0L
for (k in seq_along(years)) {
  ay <- years[k]
  n_claims <- rpois(1, premium[k] / 25000)
  max_age <- valuation_year - ay + 1L
  for (c in seq_len(n_claims)) {
    id <- id + 1L
    sev <- rlnorm(1, meanlog = log(9000), sdlog = 1.1)
    ages <- seq_len(max_age)
    incr <- sev * pay_pattern[ages] * exp(rnorm(length(ages), 0, 0.5))
    # Occasional recoveries (salvage/subrogation) produce negative increments.
    rec <- runif(length(ages)) < 0.02
    incr[rec] <- -0.3 * sev * runif(sum(rec))
    keep <- runif(length(ages)) < 0.7 | ages == 1L   # not every claim moves every year
    incurred_change <- incr
    incurred_change[1] <- incurred_change[1] + sev * 0.55   # initial case reserve
    incurred_change[ages > 1] <- incurred_change[ages > 1] - sev * 0.55 * pay_pattern[ages[ages > 1]] / sum(pay_pattern[-1])
    rows[[id]] <- data.frame(claim_id = id, accident_year = ay, dev_age = ages[keep],
                             paid = round(incr[keep], 2),
                             incurred = round(incurred_change[keep], 2))
  }
}
claims_synthetic <- do.call(rbind, rows)
rownames(claims_synthetic) <- NULL
premium_synthetic <- data.frame(accident_year = years, earned_premium = premium)

save(raa, file = "data/raa.rda", compress = "xz")
save(taylor_ashe, file = "data/taylor_ashe.rda", compress = "xz")
save(claims_synthetic, file = "data/claims_synthetic.rda", compress = "xz")
save(premium_synthetic, file = "data/premium_synthetic.rda", compress = "xz")
