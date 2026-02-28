## data-raw/make_enrichment.R
## Creates the `enrichment` dataset for the aggtreat package.
##
## Source: Childhood Development Supplement (CDS) of the Panel Study of
##   Income Dynamics (PSID), 2019 wave. Sub-sample of low-SES children used
##   in Caetano, Caetano, Callaway, and Dyal (2025), Section 5.
##
## This script requires the file `replication_files/002_DataEstimation.dta`
## (not included in the package due to data redistribution restrictions).
## Researchers who wish to reproduce the results from scratch should obtain
## the data from the PSID and apply the construction steps described in
## Supplementary Appendix SB.1 of Caetano, Caetano, Callaway, and Dyal (2025).

library(haven)

## --------------------------------------------------------------------------
## Load raw data
## --------------------------------------------------------------------------
raw_path <- here::here("replication_files", "002_DataEstimation.dta")
if (!file.exists(raw_path)) {
  raw_path <- file.path(
    dirname(dirname(rstudioapi::getSourceEditorContext()$path)),
    "replication_files", "002_DataEstimation.dta"
  )
}

df <- haven::read_dta(raw_path)

## --------------------------------------------------------------------------
## Step 1: Drop NAs in outcome and sub-treatment variables
## --------------------------------------------------------------------------
df <- df[!is.na(df$noncog) &
           !is.na(df$XX1_lessons) &
           !is.na(df$XX1_sports_struct) &
           !is.na(df$XX1_volunteer) &
           !is.na(df$XX1_BAschool), ]
# n = 5740

## --------------------------------------------------------------------------
## Step 2: Filter to low-SES children in 2019
## --------------------------------------------------------------------------
Q1_ses <- as.numeric(quantile(df$ses)[2])  # 25th percentile
df <- df[df$year == 2019 & df$ses < Q1_ses, ]
# n = 1341 initially, then n = 214 after further steps below

## --------------------------------------------------------------------------
## Step 3: Round sub-treatments to nearest half-hour
## --------------------------------------------------------------------------
round_half <- function(x) round(x * 2) / 2

## Apply two manual adjustments from the replication script to ensure D=3
## is supported in the discrete treatment (observations 115 and 116 in the
## ordered data, after filtering to low-SES 2019).
df_ordered <- df[order(df$X1_enrich), ]  # order by enrichment for stable indexing

## Identify the rows and set lessons to 2.5h
## (This matches Derek Dyal's replication script lines 110-111)
row115 <- rownames(df_ordered)[115]
row116 <- rownames(df_ordered)[116]
df[row115, "XX1_lessons"] <- 2.5
df[row116, "XX1_lessons"] <- 2.5

S1 <- round_half(df$XX1_lessons)        # Lessons
S2 <- round_half(df$XX1_sports_struct)  # Structured sports
S3 <- round_half(df$XX1_volunteer)      # Volunteering
S4 <- round_half(df$XX1_BAschool)       # Before & After school

D <- S1 + S2 + S3 + S4  # Discrete aggregated treatment
Y <- df$noncog           # Noncognitive skill index (mean 0, SD 1)

## --------------------------------------------------------------------------
## Step 4: Assemble and restrict to D <= 3 (paper's main sample)
## --------------------------------------------------------------------------
enrichment <- data.frame(
  noncog    = Y,
  D         = D,
  S_lessons = S1,
  S_sports  = S2,
  S_volunteer = S3,
  S_BAschool  = S4,
  row.names = NULL
)

## Filter to D in {0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0}: n = 214
enrichment <- enrichment[enrichment$D <= 3.0, ]
stopifnot(nrow(enrichment) == 214)

cat("Sample size:", nrow(enrichment), "\n")
cat("D distribution:\n")
print(table(enrichment$D))
cat("Mean noncog:", round(mean(enrichment$noncog), 4), "\n")
cat("SD noncog:", round(sd(enrichment$noncog), 4), "\n")

## --------------------------------------------------------------------------
## Step 5: Save as package data
## --------------------------------------------------------------------------
usethis::use_data(enrichment, overwrite = TRUE)
cat("Saved data/enrichment.rda\n")
