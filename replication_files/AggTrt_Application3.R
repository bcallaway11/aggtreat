################################################################################
## AGGREGATE TREATMENT APPLICATION III
## Caetano, Caetano, Callaway, and Dyal (2025)
##
## Author:  Derek Dyal (ddyal@uga.edu)
## Updated: 04/13/2025
################################################################################

# ==============================================================================
# HOUSEKEEPING
# ==============================================================================
library(tidyverse)
library(haven)
library(lpSolve)
library(purrr)
library(xtable)

# Set working directory to folder containing this script and 002_DataEstimation.dta
# setwd("path/to/replication_files")

# ==============================================================================
# [I] DATA LOADING AND PREPARATION
# ==============================================================================

df_estimation <- read_dta("002_DataEstimation.dta")  # 6603 x 524; hours per week

df_estimation1 <- subset(
  df_estimation,
  !is.na(cog) & !is.na(noncog) &
    !is.na(XX1_lessons) & !is.na(XX1_sports_struct) &
    !is.na(XX1_BAschool) & !is.na(XX1_volunteer)
)  # n = 5740 after dropping NAs

# Filter to low-SES children in 2019
Q1 <- as.numeric(quantile(df_estimation1$ses)[2])
df_estimation2 <- df_estimation1 %>% filter(year == 2019, ses < Q1)  # n = 1341

D2 <- df_estimation2$X1_enrich  # aggregated treatment (continuous, hours/week)
Y2 <- df_estimation2$noncog     # outcome: non-cognitive skills

# ==============================================================================
# [I.1] EDA: CONTINUOUS SUB-TREATMENT COMPOSITION
# ==============================================================================

y_lessons   <- df_estimation2$XX1_lessons
y_sports    <- df_estimation2$XX1_sports_struct
y_volunteer <- df_estimation2$XX1_volunteer
y_BAschool  <- df_estimation2$XX1_BAschool

lomodel_lessons   <- loess(y_lessons   ~ D2, family = "gaussian", degree = 1)
lomodel_sports    <- loess(y_sports    ~ D2, family = "gaussian", degree = 1)
lomodel_volunteer <- loess(y_volunteer ~ D2, family = "gaussian", degree = 1)
lomodel_BAschool  <- loess(y_BAschool  ~ D2, family = "gaussian", degree = 1)

hrs   <- 2.5
lo.D2 <- seq(min(D2), max(D2), length.out = 1000)
lo.y_lessons   <- predict(lomodel_lessons,   lo.D2)
lo.y_sports    <- predict(lomodel_sports,    lo.D2)
lo.y_volunteer <- predict(lomodel_volunteer, lo.D2)
lo.y_BAschool  <- predict(lomodel_BAschool,  lo.D2)

location_at_hrs <- which.min(abs(lo.D2 - hrs))
grid_at_hrs     <- lo.D2[location_at_hrs]
grid_hrs        <- lo.D2[1:location_at_hrs]

# Proportional slopes from origin at 0.5-hour mark
slope_lessons   <- predict(lomodel_lessons,   0.5) / 0.5
slope_sports    <- predict(lomodel_sports,    0.5) / 0.5
slope_volunteer <- predict(lomodel_volunteer, 0.5) / 0.5
slope_BAschool  <- predict(lomodel_BAschool,  0.5) / 0.5

# Plot: mean sub-treatment hours by total enrichment hours
plot(y_lessons ~ D2, col = "white", ylim = c(0, hrs), xlim = c(0, hrs),
     xlab = "", ylab = "",
     main = "Mean Enrichment Hours per Week by Sub-treatment")
mtext("Low SES Children, 2019", side = 3, line = 0.2)
mtext(expression(paste(D, " (total hours of treatment)")), side = 1, line = 2.75)
mtext(expression(paste(S[k], " (hours per sub-treatment)")), side = 2, line = 2.75)
polygon(c(0, 0, hrs), c(0, hrs, hrs), col = "grey", border = "black")

lines(grid_hrs, lo.y_lessons[1:location_at_hrs],   col = "#1f77b4", lwd = 2.5)
lines(grid_hrs, lo.y_sports[1:location_at_hrs],    col = "#ff7f0e", lwd = 2.5)
lines(grid_hrs, lo.y_volunteer[1:location_at_hrs], col = "#2ca02c", lwd = 2.5)
lines(grid_hrs, lo.y_BAschool[1:location_at_hrs],  col = "#d62728", lwd = 2.5)

segments(x0 = 0.05, y0 = predict(lomodel_lessons,   0.05),
         x1 = grid_at_hrs, y1 = slope_lessons   * grid_at_hrs, col = "#1f77b4", lwd = 1, lty = 2)
segments(x0 = 0.05, y0 = predict(lomodel_sports,    0.05),
         x1 = grid_at_hrs, y1 = slope_sports    * grid_at_hrs, col = "#ff7f0e", lwd = 1, lty = 2)
segments(x0 = 0.05, y0 = predict(lomodel_volunteer, 0.05),
         x1 = grid_at_hrs, y1 = slope_volunteer * grid_at_hrs, col = "#2ca02c", lwd = 1, lty = 2)
segments(x0 = 0.05, y0 = predict(lomodel_BAschool,  0.05),
         x1 = grid_at_hrs, y1 = slope_BAschool  * grid_at_hrs, col = "#d62728", lwd = 1, lty = 2)

for (x in seq(0.5, 8, 0.5)) segments(x0 = x, y0 = 0, x1 = x, y1 = x, col = "grey", lwd = 1)
segments(x0 = 0, y0 = 0, x1 = grid_at_hrs, y1 = 0, col = "black", lwd = 1)

legend(x = 0.25, y = hrs - 0.25, title = "Sub-treatment:",
       legend = c("Lessons", "Sports", "Volunteer", "B&A School"),
       lty = c(1, 1, 1, 1), col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
       cex = 0.65, lwd = 2.5)

# ==============================================================================
# [I.2] DISCRETIZATION
# ==============================================================================

round_half <- function(x) round(x * 2) / 2

# Two observations adjusted to ensure D=3 is supported in the discrete treatment
df_estimation2$XX1_lessons[115] <- 2.5
df_estimation2$XX1_lessons[116] <- 2.5

S1 <- round_half(df_estimation2$XX1_lessons)
S2 <- round_half(df_estimation2$XX1_sports_struct)
S3 <- round_half(df_estimation2$XX1_volunteer)
S4 <- round_half(df_estimation2$XX1_BAschool)

D2_tilde <- S1 + S2 + S3 + S4  # discretized aggregated treatment
Y2       <- df_estimation2$noncog
n2       <- length(Y2)

# Conditional sub-treatment means by D level
D_grid <- seq(0, 3, 0.5)
cmeans1 <- cmeans2 <- cmeans3 <- cmeans4 <- numeric(length(D_grid))
for (i in seq_along(D_grid)) {
  d_val    <- D_grid[i]
  cmeans1[i] <- mean(S1[D2_tilde == d_val])
  cmeans2[i] <- mean(S2[D2_tilde == d_val])
  cmeans3[i] <- mean(S3[D2_tilde == d_val])
  cmeans4[i] <- mean(S4[D2_tilde == d_val])
}

# ==============================================================================
# [I.3] DISCRETE SUB-TREATMENT COMPOSITION PLOT
# ==============================================================================

hrs <- 3.0
location_at_hrs <- which.min(abs(lo.D2 - hrs))
grid_at_hrs     <- lo.D2[location_at_hrs]

plot(y_lessons ~ D2, col = "white", ylim = c(0, hrs), xlim = c(0, hrs),
     xlab = "", ylab = "",
     main = "Mean Enrichment Hours per Week by Sub-treatment")
mtext("Low SES Children, 2019", side = 3, line = 0.2)
mtext(expression(paste(D, " (total hours of treatment)")), side = 1, line = 2.75)
mtext(expression(paste(S[k], " (hours per sub-treatment)")), side = 2, line = 2.75)
polygon(c(0, 0, hrs), c(0, hrs, hrs), col = "grey", border = "black")

for (x in seq(0.5, 8, 0.5)) segments(x0 = x, y0 = 0, x1 = x, y1 = x, col = "grey", lwd = 1)
segments(x0 = 0, y0 = 0, x1 = grid_at_hrs, y1 = 0, col = "black", lwd = 1)

lines( D_grid, cmeans1, col = "#1f77b4", lty = 1, lwd = 2.5)
points(D_grid, cmeans1, col = "#1f77b4", pch = 16, cex = 1.5)
lines( D_grid, cmeans2, col = "#ff7f0e", lty = 1, lwd = 2.5)
points(D_grid, cmeans2, col = "#ff7f0e", pch = 15, cex = 1.5)
lines( D_grid, cmeans3, col = "#2ca02c", lty = 1, lwd = 2.5)
points(D_grid, cmeans3, col = "#2ca02c", pch = 17, cex = 1.5)
lines( D_grid, cmeans4, col = "#d62728", lty = 1, lwd = 2.5)
points(D_grid, cmeans4, col = "#d62728", pch = 18, cex = 1.5)

legend(x = 0.25, y = hrs - 0.25, title = "Sub-treatment:",
       legend = c("Lessons", "Sports", "Volunteer", "B&A School"),
       lty = 1, pch = c(16, 15, 17, 18),
       col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
       cex = 0.8, lwd = 2.5)

# ==============================================================================
# [I.4] DISCRETE TREATMENT DISTRIBUTION PLOTS
# ==============================================================================

df_tilde <- as.data.frame(cbind(Y2, D2_tilde, S1, S2, S3, S4))

ggplot(df_tilde, aes(x = D2_tilde)) +
  geom_bar(aes(y = ..prop.., group = 1), stat = "count",
           color = "black", fill = "dodgerblue", alpha = 0.7) +
  annotate("segment", x = 0, xend = round(max(df_tilde$D2_tilde)),
           y = 0, yend = 0, linetype = "solid", color = "black", size = 0.5) +
  labs(x = "D (hours per week)", y = "Probability, P(D = d)",
       title = "Empirical PMF: Hours of Enrichment Activity",
       subtitle = "Low SES (2019)") +
  theme_bw()

Q1_D2 <- as.numeric(quantile(D2_tilde)[2])
Q2_D2 <- as.numeric(quantile(D2_tilde)[3])
Q3_D2 <- as.numeric(quantile(D2_tilde)[4])
quartile_pts <- data.frame(quartile = c(Q1_D2, Q2_D2, Q3_D2),
                           Fd = c(0.25, 0.5, 0.75),
                           labelz = c("Q1", "Q2", "Q3"))

ggplot(as.data.frame(D2_tilde), aes(x = D2_tilde)) +
  stat_ecdf(geom = "step", color = "black", size = 0.55) +
  geom_point(data = quartile_pts, aes(x = quartile, y = Fd, color = labelz), size = 4) +
  scale_color_manual(values = c("Q3" = "steelblue4", "Q2" = "steelblue3", "Q1" = "steelblue2"),
                     limits = c("Q3", "Q2", "Q1")) +
  theme(legend.position = c(0.86, 0.15)) +
  labs(x = "D", y = "Cumulative Probability, P(D \u2264 d)",
       title = "Empirical CDF: Hours of Enrichment Activity",
       subtitle = "Low SES (2019)", color = "Quartile:") +
  theme_bw()

# ==============================================================================
# [II.1] REGRESSION
# ==============================================================================

lin_model <- lm(Y2 ~ D2_tilde)
alpha2    <- as.numeric(lin_model$coefficients[2])
round(alpha2, 3)  # -0.056

# Bootstrap SE (B = 1000)
set.seed(15)
B <- 1000
boot_reg <- numeric(B)
for (b in 1:B) {
  idx        <- sample(1:n2, size = n2, replace = TRUE)
  boot_reg[b] <- coef(lm(Y2[idx] ~ D2_tilde[idx]))[2]
}
se_regression <- sd(boot_reg)
print(se_regression)

# ==============================================================================
# [II.2] CONGRUENT AGGREGATE MARGINAL PARAMETER: E[AMATT+(D) | D>0]
# ==============================================================================

df_tilde <- as.data.frame(cbind(Y2, D2_tilde, S1, S2, S3, S4))
df_tilde <- df_tilde %>%
  mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))

# Mean Y by sub-treatment vector
means_by_S <- df_tilde %>%
  group_by(across(starts_with("S"))) %>%
  summarise(D = unique(D2_tilde), mean_Y = mean(Y2), n = n(), .groups = "drop")

# Build S matrix and identify congruent pairs (L1-distance = 0.5, |D_i - D_j| = 0.5)
S_matrix <- means_by_S %>% select(starts_with("S")) %>% as.data.frame()
rownames(S_matrix) <- means_by_S$S_key
S_matrix <- S_matrix %>% select(-S_key)

index_pairs <- combn(nrow(S_matrix), 2, simplify = FALSE)
adj_pairs <- keep(index_pairs, function(pair) {
  i <- pair[1]; j <- pair[2]
  (sum(abs(means_by_S[i, 1:4] - means_by_S[j, 1:4])) == 0.5) &
    (abs(means_by_S$D[i] - means_by_S$D[j]) == 0.5)
})

# Compute delta_Y and product weights for each congruent pair
compute_results <- function(adj_pairs, means_by_S) {
  map_dfr(adj_pairs, function(pair) {
    i <- pair[1]; j <- pair[2]
    row_i <- means_by_S[i, ]; row_j <- means_by_S[j, ]
    D_i <- means_by_S$D[i];   D_j <- means_by_S$D[j]
    P_i <- row_i$n / sum(means_by_S$n[means_by_S$D == D_i])
    P_j <- row_j$n / sum(means_by_S$n[means_by_S$D == D_j])
    if (row_i$D == row_j$D) return(NULL)
    if (row_i$D > row_j$D) {
      tibble(D_from = row_j$D, D_to = row_i$D,
             from_S = row_j$S_key, to_S = row_i$S_key,
             delta_Y = row_i$mean_Y - row_j$mean_Y, wts_product = P_i * P_j)
    } else {
      tibble(D_from = row_i$D, D_to = row_j$D,
             from_S = row_i$S_key, to_S = row_j$S_key,
             delta_Y = row_j$mean_Y - row_i$mean_Y, wts_product = P_i * P_j)
    }
  })
}
results <- compute_results(adj_pairs, means_by_S)

# Normalize product weights within each D group (scaled product weights, Eq. 7)
D_idx_cong <- sort(unique(results$D_to))  # support with congruent comparisons
denoms     <- as.numeric(tapply(results$wts_product, results$D_to, sum))
wts_prod_scaled <- numeric(nrow(results))
for (j in 1:nrow(results)) {
  wts_prod_scaled[j] <- results$wts_product[j] / denoms[which(D_idx_cong == results$D_to[j])]
}
results <- cbind(results, wts_prod_scaled)

# Weight by P(D=d | D>0) to get overall E[AMATT+(D) | D>0]
D_idx_full <- seq(0.5, max(D2_tilde), 0.5)  # full positive support
P_greater0 <- 1 - (means_by_S$n[means_by_S$D == 0] / sum(means_by_S$n))
PD_greater0 <- numeric(length(D_idx_full))
for (k in seq_along(D_idx_full)) {
  PD_greater0[k] <- (sum(means_by_S$n[means_by_S$D == D_idx_full[k]]) /
                       sum(means_by_S$n)) / P_greater0
}
wts_prod_scaled2 <- numeric(nrow(results))
for (j in 1:nrow(results)) {
  wts_prod_scaled2[j] <- wts_prod_scaled[j] *
    PD_greater0[which(D_idx_full == results$D_to[j])]
}
results   <- cbind(results, wts_prod_scaled2)
OMATTilde <- sum(results$delta_Y * wts_prod_scaled2)
OMATTilde  # E[AMATT+(D) | D>0] = -0.087

# d-specific AMATT+(d) point estimates
original_supp_length <- length(D_idx_cong)
D_idx_margin         <- c(0, D_idx_cong)
matt_tilde_d         <- numeric(original_supp_length)
for (i in seq_len(original_supp_length)) {
  df_m <- results[results$D_from == D_idx_margin[i] & results$D_to == D_idx_margin[i + 1], ]
  matt_tilde_d[i] <- sum(df_m$wts_product * df_m$delta_Y) / sum(df_m$wts_product)
}

# --------------------------------------------------
# Bootstrap: d-specific AMATT+(d) percentile 95% CIs
# Stratified by D level; keeps only iterations with full support
# --------------------------------------------------
set.seed(117)
B    <- 1000
boot_MATTildePlus_estimates <- matrix(NA, nrow = B, ncol = original_supp_length)

compute_amatt_from_df <- function(df_boot_in) {
  df_b <- df_boot_in %>%
    mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))
  mbs <- df_b %>%
    group_by(across(starts_with("S"))) %>%
    summarise(D = unique(D2_tilde), mean_Y = mean(Y2), n = n(), .groups = "drop")
  Sm <- mbs %>% select(starts_with("S")) %>% as.data.frame()
  rownames(Sm) <- mbs$S_key
  Sm <- Sm %>% select(-S_key)
  ip <- combn(nrow(Sm), 2, simplify = FALSE)
  ap <- keep(ip, function(pair) {
    i <- pair[1]; j <- pair[2]
    (sum(abs(mbs[i, 1:4] - mbs[j, 1:4])) == 0.5) & (abs(mbs$D[i] - mbs$D[j]) == 0.5)
  })
  res <- compute_results(ap, mbs)
  if (nrow(res) == 0) return(list(matt = NULL, overall = NA))
  D_idx_b  <- sort(unique(res$D_to))
  denom_b  <- as.numeric(tapply(res$wts_product, res$D_to, sum))
  wts_b    <- numeric(nrow(res))
  for (j in 1:nrow(res)) wts_b[j] <- res$wts_product[j] / denom_b[which(D_idx_b == res$D_to[j])]
  res <- cbind(res, wts_prod_scaled = wts_b)
  P_gt0 <- 1 - (mbs$n[mbs$D == 0] / sum(mbs$n))
  PD_gt0 <- numeric(length(D_idx_full))
  for (k in seq_along(D_idx_full)) {
    PD_gt0[k] <- (sum(mbs$n[mbs$D == D_idx_full[k]]) / sum(mbs$n)) / P_gt0
  }
  wts_b2 <- numeric(nrow(res))
  for (j in 1:nrow(res)) {
    wts_b2[j] <- res$wts_prod_scaled[j] * PD_gt0[which(D_idx_full == res$D_to[j])]
  }
  res <- cbind(res, wts_prod_scaled2 = wts_b2)
  list(results = res, D_idx_b = D_idx_b)
}

b <- 1
while (b <= B) {
  df_boot <- do.call(rbind, lapply(
    split(df_tilde, df_tilde$D2_tilde),
    function(sub) sub[sample(nrow(sub), replace = TRUE), ]
  ))
  rownames(df_boot) <- NULL
  out <- compute_amatt_from_df(df_boot)
  if (!is.null(out$results) &&
      length(c(0, out$D_idx_b)) == (original_supp_length + 1)) {
    D_idx_mb <- c(0, out$D_idx_b)
    matt_b   <- numeric(original_supp_length)
    for (i in seq_len(original_supp_length)) {
      df_m <- out$results[out$results$D_from == D_idx_mb[i] &
                            out$results$D_to == D_idx_mb[i + 1], ]
      matt_b[i] <- sum(df_m$wts_product * df_m$delta_Y) / sum(df_m$wts_product)
    }
    boot_MATTildePlus_estimates[b, ] <- matt_b
    b <- b + 1
  }
}
percCI_MATTildePlus <- t(apply(boot_MATTildePlus_estimates, 2,
                                quantile, probs = c(0.025, 0.975)))
colnames(percCI_MATTildePlus) <- c("lower95", "upper95")

# --------------------------------------------------
# Bootstrap: overall E[AMATT+(D) | D>0] SE
# --------------------------------------------------
set.seed(444)
B <- 1000
boot_MATTilde_estimates <- numeric(B)
for (b in 1:B) {
  idx     <- sample(1:n2, size = n2, replace = TRUE)
  df_boot <- df_tilde[idx, ]
  out     <- compute_amatt_from_df(df_boot)
  if (!is.null(out$results)) {
    boot_MATTilde_estimates[b] <- sum(out$results$delta_Y * out$results$wts_prod_scaled2)
  }
}
se_MATTilde <- sd(boot_MATTilde_estimates)
print(se_MATTilde)

# ==============================================================================
# [II.3] AGGREGATE ATT: AATT(d) = E[Y|D=d] - E[Y|D=0]
# ==============================================================================

df_tilde <- as.data.frame(cbind(Y2, D2_tilde, S1, S2, S3, S4))

# D3 = D2_tilde without collapsing (used for AATT point estimates and d-specific CIs)
D3       <- D2_tilde
D3_idx0  <- sort(unique(D3))   # full support including 0
D3_idx   <- D3_idx0[-1]        # positive support (7 values: 0.5 to 3.5)

cond_means <- as.numeric(tapply(Y2, D3, mean))
aatt       <- cond_means[-1] - cond_means[1]  # AATT(d) for d in D3_idx

# Overall E[AATT(D) | D>0]
D3_wts  <- as.numeric(table(D3)) / length(D3)  # P(D=d)
D_wts3  <- D3_wts[-1] / (1 - D3_wts[1])         # P(D=d | D>0)
OAATT   <- sum(D_wts3 * aatt)
print(round(OAATT, 3))  # -0.167

# Bootstrap percentile 95% CIs for AATT(d) (pair-wise sampling)
set.seed(117)
B    <- 10000
boot_AATT_estimates <- matrix(NA, nrow = B, ncol = length(D3_idx))
for (b in 1:B) {
  for (d in seq_along(D3_idx)) {
    trt_idx  <- sample(which(D3 == D3_idx[d]), replace = TRUE)
    ctrl_idx <- sample(which(D3 == 0), replace = TRUE)
    boot_AATT_estimates[b, d] <- mean(df_tilde$Y2[trt_idx]) -
      mean(df_tilde$Y2[ctrl_idx])
  }
}
percCI_AATT <- t(apply(boot_AATT_estimates, 2, quantile, probs = c(0.025, 0.975)))

# Bootstrap SE for overall E[AATT(D) | D>0]
set.seed(117)
B <- 1000
boot_EDATT_estimates <- numeric(B)
for (b in 1:B) {
  idx         <- sample(1:n2, size = n2, replace = TRUE)
  X_b         <- df_tilde$D2_tilde[idx]
  y_b         <- df_tilde$Y2[idx]
  mv_b        <- as.numeric(tapply(y_b, X_b, mean))
  wts_b       <- as.numeric(table(X_b)) / length(X_b)
  wts_b2      <- wts_b[-1] / (1 - wts_b[1])
  boot_EDATT_estimates[b] <- sum(wts_b2 * (mv_b[-1] - mv_b[1]))
}
se_Edatt <- sd(boot_EDATT_estimates)
print(round(se_Edatt, 3))

# Plot: AATT(d) with 95% percentile CIs (inverted axes)
transparent_green <- rgb(0, 100/255, 0, alpha = 0.3)

plot(aatt, D3_idx, col = "black", cex = 1.5, pch = 20,
     xlim = c(-max(abs(percCI_AATT)) - 0.1, max(abs(percCI_AATT)) + 0.1),
     ylim = c(0, 3.05),
     xlab = "", ylab = "", main = "Aggregate ATT(d) Estimates")
mtext("Low SES Children, 2019", side = 3, line = 0.2)
mtext(expression(paste(D, " (total hours of treatment)")), side = 2, line = 2.75)
mtext("Estimate", side = 1, line = 2.75)
segments(x0 = 0, y0 = 0, x1 = 0, y1 = 3.1, col = "grey", lwd = 1, lty = 2)
points(0, 0, col = "white", pch = 20, cex = 2)
points(0, 0, col = "black", pch = 1,  cex = 1.25)

segments(x0 = OAATT, y0 = 0, x1 = OAATT, y1 = 3.1,
         col = transparent_green, lwd = 1.5, lty = 3)
segments(x0 = OAATT, y0 = 0, x1 = OAATT, y1 = 3.1,
         col = transparent_green, lwd = 1,   lty = 1)
points(OAATT, 0, col = "white", pch = 20, cex = 2)

for (i in seq_along(aatt)) {
  segments(y0 = D3_idx[i], x0 = aatt[i], y1 = D3_idx[i],
           x1 = percCI_AATT[i, 2], col = "burlywood", lwd = 1.25)
  segments(y0 = D3_idx[i], x0 = aatt[i], y1 = D3_idx[i],
           x1 = percCI_AATT[i, 1], col = "burlywood", lwd = 1.25)
}
points(aatt, D3_idx, col = "black", pch = 20, cex = 1.5)

legend(x = 0.1, y = 0.5, legend = c("AATT(d)", "Overall AATT(d)"),
       lty = c(1, 1), pch = c(20, 1), col = c("black", transparent_green),
       cex = 0.95, lwd = c(NA, 1.5), pt.cex = c(1.5, NA), bty = "n")

# ==============================================================================
# [II.4.1] SCALED AGGREGATE ATT: E[AATT(D)/D | D>0]
# ==============================================================================

aatt_d     <- aatt / D3_idx  # AATT(d)/d  (7 elements, D=0.5 to 3.5)
OAATT_divD <- sum(aatt_d * D_wts3)
print(OAATT_divD)  # -0.218

# For CIs: collapse D=3.5 into D=3 to stabilize estimates near the tail
D3_coll       <- D2_tilde
D3_coll[D3_coll == 3.5] <- 3
trt_D_coll    <- sort(unique(D3_coll))[-1]  # 6 values: 0.5 to 3.0

aatt_d_coll <- (as.numeric(tapply(Y2, D3_coll, mean))[-1] -
                  as.numeric(tapply(Y2, D3_coll, mean))[1]) / trt_D_coll

# Bootstrap percentile 95% CIs for AATT(d)/d
set.seed(7)
B <- 10000
boot_AATTdivD_estimates <- matrix(NA, nrow = B, ncol = length(trt_D_coll))
for (b in 1:B) {
  for (d in seq_along(trt_D_coll)) {
    trt_idx  <- sample(which(D3_coll == trt_D_coll[d]), replace = TRUE)
    ctrl_idx <- sample(which(D3_coll == 0), replace = TRUE)
    boot_AATTdivD_estimates[b, d] <-
      (mean(df_tilde$Y2[trt_idx]) - mean(df_tilde$Y2[ctrl_idx])) / trt_D_coll[d]
  }
}
percCI_AATTdivD <- t(apply(boot_AATTdivD_estimates, 2, quantile, probs = c(0.025, 0.975)))

# Bootstrap SE for overall E[AATT(D)/D | D>0]
set.seed(8080)
B <- 500
boot_EAATTdivD_estimates <- numeric(B)
for (b in 1:B) {
  idx       <- sample(1:n2, size = n2, replace = TRUE)
  X_b       <- D3_coll[idx]
  y_b       <- df_tilde$Y2[idx]
  Dboot_idx <- sort(unique(X_b))
  mv_b      <- as.numeric(tapply(y_b, X_b, mean))
  wts_b     <- as.numeric(table(X_b)) / length(X_b)
  wts_b2    <- wts_b[-1] / (1 - wts_b[1])
  boot_EAATTdivD_estimates[b] <- sum(wts_b2 * (mv_b[-1] - mv_b[1]) / Dboot_idx[-1])
}
se_EAATTdivD <- sd(boot_EAATTdivD_estimates)
print(round(se_EAATTdivD, 3))

# Plot: AATT(d)/d with 95% percentile CIs (inverted axes)
plot(aatt_d_coll, trt_D_coll, col = "black", cex = 1.5, pch = 20,
     xlim = c(-max(abs(percCI_AATTdivD)) - 0.1, max(abs(percCI_AATTdivD)) + 0.1),
     ylim = c(0, 3.05),
     xlab = "", ylab = "", main = "Aggregate ATT(d)/d Estimates")
mtext("Low SES Children, 2019", side = 3, line = 0.2)
mtext(expression(paste(D, " (total hours of treatment)")), side = 2, line = 2.75)
mtext("Estimate", side = 1, line = 2.75)
segments(x0 = 0, y0 = 0, x1 = 0, y1 = 3.1, col = "grey", lwd = 1, lty = 2)
points(0, 0, col = "white", pch = 20, cex = 2)
points(0, 0, col = "black", pch = 1,  cex = 1.25)
lines(rep(OAATT_divD, length(trt_D_coll) + 1), c(0, trt_D_coll),
      col = "darkgreen", lwd = 1.5, lty = 5)
points(OAATT_divD, 0, col = "white", pch = 20, cex = 2)

for (i in seq_along(aatt_d_coll)) {
  segments(x0 = aatt_d_coll[i], y0 = trt_D_coll[i],
           x1 = percCI_AATTdivD[i, 2], y1 = trt_D_coll[i], col = "burlywood", lwd = 1)
  segments(x0 = aatt_d_coll[i], y0 = trt_D_coll[i],
           x1 = percCI_AATTdivD[i, 1], y1 = trt_D_coll[i], col = "burlywood", lwd = 1)
}
points(aatt_d_coll, trt_D_coll, col = "black", pch = 20, cex = 1.5)

legend(x = 0.22, y = 0.65, legend = c("AATT(d)/d", "95% CI", "Overall AATT(d)/d"),
       lty = c(1, 1, 5), pch = c(20, 1, 1),
       col = c("black", "burlywood", "darkgreen"),
       cex = 0.85, lwd = c(NA, 1.5, 1.5), pt.cex = c(1.5, NA, NA), bty = "n")

# ==============================================================================
# [II.4.2] VERIFICATION: E[DATT(S)/D | D>0] (should match E[AATT/D])
# ==============================================================================

df_tilde <- as.data.frame(cbind(Y2, D2_tilde, S1, S2, S3, S4))
df_tilde <- df_tilde %>%
  mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))

means_by_S <- df_tilde %>%
  group_by(across(starts_with("S"))) %>%
  summarise(D = unique(D2_tilde), mean_Y = mean(Y2), n = n(), .groups = "drop")

Ngrand       <- sum(means_by_S$n)
datt         <- means_by_S$mean_Y[-1] - means_by_S$mean_Y[1]
prob_SgivenD <- (means_by_S$n[-1] / Ngrand) / (1 - means_by_S$n[1] / Ngrand)

cat("E[DATT(S)   | D>0]:", round(sum(datt * prob_SgivenD), 4), "\n")           # -0.167
cat("E[DATT(S)/D | D>0]:", round(sum((datt / means_by_S$D[-1]) * prob_SgivenD), 4), "\n")  # -0.218

# ==============================================================================
# [II.5] MARGINAL COMPARISONS: delta(d) and AMATT+(d)
# ==============================================================================

# D3 without collapsing (same as D2_tilde)
D3      <- D2_tilde
df_D3   <- as.data.frame(cbind(Y2, D3))
D3_idx0 <- sort(unique(D3))
D3_idx  <- D3_idx0[-1]

# delta(d) = E[Y|D=d] - E[Y|D=d-1]
matt_d <- numeric(length(D3_idx))
for (i in seq_along(D3_idx0[-length(D3_idx0)])) {
  matt_d[i] <- mean(df_D3$Y2[df_D3$D3 == D3_idx0[i + 1]]) -
    mean(df_D3$Y2[df_D3$D3 == D3_idx0[i]])
}

# Overall E[delta(D) | D>0]
D3_wts_m <- as.numeric(table(D3)) / length(D3)
D_wts3_m <- D3_wts_m[-1] / (1 - D3_wts_m[1])
OMATT    <- sum(D_wts3_m * matt_d)
round(OMATT, 3)  # -0.040

# Bootstrap percentile 95% CIs for delta(d)
set.seed(117)
B  <- 1000
n3 <- nrow(df_D3)
boot_delta_d_estimates <- matrix(0, nrow = B, ncol = length(D3_idx))
for (b in 1:B) {
  for (i in seq_along(D3_idx0[-length(D3_idx0)])) {
    idx_d   <- sample(which(D3 == D3_idx0[i]),     replace = TRUE)
    idx_d1  <- sample(which(D3 == D3_idx0[i + 1]), replace = TRUE)
    boot_delta_d_estimates[b, i] <- mean(df_tilde$Y2[idx_d1]) - mean(df_tilde$Y2[idx_d])
  }
}
perc_CI_matt_d <- t(apply(boot_delta_d_estimates, 2, quantile, probs = c(0.025, 0.975)))
colnames(perc_CI_matt_d) <- c("lower95", "upper95")

# Bootstrap SE for overall E[delta(D) | D>0]
set.seed(55)
B <- 1000
boot_OMATT_estimates <- numeric(B)
for (b in 1:B) {
  idx      <- sample(1:n3, size = n3, replace = TRUE)
  X_b      <- D3[idx]
  y_b      <- df_tilde$Y2[idx]
  Db_idx   <- sort(unique(X_b))
  df_b     <- data.frame(y_b, X_b)
  matt_b   <- numeric(length(Db_idx) - 1)
  for (i in seq_along(Db_idx[-length(Db_idx)])) {
    matt_b[i] <- mean(df_b$y_b[df_b$X_b == Db_idx[i + 1]]) -
      mean(df_b$y_b[df_b$X_b == Db_idx[i]])
  }
  wts_b  <- as.numeric(table(X_b)) / length(X_b)
  wts_b2 <- wts_b[-1] / (1 - wts_b[1])
  boot_OMATT_estimates[b] <- sum(wts_b2 * matt_b)
}
round(sd(boot_OMATT_estimates), 3)

# Align AMATT+(d) for comparison (D=3 has no congruent comparisons => NA)
matt_tilde_d_plot <- c(matt_tilde_d, NA)  # 6 elements: D=0.5 to 3.0
# Note: D=3.5 is in D3_idx but has no AMATT+ estimate; both matt_d and matt_tilde_d_plot
# have 6 elements for D in {0.5, 1.0, 1.5, 2.0, 2.5, 3.0}.
matt_d6           <- matt_d[-length(matt_d)]  # drop D=3.5 entry for comparison
D3_idx6           <- D3_idx[-length(D3_idx)]

perc_change_deltas <- round((matt_tilde_d_plot - matt_d6) / matt_d6, 4) * 100

# Plot: delta(d) and AMATT+(d) with CIs
ylim_margin   <- 1.0
jiggle        <- 0.045
segment_color <- "grey95"

plot(D3_idx6, matt_d6, pch = 20, col = "white",
     xlim = c(-0.01, 3.01), ylim = c(-ylim_margin, ylim_margin),
     yaxt = "n", xlab = "", ylab = "", main = "Marginal Parameter Estimates")
axis(side = 2, at = seq(-ylim_margin, ylim_margin, by = 0.25))
mtext("Low SES Children, 2019", side = 3, line = 0.2)
mtext(expression(paste(D, " (total hours of treatment)")), side = 1, line = 2.75)
mtext(expression(paste(Estimate)), side = 2, line = 2.5)

for (x in seq(0, 3, 0.5)) {
  segments(x0 = x, y0 = -ylim_margin - 0.5, x1 = x, y1 = ylim_margin + 0.5,
           col = segment_color, lwd = 1)
}
lines(D3_idx0, rep(0, length(D3_idx0)), col = "grey", lty = 2)

# delta(d) with CIs
for (i in seq_along(matt_d6)) {
  segments(x0 = D3_idx6[i] - jiggle, y0 = matt_d6[i],
           x1 = D3_idx6[i] - jiggle, y1 = perc_CI_matt_d[i, 2], col = "pink", lwd = 1.25)
  segments(x0 = D3_idx6[i] - jiggle, y0 = matt_d6[i],
           x1 = D3_idx6[i] - jiggle, y1 = perc_CI_matt_d[i, 1], col = "pink", lwd = 1.25)
}
points(D3_idx6 - jiggle, matt_d6, col = "darkred", pch = 20, cex = 1.75)
points(D3_idx6 - jiggle, matt_d6, col = "white",   pch = 20, cex = 1.25)

# AMATT+(d) with CIs
for (i in seq_len(original_supp_length)) {
  segments(x0 = D_idx_cong[i] + jiggle, y0 = matt_tilde_d[i],
           x1 = D_idx_cong[i] + jiggle, y1 = percCI_MATTildePlus[i, 2], col = "skyblue", lwd = 1.25)
  segments(x0 = D_idx_cong[i] + jiggle, y0 = matt_tilde_d[i],
           x1 = D_idx_cong[i] + jiggle, y1 = percCI_MATTildePlus[i, 1], col = "skyblue", lwd = 1.25)
}
points(D_idx_cong + jiggle, matt_tilde_d, col = "darkblue", pch = 20, cex = 1.75)

# Annotations: estimates and percent changes
text(D3_idx6,       rep(-ylim_margin + 0.07, 6),
     labels = round(matt_d6, 3), pos = 3, cex = 0.8, col = "darkred")
text(D3_idx6[1:5],  rep(-ylim_margin, 5),
     labels = round(matt_tilde_d_plot[1:5], 3), pos = 3, cex = 0.8, col = "darkblue")
text(D3_idx6[6],    -ylim_margin, labels = "NA", pos = 3, cex = 0.8, col = "darkblue")
text(D3_idx6[1:2],  rep(-ylim_margin - 0.07, 2),
     labels = paste0(perc_change_deltas[1:2], "%"), pos = 3, cex = 0.8, col = "black")
text(D3_idx6[3:5],  rep(-ylim_margin - 0.07, 3),
     labels = paste0(perc_change_deltas[3:5], "%"), pos = 3, cex = 0.8, col = "purple")
text(D3_idx6[6],    -ylim_margin - 0.07, labels = "NA", pos = 3, cex = 0.8, col = "purple")

legend(x = 0, y = ylim_margin + 0.08,
       legend = expression(delta(d), AMATT^"+"*(d)),
       lty = 1, pch = 20,
       col = c("darkred", "darkblue"),
       cex = 0.95, lwd = NA, pt.cex = 1.75, bty = "n")

cat("% change in overall estimate (OMATTilde vs OMATT):",
    round((OMATTilde - OMATT) / OMATT * 100, 1), "%\n")

# ==============================================================================
# [III] MINIMALLY INCONGRUENT WEIGHT ALGORITHM (LP)
# ==============================================================================

# Generate all sub-treatment vectors summing to d with K components
generate_sd <- function(d, k) {
  if (k == 1) return(matrix(d, nrow = 1, ncol = 1))
  result <- list()
  for (i in 0:d) result[[i + 1]] <- cbind(i, generate_sd(d - i, k - 1))
  do.call(rbind, result)
}

# Build M(d), M+(d), M-(d)
compute_matrices <- function(d, K) {
  gen <- function(d, k) {
    if (k == 1) return(matrix(d, nrow = 1, ncol = 1))
    result <- list()
    for (i in 0:d) result[[i + 1]] <- cbind(i, gen(d - i, k - 1))
    do.call(rbind, result)
  }
  s_d    <- gen(d, K)
  s_dmin <- gen(d - 1, K)
  M_d    <- expand.grid(s_d_index = 1:nrow(s_d), s_d_minus_1_index = 1:nrow(s_dmin))
  M_d$pair_s_d        <- apply(M_d, 1, function(r) paste(s_d[as.numeric(r[1]), ],    collapse = ","))
  M_d$pair_s_d_minus_1 <- apply(M_d, 1, function(r) paste(s_dmin[as.numeric(r[2]), ], collapse = ","))

  is_in_M_plus <- function(sd, sdmin1) all(sd - sdmin1 >= 0) && sum(sd - sdmin1) == 1
  M_d$in_M_plus <- apply(M_d, 1, function(r) {
    is_in_M_plus(s_d[as.numeric(r["s_d_index"]), ], s_dmin[as.numeric(r["s_d_minus_1_index"]), ])
  })
  list(M = M_d, M_plus = M_d[M_d$in_M_plus, ], M_minus = M_d[!M_d$in_M_plus, ])
}

# Solve LP for minimally incongruent weights
solve_optimization <- function(d, K, psd, psdmin1) {
  mats      <- compute_matrices(d, K)
  M         <- mats$M
  num_vars  <- nrow(M)
  objective <- as.numeric(!M$in_M_plus)
  num_sd    <- length(psd); num_sdm <- length(psdmin1)
  A <- matrix(0, nrow = 1 + num_sd + num_sdm, ncol = num_vars)
  b <- c(1, psdmin1, psd)
  A[1, ] <- 1
  for (j in 1:num_sdm) A[1 + j, M$s_d_minus_1_index == j] <- 1
  for (i in 1:num_sd)  A[1 + num_sdm + i, M$s_d_index == i] <- 1
  result <- lp("min", objective, A, rep("=", nrow(A)), b)
  if (result$status == 0) {
    M$weight             <- result$solution
    M$incongruent_weight <- (!M$in_M_plus) * M$weight
    return(M)
  }
  message("No feasible solution at d = ", d); return(NULL)
}

# --------------------------------------------------
# Collect P(S=s | D=d) from observed data
# --------------------------------------------------
df_tilde <- as.data.frame(cbind(Y2, D2_tilde, S1, S2, S3, S4))
df_tilde <- df_tilde %>%
  mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))
means_by_S <- df_tilde %>%
  group_by(across(starts_with("S"))) %>%
  summarise(D = unique(D2_tilde), mean_Y = mean(Y2), n = n(), .groups = "drop")

S_counts      <- means_by_S[order(means_by_S$D), ]
prob_S_givenD <- c()
for (d_val in D3_idx0) {
  sub          <- S_counts[S_counts$D == d_val, ]
  prob_S_givenD <- c(prob_S_givenD, sub$n / sum(sub$n))
}
S_counts <- cbind(S_counts, prob_S_givenD)

K <- 4  # four sub-treatments

# --------------------------------------------------
# Apply LP at each treatment level
# --------------------------------------------------

# D = 0.5 (d=1)
d       <- 1
psd     <- c(0, 0, 0, 1)
psdmin1 <- c(1)
solution1 <- solve_optimization(d, K, psd, psdmin1)

# D = 1.0 (d=2)
d       <- 2
psd     <- c(0, 0, 0, 0, 0, 0, 0.06666667, 0.26666667, 0, 0.66666667)
psdmin1 <- c(0, 0, 0, 1)
solution2 <- solve_optimization(d, K, psd, psdmin1)

# D = 1.5 (d=3)
d   <- 3
sd  <- generate_sd(d, K)
psd <- integer(nrow(sd))
psd[1]  <- S_counts$prob_S_givenD[6];   psd[4]  <- S_counts$prob_S_givenD[7]
psd[10] <- S_counts$prob_S_givenD[8];   psd[11] <- S_counts$prob_S_givenD[9]
psd[13] <- S_counts$prob_S_givenD[10];  psd[15] <- S_counts$prob_S_givenD[11]
psd[20] <- S_counts$prob_S_givenD[12]
psdmin1 <- c(0, 0, 0, 0, 0, 0, 0.06666667, 0.26666667, 0, 0.66666667)
solution3 <- solve_optimization(d, K, psd, psdmin1)

# D = 2.0 (d=4)
d      <- 4
sd     <- generate_sd(d, K)
sdmin1 <- generate_sd(d - 1, K)
psd    <- integer(nrow(sd))
psd[1]  <- S_counts$prob_S_givenD[13];  psd[5]  <- S_counts$prob_S_givenD[14]
psd[15] <- S_counts$prob_S_givenD[15];  psd[19] <- S_counts$prob_S_givenD[16]
psd[35] <- S_counts$prob_S_givenD[17]
psdmin1 <- integer(nrow(sdmin1))
psdmin1[1]  <- S_counts$prob_S_givenD[6];   psdmin1[4]  <- S_counts$prob_S_givenD[7]
psdmin1[10] <- S_counts$prob_S_givenD[8];   psdmin1[11] <- S_counts$prob_S_givenD[9]
psdmin1[13] <- S_counts$prob_S_givenD[10];  psdmin1[15] <- S_counts$prob_S_givenD[11]
psdmin1[20] <- S_counts$prob_S_givenD[12]
solution4 <- solve_optimization(d, K, psd, psdmin1)

# D = 2.5 (d=5)
d      <- 5
sd     <- generate_sd(d, K)
sdmin1 <- generate_sd(d - 1, K)
psd    <- integer(nrow(sd))
psd[1]  <- S_counts$prob_S_givenD[18];  psd[22] <- S_counts$prob_S_givenD[19]
psd[26] <- S_counts$prob_S_givenD[20];  psd[27] <- S_counts$prob_S_givenD[21]
psd[40] <- S_counts$prob_S_givenD[22]
psdmin1 <- integer(nrow(sdmin1))
psdmin1[1]  <- S_counts$prob_S_givenD[13];  psdmin1[5]  <- S_counts$prob_S_givenD[14]
psdmin1[15] <- S_counts$prob_S_givenD[15];  psdmin1[19] <- S_counts$prob_S_givenD[16]
psdmin1[35] <- S_counts$prob_S_givenD[17]
solution5 <- solve_optimization(d, K, psd, psdmin1)

# D = 3.0 (d=6)
d      <- 6
sd     <- generate_sd(d, K)
sdmin1 <- generate_sd(d - 1, K)
psd    <- integer(nrow(sd))
psd[65] <- S_counts$prob_S_givenD[23];  psd[82] <- S_counts$prob_S_givenD[24]
psdmin1 <- integer(nrow(sdmin1))
psdmin1[1]  <- S_counts$prob_S_givenD[18];  psdmin1[22] <- S_counts$prob_S_givenD[19]
psdmin1[26] <- S_counts$prob_S_givenD[20];  psdmin1[27] <- S_counts$prob_S_givenD[21]
psdmin1[40] <- S_counts$prob_S_givenD[22]
solution6 <- solve_optimization(d, K, psd, psdmin1)

# --------------------------------------------------
# Summary: total minimally incongruent weight by D
# --------------------------------------------------
cat("Minimally incongruent weight by D level:\n")
cat(" D=0.5:", round(sum(solution1$incongruent_weight), 4), "\n")  # 0
cat(" D=1.0:", round(sum(solution2$incongruent_weight), 4), "\n")  # 0
cat(" D=1.5:", round(sum(solution3$incongruent_weight), 4), "\n")  # 0.375
cat(" D=2.0:", round(sum(solution4$incongruent_weight), 4), "\n")  # 0.3375
cat(" D=2.5:", round(sum(solution5$incongruent_weight), 4), "\n")  # 0.40
cat(" D=3.0:", round(sum(solution6$incongruent_weight), 4), "\n")  # 1.00

# --------------------------------------------------
# Assemble results table (Table 1 in paper)
# --------------------------------------------------
is_active <- function(sol) sol$weight > 0 | sol$incongruent_weight > 0
results_algo <- rbind(
  solution1[is_active(solution1), ],
  solution2[is_active(solution2), ],
  solution3[is_active(solution3), ],
  solution4[is_active(solution4), ],
  solution5[is_active(solution5), ],
  solution6[is_active(solution6), ]
)
results_algo <- results_algo[, -c(1, 2)]  # remove integer index columns

Dlevel <- c(
  rep(1, sum(is_active(solution1))),
  rep(2, sum(is_active(solution2))),
  rep(3, sum(is_active(solution3))),
  rep(4, sum(is_active(solution4))),
  rep(5, sum(is_active(solution5))),
  rep(6, sum(is_active(solution6)))
)
results_algo <- as.data.frame(cbind(Dlevel, results_algo))

# LaTeX table (booktabs style)
print(
  xtable(results_algo,
         caption = "Minimally Incongruent Weights by Treatment Level",
         label   = "tab:incongruent_weights",
         digits  = c(1, 0, 0, 0, 0, 4, 4)),
  include.rownames = FALSE,
  booktabs         = TRUE
)

# Also print the congruent comparison pairs with delta_Y and product weights
results_table <- as.data.frame(results)
results_table <- results_table[, c("from_S", "to_S", "D_from", "D_to",
                                    "delta_Y", "wts_prod_scaled")]
print(xtable(results_table, digits = c(1, 0, 0, 1, 1, 3, 3)))
