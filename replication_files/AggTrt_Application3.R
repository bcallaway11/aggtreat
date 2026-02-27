#######################################################################################
#######################################################################################
#######################################################################################
## ------------------------------------- ------------------------------------- ----- ##
## AGGREGATE TREATMENT APPLICATION III                                               ##
## ------------------------------------- ------------------------------------- ----- ##
## Author: Derek Payton Dyal                                                         ##
## Contact: ddyal@uga.edu                                                            ##
## Date: 04/13/2025                                                                  ##
## ------------------------------------- ------------------------------------- ----- ##
#######################################################################################
#######################################################################################
#######################################################################################

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# HOUSEKEEPING
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
install.packages("tidyverse")
install.packages("lpSolve") # linear programming solver. 
#install.packages("latex2exp")

library(tidyverse)
library(haven) # for read_dta stata files. 
library(lpSolve) # linear programming solver. 
#library(latex2exp)
#?lp
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================

# ########################################################################################## #
# ########################################################################################## #
# ########################################################################################## #
#                                                                                            #
# [I] APPLICATION: Time-Use Data                                                             #
#                                                                                            #
# ########################################################################################## #
# ########################################################################################## #
# ########################################################################################## #

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# Time-Use Data:
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
getwd()
setwd("/Users/derekdyal/Desktop/Search/Callaway_Projects/AggregateTreatment_Project/Application-TimeUse/Caetano_Greg")

# -----------------------------
# 002_DataEstimation: 
# (***) Use this data set for models. Outcomes: "noncog" & "cog".
# -----------------------------
df_estimation <- read_dta("002_DataEstimation.dta") # *data is in hours per week.
dim(df_estimation) # 6603x524
#names(df_estimation)
df_estimation1 <- subset(df_estimation, !is.na(cog) & !is.na(noncog) 
                         & !is.na(XX1_lessons) & !is.na(XX1_sports_struct) 
                         & !is.na(XX1_BAschool) & !is.na(XX1_volunteer)) # drop NA's, 5740x524.
dim(df_estimation1) # n=5740
# ---------
# Remember: 
# ---------
# this is panel data. 
# ---------

# ---------
# Note 1: 
# ---------
# "X1_enrich" is enrichment variable. This consists of all the "XX1_" variables:
# (1) XX1_lessons
# (2) XX1_sports_struct
# (3) XX1_BAschool
# (4) XX1_volunteer
# ---------

# -----------------------------

# -----------------------------
# Data Cleaning & Preperation: 
# -----------------------------
D <- df_estimation1$X1_enrich # aggregated treatment. 

# NOTE: If we want to trim the curve, we need to remove observations where data are 
# sparse. Recommendation is truncating btw 4 to 6 hours due to noise (low sample size) 
# near largest D. 
Q1 <- as.numeric(quantile(df_estimation1$ses)[2]) # first quartile of socioeconomic status.
Q3 <- as.numeric(quantile(df_estimation1$ses)[3]) # first quartile of socioeconomic status.
Q3
sort(unique(df_estimation1$year)) # panel years.
df_estimation2 <- df_estimation1 %>% filter(#Grade=="H" & 
  year==2019 &
    #ChildMale==0 & 
    #HomeSchool==0 & 
    #PrivateSchool==0 &
    #ChildSpecEdu==0 & 
    #Qses1==1 #& 
    ses<Q1 #&
  #ses>Q3 #&
  #Qses2==0 & 
  #Qses3==0 #& 
  #MomMarital==1 &
  #ChildBlack==1 &
  #ChildWhite==1 & 
  #HasSiblings==1
) # data by co-variate. 
dim(df_estimation2) # n=1341 observations. 
D2 <- df_estimation2$X1_enrich # aggregated treatment.
#D2 <- round_half(D2)
Y2 <- df_estimation2$noncog # non-cognitive skills, outcome.
#sum(df_estimation1$ChildMale==0) + sum(df_estimation1$ChildMale==1)
#sum(df_estimation1$dum_ChildMale==0)
# -----------------------------

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================



# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# Exploratory Data Analysis & Plots for Aggregated Treatment (Enrichment Activities):
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================

# ---------------------------------------------------------------------------------
# Non-parametric Regression w/ Covariates:
# ---------------------------------------------------------------------------------
# (*) If we want to trim the curve, we need to remove observations where data are 
# sparse. Recommendation is truncating btw 4 to 6 hours due to noise (low sample size) 
# near largest D. 

# --------------------------------
# ST_1: XX1_lessons
# --------------------------------
# LOESS Option: local polynomial fit - Gaussian/Uniform weights
y_lessons <- df_estimation2$XX1_lessons
lomodel_lessons <- loess(y_lessons~D2, family = "gaussian", degree = 1)
?loess

# prediction: 
lo.D2 <- seq(min(D2), max(D2), length.out=1000)
lo.y_lessons <- predict(lomodel_lessons,lo.D2)
# --------------------------------

# --------------------------------
# ST_2: XX1_sports_struct
# --------------------------------
# LOESS Option: local polynomial fit - Gaussian/Uniform weights
y_sports <- df_estimation2$XX1_sports_struct
lomodel_sports <- loess(y_sports~D2, family = "gaussian", degree = 1)

# prediction: 
lo.D2 <- seq(min(D2), max(D2), length.out=1000)
lo.y_sports <- predict(lomodel_sports,lo.D2)
# --------------------------------

# --------------------------------
# ST_3: XX1_BAschool
# --------------------------------
# LOESS Option: local polynomial fit - Gaussian/Uniform weights
y_BAschool <- df_estimation2$XX1_BAschool
lomodel_BAschool <- loess(y_BAschool~D2, family = "gaussian", degree = 1)

# prediction: 
lo.D2 <- seq(min(D2), max(D2), length.out=1000)
lo.y_BAschool <- predict(lomodel_BAschool,lo.D2)
# --------------------------------

# --------------------------------
# ST_4: XX1_volunteer
# --------------------------------
# LOESS Option: local polynomial fit - Gaussian/Uniform weights
y_volunteer <- df_estimation2$XX1_volunteer
lomodel_volunteer <- loess(y_volunteer~D2, family = "gaussian", degree = 1)

# prediction: 
lo.D2 <- seq(min(D2), max(D2), length.out=1000)
lo.y_volunteer <- predict(lomodel_volunteer,lo.D2)
# --------------------------------

# ---------------------------------------------------------------------------------

# --------------------------------
# Level of Treatment: proportion to baseline, FULL 0-4 HOURS, saved 650x650.
# --------------------------------
hrs <- 2.5 # number of max hours of enrichment. 
plot(y_lessons~D2, col="white", cex=0.2, ylim=c(0,hrs), xlim=c(0,hrs),
     #xlab= expression(paste(D, " (total hours of treatment)")), 
     xlab = "",
     #ylab= expression(paste(S[k], " (hours per sub-treatment)")), 
     ylab = "",
     main="Mean Enrichment Hours per Week by Sub-treatment")
mtext("Low SES Children, 2019", 
      side = 3,           # Top margin
      line = 0.2,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(D, " (total hours of treatment)")), 
      side = 1,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(S[k], " (hours per sub-treatment)")), 
      side = 2,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color

# TRIANGLE:
# Define the x and y coordinates of the triangle vertices
x_coords <- c(0.0, 0.0, hrs)  # X coordinates of the triangle's vertices
y_coords <- c(0.0, hrs, hrs)  # Y coordinates of the triangle's vertices
# Draw the grey triangle
polygon(x_coords, y_coords, col = "grey", border = "black")

# Legend 1: with counter-factual proportional lines
legend(x=0.25, y=(hrs-0.25), title = "Sub-treatment:", 
       legend = c("Lessons", " ", "Sports", " ","Volunteer", " ","B&A School", ""), 
       lty = c(1,2,1,2,1,2,1,2), 
       col = c("#1f77b4","#1f77b4", 
               "#ff7f0e", "#ff7f0e",
               "#2ca02c","#2ca02c",
               "#d62728", "#d62728"),
       cex=0.65, lwd=c(2.5,1,2.5,1,2.5,1,2.5,1), pt.cex = 4)
# Legend 2: no counter-factual proportional lines
legend(x=0.25, y=(hrs-0.25), title = "Sub-treatment:", 
       legend = c("Lessons", "Sports", "Volunteer", "B&A School"), 
       lty = c(1,1,1,1), 
       col = c("#1f77b4",
               "#ff7f0e",
               "#2ca02c",
               "#d62728"),
       cex=0.65, lwd=c(2.5,2.5,2.5,2.5), pt.cex = 4)

# Alternative 1:
#lines(lo.D[1:491], lo.y_lessons[1:491]+lo.y_sports[1:491]+lo.y_volunteer[1:491]+lo.y_BAschool[1:491],
#      col="#1f77b4", lw=2.5)
#lines(lo.D[1:491], lo.y_sports[1:491]+lo.y_volunteer[1:491]+lo.y_BAschool[1:491], col="#ff7f0e", lw=2.5)
#lines(lo.D[1:491], lo.y_volunteer[1:491]+lo.y_BAschool[1:491], col="#2ca02c", lw=2.5)
#lines(lo.D[1:491], lo.y_BAschool[1:491], col="#d62728", lw=2.5)

# (***)  Finding 4 hours through the new D2 grid:
location_at4hrs <- which.min(abs(lo.D2 - hrs)) # grid coordinate position of max-hrs. 
grid_at4hrs <- lo.D2[location_at4hrs] # value of grid at 4hrs.
grid_4hrs <- (lo.D2[1:location_at4hrs]) # grid values until 4hrs. 

# Alternative 2:
lines(grid_4hrs, lo.y_lessons[1:location_at4hrs],
      col="#1f77b4", lw=2.5)
lines(grid_4hrs, lo.y_sports[1:location_at4hrs], col="#ff7f0e", lw=2.5)
lines(grid_4hrs, lo.y_volunteer[1:location_at4hrs], col="#2ca02c", lw=2.5)
lines(grid_4hrs, lo.y_BAschool[1:location_at4hrs], col="#d62728", lw=2.5)
#points(D2, cex=0.2, col="grey")

#abline(a=0,b=1,col="black",lty=1) # 45 degree line. 
segments(x0 = 0.5, y0 = 0, x1 = 0.5, y1 = 0.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 1.0, y0 = 0, x1 = 1.0, y1 = 1, col = "grey", lwd = 1, lty=1)
segments(x0 = 1.5, y0 = 0, x1 = 1.5, y1 = 1.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 2.0, y0 = 0, x1 = 2.0, y1 = 2, col = "grey", lwd = 1, lty=1)
segments(x0 = 2.5, y0 = 0, x1 = 2.5, y1 = 2.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 3.0, y0 = 0, x1 = 3.0, y1 = 3, col = "grey", lwd = 1, lty=1)
segments(x0 = 3.5, y0 = 0, x1 = 3.5, y1 = 3.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 4.0, y0 = 0, x1 = 4.0, y1 = 4, col = "grey", lwd = 1, lty=1)
segments(x0 = 4.5, y0 = 0, x1 = 4.5, y1 = 4.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 5.0, y0 = 0, x1 = 5.0, y1 = 5, col = "grey", lwd = 1, lty=1)
segments(x0 = 5.5, y0 = 0, x1 = 5.5, y1 = 5.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 6.0, y0 = 0, x1 = 6.0, y1 = 6, col = "grey", lwd = 1, lty=1)
segments(x0 = 6.5, y0 = 0, x1 = 6.5, y1 = 6.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 7.0, y0 = 0, x1 = 7.0, y1 = 7, col = "grey", lwd = 1, lty=1)
segments(x0 = 7.5, y0 = 0, x1 = 7.5, y1 = 7.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 8.0, y0 = 0, x1 = 8.0, y1 = 8, col = "grey", lwd = 1, lty=1)
#segments(x0 = 0.0, y0 = 0, x1 = max(lo.D), y1 = max(lo.D), 
#         col = "black", lwd = 1, lty=1) # 45 deg line.
segments(x0 = 0.0, y0 = 0, x1 = grid_at4hrs, y1 = 0, 
         col = "black", lwd = 1, lty=1) # 00 deg line.

# -------
# slopes: y - y0 = m*(x - x0)
# -------
# Alternative 2: 1/2 hour interval
slope_0.05_lessons <- predict(lomodel_lessons, 0.5)/0.5 # 0.8469022
slope_0.05_sports <- predict(lomodel_sports, 0.5)/0.5 # 0.06881234
slope_0.05_volunteer <- predict(lomodel_volunteer, 0.5)/0.5 # 0.07725631
slope_0.05_BAschool <- predict(lomodel_BAschool, 0.5)/0.5 # 0.007029172

# Alternative 2: 1 hour interval
#slope_0.05_lessons <- predict(lomodel_lessons, 1)/1 # 0.8469022
#slope_0.05_sports <- predict(lomodel_sports, 1)/1 # 0.06881234
#slope_0.05_volunteer <- predict(lomodel_volunteer, 1)/1 # 0.07725631
#slope_0.05_BAschool <- predict(lomodel_BAschool, 1)/1 # 0.007029172

# Alternative 1:
#slope_0.05_lessons <- predict(lomodel_lessons, 0.5)/0.5 
#slope_0.05_sports <- predict(lomodel_sports, 0.5)/0.5 + 
#  predict(lomodel_volunteer, 0.5)/0.5 + 
#  predict(lomodel_BAschool, 0.5)/0.5
#slope_0.05_volunteer <- predict(lomodel_volunteer, 0.5)/0.5 + 
#  predict(lomodel_BAschool, 0.5)/0.5
#slope_0.05_BAschool <- predict(lomodel_BAschool, 0.5)/0.5

# lessons: at 0.05 to 1.
segments(x0 = 0.05, 
         y0 = predict(lomodel_lessons, 0.05), 
         x1 = grid_at4hrs, 
         y1 = slope_0.05_lessons*grid_at4hrs, #predict(lomodel_lessons, 2.0) 
         col = "#1f77b4", lwd = 1, lty=2)
# lessons: at 0.05 to 1. 
segments(x0 = 0.05, 
         y0 = predict(lomodel_sports, 0.05), 
         x1 = grid_at4hrs, 
         y1 = slope_0.05_sports*grid_at4hrs + 0, 
         col = "#ff7f0e", lwd = 1, lty=2)
# volunteer: at 0.05 to 1. 
segments(x0 = 0.05, 
         y0 = predict(lomodel_volunteer, 0.05), 
         x1 = grid_at4hrs, 
         y1 = slope_0.05_volunteer*grid_at4hrs + 0, 
         col = "#2ca02c", lwd = 1, lty=2)
# BAschool: at 0.05 to 1. 
segments(x0 = 0.05, 
         y0 = predict(lomodel_BAschool, 0.05), 
         x1 = grid_at4hrs, 
         y1 = slope_0.05_BAschool*grid_at4hrs + 0, 
         col = "#d62728", lwd = 1, lty=2)
# -------

# --------------------------------


# --------------------------------
# DENSITY PLOT (ePDF):
# --------------------------------
ggplot(df_estimation2, aes(x = X1_enrich)) +
  geom_density(color = 'black', fill = "dodgerblue", alpha = 0.7) +
  labs(
    x = "D (hours per week)",
    y = "Density, f(D)", 
    title = "Empirical PDF: Hours of Enrichment Activity"
  ) +
  annotate(
    "segment",
    x = 0, xend = 0, y = 0, yend = 1.125, # Line coordinates
    linetype = "solid",
    color = "black",
    size = 0.5
  ) +
  annotate(
    "segment",
    x = 0, xend = round(max(D2)+1), y = 0, yend = 0, # Line coordinates
    linetype = "solid",
    color = "black",
    size = 0.5
  ) +
  theme_bw()

sum(hrs>D2 & D2>=0) # number of observations >0hrs but less than #hrs. 
length(D2) # sample size.
sum(hrs>D2 & D2>=0)/length(D2) # proportion of sample 0 <= D2 < max_hrs.
1-sum(D2==0)/length(D2) # P(D>0)
head(sort(D2, decreasing = TRUE))
# --------------------------------

# --------------------------------
# eCDF PLOT:
# --------------------------------
# Create quartile points for ggplot eCDF: 
quantile(D2)[2:4] # Quartiles.
Q1_D2 <- as.numeric(quantile(D2)[2])
Q2_D2 <- as.numeric(quantile(D2)[3])
Q3_D2 <- as.numeric(quantile(D2)[4])
quartile_points <- data.frame(
  quartile = c(Q1_D2, Q2_D2, Q3_D2),  # x-coordinates of the points (on the x-axis)
  Fd = c(0.25, 0.5, 0.75),  # y-coordinates (CDF values) of the points
  labelz=c("Q1", "Q2", "Q3")
)

ggplot(as.data.frame(D2), aes(x=D2)) + 
  stat_ecdf(geom="step", aes(color="eCDF"), color="black", size=0.55) + 
  theme_bw() + 
  geom_point(data=quartile_points, aes(x=quartile, y=Fd, color=labelz), size=4) + 
  scale_color_manual(values = c("Q3"="steelblue4", "Q2"="steelblue3", "Q1"="steelblue2"), 
                     limits = c("Q3","Q2","Q1")) +
  theme(legend.position=c(0.86,0.15)) +
  labs(x="D", y="Cumulative Probability, F(D)", 
       title="Empirical CDF: Hours of Enrichment Activity", color="Quartile:") 
# Note: image saved at (500 width)x(500 length).
# --------------------------------
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================



# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# Discretization of Data & Sub-treatment Plots:
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================

# ---------------
# [0] Discretize 
# ---------------
# round to half-numbers function:
round_half <- function(x) {
  round(x * 2) / 2
}

D2_star <- round_half(D2)
n2 <- length(D2_star) # n=214
1-sum(D2_star>0)/length(D2_star) # P(D=0)?
hist(D2_star)
table(D2_star)

# sub-treatments:
df_estimation2$XX1_lessons[115] <- 2.5 # to make D=3.
df_estimation2$XX1_lessons[116] <- 2.5 # to make D=3.
S1 <- round_half(df_estimation2$XX1_lessons)
S2 <- round_half(df_estimation2$XX1_sports_struct)
S3 <- round_half(df_estimation2$XX1_volunteer)
S4 <- round_half(df_estimation2$XX1_BAschool) 
length(S1) 
D2_tilde  <- S1 + S2 + S3 + S4
table(D2_tilde)
sum(D2_star != D2_tilde)
tapply(Y2, D2_tilde, mean)

# df of S: 
df_S <- as.data.frame(cbind(S1, S2, S3, S4, D2_tilde))
head(df_S)
df_S[which(df_S$D2_tilde==3.5),] # observations with D=3.5. 
D2[116]

as.numeric(tapply(Y2, S1, mean))
as.numeric(tapply(Y2, S2, mean))
as.numeric(tapply(Y2, S3, mean))
as.numeric(tapply(Y2, S4, mean))

cmeans1 <- c()
cmeans2 <- c()
cmeans3 <- c()
cmeans4 <- c()
for (i in 1:length(seq(0,3,0.5))) {
  cmeans1[i] <- ( mean(S1[D2_tilde==seq(0,3,0.5)[i]]) )
  cmeans2[i] <- ( mean(S2[D2_tilde==seq(0,3,0.5)[i]]) )
  cmeans3[i] <- ( mean(S3[D2_tilde==seq(0,3,0.5)[i]]) )
  cmeans4[i] <- ( mean(S4[D2_tilde==seq(0,3,0.5)[i]]) )
}
rbind(cmeans1, cmeans2, cmeans3, cmeans4)
colSums(rbind(cmeans1, cmeans2, cmeans3, cmeans4))
rowSums(rbind(cmeans1, cmeans2, cmeans3, cmeans4))

# ------------------------
# Discrete Treatment Plot: Avg's
# ------------------------
plot(seq(0,3,0.5),cmeans1, col="blue", pch=20, ylim=c(0,3.05),
     xlab = "D (total hours)", ylab="ST hours")
lines(seq(0,3,0.5), cmeans1, col="blue", pch=20)
points(seq(0,3,0.5), cmeans2, col="orange", pch=20)
lines(seq(0,3,0.5), cmeans2, col="orange", pch=20)
points(seq(0,3,0.5), cmeans3, col="darkgreen", pch=20)
lines(seq(0,3,0.5), cmeans3, col="darkgreen", pch=20)
points(seq(0,3,0.5), cmeans4, col="darkred", pch=20)
lines(seq(0,3,0.5), cmeans4, col="darkred", pch=20)

hrs <- 3 # number of max hours of enrichment. 
plot(y_lessons~D2, col="white", cex=0.2, ylim=c(0,hrs), xlim=c(0,hrs),
     #xlab= expression(paste(D, " (total hours of treatment)")), 
     xlab = "",
     #ylab= expression(paste(S[k], " (hours per sub-treatment)")), 
     ylab = "",
     main="Mean Enrichment Hours per Week by Sub-treatment")
mtext("Low SES Children, 2019", 
      side = 3,           # Top margin
      line = 0.2,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(D, " (total hours of treatment)")), 
      side = 1,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(S[k], " (hours per sub-treatment)")), 
      side = 2,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color

# TRIANGLE:
# Define the x and y coordinates of the triangle vertices
x_coords <- c(0.0, 0.0, hrs)  # X coordinates of the triangle's vertices
y_coords <- c(0.0, hrs, hrs)  # Y coordinates of the triangle's vertices
# Draw the grey triangle
polygon(x_coords, y_coords, col = "grey", border = "black")

# Legend 1: no counter-factual proportional lines
legend(x=0.25, y=(hrs-0.25), title = "Sub-treatment:", 
       legend = c("Lessons", "Sports", "Volunteer", "B&A School"), 
       lty = c(1,1,1,1), 
       col = c("#1f77b4",
               "#ff7f0e",
               "#2ca02c",
               "#d62728"),
       cex=0.8, lwd=c(2.5,2.5,2.5,2.5), pt.cex = 4)

# Legend 2: LINE TYPE DISTINCTION
legend(x=0.25, y=(hrs-0.25), title = "Sub-treatment:", 
       legend = c("Lessons", "Sports", "Volunteer", "B&A School"), 
       lty = c(1,2,3,4), 
       col = c("#1f77b4",
               "#ff7f0e",
               "#2ca02c",
               "#d62728"),
       cex=0.8, lwd=c(2.5,2.5,2.5,2.5), pt.cex = 4)

# Legend 3: POINT TYPE DISTINCTION
legend(x=0.25, y=(hrs-0.25), title = "Sub-treatment:", 
       legend = c("Lessons", "Sports", "Volunteer", "B&A School"), 
       lty = c(1,1,1,1),
       pch = c(16,15,17,18),
       col = c("#1f77b4",
               "#ff7f0e",
               "#2ca02c",
               "#d62728"),
       cex=0.8, lwd=c(2.5,2.5,2.5,2.5), pt.cex = 1.25)
points(0.4364, 2.485, col="white", pch=16, cex=0.75) # add white space.
points(0.4364, 2.352, col="white", pch=15, cex=0.75) # add white space.
points(0.4364, 2.220, col="white", pch=17, cex=0.75) # add white space.
points(0.4364, 2.087, col="white", pch=18, cex=0.75) # add white space.

# Alternative 1:
#lines(lo.D[1:491], lo.y_lessons[1:491]+lo.y_sports[1:491]+lo.y_volunteer[1:491]+lo.y_BAschool[1:491],
#      col="#1f77b4", lw=2.5)
#lines(lo.D[1:491], lo.y_sports[1:491]+lo.y_volunteer[1:491]+lo.y_BAschool[1:491], col="#ff7f0e", lw=2.5)
#lines(lo.D[1:491], lo.y_volunteer[1:491]+lo.y_BAschool[1:491], col="#2ca02c", lw=2.5)
#lines(lo.D[1:491], lo.y_BAschool[1:491], col="#d62728", lw=2.5)

# (***)  Finding 4 hours through the new D2 grid:
location_at4hrs <- which.min(abs(lo.D2 - hrs)) # grid coordinate position of max-hrs. 
grid_at4hrs <- lo.D2[location_at4hrs] # value of grid at 4hrs.
grid_4hrs <- (lo.D2[1:location_at4hrs]) # grid values until 4hrs. 

#abline(a=0,b=1,col="black",lty=1) # 45 degree line. 
segments(x0 = 0.5, y0 = 0, x1 = 0.5, y1 = 0.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 1.0, y0 = 0, x1 = 1.0, y1 = 1, col = "grey", lwd = 1, lty=1)
segments(x0 = 1.5, y0 = 0, x1 = 1.5, y1 = 1.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 2.0, y0 = 0, x1 = 2.0, y1 = 2, col = "grey", lwd = 1, lty=1)
segments(x0 = 2.5, y0 = 0, x1 = 2.5, y1 = 2.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 3.0, y0 = 0, x1 = 3.0, y1 = 3, col = "grey", lwd = 1, lty=1)
segments(x0 = 3.5, y0 = 0, x1 = 3.5, y1 = 3.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 4.0, y0 = 0, x1 = 4.0, y1 = 4, col = "grey", lwd = 1, lty=1)
segments(x0 = 4.5, y0 = 0, x1 = 4.5, y1 = 4.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 5.0, y0 = 0, x1 = 5.0, y1 = 5, col = "grey", lwd = 1, lty=1)
segments(x0 = 5.5, y0 = 0, x1 = 5.5, y1 = 5.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 6.0, y0 = 0, x1 = 6.0, y1 = 6, col = "grey", lwd = 1, lty=1)
segments(x0 = 6.5, y0 = 0, x1 = 6.5, y1 = 6.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 7.0, y0 = 0, x1 = 7.0, y1 = 7, col = "grey", lwd = 1, lty=1)
segments(x0 = 7.5, y0 = 0, x1 = 7.5, y1 = 7.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 8.0, y0 = 0, x1 = 8.0, y1 = 8, col = "grey", lwd = 1, lty=1)
#segments(x0 = 0.0, y0 = 0, x1 = max(lo.D), y1 = max(lo.D), 
#         col = "black", lwd = 1, lty=1) # 45 deg line.
segments(x0 = 0.0, y0 = 0, x1 = grid_at4hrs, y1 = 0, 
         col = "black", lwd = 1, lty=1) # 00 deg line.

# Alternative 2:
#lines(grid_4hrs, lo.y_lessons[1:location_at4hrs],col="#1f77b4", lw=2.5)
#lines(grid_4hrs, lo.y_sports[1:location_at4hrs], col="#ff7f0e", lw=2.5)
#lines(grid_4hrs, lo.y_volunteer[1:location_at4hrs], col="#2ca02c", lw=2.5)
#lines(grid_4hrs, lo.y_BAschool[1:location_at4hrs], col="#d62728", lw=2.5)
#points(D2, cex=0.2, col="grey")

# Alternative 3: Discrete Treatments
#points(seq(0,hrs,0.5), cmeans1[-7], col="#1f77b4", pch=20, cex=1.5)
#lines(seq(0,hrs,0.5), cmeans1[-7], col="#1f77b4", pch=20, lw=2.5)
#points(seq(0,hrs,0.5), cmeans2[-7], col="#ff7f0e", pch=20, cex=1.5)
#lines(seq(0,hrs,0.5), cmeans2[-7], col="#ff7f0e", pch=20, lw=2.5)
#points(seq(0,hrs,0.5), cmeans3[-7], col="#2ca02c", pch=20, cex=1.5)
#lines(seq(0,hrs,0.5), cmeans3[-7], col="#2ca02c", pch=20, lw=2.5)
#points(seq(0,hrs,0.5), cmeans4[-7], col="#d62728", pch=20, cex=1.5)
#lines(seq(0,hrs,0.5), cmeans4[-7], col="#d62728", pch=20, lw=2.5)

# Line Type Distinction: 
points(seq(0,hrs,0.5), cmeans1, col="#1f77b4", pch=20, cex=1.5)
lines(seq(0,hrs,0.5), cmeans1, col="#1f77b4", lty=1, lw=2.5)
points(seq(0,hrs,0.5), cmeans2, col="#ff7f0e", pch=20, cex=1.5)
lines(seq(0,hrs,0.5), cmeans2, col="#ff7f0e", lty=2, lw=2.5)
points(seq(0,hrs,0.5), cmeans3, col="#2ca02c", pch=20, cex=1.5)
lines(seq(0,hrs,0.5), cmeans3, col="#2ca02c", lty=3, lw=2.5)
points(seq(0,hrs,0.5), cmeans4, col="#d62728", pch=20, cex=1.5)
lines(seq(0,hrs,0.5), cmeans4, col="#d62728", lty=4, lw=2.5)

# Point Type Distinction: 
lines(seq(0,hrs,0.5), cmeans1, col="#1f77b4", lty=1, lw=2.5)
points(seq(0,hrs,0.5), cmeans1, col="#1f77b4", pch=16, cex=1.5)
points(seq(0,hrs,0.5), cmeans1, col="white", pch=16, cex=1.0)

lines(seq(0,hrs,0.5), cmeans2, col="#ff7f0e", lty=1, lw=2.5)
points(seq(0,hrs,0.5), cmeans2, col="#ff7f0e", pch=15, cex=1.5)
points(seq(0,hrs,0.5), cmeans2, col="white", pch=15, cex=1.0)

lines(seq(0,hrs,0.5), cmeans3, col="#2ca02c", lty=1, lw=2.5)
points(seq(0,hrs,0.5), cmeans3, col="#2ca02c", pch=17, cex=1.5)
points(seq(0,hrs,0.5), cmeans3, col="white", pch=17, cex=1.0)

lines(seq(0,hrs,0.5), cmeans4, col="#d62728", lty=1, lw=2.5)
points(seq(0,hrs,0.5), cmeans4, col="#d62728", pch=18, cex=1.5)
points(seq(0,hrs,0.5), cmeans4, col="white", pch=18, cex=1.0)
# (*) Image Save Dimensions: 625x575
# ------------------------

# ------------------------
# Discrete Treatment Plot: sums
# ------------------------
csums1 <- c()
csums2 <- c()
csums3 <- c()
csums4 <- c()
for (i in 1:length(seq(0,3,0.5))) {
  csums1[i] <- ( sum(S1[D2_tilde==seq(0,3,0.5)[i]]) )
  csums2[i] <- ( sum(S2[D2_tilde==seq(0,3,0.5)[i]]) )
  csums3[i] <- ( sum(S3[D2_tilde==seq(0,3,0.5)[i]]) )
  csums4[i] <- ( sum(S4[D2_tilde==seq(0,3,0.5)[i]]) )
}
rbind(csums1, csums2, csums3, csums4)
colSums(rbind(csums1, csums2, csums3, csums4))
rowSums(rbind(csums1, csums2, csums3, csums4))


hrs <- 2.5 # number of max hours of enrichment. 
plot(csums1~sort(unique(D2_tilde))[-8], 
     col="white", cex=0.2, 
     ylim=c(0,max(csums1)+0.1), 
     xlim=c(0,hrs),
     #xlab= expression(paste(D, " (total hours of treatment)")), 
     xlab = "",
     #ylab= expression(paste(S[k], " (hours per sub-treatment)")), 
     ylab = "",
     main="Total Enrichment Hours per Week by Sub-treatment")
mtext("Low SES Children, 2019", 
      side = 3,           # Top margin
      line = 0.2,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(D, " (total hours of treatment)")), 
      side = 1,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(S[k], " (hours per sub-treatment)")), 
      side = 2,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color

# (***)  Finding 4 hours through the new D2 grid:
location_at4hrs <- which.min(abs(lo.D2 - hrs)) # grid coordinate position of max-hrs. 
grid_at4hrs <- lo.D2[location_at4hrs] # value of grid at 4hrs.
grid_4hrs <- (lo.D2[1:location_at4hrs]) # grid values until 4hrs. 

#abline(a=0,b=1,col="black",lty=1) # 45 degree line. 
segments(x0 = 0.5, y0 = 0, x1 = 0.5, y1 = 0.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 1.0, y0 = 0, x1 = 1.0, y1 = 1, col = "grey", lwd = 1, lty=1)
segments(x0 = 1.5, y0 = 0, x1 = 1.5, y1 = 1.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 2.0, y0 = 0, x1 = 2.0, y1 = 2, col = "grey", lwd = 1, lty=1)
segments(x0 = 2.5, y0 = 0, x1 = 2.5, y1 = 2.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 3.0, y0 = 0, x1 = 3.0, y1 = 3, col = "grey", lwd = 1, lty=1)
segments(x0 = 3.5, y0 = 0, x1 = 3.5, y1 = 3.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 4.0, y0 = 0, x1 = 4.0, y1 = 4, col = "grey", lwd = 1, lty=1)
segments(x0 = 4.5, y0 = 0, x1 = 4.5, y1 = 4.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 5.0, y0 = 0, x1 = 5.0, y1 = 5, col = "grey", lwd = 1, lty=1)
segments(x0 = 5.5, y0 = 0, x1 = 5.5, y1 = 5.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 6.0, y0 = 0, x1 = 6.0, y1 = 6, col = "grey", lwd = 1, lty=1)
segments(x0 = 6.5, y0 = 0, x1 = 6.5, y1 = 6.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 7.0, y0 = 0, x1 = 7.0, y1 = 7, col = "grey", lwd = 1, lty=1)
segments(x0 = 7.5, y0 = 0, x1 = 7.5, y1 = 7.5, col = "grey", lwd = 1, lty=1)
segments(x0 = 8.0, y0 = 0, x1 = 8.0, y1 = 8, col = "grey", lwd = 1, lty=1)
#segments(x0 = 0.0, y0 = 0, x1 = max(lo.D), y1 = max(lo.D), 
#         col = "black", lwd = 1, lty=1) # 45 deg line.
segments(x0 = 0.0, y0 = 0, x1 = grid_at4hrs, y1 = 0, 
         col = "black", lwd = 1, lty=1) # 00 deg line.

# Alternative 3: Discrete Treatment Totals
points(seq(0,hrs,0.5), csums1[-7], col="#1f77b4", pch=20, cex=1.5)
lines(seq(0,hrs,0.5), csums1[-7], col="#1f77b4", pch=20, lw=2.5)
points(seq(0,hrs,0.5), csums2[-7], col="#ff7f0e", pch=20, cex=1.5)
lines(seq(0,hrs,0.5), csums2[-7], col="#ff7f0e", pch=20, lw=2.5)
points(seq(0,hrs,0.5), csums3[-7], col="#2ca02c", pch=20, cex=1.5)
lines(seq(0,hrs,0.5), csums3[-7], col="#2ca02c", pch=20, lw=2.5)
points(seq(0,hrs,0.5), csums4[-7], col="#d62728", pch=20, cex=1.5)
lines(seq(0,hrs,0.5), csums4[-7], col="#d62728", pch=20, lw=2.5)
# (*) Image Save Dimensions: 625x575
# ---------------------------------------------
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================

# ########################################################################################## #
# ########################################################################################## #
# ########################################################################################## #


# ########################################################################################## #
# ########################################################################################## #
# ########################################################################################## #
#                                                                                            #
# [II] ESTIMATES OF PARAMETERS OF INTEREST:                                                  #
#                                                                                            #
# ########################################################################################## #
# ########################################################################################## #
# ########################################################################################## #

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# [II.1] Regression
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================

# --------------------------------------------
# (*) NOTE: 
# --------------------------------------------
# Analyses [1]-[3] have used D2_tilde. But in analyses [3] & [4], 
# D3 was used due to low sample size at total_hours=3 and 3.5. This 
# was done for the sake of making stable CI's. See the counts below 
# to verify. 
table(D2_tilde)
table(D3)
# We will use D3 to compare weighting schemes. Should circle back 
# and use estimates based fully on D3 in [1]-[3]. 
# --------------------------------------------

lin_model <- lm(Y2~D2_tilde)
summary(lin_model)
alpha2 <- as.numeric(lin_model$coefficients[2]) # -0.05598801
round(alpha2,3)
n2 <- length(Y2) # sample size. 

# ---------------
# regression bootstrap: 
# ---------------
set.seed(15)
B <- 1000 # number of bootstrap iterations.  
boot_estimates <- matrix(0,nrow=B,ncol=1)

# Non-parametric Bootstrap:
for (b in 1:B){
  # draw bootstrap sample
  boot_idx <- sample(1:n2, size=n2, replace=TRUE)
  X_b <- D2_tilde[boot_idx]
  y_b <- Y2[boot_idx]
  # estimate coefficients:
  lin_boot <- lm(y_b~X_b)
  boot_estimates[b] <- as.numeric(lin_boot$coefficients[2])
}
boot_estimates
se_regression <- apply(boot_estimates,2,sd)
print(se_regression)
# ---------------

# ------------------------------------
# Weights in the regression parameter: 
# ------------------------------------
# (i) marginal type regression weights:

# instantiate prob's and conditional means: 
#prob_reg_marginal <- c()
#cmean_reg_marginal <- c()
#n_reg <- length(D3) # sample size: 214. 
#seq(0.5,3,0.5)
#sort(unique(D3))
#for (i in 1:length(D3_idx)) {
  # (1) Set D=d: 
#  d <- D3_idx[i] 
  #d <- seq(0,3,0.5)[i] 
#  print(d)
  
  # (2) Compute P(D >=d): 
#  prob_reg_marginal[i] <- mean(D3 >= d) #sum(ifelse(D3>=d, 1, 0))/n_reg
  
  # (3) Compute E[D |D>=d]: 
#  cmean_reg_marginal[i] <- mean(D3[D3>=d])
#}
#prob_reg_marginal 
#cmean_reg_marginal 

# Regression estimate under D3: -0.0607113
#summary(lm(Y2~D3))$coefficients[2]
#D3_idx

# Regression weights w^{reg}(d) for D3: 
# { (E[D |D>=d] - E[D]) * P(D >=d) } / Var(D)
# Note that d>=1. 
#table(D3)
#meanD3 <- mean(D3) # mean of D3: 0.4299065
#varD3 <- var(D3) # variance of D3: 0.5420122.

#wts_reg_marginal <- ((cmean_reg_marginal - meanD3)*prob_reg_marginal) / varD3
# Note: notice that they are all positive, and are largest for 
# values of D that are closest to E[D]=0.4299. 
#cbind(D3_idx, wts_reg_marginal, prob_reg_marginal, cmean_reg_marginal)
#sum( (cmean_reg_marginal - meanD3)*prob_reg_marginal )
#sum(prob_reg_marginal)
#sum(wts_reg_marginal)
#?var
#varD3
#sum((D3-meanD3)^2)/n_reg
# Not sure why Var(D3) != sum( (cmean_reg_marginal - meanD3)*prob_reg_marginal )...

# (ii) non-marginal type regression weights:

# ------------------------------------

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================



# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# [II.2] Congruent Aggregate Marginal Parameter: E[MATT^{+_tilde}(D) | D>0]
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# (***)
# Make a new aggregate D that aggregates 3.5 and 3 together due to low sample size: 
#D3 <- D2_tilde
#table(D3)
#D3[which(D3==3.5)] <- 3 # remove D=3.5 due to small sample size = 1.
#table(D3)
# (***)

# (1) Create dataframe with ST's and D:
#D2_tilde <- D2_tilde[-which(D2_tilde==3.5)] # remove D=3.5... only 2 observations.
df_tilde <- (as.data.frame(cbind(Y2,D2_tilde,S1,S2,S3,S4)))
#df_tilde <- df_tilde[-which(D2_tilde==3.5),] # remove D=3.5... only 2 observations.
head(df_tilde)
dim(df_tilde)

#df_tilde <- (as.data.frame(cbind(Y2,D3,S1,S2,S3,S4)))
#head(df_tilde) # with D3 instead. 

# (2) Create a Unique Key for Each Vector S:
df_tilde <- df_tilde %>%
  mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))
head(df_tilde)

# (3) Compute mean Y by S: 
means_by_S <- df_tilde %>%
  group_by(across(starts_with("S"))) %>%
  summarise(
    D = unique(D2_tilde),
    mean_Y = mean(Y2),
    n = n(),
    .groups = "drop"
  ) #%>%
  #mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))
head(means_by_S)

# (4) Create matching pairs btw S_d and S_{d-1}
# Extract S vectors as a matrix
# STEP 1: Extract the S matrix with named rows
S_matrix <- means_by_S %>%
  select(starts_with("S")) %>%
  as.data.frame()  # <- ensure it's a data frame, not a tibble
rownames(S_matrix) <- means_by_S$S_key
S_matrix <- S_matrix %>% select(-S_key)
str(S_matrix)
dim(S_matrix)

library(purrr)
# Helper: function to compute if two vectors differ by +1/-1
# STEP 2: Helper function to check adjacency
is_adjacent <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  diff <- x - y
  sum(diff) == 0 && sum(abs(diff)) == 2 && all(diff %in% c(-0.5, 0, 0.5))
}

# STEP 3: Generate all row index pairs
index_pairs <- combn(nrow(S_matrix), 2, simplify = FALSE)

# STEP 4: Keep only adjacent pairs where D differs by 1
adj_pairs <- keep(index_pairs, function(pair) {
  i <- pair[1]; j <- pair[2]
  # ------------------------------------
  # congruent & incongruent comparisons:
  # ------------------------------------
  #D_i <- means_by_S$D[i]
  #D_j <- means_by_S$D[j]
  #abs(D_i - D_j) == 0.5  # just see if any such pairs exist 
  # ------------------------------------
  
  # ------------------------------------
  # ONLY congruent comparisons:
  # ------------------------------------
  (sum(abs(means_by_S[i,1:4] - means_by_S[j,1:4])) == 0.5) & (abs(means_by_S$D[i] - means_by_S$D[j]) == 0.5)
  # ------------------------------------
  
  # ------------------------------------
  # ONLY incongruent comparisons:
  # ------------------------------------
  #(sum(abs(means_by_S[i,1:4] - means_by_S[j,1:4])) != 0.5) & (abs(means_by_S$D[i] - means_by_S$D[j]) == 0.5) 
  # ------------------------------------
})
length(adj_pairs)

# STEP 5: Compute differences in mean Y
results <- map_dfr(adj_pairs, function(pair) {
  i <- pair[1]; j <- pair[2]
  row_i <- means_by_S[i, ]
  row_j <- means_by_S[j, ]
  
  # ---------
  # weights: 
  # ---------
  N <- sum(means_by_S$n) # total sample size. 
  D_i <- means_by_S$D[i]
  D_j <- means_by_S$D[j] 
  Nd_i <- sum(means_by_S$n[means_by_S$D==D_i]) # total counts of D=d. 
  P_i <- row_i$n/Nd_i # P(S_i=s_i | D=d)
  Nd_j <- sum(means_by_S$n[means_by_S$D==D_j]) # total counts of D=d. 
  P_j <- row_j$n/Nd_j # P(S_j=s_j | D=d)
  # ---------
  
  if (row_i$D != row_j$D) {
    if (row_i$D > row_j$D) {
      delta_Y <- row_i$mean_Y - row_j$mean_Y
      D_from <- row_j$D
      D_to <- row_i$D
      from_S <- row_j$S_key
      to_S <- row_i$S_key
      wts_product <- P_i*P_j
    } else {
      delta_Y <- row_j$mean_Y - row_i$mean_Y
      D_from <- row_i$D
      D_to <- row_j$D
      from_S <- row_i$S_key
      to_S <- row_j$S_key
      wts_product <- P_i*P_j
    }
    
    tibble(
      D_from = D_from,
      D_to = D_to,
      from_S = from_S,
      to_S   = to_S,
      delta_Y = delta_Y,
      wts_product = wts_product
    )
  } else {
    return(NULL)  # if D_i == D_j, skip this pair
  }
})
#str(as.data.frame(results))
#class(results)
dim(results) # 91 total comparisons. congruent/incongruent (19/72).
head(results)
head(means_by_S)
sort(unique(means_by_S$D))
sort(unique(results$D_to)) # Notice: there's no congruent comparisons from 2.5 to 3. 

# Normalize product weights for each D=d group: 
denoms <- as.numeric(tapply(results$wts_product, results$D_to, sum)) # total weight at D_to. 
D_idx <- sort(unique(results$D_to)) # index of support of D_to.
wts_prod_scaled <- c() # initialize empty vector to stroe new weights. 
for (j in 1:nrow(results)){
  row_j <- results[j, ]
  wts_prod_scaled[j] <- (row_j$wts_product/denoms[which(D_idx==row_j$D_to)]) 
}
wts_prod_scaled
results <- cbind(results, wts_prod_scaled)
head(results)
#sum(results[results$D_to==2,]$wts_prod_scaled)

sum(results$delta_Y*wts_prod_scaled) # (1) sum of MATTilde(d)'s.

# On the path to overall average on MATTilde: 
#wts_prod_scaled2 <- (results$wts_prod_scaled/sum(results$wts_prod_scaled)) # scaled product weights?
#sum(results$delta_Y*wts_prod_scaled2) # (2) An average for MATTilde(d)'s ? Not quite.. see below.
# Need to average over D|D>0 distribution. This just sums all the scaled product weights.

D_idx <- seq(0.5,max(D2_tilde),0.5)
storage <- c()
for (i in 1:length(D_idx)) {
  idx <- D_idx
  storage[i] <- ( sum(D2_tilde==idx[i])/length(D2_tilde) )
}
sum( storage/(sum(D2_tilde>0)/length(D2_tilde)) )

P_greater0 <- 1-(means_by_S$n[means_by_S$D==0]/sum(means_by_S$n)) # P(D>0) = 1 - P(D=0).
PD_greater0 <- c() # P(D=d|D>0) vector. 
for (k in 1:length(D_idx)){
  #idx <- D_idx
  PD_greater0[k] <- (sum(means_by_S$n[means_by_S$D==D_idx[k]])/sum(means_by_S$n))/P_greater0
}
sum(PD_greater0)

wts_prod_scaled2 <- c() # initialize empty vector to stroe new weights. 
for (j in 1:nrow(results)){
  row_j <- results[j, ]
  wts_prod_scaled2[j] <- (row_j$wts_prod_scaled*PD_greater0[which(D_idx==row_j$D_to)]) 
}
wts_prod_scaled2
sum(wts_prod_scaled2)
results <- cbind(results, wts_prod_scaled2)
head(results)
OMATTilde <- sum(results$delta_Y*wts_prod_scaled2) # (3) E[ MATTilde | D>0] ? YES CORRECT USE THIS ONE. 
OMATTilde # -0.08655743



# ==============================================================
# Get the MATT^{+}(d)'s separately: 
# ==============================================================
matt_tilde_d <- c() # instantiate empty vector for mattilde terms. 
D_idx_margin <- c(0,sort(unique(results$D_to))) # support of D from results. 
for (i in 1:length(D_idx_margin)) {
  print(i)
  # [1] create D_from & D_to specific dataframe: 
  if(i<(length(D_idx_margin))){
    df_margin <- results[results$D_from==D_idx_margin[i] & 
                           results$D_to==D_idx_margin[i+1],]
    print(head(df_margin))
    
    # [2] compute \tilde{MATT}^{+}(d) terms:
    matt_tilde_d[i] <- sum(df_margin$wts_product * df_margin$delta_Y)/sum(df_margin$wts_product) #sum(df_margin$wts_prod_scaled * df_margin$delta_Y)
    print(matt_tilde_d)
  }
  else{
    break # don't go past support size. 
  }
}
print(matt_tilde_d)
length(matt_tilde_d)
abs(matt_d - c(matt_tilde_d,0)) # absolute distance. 


# ------------------------------------------------
# Separate \tilde{MATT}^{+}(d) bootstrap 95% CI's:
# ------------------------------------------------
# **********************
# Code for stratified sampling:
#set.seed(60) # expedition 60.
#bootstrap_df <- do.call(rbind, lapply(
#  split(df_tilde,    df_tilde$D2_tilde),        # 1) split into a list by D=d
#  function(sub) {
#    sub[sample(nrow(sub),    # 2) resample rows _within_ that stratum
#               size = nrow(sub),
#               replace = TRUE), ]
#  }
#))
#rownames(bootstrap_df) <- NULL # Reset the row‐names so they go 1,2,3,…n
#head(bootstrap_df)
#dim(bootstrap_df)
#table(bootstrap_df$D2_tilde)
#table(df_tilde$D2_tilde)
#bootstrap_df[bootstrap_df$D2_tilde==2.5,]
# ********************** 

original_supp_length <- length(sort(unique(results$D_to))) # *very important to keep seperate.

set.seed(117)
B <- 1000 # number of bootstrap iterations.  
D_idx_margin_plus <- sort(unique(results$D_to)) # support of D for congruent comparisons, drop D=0.
boot_MATTildePlus_estimates <- matrix(NA, nrow = B, ncol = original_supp_length)
dim(boot_MATTildePlus_estimates)
#boot_MATTilde_estimates <- matrix(0,nrow=B,ncol=length(D3_idx))

# Non-parametric Bootstrap:
b <- 1 # initialize loop index. 
while (b <= B){
  print(paste0("iter: ", b)) # iteration number. 
  # ------------------------------------
  # (1) draw bootstrap sample
  # ------------------------------------
  df_boot <- do.call(rbind, lapply(
    split(df_tilde,    df_tilde$D2_tilde),        # 1) split into a list by D=d
    function(sub) {
      sub[sample(nrow(sub),    # 2) resample rows _within_ that stratum
                 size = nrow(sub),
                 replace = TRUE), ]
    }
  ))
  rownames(df_boot) <- NULL # Reset the row‐names so they go 1,2,3,…n
  #boot_idx <- sample(1:n2, size=n2, replace=TRUE)
  #df_boot <- df_tilde[boot_idx,]
  # ------------------------------------
  
  # ------------------------------------
  # (2) Create a Unique Key for Each Vector S:
  # ------------------------------------
  df_boot <- df_boot %>%
    mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))
  # ------------------------------------
  
  # ------------------------------------
  # (3) Compute mean Y by S: 
  # ------------------------------------
  means_by_S <- df_boot %>%
    group_by(across(starts_with("S"))) %>%
    summarise(
      D = unique(D2_tilde),
      mean_Y = mean(Y2),
      n = n(),
      .groups = "drop"
    ) 
  #head(means_by_S)
  # ------------------------------------
  
  # ------------------------------------
  # (4) Create matching pairs btw S_d and S_{d-1}:
  # ------------------------------------
  S_matrix <- means_by_S %>%
    select(starts_with("S")) %>%
    as.data.frame()  # <- ensure it's a data frame, not a tibble
  rownames(S_matrix) <- means_by_S$S_key
  S_matrix <- S_matrix %>% select(-S_key)
  # ------------------------------------
  
  # ------------------------------------
  # (5) Generate all row index pairs
  # ------------------------------------
  index_pairs <- combn(nrow(S_matrix), 2, simplify = FALSE)
  # ------------------------------------
  
  # ------------------------------------
  # (6) Keep only adjacent pairs where D differs by 1
  # ------------------------------------
  adj_pairs <- keep(index_pairs, function(pair) {
    i <- pair[1]; j <- pair[2]
    # ------------------------------------
    # congruent & incongruent comparisons:
    # ------------------------------------
    #D_i <- means_by_S$D[i]
    #D_j <- means_by_S$D[j]
    #abs(D_i - D_j) == 0.5  # just see if any such pairs exist 
    # ------------------------------------
    
    # ------------------------------------
    # ONLY congruent comparisons:
    # ------------------------------------
    (sum(abs(means_by_S[i,1:4] - means_by_S[j,1:4])) == 0.5) & (abs(means_by_S$D[i] - means_by_S$D[j]) == 0.5)
    # ------------------------------------
    
    # ------------------------------------
    # ONLY incongruent comparisons:
    # ------------------------------------
    #(sum(abs(means_by_S[i,1:4] - means_by_S[j,1:4])) != 0.5) & (abs(means_by_S$D[i] - means_by_S$D[j]) == 0.5) 
    # ------------------------------------
  })
  # ------------------------------------
  
  # ------------------------------------
  # (7) Compute differences in mean Y
  # ------------------------------------
  results <- map_dfr(adj_pairs, function(pair) {
    i <- pair[1]; j <- pair[2]
    row_i <- means_by_S[i, ]
    row_j <- means_by_S[j, ]
    
    # ---------
    # weights: 
    # ---------
    N <- sum(means_by_S$n) # total sample size. 
    D_i <- means_by_S$D[i]
    D_j <- means_by_S$D[j] 
    Nd_i <- sum(means_by_S$n[means_by_S$D==D_i]) # total counts of D=d. 
    P_i <- row_i$n/Nd_i # P(S_i=s_i | D=d)
    Nd_j <- sum(means_by_S$n[means_by_S$D==D_j]) # total counts of D=d. 
    P_j <- row_j$n/Nd_j # P(S_j=s_j | D=d)
    # ---------
    
    if (row_i$D != row_j$D) {
      if (row_i$D > row_j$D) {
        delta_Y <- row_i$mean_Y - row_j$mean_Y
        D_from <- row_j$D
        D_to <- row_i$D
        from_S <- row_j$S_key
        to_S <- row_i$S_key
        wts_product <- P_i*P_j
      } else {
        delta_Y <- row_j$mean_Y - row_i$mean_Y
        D_from <- row_i$D
        D_to <- row_j$D
        from_S <- row_i$S_key
        to_S <- row_j$S_key
        wts_product <- P_i*P_j
      }
      
      tibble(
        D_from = D_from,
        D_to = D_to,
        from_S = from_S,
        to_S   = to_S,
        delta_Y = delta_Y,
        wts_product = wts_product
      )
    } else {
      return(NULL)  # if D_i == D_j, skip this pair
    }
  })
  # ------------------------------------
  
  # ------------------------------------
  # (8) Normalize product weights for each D=d group: 
  # ------------------------------------
  denoms <- as.numeric(tapply(results$wts_product, results$D_to, sum)) # total weight at D_to. 
  D_idx <- sort(unique(results$D_to)) # index of support of D_to.
  wts_prod_scaled <- c() # initialize empty vector to stroe new weights. 
  for (j in 1:nrow(results)){
    row_j <- results[j, ]
    wts_prod_scaled[j] <- (row_j$wts_product/denoms[which(D_idx==row_j$D_to)]) 
  }
  #wts_prod_scaled
  results <- cbind(results, wts_prod_scaled)
  
  # (9) Compute Estimates:
  D_idx <- seq(0.5,max(D2_tilde),0.5)
  P_greater0 <- 1-(means_by_S$n[means_by_S$D==0]/sum(means_by_S$n)) # P(D>0) = 1 - P(D=0).
  PD_greater0 <- c() # P(D=d|D>0) vector. 
  for (k in 1:length(D_idx)){
    #idx <- D_idx
    PD_greater0[k] <- (sum(means_by_S$n[means_by_S$D==D_idx[k]])/sum(means_by_S$n))/P_greater0
  }
  #sum(PD_greater0)
  
  wts_prod_scaled2 <- c() # initialize empty vector to stroe new weights. 
  for (j in 1:nrow(results)){
    row_j <- results[j, ]
    wts_prod_scaled2[j] <- (row_j$wts_prod_scaled*PD_greater0[which(D_idx==row_j$D_to)]) 
  }
  #wts_prod_scaled2
  #sum(wts_prod_scaled2)
  results <- cbind(results, wts_prod_scaled2)
  #head(results)
  
  matt_tilde_d_boot <- c() # instantiate empty vector for mattilde terms. 
  D_idx_margin_boot <- c(0,sort(unique(results$D_to))) # support of D from results. 
  if(length(D_idx_margin_boot)==(original_supp_length+1)){ # Do if fully supported:
    for (i in 1:length(D_idx_margin_boot)) {
      #print(i)
      # [1] create D_from & D_to specific dataframe: 
      if(i<(length(D_idx_margin_boot))){
        df_margin_boot <- results[results$D_from==D_idx_margin_boot[i] & 
                               results$D_to==D_idx_margin_boot[i+1],]
        #print(head(df_margin_boot))
        
        # [2] compute \tilde{MATT}^{+}(d) terms:
        matt_tilde_d_boot[i] <- sum(df_margin_boot$wts_product * df_margin_boot$delta_Y)/
          sum(df_margin_boot$wts_product) #sum(df_margin$wts_prod_scaled * df_margin$delta_Y)
        #print(matt_tilde_d_boot)
      }
      else{
        break # don't go past support size. 
      }
    }
    # store results by row:
    boot_MATTildePlus_estimates[b,] <- matt_tilde_d_boot
    #boot_MATTilde_estimates[b] <- sum(results$delta_Y*wts_prod_scaled2)
    
    #Count iteration:
    b <- b+1 
  }
  #else{
  #  break # Not enough support on D=d. 
  #}
  # ------------------------------------
}
head(boot_MATTildePlus_estimates)

# Percentile CI's: 
percCI_MATTildePlus <- matrix(NA, nrow = length(D_idx_margin_plus), ncol = 2)
for (d in 1:length(D_idx_margin_plus)){
  percCI_MATTildePlus[d,] <- as.numeric( quantile(boot_MATTildePlus_estimates[,d], probs = c(0.025,0.975)) )
}
colnames(percCI_MATTildePlus) <- c("lower95", "upper95")
cbind(percCI_MATTildePlus, matt_tilde_d)
# ------------------------------------------------

# ==============================================================

# ---------------
# MATTilde bootstrap: 
# ---------------
set.seed(444)
B <- 1000 # number of bootstrap iterations.  
boot_MATTilde_estimates <- matrix(0,nrow=B,ncol=1)

# Non-parametric Bootstrap:
for (b in 1:B){
  print(paste0("iter: ", b)) # iteration number. 
  # ------------------------------------
  # (1) draw bootstrap sample
  # ------------------------------------
  boot_idx <- sample(1:n2, size=n2, replace=TRUE)
  df_boot <- df_tilde[boot_idx,]
  # ------------------------------------
  
  # ------------------------------------
  # (2) Create a Unique Key for Each Vector S:
  # ------------------------------------
  df_boot <- df_boot %>%
    mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))
  # ------------------------------------
  
  # ------------------------------------
  # (3) Compute mean Y by S: 
  # ------------------------------------
  means_by_S <- df_boot %>%
    group_by(across(starts_with("S"))) %>%
    summarise(
      D = unique(D2_tilde),
      mean_Y = mean(Y2),
      n = n(),
      .groups = "drop"
    ) 
  #head(means_by_S)
  # ------------------------------------
  
  # ------------------------------------
  # (4) Create matching pairs btw S_d and S_{d-1}:
  # ------------------------------------
  S_matrix <- means_by_S %>%
    select(starts_with("S")) %>%
    as.data.frame()  # <- ensure it's a data frame, not a tibble
  rownames(S_matrix) <- means_by_S$S_key
  S_matrix <- S_matrix %>% select(-S_key)
  # ------------------------------------
  
  # ------------------------------------
  # (5) Generate all row index pairs
  # ------------------------------------
  index_pairs <- combn(nrow(S_matrix), 2, simplify = FALSE)
  # ------------------------------------
  
  # ------------------------------------
  # (6) Keep only adjacent pairs where D differs by 1
  # ------------------------------------
  adj_pairs <- keep(index_pairs, function(pair) {
    i <- pair[1]; j <- pair[2]
    # ------------------------------------
    # congruent & incongruent comparisons:
    # ------------------------------------
    #D_i <- means_by_S$D[i]
    #D_j <- means_by_S$D[j]
    #abs(D_i - D_j) == 0.5  # just see if any such pairs exist 
    # ------------------------------------
    
    # ------------------------------------
    # ONLY congruent comparisons:
    # ------------------------------------
    (sum(abs(means_by_S[i,1:4] - means_by_S[j,1:4])) == 0.5) & (abs(means_by_S$D[i] - means_by_S$D[j]) == 0.5)
    # ------------------------------------
    
    # ------------------------------------
    # ONLY incongruent comparisons:
    # ------------------------------------
    #(sum(abs(means_by_S[i,1:4] - means_by_S[j,1:4])) != 0.5) & (abs(means_by_S$D[i] - means_by_S$D[j]) == 0.5) 
    # ------------------------------------
  })
  # ------------------------------------
  
  # ------------------------------------
  # (7) Compute differences in mean Y
  # ------------------------------------
  results <- map_dfr(adj_pairs, function(pair) {
    i <- pair[1]; j <- pair[2]
    row_i <- means_by_S[i, ]
    row_j <- means_by_S[j, ]
    
    # ---------
    # weights: 
    # ---------
    N <- sum(means_by_S$n) # total sample size. 
    D_i <- means_by_S$D[i]
    D_j <- means_by_S$D[j] 
    Nd_i <- sum(means_by_S$n[means_by_S$D==D_i]) # total counts of D=d. 
    P_i <- row_i$n/Nd_i # P(S_i=s_i | D=d)
    Nd_j <- sum(means_by_S$n[means_by_S$D==D_j]) # total counts of D=d. 
    P_j <- row_j$n/Nd_j # P(S_j=s_j | D=d)
    # ---------
    
    if (row_i$D != row_j$D) {
      if (row_i$D > row_j$D) {
        delta_Y <- row_i$mean_Y - row_j$mean_Y
        D_from <- row_j$D
        D_to <- row_i$D
        from_S <- row_j$S_key
        to_S <- row_i$S_key
        wts_product <- P_i*P_j
      } else {
        delta_Y <- row_j$mean_Y - row_i$mean_Y
        D_from <- row_i$D
        D_to <- row_j$D
        from_S <- row_i$S_key
        to_S <- row_j$S_key
        wts_product <- P_i*P_j
      }
      
      tibble(
        D_from = D_from,
        D_to = D_to,
        from_S = from_S,
        to_S   = to_S,
        delta_Y = delta_Y,
        wts_product = wts_product
      )
    } else {
      return(NULL)  # if D_i == D_j, skip this pair
    }
  })
  # ------------------------------------
  
  # ------------------------------------
  # (8) Normalize product weights for each D=d group: 
  # ------------------------------------
  denoms <- as.numeric(tapply(results$wts_product, results$D_to, sum)) # total weight at D_to. 
  D_idx <- sort(unique(results$D_to)) # index of support of D_to.
  wts_prod_scaled <- c() # initialize empty vector to stroe new weights. 
  for (j in 1:nrow(results)){
    row_j <- results[j, ]
    wts_prod_scaled[j] <- (row_j$wts_product/denoms[which(D_idx==row_j$D_to)]) 
  }
  #wts_prod_scaled
  results <- cbind(results, wts_prod_scaled)
  
  # (9) Compute Estimates:
  D_idx <- seq(0.5,max(D2_tilde),0.5)
  P_greater0 <- 1-(means_by_S$n[means_by_S$D==0]/sum(means_by_S$n)) # P(D>0) = 1 - P(D=0).
  PD_greater0 <- c() # P(D=d|D>0) vector. 
  for (k in 1:length(D_idx)){
    #idx <- D_idx
    PD_greater0[k] <- (sum(means_by_S$n[means_by_S$D==D_idx[k]])/sum(means_by_S$n))/P_greater0
  }
  #sum(PD_greater0)
  
  wts_prod_scaled2 <- c() # initialize empty vector to stroe new weights. 
  for (j in 1:nrow(results)){
    row_j <- results[j, ]
    wts_prod_scaled2[j] <- (row_j$wts_prod_scaled*PD_greater0[which(D_idx==row_j$D_to)]) 
  }
  #wts_prod_scaled2
  #sum(wts_prod_scaled2)
  results <- cbind(results, wts_prod_scaled2)
  #head(results)
  boot_MATTilde_estimates[b] <- sum(results$delta_Y*wts_prod_scaled2)
  # ------------------------------------
}
head(boot_MATTilde_estimates) # 
se_MATTilde <- apply(boot_MATTilde_estimates,2,sd)
print(se_MATTilde)
# ---------------

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================


# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# [II.3] Aggregate ATT Parameter: E[AATT(D) | D>0]
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
conditional_means <- as.numeric(tapply(Y2, D2_tilde, mean))

# Store AATT's: 
aatt <- c()
for (i in 1:(length(conditional_means)-1)){
  aatt[i] <- (conditional_means[i+1]-conditional_means[1])
}
print(aatt)

# Store Prob's:
prob_Y2givenD2 <- as.numeric(table(D2_star)/n2)
p0 <- prob_Y2givenD2[1] # P(D=0)

df_tilde <- (as.data.frame(cbind(Y2,D2_tilde,S1,S2,S3,S4)))
mean_vec <- as.numeric(tapply(Y2, D2_tilde, mean))
mean_vec - mean_vec[1]
D_wts <- (as.numeric(table(D2_tilde))/length(D2_tilde)) 
D_wts2 <- (D_wts[-1] / (1-D_wts[1])) # remove D=0 term & weight by P(D>0).
length(D_wts2)
OAATT <- sum(D_wts2 * (mean_vec[-1] - mean_vec[1])) # E[ATT(D) | D>0] = -0.167
print(round(OAATT,3)) # Overall estimate.

# -------------------------
# E[AATT(D)|D>0] bootstrap: 
# -------------------------
set.seed(117)
B <- 1000 # number of bootstrap iterations.  
boot_EDATT_estimates <- matrix(0,nrow=B,ncol=1)

# Non-parametric Bootstrap:
for (b in 1:B){
  # draw bootstrap sample
  boot_idx <- sample(1:n2, size=n2, replace=TRUE)
  X_b <- df_tilde$D2_tilde[boot_idx]
  y_b <- df_tilde$Y2[boot_idx]
  # estimate coefficients:
  mean_vec_boot <- as.numeric(tapply(y_b, X_b, mean))
  D_wts_boot <- (as.numeric(table(X_b))/length(X_b)) 
  D_wts_boot2 <- (D_wts_boot[-1] / (1-D_wts_boot[1]))
  boot_EDATT_estimates[b] <- sum(D_wts_boot2 * (mean_vec_boot[-1] - mean_vec_boot[1]))
}
boot_EDATT_estimates
se_Edatt <- apply(boot_EDATT_estimates,2,sd)
print(round(se_Edatt,3)) # SE of overall estimate. 
# -------------------------

# ---------------
# Plot: ATT(d)'s
# ---------------
plot(D_idx,aatt, col="black", cex=1.5, pch=20,
     ylim=c(-max(abs(aatt))-0.1,max(abs(aatt))+0.1), xlim=c(0,3.55),
     xlab = "",
     ylab = "",
     main="Aggregate ATT(d)")
points(0, 0, col="black", pch=1, cex=1.25)
mtext("Low SES Children, 2019", 
      side = 3,           # Top margin
      line = 0.2,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(D, " (total hours of treatment)")), 
      side = 1,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(AATT(d), "")), 
      side = 2,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color

#abline(a=0,b=1,col="black",lty=1) # 45 degree line. 
#segments(x0 = 0.5, y0 = -0.5, x1 = 0.5, y1 = 0.5, col = "grey", lwd = 1, lty=1)
#segments(x0 = 1.0, y0 = -0.5, x1 = 1.0, y1 = 1.0, col = "grey", lwd = 1, lty=1)
#segments(x0 = 1.5, y0 = -0.5, x1 = 1.5, y1 = 1.5, col = "grey", lwd = 1, lty=1)
#segments(x0 = 2.0, y0 = -0.5, x1 = 2.0, y1 = 2.0, col = "grey", lwd = 1, lty=1)
#segments(x0 = 2.5, y0 = -0.5, x1 = 2.5, y1 = 2.5, col = "grey", lwd = 1, lty=1)
#segments(x0 = 3.0, y0 = -0.5, x1 = 3.0, y1 = 3.0, col = "grey", lwd = 1, lty=1)
#segments(x0 = 3.5, y0 = -0.5, x1 = 3.5, y1 = 3.5, col = "grey", lwd = 1, lty=1)
#segments(x0 = 0.0, y0 = 0, x1 = grid_at4hrs, y1 = 0, 
#         col = "black", lwd = 1, lty=1) # 00 deg line.
segments(x0 = -0.5, y0 = 0, x1 = 4.5, y1 = 0, col = "grey", lwd = 1, lty=2)
points(D_idx, aatt, col="black", pch=20, cex=1.5)

#lines(seq(0.5,3.5,0.5), aatt, col="black", pch=20, lw=2) # AATT(d) lines. 
# (*) Image Save Dimensions: 625x575
table(D2_tilde)
# ---------------

# ---------------------
# AATT(d) SE bootstrap: 
# ---------------------
set.seed(117)
B <- 10000 # number of bootstrap iterations.  
#boot_AATT_estimates <- matrix(NA, nrow=B, ncol=length(aatt))

all_D_values <- sort(unique(df_tilde$D2_tilde)) 
boot_AATT_estimates <- matrix(NA, nrow = B, ncol = length(all_D_values) - 1)

# Make a new aggregate D that aggregates 3.5 and 3 together due to low sample size: 
D3 <- D2_tilde
table(D3)
#D3[which(D3==3.5)] <- 3 # remove D=3.5 due to small sample size = 1.
table(D3)

all_D_values <- sort(unique(D3)) 
trt_D_values <- all_D_values[-1]
boot_AATT_estimates <- matrix(NA, nrow = B, ncol = length(trt_D_values))

# Non-parametric Bootstrap:
for (b in 1:B){
  for (d in 1:length(trt_D_values)) {
    # draw bootstrap sample
    boot_trt_idx <- sample(which(D3==trt_D_values[d]), replace=TRUE)
    boot_ctrl_idx <- sample(which(D3==0), replace=TRUE)
    y_b_trt <- df_tilde$Y2[boot_trt_idx] # treated D=d.
    y_b_ctrl <- df_tilde$Y2[boot_ctrl_idx] # control D=0.
    
    # compute/store AATT's: 
    boot_AATT_estimates[b,d] <- mean(y_b_trt) - mean(y_b_ctrl)
  }
}
#boot_AATT_estimates_clean <- boot_AATT_estimates[complete.cases(boot_AATT_estimates), ] 
boot_AATT_estimates
#quantile(boot_AATT_estimates, probs = c(0.025,0.975))
percCI <- matrix(NA, nrow = length(trt_D_values), ncol = 2)
for (d in 1:length(trt_D_values)){
  percCI[d,] <- as.numeric( quantile(boot_AATT_estimates[,d], probs = c(0.025,0.975)) )
}
percCI
# ---------------------

# ---------------------------------------------------
# Plot: ATT(d)'s with non-parametric percentile CI's:
# ---------------------------------------------------
conditional_means <- as.numeric(tapply(Y2, D3, mean))

# Store AATT's: 
aatt <- c()
for (i in 1:(length(conditional_means)-1)){
  aatt[i] <- (conditional_means[i+1]-conditional_means[1])
}
print(aatt)
D3_idx <- sort(unique(D3))[-1]

plot(D3_idx,aatt, col="black", cex=1.5, pch=20,
     ylim=c(-max(abs(percCI))-0.1, max(abs(percCI))+0.1), xlim=c(0,3.05),
     xlab = "",
     ylab = "",
     main="Aggregate ATT(d)")
points(0, 0, col="black", pch=1, cex=1.25) # empty zero point.
mtext("Low SES Children, 2019", 
      side = 3,           # Top margin
      line = 0.2,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(D, " (total hours of treatment)")), 
      side = 1,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(AATT(d), "")), 
      side = 2,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
segments(x0 = -0.5, y0 = 0, x1 = 4.5, y1 = 0, col = "grey", lwd = 1, lty=2) # zero dashed line.
# Add Overall ATT(D) estimate: 
lines(c(0,D3_idx), rep(OAATT,length(D3_idx)+1), col="darkred", lw=1.5, lty=4) 
points(0, OAATT, col="white", pch=20, cex=1.5) # remove D=0 case. 

# Add NP percentile 95% CI's: 
for (i in 1:length(aatt)){
  # upper bound:
  segments(x0 = D_idx[i], y0 = aatt[i], 
           x1 = D_idx[i], y1 = percCI[i,2], 
           col = "burlywood", lwd = 1, lty=1)
  # lower bound:
  segments(x0 = D_idx[i], y0 = aatt[i], 
           x1 = D_idx[i], y1 = percCI[i,1], 
           col = "burlywood", lwd = 1, lty=1)
}
points(D3_idx, aatt, col="black", pch=20, cex=1.5) # add ATT(d)'s back.
#points(D3_idx, aatt_d, col="darkgreen", pch=8, cex=1.5) # add ATT(d)/d's, if desired.

# Legend: 
legend(x=0.15, y=0.75, title = "", 
       legend = c("AATT(d)", "95% CI","Overall AATT(d)"), 
       lty = c(1,1,4), 
       pch = c(20,1,1),
       col = c("black","burlywood","darkred"),
       cex=0.75, lwd=c(NA,1.5,1.5), pt.cex = c(1.5,NA,NA), bty = "n")

# (*) Image Save Dimensions: 625x575
# ---------------------------------------------------

# ---------------------------------------------------
# Inverted AATT(d) Plot: 
# ---------------------------------------------------

# -----------------------------------------
# Base plot: 
# -----------------------------------------
plot(aatt, D3_idx, col="black", cex=1.5, pch=20,
     xlim=c(-max(abs(percCI))-0.1, max(abs(percCI))+0.1), ylim=c(0.0,3.05),
     xlab = "",
     ylab = "",
     main="Aggregate ATT(d) Estimates")
mtext("Low SES Children, 2019", 
      side = 3,           # Top margin
      line = 0.2,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(D, " (total hours of treatment)")), 
      side = 2,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext("Estimate", #expression(paste(AATT(d), "")), 
      side = 1,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
segments(x0 = 0, y0 = 0, x1 = 0, y1 = 3.1, col = "grey", lwd = 1, lty=2) # zero dashed line.
points(0, 0, col="white", pch=20, cex=2) # remove D=0 case. 
points(0, 0, col="black", pch=1, cex=1.25) # empty zero point.
# -----------------------------------------

# -----------------------------------------
# Add dashed lines for treatment intensity: 
# -----------------------------------------
#for (l in 1:length(D3_idx)) {
#  segments(x0 = aatt[l], y0 = 0, 
#           x1 = aatt[l], y1 = D3_idx[l], 
#           col = "grey", lwd = 1, lty=2)
  #points(aatt[l], 0, col="white", pch=20, cex=1) # remove D=0 case. 
#}
#points(aatt, D3_idx, col="black", pch=20, cex=1.5) # add ATT(d)'s back.
#segments(x0 = -0.72, y0 = 0, 
#         x1 = 0.72, y1 = 0, 
#         col = "black", lwd = 1, lty=1) # bottom zero line.
#points(0, 0, col="white", pch=20, cex=2) # remove D=0 case. 
#points(0, 0, col="black", pch=1, cex=1.25) # empty zero point.
# -----------------------------------------

# ------------------------------
# Add Overall ATT(D) estimate: 
# ------------------------------
# rgb(red, green, blue, alpha) with each 0–1
transparent_green <- rgb(0/255, 100/255, 0/255, alpha = 0.3) # color.
segments(x0 = OAATT, y0 = 0, 
         x1 = OAATT, y1 = 3.1, col = transparent_green, 
         lwd = 1.5, lty=3)
segments(x0 = OAATT, y0 = 0, 
         x1 = OAATT, y1 = 3.1, col = transparent_green, 
         lwd = 1, lty=1)
points(OAATT, 0, col="white", pch=20, cex=2) # remove D=0 case. 
#points(OAATT, 0, col="darkred", pch=20, cex=1.0) # Add OAATT at D=0.
#points(OAATT, 0, col="darkred", pch=21, cex=1.5) # Add OAATT at D=0. 
# ------------------------------

# ------------------------------
# Add NP percentile 95% CI's: 
# ------------------------------
for (i in 1:length(aatt)){
  # upper bound:
  segments(y0 = D_idx[i], x0 = aatt[i], 
           y1 = D_idx[i], x1 = percCI[i,2], 
           col = "burlywood", lwd = 1.25, lty=1)
  # lower bound:
  segments(y0 = D_idx[i], x0 = aatt[i], 
           y1 = D_idx[i], x1 = percCI[i,1], 
           col = "burlywood", lwd = 1.25, lty=1)
}
points(aatt, D3_idx, col="black", pch=20, cex=1.5) # add ATT(d)'s back.
#points(D3_idx, aatt_d, col="darkgreen", pch=8, cex=1.5) # add ATT(d)/d's, if desired.
# ------------------------------

# ------------------------------
# Legend: 
# ------------------------------
legend(x=0.1, y=0.5, title = "", 
       legend = c("AATT(d)","Overall AATT(d)"), 
       lty = c(1,3), 
       pch = c(20,1),
       col = c("burlywood", transparent_green),
       cex=0.95, lwd=c(1.25,1.5), pt.cex = c(1.5,NA), bty = "n")
legend(x=0.1, y=0.5, title = "", 
       legend = c("AATT(d)", "Overall AATT(d)"), 
       lty = c(1,1), 
       pch = c(20,1),
       col = c("black", transparent_green),
       cex=0.95, lwd=c(NA,1.5), pt.cex = c(1.5,NA), bty = "n")

#legend(x=0.25, y=0.65, title = "", 
#       legend = c("AATT(d)","Overall AATT(d)", "95% CI"), 
#       lty = c(1,3,1), 
#       pch = c(20,21,1),
#       #pt.bg = c(NA, "darkred", NA),
#       col = c("black","darkred","burlywood"),
#       cex=0.75, lwd=c(NA,NA,1.5), pt.cex = c(1.5,1.5,NA), bty = "n")
#points(0.3295, 0.27, col="darkred", pch=20, cex=1.0) # fill OAATT(d) in legend.
# ------------------------------

# (*) Image Save Dimensions: 625x575
# ---------------------------------------------------


# ------------------------
# Add asymptotic 95% CI's: 
# ------------------------
z95 <- qnorm(0.975) # ~1.96
for (i in 1:length(aatt)) {
  # upper bound:
  segments(x0 = D_idx[i], y0 = aatt[i], 
           x1 = D_idx[i], y1 = aatt[i]+z95*se_aatt[i], 
           col = "tomato", lwd = 1, lty=1)
  # lower bound:
  segments(x0 = D_idx[i], y0 = aatt[i], 
           x1 = D_idx[i], y1 = aatt[i]-z95*se_aatt[i], 
           col = "tomato", lwd = 1, lty=1)
}
points(D_idx, aatt, col="black", pch=20, cex=1.5)
lines(seq(0,3.5,0.5), c(0,aatt), col="black", pch=20, lw=2) # AATT(d) lines.
# ------------------------

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================



# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# [II.4.1] Scaled Aggregate ATT Parameter: E[AATT(D)/D |D>0]: 
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
rbind((aatt),(D3_idx))
aatt_d <- (aatt/D3_idx) # AATT(D)/D. 
points(D3_idx, aatt/D3_idx, col="dodgerblue", pch=5, cex=1.5) # add ATT(d)/d's.

D3_wts <- (as.numeric(table(D3))/length(D3)) # no D=3.5 data.
D_wts3 <- (D3_wts[-1] / (1-D3_wts[1])) # remove D=0 term & weight by P(D>0).
OAATT_divD <- sum(aatt_d*D_wts3) # Overall AATT(D)/D.
print(OAATT_divD)
sum(aatt*D_wts3) # Overall AATT(D).


# ---------------
# AATT(d)/d SE bootstrap: 
# ---------------
set.seed(7)
B <- 10000 # number of bootstrap iterations.  

# Make a new aggregate D that aggregates 3.5 and 3 together due to low sample size: 
D3 <- D2_tilde
table(D3)
D3[which(D3==3.5)] <- 3 # remove D=3.5 due to small sample size = 1.
table(D3)

all_D_values <- sort(unique(D3)) 
trt_D_values <- all_D_values[-1]
boot_AATTdivD_estimates <- matrix(NA, nrow = B, ncol = length(trt_D_values))

# Non-parametric Bootstrap:
for (b in 1:B){
  for (d in 1:length(trt_D_values)) {
    # draw bootstrap sample
    boot_trt_idx <- sample(which(D3==trt_D_values[d]), replace=TRUE)
    boot_ctrl_idx <- sample(which(D3==0), replace=TRUE)
    y_b_trt <- df_tilde$Y2[boot_trt_idx] # treated D=d.
    y_b_ctrl <- df_tilde$Y2[boot_ctrl_idx] # control D=0.
    
    # compute/store AATT's: 
    boot_AATTdivD_estimates[b,d] <- (mean(y_b_trt) - mean(y_b_ctrl))/trt_D_values[d]
  }
}
#boot_AATT_estimates_clean <- boot_AATT_estimates[complete.cases(boot_AATT_estimates), ] 
boot_AATTdivD_estimates
dim(boot_AATTdivD_estimates)
#quantile(boot_AATT_estimates, probs = c(0.025,0.975))
percCI_AATTdivD <- matrix(NA, nrow = length(trt_D_values), ncol = 2)
for (d in 1:length(trt_D_values)){
  percCI_AATTdivD[d,] <- as.numeric( quantile(boot_AATTdivD_estimates[,d], 
                                              probs = c(0.025,0.975)) )
}
percCI_AATTdivD
# ---------------

# ---------------
# Plot: ATT(d)/d's with non-parametric percentile CI's:
# ---------------
D3_idx <- sort(unique(D3))[-1]

# (*) Inverted!
plot(aatt_d,D3_idx, col="black", cex=1.5, pch=20,
     xlim=c(-max(abs(percCI_AATTdivD))-0.1, 
            max(abs(percCI_AATTdivD))+0.1), 
     ylim=c(0,3.05),
     xlab = "",
     ylab = "",
     main="Aggregate ATT(d)/d Estimates")
mtext("Low SES Children, 2019", 
      side = 3,           # Top margin
      line = 0.2,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(D, " (total hours of treatment)")), 
      side = 2,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext("Estimate", #expression(paste(AATT(d)/d, "")), 
      side = 1,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
segments(x0 = 0, y0 = 0, x1 = 0, y1 = 3.1, col = "grey", lwd = 1, lty=2) # zero dashed line.
points(0, 0, col="white", pch=20, cex=2) # remove D=0 case. 
points(0, 0, col="black", pch=1, cex=1.25) # D=0 case. 

# Add Overall ATT(D)/D estimate: 
lines(rep(OAATT_divD,length(D3_idx)+1), c(0,D3_idx), col="darkgreen", lw=1.5, lty=5) 
points(OAATT_divD, 0, col="white", pch=20, cex=2) # remove D=0 case. 

# Add NP percentile 95% CI's: 
for (i in 1:length(aatt)){
  # upper bound:
  segments(x0 = aatt_d[i], y0 = D_idx[i], 
           x1 = percCI_AATTdivD[i,2], y1 = D_idx[i], 
           col = "burlywood", lwd = 1, lty=1)
  # lower bound:
  segments(x0 = aatt_d[i], y0 = D_idx[i], 
           x1 = percCI_AATTdivD[i,1], y1 = D_idx[i], 
           col = "burlywood", lwd = 1, lty=1)
}
points(aatt_d, D3_idx, col="black", pch=20, cex=1.5) # add ATT(d)'s back.

# Legend: 
legend(x=0.22, y=0.65, title = "", 
       legend = c("AATT(d)/d", "95% CI","Overall AATT(d)/d"), 
       lty = c(1,1,5), 
       pch = c(20,1,1),
       col = c("black","burlywood","darkgreen"),
       cex=0.85, lwd=c(NA,1.5,1.5), pt.cex = c(1.5,NA,NA), bty = "n")
# ---------------
# ---------------------------------------------

# ------------------------------
# E[AATT(D)/D|D>0] bootstrap: (*** WIP ***) incorrect! needs mandatory sampling from low D.
# ------------------------------
set.seed(8080)
B <- 500 # number of bootstrap iterations.  
boot_EAATTdivD_estimates <- matrix(0,nrow=B,ncol=1)

# Non-parametric Bootstrap:
for (b in 1:B){
  print(paste0("iter: ", b)) # iteration number. 
  
  # draw bootstrap sample
  boot_idx <- sample(1:n2, size=n2, replace=TRUE)
  X_b <- D3[boot_idx]
  y_b <- df_tilde$Y2[boot_idx]
  Dboot_idx <- sort(unique(X_b)) # support of bootstrap D. 
  
  # estimate coefficients:
  mean_vec_boot <- as.numeric(tapply(y_b, X_b, mean))
  D_wts_boot <- (as.numeric(table(X_b))/length(X_b)) 
  D_wts_boot2 <- (D_wts_boot[-1] / (1-D_wts_boot[1]))
  boot_EAATTdivD_estimates[b] <- sum(D_wts_boot2 * (mean_vec_boot[-1] - mean_vec_boot[1]) / Dboot_idx[-1])
}
boot_EAATTdivD_estimates
se_EAATTdivD <- apply(boot_EAATTdivD_estimates,2,sd)
print(round(se_EAATTdivD,3))
# ------------------------------

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================



# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# [II.4.2] Scaled Dis-aggregate ATT Parameter E[DATT(S)/D | D>0]
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# NOTE: This should result in the same overall estimate as in section [4.1].

# (1) Create dataframe with ST's and D:
df_tilde <- (as.data.frame(cbind(Y2,D2_tilde,S1,S2,S3,S4)))
head(df_tilde)

# (2) Create a Unique Key for Each Vector S:
df_tilde <- df_tilde %>%
  mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))
head(df_tilde)

# (3) Compute mean Y by S: 
means_by_S <- df_tilde %>%
  group_by(across(starts_with("S"))) %>%
  summarise(
    D = unique(D2_tilde),
    mean_Y = mean(Y2),
    n = n(),
    .groups = "drop"
  ) #%>%
#mutate(S_key = apply(select(., starts_with("S")), 1, paste, collapse = "_"))
head(means_by_S)

# (4) Create matching pairs btw S_d and S_{d-1}
# Extract S vectors as a matrix
# STEP 1: Extract the S matrix with named rows
S_matrix <- means_by_S %>%
  select(starts_with("S")) %>%
  as.data.frame()  # <- ensure it's a data frame, not a tibble
rownames(S_matrix) <- means_by_S$S_key
S_matrix <- S_matrix %>% select(-S_key)
str(S_matrix)
dim(S_matrix)

# (4) Compute contrasts DATT(s): 
# instantiate empty datt vector: 
datt <- c()
prob_SgivenD <- c()
Ngrand <- sum(means_by_S$n) # total sample size. 

for (i in 1:(nrow(means_by_S)-1)) {
  datt[i] <- means_by_S$mean_Y[i+1] - means_by_S$mean_Y[1]
  prob_SgivenD[i] <- (means_by_S$n[i+1]/Ngrand) / (1-(means_by_S$n[1]/Ngrand))
}
length(prob_SgivenD)
sum(prob_SgivenD)
# combine with means_by_S df:
datt_df <- (cbind(means_by_S[-1,],datt,prob_SgivenD))
head(datt_df)

# Estimate of E[DATT(S)|D>0]:
sum(datt*prob_SgivenD) # -0.1669343
sum( (datt_df$datt) * prob_SgivenD)

# Estimate of E[DATT(S)/D|D>0]:
sum( (datt_df$datt/datt_df$D) * prob_SgivenD) # -0.218268

# ---------------------------------------------

# ==========================================================================================
# ==========================================================================================
# ==========================================================================================


# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# [II.5] Aggregate Marginal Comparisons: \delta_w(d) or MATT(d)
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================
# NOTE: Includes comparisons to \tilde{MATT}^{+}(d).
table(D2_tilde)

# --------------------------------------------------
# Get MATT(d) := \delta_w(d) = E[Y|D=d] - E[Y|D=d-1]
# --------------------------------------------------
# instantiate empty vector to store MATT(d)'s:
matt_d <- c() 
D3_idx0 <- sort(unique(D3)) # full support of D3 with 0. 

# create dataframe with Y2 and D3 only: 
df_D3 <- as.data.frame(cbind(Y2, D3))
dim(df_D3)
head(df_D3)
#mean(df_D3[df_D3$D3==2,]$Y2)-mean(df_D3[df_D3$D3==1.5,]$Y2)

for (i in 1:length(D3_idx0)) {
  if(i!=length(D3_idx0)){
    d_1 <- D3_idx0[i] # D="d-1".
    d <- D3_idx0[i+1] # D="d". 
    matt_d[i] <- mean(df_D3$Y2[df_D3$D3==d]) - mean(df_D3$Y2[df_D3$D3==(d_1)])
  }
  else{break}
}
matt_d # display MATT(d)'s. 
plot(matt_d, pch=20) # take a peek.
# --------------------------------------------------

# ----------------------------------------
# Compute Bootstrap SE's for each MATT(d):
# ----------------------------------------
set.seed(117)
B <- 1000 # number of bootstrap iterations.  
boot_delta_d_estimates <- matrix(0,nrow=B,ncol=length(D3_idx))
n3 <- nrow(df_D3) # n=214

for (b in 1:B) {
  print(paste0("iter: ", b)) # iteration number. 
    # draw bootstrap sample:
    for (i in 1:length(D3_idx0)) {
      if(i!=length(D3_idx0)){
        boot_d_idx <- sample(which(D3==D3_idx0[i]), replace=TRUE)
        boot_dplus1_idx <- sample(which(D3==D3_idx0[i+1]), replace=TRUE)
        y_boot_d <- df_tilde$Y2[boot_d_idx] # treated D=d.
        y_boot_dplus1 <- df_tilde$Y2[boot_dplus1_idx] # treated D=d-1.
        
        # compute/store MATTilde^{+}(d)'s: 
        boot_delta_d_estimates[b,i] <- mean(y_boot_dplus1) - mean(y_boot_d) 
        
        #d_1 <- D3_idx0[i] # D="d-1".
        #d <- D3_idx0[i+1] # D="d". 
        #matt_d[i] <- mean(df_D3$Y2[df_D3$D3==d]) - mean(df_D3$Y2[df_D3$D3==(d_1)])
    }
    else{break}
  }
}
boot_delta_d_estimates
apply(boot_delta_d_estimates, 2, sd) # bootsrap SE's. 
perc_CI_matt_d <- t(apply(boot_delta_d_estimates, 
                          2, quantile, 
                          probs = c(0.025, 0.975))) # 95% percentile CI's.
colnames(perc_CI_matt_d) <- c("lower95", "upper95")
abs(perc_CI_matt_d[,1] - perc_CI_matt_d[,2]) # CI size. 
# Note: Progressively increases due to smaller sample size. 
cbind(perc_CI_matt_d, matt_d) # LOOKS GOOD - check mark!
# ----------------------------------------

# ------------------------------------------------------------
# Compute overall MATT(d), E[MATT(d)|D>0] or E[\delta_w | D>0]
# ------------------------------------------------------------
OMATT <- sum(D_wts3*matt_d) # Overall \delta_w = E[MATT(d)|D>0]
OMATT # -0.0399223
round(OMATT,3)
# ------------------------------------------------------------

# -------------------------------------------------------------------------------
# Compute Bootstrap SE's for overall MATT(d), E[MATT(d)|D>0] or E[\delta_w | D>0]
# -------------------------------------------------------------------------------
set.seed(55)
B <- 1000 # number of bootstrap iterations.  
boot_OMATT_estimates <- matrix(0,nrow=B,ncol=1)
n3 <- nrow(df_D3) # n=214

# Non-parametric Bootstrap:
for (b in 1:B){
  print(paste0("iter: ", b)) # iteration number. 
  
  # draw bootstrap sample
  boot_idx <- sample(1:n3, size=n3, replace=TRUE)
  X_b <- D3[boot_idx]
  y_b <- df_tilde$Y2[boot_idx]
  Dboot_idx <- sort(unique(X_b)) # support of bootstrap D. 
  
  # bootstrap estimates:
  df_boot <- as.data.frame(cbind(y_b, X_b)) # bootstrap dataframe.
  matt_d_boot <- c() # instatiate bootstrap vector for MATT(d)'s. 
  for (i in 1:length(Dboot_idx)) {
    if(i!=length(Dboot_idx)){
      d_1 <- Dboot_idx[i] # D="d-1".
      d <- Dboot_idx[i+1] # D="d". 
      matt_d_boot[i] <- mean(df_boot$y_b[df_boot$X_b==d]) - mean(df_boot$y_b[df_boot$X_b==(d_1)])
    }
    else{break}
  }
  
  # create bootstrap D-distribution weights: 
  D_wts_boot <- (as.numeric(table(X_b))/length(X_b)) 
  D_wts_boot2 <- (D_wts_boot[-1] / (1-D_wts_boot[1]))
  #D3_wts_b <- (as.numeric(table(X_b))/length(X_b)) # no D=3.5 data.
  #D_wts3_b <- (D3_wts_b[-1] / (1-D3_wts_b[1])) # remove D=0 term & weight by P(D>0).
  
  # estimate coefficients:
  boot_OMATT_estimates[b] <- sum(D_wts_boot2 * matt_d_boot)
}
head(boot_OMATT_estimates)
round(sd(boot_OMATT_estimates),3) # bootstrap estimate of SE(E[\delta_w | D>0]).
# -------------------------------------------------------------------------------

# ------------------------------------------------------
# Plots of \tilde{MATT}^{+}(d) & \delta_w(d) or MATT(d):
# ------------------------------------------------------
# - plot both MATT(d) and \tilde{MATT}^{w}(d). 
# - may include CI's for them. 
# - (*) recall that \tilde{MATT}^{w}(d) currently NOT using D3, but MATT(d) does. 

matt_tilde_d <- c(matt_tilde_d,0) # no congruent comparisons possible at D=3. 
matt_tilde_d <- matt_tilde_d[-6]
matt_tilde_d
matt_d
#matt_d <- c(matt_d)
cbind(matt_d, matt_tilde_d,D3_idx)
round(abs(matt_d - c(matt_tilde_d,0)),4) # absolute differences. 

# ---------------------------
# Base plot:
# ---------------------------
ylim_margin <- 1 # set limits of y-axis in plot for marginal estimates. 
plot(D3_idx, matt_d, 
     pch=20, col="white",
     xlim = c(-0.01,3.01), 
     #ylim = c(min(c(perc_CI_matt_d,percCI_MATTildePlus))+0.1,
      #                              max(c(perc_CI_matt_d,percCI_MATTildePlus))-0.05),
     ylim = c(-ylim_margin,ylim_margin),
     yaxt = "n",
     xlab = "", ylab = "", main = "Marginal Parameter Estimates", 
     )
axis(side = 2, 
     at = seq(-ylim_margin, ylim_margin, by = 0.25), 
     labels = paste0(seq(-ylim_margin, ylim_margin, by = 0.25))) # add y-axis.

mtext("Low SES Children, 2019", 
      side = 3,           # Top margin
      line = 0.2,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(D, " (total hours of treatment)")), 
      side = 1,           # Top margin
      line = 2.75,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
mtext(expression(paste(Estimate)),  #expression(paste(widehat(delta[w]), " ",(d), "")), 
      side = 2,           # Top margin
      line = 2.5,         # Slightly below the main title (higher values move it higher)
      cex = 1,          # Font size
      col = "black")       # Font color
# ---------------------------

# ---------------------------
# Background & aesthetics:
# ---------------------------
segment_color <- "grey95"
segments(x0 = 0, y0 = -ylim_margin-0.5, x1 = 0, y1 = ylim_margin+0.5, col = segment_color, lwd = 1, lty=1)
segments(x0 = 0.5, y0 = -ylim_margin-0.5, x1 = 0.5, y1 = ylim_margin+0.5, col = segment_color, lwd = 1, lty=1)
segments(x0 = 1.0, y0 = -ylim_margin-0.5, x1 = 1.0, y1 = ylim_margin+0.5, col = segment_color, lwd = 1, lty=1)
segments(x0 = 1.5, y0 = -ylim_margin-0.5, x1 = 1.5, y1 = ylim_margin+0.5, col = segment_color, lwd = 1, lty=1)
segments(x0 = 2.0, y0 = -ylim_margin-0.5, x1 = 2.0, y1 = ylim_margin+0.5, col = segment_color, lwd = 1, lty=1)
segments(x0 = 2.5, y0 = -ylim_margin-0.5, x1 = 2.5, y1 = ylim_margin+0.5, col = segment_color, lwd = 1, lty=1)
segments(x0 = 3.0, y0 = -ylim_margin-0.5, x1 = 3.0, y1 = ylim_margin+0.5, col = segment_color, lwd = 1, lty=1)

lines(D3_idx0,rep(0,length(D3_idx0)),col="grey",lty=2) # zero effect line. 
#points(0, 0, col="white", pch=20, cex=1.5) # create white space at D=0. 
#points(0, 0, col="black", pch=1, cex=1) # null values at D=0. 
# ---------------------------

# ---------------------------
# Add MATT(d) points & lines:
# ---------------------------
jiggle <- 0.045 # for shifting the points. (jitter)
#lines(D3_idx-jiggle, matt_d, col="darkred",cex=1) # lines for delta's. 
#lines(c(0,0.5)-jiggle, c(0,matt_d[1]), col="darkred",cex=1) # add line from baseline. 
points(D3_idx-jiggle, matt_d, col="darkred", pch=20, cex=1.75) # delta's/MATT(D)'s.
points(D3_idx-jiggle, matt_d, col="white", pch=20, cex=1.25) # create white space to 'fill' points. 

#lines(D3_idx[-6]+jiggle, matt_tilde_d, col="darkblue",cex=1) # lines for delta's. 
#lines(c(0,0.5)+jiggle, c(0,matt_tilde_d[1]), col="darkblue",cex=1) # add line from baseline. 
points(D3_idx[-6]+jiggle, matt_tilde_d, col="darkblue", pch=20, cex=1.75) # \tilde{MATT}^{+}(d)'s. 
# ---------------------------

# -----------------------------------------
# Add NP percentile 95% CI's for delta_d's: 
# -----------------------------------------
for (i in 1:length(matt_d)){
  # upper bound:
  segments(x0 = (D_idx[i]-jiggle), y0 = matt_d[i], 
           x1 = (D_idx[i]-jiggle), y1 = perc_CI_matt_d[i,2], 
           col = "pink", lwd = 1.25, lty=1)
  # lower bound:
  segments(x0 = (D_idx[i]-jiggle), y0 = matt_d[i], 
           x1 = (D_idx[i]-jiggle), y1 = perc_CI_matt_d[i,1], 
           col = "pink", lwd = 1.25, lty=1)
}
points((D3_idx-jiggle), matt_d, col="darkred", pch=20, cex=1.75) # add delta(d)'s back.
points(D3_idx-jiggle, matt_d, col="white", pch=20, cex=1.25) # create white space to 'fill' points. 
# -----------------------------------------

# -------------------------------------------------
# Add NP percentile 95% CI's for MATTilde^{+}(d)'s: 
# -------------------------------------------------
for (i in 1:length(matt_tilde_d)){
  # upper bound:
  segments(x0 = (D_idx[i]+jiggle), y0 = matt_tilde_d[i], 
           x1 = (D_idx[i]+jiggle), y1 = percCI_MATTildePlus[i,2], 
           col = "skyblue", lwd = 1.25, lty=1)
  # lower bound:
  segments(x0 = (D_idx[i]+jiggle), y0 = matt_tilde_d[i], 
           x1 = (D_idx[i]+jiggle), y1 = percCI_MATTildePlus[i,1], 
           col = "skyblue", lwd = 1.25, lty=1)
}
points((D3_idx[-6]+jiggle), matt_tilde_d, col="darkblue", pch=20, cex=1.75) # add MATTilde(d)'s back.
#points((D3_idx[-6]+jiggle), matt_tilde_d, col="white", pch=20, cex=1.0)
# -------------------------------------------------

# -------------------------------------------
# Add absolute differences in estimates text: 
# -------------------------------------------
abs_diff_deltas <- round(abs(matt_d - c(matt_tilde_d,0)),3) # absolute differences in marginal parm's. 
abs_diff_deltas <- format(abs_diff_deltas, scientific = FALSE)
abs_diff_deltas[1] <- 0 # no difference. 
abs_diff_deltas[2] <- 0 # no difference. 
#abs_diff_deltas[6] <- NA 
# Note: comparison cannot be made since only incongruent comparisons are made btw D=3 & D=2.5. 
abs_diff_deltas

perc_change_deltas <- round( (c(matt_tilde_d,0) - matt_d) / matt_d, 4)*100 
perc_change_deltas # %-change between marginal parameters. 
perc_change_deltas <- paste0(perc_change_deltas, "%") # make them character strings with “%” on the end
perc_change_deltas

# print text: 


# delta(d)'s for absolute differences = Zeros:
text(D3_idx[1:2], rep(-ylim_margin+0.07,length(D3_idx[1:2])), 
     labels=round(matt_d,3)[1:2],
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="darkred")    
# delta(d)'s for absolute differences = Non-Zeros:
text(D3_idx[3:6], rep(-ylim_margin+0.07,length(D3_idx[3:6])), 
     labels=round(matt_d,3)[3:6],
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="darkred")     

# MATTilde(d)'s for absolute differences = Zeros:
text(D3_idx[1:2], rep(-ylim_margin,length(D3_idx[1:2])), 
     labels=round(c(matt_tilde_d,0),3)[1:2],
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="darkblue")    
# MATTilde(d)'s for absolute differences = Non-Zeros:
text(D3_idx[3:5], rep(-ylim_margin,length(D3_idx[3:5])), 
     labels=round(c(matt_tilde_d,0),3)[3:5],
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="darkblue")  
text(D3_idx[6], rep(-ylim_margin,length(D3_idx[6])), 
     labels="NA",
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="darkblue")  

# Zeros:
text(D3_idx[1:2], rep(-ylim_margin-0.07,length(D3_idx[1:2])), 
     labels=perc_change_deltas[1:2],
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="black")    
# Non-Zeros:
text(D3_idx[3], rep(-ylim_margin-0.07,length(D3_idx[3])), 
     labels=perc_change_deltas[3],
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="purple")    
text(D3_idx[4], rep(-ylim_margin-0.07,length(D3_idx[4])), 
     labels=perc_change_deltas[4],
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="purple") 
text(D3_idx[5], rep(-ylim_margin-0.07,length(D3_idx[5])), 
     labels=perc_change_deltas[5],
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="purple") 
text(D3_idx[6], rep(-ylim_margin-0.07,length(D3_idx[6])), 
     labels="NA",
     pos=3,                 # 1=below, 2=left, 3=above, 4=right
     cex=0.8,               # font size
     col="purple") 
# -------------------------------------------

# ---------------------------------
# Plot overall marginal parameters:
# ---------------------------------
# rgb(red, green, blue, alpha) with each 0–1
transparent_red <- rgb(139/255, 0, 0, alpha = 0.2)
#lines(c(0,D3_idx), rep(OMATT,length(D3_idx)+1), col=transparent_red, lw=3, lty=3) 
#lines(c(0,D3_idx), rep(OMATT,length(D3_idx)+1), col=transparent_red, lw=1, lty=1) 
#points(0, OMATT, col="white", pch=20, cex=2) # remove D=0 case.

transparent_blue <- rgb(0/255, 0/255, 139/255, alpha = 0.2)
#lines(c(0,D3_idx), rep(OMATTilde,length(D3_idx)+1), col=transparent_blue, lw=3, lty=3) 
#lines(c(0,D3_idx), rep(OMATTilde,length(D3_idx)+1), col=transparent_blue, lw=1, lty=1) 
#points(0, OMATTilde, col="white", pch=20, cex=2) # remove D=0 case.
# ---------------------------------

# ---------------------------
# Legend: 
# ---------------------------
#legend(
#  x      = 0.10,
#  y      = 0.3,
#  legend = expression(
#    delta(d),                          # delta(d)
#    widetilde(MATT)^" +"*(d)            # wide tilde over MATT, superscript +, then (d)
#  ),
#  lty     = c(1, 1),
#  pch     = c(20, 20),
#  col     = c("darkred", "darkblue"),
#  cex     = 0.85,
#  lwd     = c(1.5, 1.5),
#  pt.cex  = c(1.5, 1.5),
#  bty     = "n"
#)

legend(
  x      = 0.00,
  y      = ylim_margin+0.08, # 0.75
  legend = expression(
    delta(d), #delta[w]*(d),                          # delta(d)
    AMATT^"+"*(d)            # wide tilde over MATT, superscript +, then (d)
    #"% change"
  ),
  lty     = c(1, 1),
  pch     = c(20, 20),
  col     = c("pink", "skyblue"),
  cex     = 0.95,
  lwd     = c(1.75, 1.75),
  pt.cex  = c(1.75, 1.75),
  bty     = "n"
)
legend(
  x      = 0.00,
  y      = ylim_margin+0.08, # 0.75
  legend = expression(
    delta(d), #delta[w]*(d),                          # delta(d)
    AMATT^"+"*(d)            # wide tilde over MATT, superscript +, then (d)
    #"% change"
  ),
  lty     = c(1, 1),
  pch     = c(20, 20),
  col     = c("darkred", "darkblue"),
  cex     = 0.95,
  lwd     = c(NA,NA),
  pt.cex  = c(1.75, 1.75),
  bty     = "n"
)
points(0.2226, 0.9749, col="white", pch=20, cex=1.25) # create white space to 'fill' points. 
#points(0.261, 0.6906, col="white", pch=20, cex=1.0) # create white space to 'fill' points. 


# OMIT? ---------------------
legend(
  x      = 0.05,
  y      = 0.85, # 0.75
  legend = expression(
    delta(d), #delta[w]*(d),                          # delta(d)
    "Overall "*delta(d),
    widetilde(MATT)^" +"*(d),            # wide tilde over MATT, superscript +, then (d)
    "Overall "*widetilde(MATT)^" +"*(d)
    #"% change"
  ),
  lty     = c(1, 3, 1, 3),
  pch     = c(20, NA, 20, NA),
  col     = c("pink", transparent_red, "skyblue", transparent_blue),
  cex     = 0.85,
  lwd     = c(1.5, 3, 1.5, 3),
  pt.cex  = c(1.5, 1.5, 1.5, 1.5),
  bty     = "n"
)
legend(
  x      = 0.05,
  y      = 0.85, # 0.75
  legend = expression(
    delta(d), #delta[w]*(d),                          # delta(d)
    "Overall "*delta(d),
    widetilde(MATT)^" +"*(d),            # wide tilde over MATT, superscript +, then (d)
    "Overall "*widetilde(MATT)^" +"*(d)
    #"% change"
  ),
  lty     = c(1, 1, 1, 1),
  pch     = c(20, NA, 20, NA),
  col     = c("darkred", transparent_red, "darkblue", transparent_blue),
  cex     = 0.85,
  lwd     = c(NA, 1, NA, 1),
  pt.cex  = c(1.5, 1.5, 1.5, 1.5),
  bty     = "n"
)
# OMIT? ---------------------

# Alternative: basic version
#legend(x=1.5, y=-0.05, title = "", 
#       legend = expression(delta(d), widetilde(MATT)(d) ),
#       lty = c(1,1), 
#       pch = c(20,20),
#       col = c("tomato","dodgerblue"),
#       cex=0.5, lwd=c(1.5,1.5), pt.cex = c(1.5,1.5), bty = "n")
#expression(paste("", delta, "(d)", sep = ""),  
#paste(" ", ~MATT+ "(d)", sep = "")), #c("MATT(d)", "tilde{MATT}^{+}(d)"), 
# ---------------------------

# Save image dimensions: 625x575
# ------------------------------------------------------

# %-change in overall estimate: 
(OMATTilde-OMATT)/OMATT * 100 
# ==========================================================================================
# ==========================================================================================
# ==========================================================================================


# ==========================================================================================
# Discrete Plots: 
# ==========================================================================================
# --------------------------------
# DENSITY PLOT (ePDF):
# --------------------------------
head(df_tilde)
ggplot(df_tilde, aes(x = D2_tilde)) +
  geom_histogram(color = 'black', fill = "dodgerblue", alpha = 0.7) +
  labs(
    x = "D (hours per week)",
    y = "Density, f(D)", 
    title = "Empirical PDF: Hours of Enrichment Activity"
  ) +
  annotate(
    "segment",
    x = 0, xend = 0, y = 0, yend = 1.125, # Line coordinates
    linetype = "solid",
    color = "black",
    size = 0.5
  ) +
  annotate(
    "segment",
    x = 0, xend = round(max(D2)+1), y = 0, yend = 0, # Line coordinates
    linetype = "solid",
    color = "black",
    size = 0.5
  ) + 
  stat_bin() +
  theme_bw()

ggplot(df_tilde, aes(x = D2_tilde)) +
  geom_bar(aes(y = ..prop.., group = 1), stat = "count", 
           color = 'black', fill = "dodgerblue", alpha = 0.7) +
  labs(
    x = "D (hours per week)",
    y = "Probability, P(D = d)",
    title = "Empirical PMF: Hours of Enrichment Activity",
    subtitle = "Low SES (2019)"
  ) +
  #annotate(
  #  "segment",
  #  x = 0, xend = 0, y = 0, yend = 1.125,
  #  linetype = "solid", color = "black", size = 0.5
  #) +
  annotate(
    "segment",
    x = 0, xend = round(max(df_tilde$D2_tilde)), y = 0, yend = 0,
    linetype = "solid", color = "black", size = 0.5
  ) +
  theme_bw()
# Note: image saved at (575 width)x(575 length).
# --------------------------------

# --------------------------------
# eCDF PLOT:
# --------------------------------
# Create quartile points for ggplot eCDF: 
quantile(D2_tilde)[2:4] # Quartiles.
Q1_D2 <- as.numeric(quantile(D2_tilde)[2])
Q2_D2 <- as.numeric(quantile(D2_tilde)[3])
Q3_D2 <- as.numeric(quantile(D2_tilde)[4])
quartile_points <- data.frame(
  quartile = c(Q1_D2, Q2_D2, Q3_D2),  # x-coordinates of the points (on the x-axis)
  Fd = c(0.25, 0.5, 0.75),  # y-coordinates (CDF values) of the points
  labelz=c("Q1", "Q2", "Q3")
)

ggplot(as.data.frame(D2_tilde), aes(x=D2_tilde)) + 
  stat_ecdf(geom="step", aes(color="eCDF"), color="black", size=0.55) + 
  theme_bw() + 
  geom_point(data=quartile_points, aes(x=quartile, y=Fd, color=labelz), size=4) + 
  scale_color_manual(values = c("Q3"="steelblue4", "Q2"="steelblue3", "Q1"="steelblue2"), 
                     limits = c("Q3","Q2","Q1")) +
  theme(legend.position=c(0.86,0.15)) +
  labs(x="D", y="Cumulative Probability, P(D ≤ d)", 
       title="Empirical CDF: Hours of Enrichment Activity", 
       subtitle = "Low SES (2019)", color="Quartile:") 
# Note: image saved at (575 width)x(575 length).
# --------------------------------
# ==========================================================================================

# ########################################################################################## #
# ########################################################################################## #
# ########################################################################################## #



# ########################################################################################## #
# ########################################################################################## #
# ########################################################################################## #
#                                                                                            #
# [III] ALGORITHM:                                                                           #
#                                                                                            #
# ########################################################################################## #
# ########################################################################################## #
# ########################################################################################## #
# NOTE: See email from Brantly Callaway. 

# =========================================================================================
# ORIGINAL CODE:
# =========================================================================================
library(lpSolve)

generate_sd <- function(d, k) {
  if (k == 1) {
    # Base case: Only one component, must equal d
    return(matrix(d, nrow = 1, ncol = 1))
  }
  
  # Recursive case: Allocate values to the first component and recurse
  result <- list()
  for (i in 0:d) {
    sub_vectors <- generate_sd(d - i, k - 1)
    result[[i + 1]] <- cbind(i, sub_vectors)
  }
  
  # Combine all results
  return(do.call(rbind, result))
}

compute_matrices <- function(d, K) {
  # Function to generate all s_d vectors
  generate_sd <- function(d, k) {
    if (k == 1) {
      # Base case: Only one component, must equal d
      return(matrix(d, nrow = 1, ncol = 1))
    }
    
    # Recursive case: Allocate values to the first component and recurse
    result <- list()
    for (i in 0:d) {
      sub_vectors <- generate_sd(d - i, k - 1)
      result[[i + 1]] <- cbind(i, sub_vectors)
    }
    
    # Combine all results
    return(do.call(rbind, result))
  }
  
  # Generate s_d and s_{d-1}
  s_d <- generate_sd(d, K)
  s_d_minus_1 <- generate_sd(d - 1, K)
  
  # Define M(d) as the Cartesian product of s_d and s_{d-1}
  M_d <- expand.grid(
    s_d_index = 1:nrow(s_d),             # Indices for s_d
    s_d_minus_1_index = 1:nrow(s_d_minus_1)  # Indices for s_{d-1}
  )
  
  # Add the actual vector pairs for clarity
  M_d$pair_s_d <- apply(M_d, 1, function(row) paste(s_d[as.numeric(row[1]), ], collapse = ","))
  M_d$pair_s_d_minus_1 <- apply(M_d, 1, function(row) paste(s_d_minus_1[as.numeric(row[2]), ], collapse = ","))
  
  # Function to check if a pair is in M_plus
  is_in_M_plus <- function(sd, sdmin1) {
    # Check if sd can be formed by adding 1 to exactly one position in sdmin1
    diff <- sd - sdmin1
    return(all(diff >= 0) && sum(diff) == 1)
  }
  
  # Classify M(d) into M_plus and M_minus
  M_d$in_M_plus <- apply(M_d, 1, function(row) {
    sd <- s_d[as.numeric(row["s_d_index"]), ]
    sdmin1 <- s_d_minus_1[as.numeric(row["s_d_minus_1_index"]), ]
    is_in_M_plus(sd, sdmin1)
  })
  
  # Separate M_plus and M_minus
  M_plus <- M_d[M_d$in_M_plus, ]
  M_minus <- M_d[!M_d$in_M_plus, ]
  
  # Return the results as a list
  return(list(M = M_d, M_plus = M_plus, M_minus = M_minus))
}

compute_matrices(2,3)

# Function to solve the optimization problem
solve_optimization <- function(d, K, psd, psdmin1) {
  # Compute M, M_plus, and M_minus
  matrices <- compute_matrices(d, K)
  M <- matrices$M
  M_plus <- matrices$M_plus
  M_minus <- matrices$M_minus
  
  # Number of variables
  num_vars <- nrow(M)
  
  # Objective function: Minimize weights in M_minus
  objective <- as.numeric(!M$in_M_plus)  # 1 for M_minus, 0 otherwise
  
  # handled automatically by lpSolve!
  
  # Constraints
  num_s_d <- length(psd)
  num_s_dmin1 <- length(psdmin1)
  A <- matrix(0, nrow = 1 + num_s_d + num_s_dmin1, ncol = num_vars)
  b <- c(1, psdmin1, psd)  # Right-hand side
  
  # Normalization constraint: Sum of all weights = 1
  A[1, ] <- 1
  
  # Row marginalization: Sum over s_d for each s_{d-1}
  for (j in 1:num_s_dmin1) {
    A[1 + j, M$s_d_minus_1_index == j] <- 1
  }
  
  # Column marginalization: Sum over s_{d-1} for each s_d
  for (i in 1:num_s_d) {
    A[1 + num_s_dmin1 + i, M$s_d_index == i] <- 1
  }
  
  # Solve the linear program
  result <- lp(
    direction = "min",
    objective.in = objective,
    const.mat = A,
    const.dir = rep("=", nrow(A)),
    const.rhs = b
  )
  
  # Process and return results
  if (result$status == 0) {
    cat("Optimal solution found:\n")
    M$weight <- result$solution
    M$incongruent_weight <- (!M$in_M_plus)*M$weight
    return(M)
  } else {
    cat("No feasible solution found.\n")
    return(NULL)
  }
}

# Example inputs
head(S_matrix)
d <- 1 # aggregate treatment level
K <- 4 # number of distinct sub-treatments
sd <- generate_sd(d,K)

# Understnading how sd works to match prob's:
sd
typeof(sd) # "double"
prod(sd[1,] == c(0,0,0,1))

sdmin1 <- generate_sd(d-1,K)
#psd <- rep(1/nrow(generate_sd(d, K)), nrow(generate_sd(d, K)))  # Uniform distribution for P(S=s_d | D=d)
#psdmin1 <- rep(1/nrow(generate_sd(d - 1, K)), nrow(generate_sd(d - 1, K)))  # Uniform for P(S=s_{d-1} | D=d-1)
nrow(sd)
nrow(sdmin1)
psd <- c(0,0,0,1) # length = nrow(sd), each element is the P(S=s_d|D=d).
#psd <- vector("numeric", length = nrow(sd))
psdmin1 <- c(1)

# Solve the optimization problem
solution <- solve_optimization(d, K, psd, psdmin1)
solution
# =========================================================================================


# =========================================================================================
# Apply Algorithm to Application:
# =========================================================================================
# [1] Get probs: 
# collect prob's of each S=s vector at each D=d:
prob_S_givenD <- c() # to store probabilities. 
S_counts <- means_by_S[ order(means_by_S$D), ] # sorted matrix on D.

for (d in D3_idx0) {
  #print(d) # loop index.
  df_aggD <- S_counts[S_counts$D==d,] # data frame for D=d.
  total_counts <- sum(df_aggD$n) # counts of realized S=s_d.
  
  # instantiate empty vec: 
  storage <- c()
  for (i in 1:nrow(df_aggD)) {
    storage[i] <- (df_aggD[i,]$n) / total_counts
  }
  # concatenate storage vector to old storage vector: 
  #print(prob_S_givenD)
  prob_S_givenD <- c(prob_S_givenD, storage)
}
prob_S_givenD # probability results.
S_counts <- as.data.frame(cbind(S_counts, prob_S_givenD))
head(S_counts)


# Example inputs
head(S_matrix)
d <- 1 # aggregate treatment level
K <- 4 # number of distinct sub-treatments
sd <- generate_sd(d,K)

# Understnading how sd works to match prob's:
sd
typeof(sd) # "double"
prod(sd[1,] == c(0,0,0,1))

sdmin1 <- generate_sd(d-1,K)
#psd <- rep(1/nrow(generate_sd(d, K)), nrow(generate_sd(d, K)))  # Uniform distribution for P(S=s_d | D=d)
#psdmin1 <- rep(1/nrow(generate_sd(d - 1, K)), nrow(generate_sd(d - 1, K)))  # Uniform for P(S=s_{d-1} | D=d-1)
nrow(sd)
nrow(sdmin1)
psd <- c(0,0,0,1) # length = nrow(sd), each element is the P(S=s_d|D=d).
#psd <- vector("numeric", length = nrow(sd))
psdmin1 <- c(1)

# Solve the optimization problem
solution <- solve_optimization(d, K, psd, psdmin1)
solution
# =========================================================================================
# Add to results matrix: 
# ----------------------
# D=1 (0.5)
# ----------------------
d <- 1 # aggregate treatment level
K <- 4 # number of distinct sub-treatments
sd <- generate_sd(d,K)
sdmin1 <- generate_sd(d-1,K)

psd <- c(0,0,0,1) # length = nrow(sd), each element is the P(S=s_d|D=d).
psdmin1 <- c(1)

# Solve the optimization problem
solution1 <- solve_optimization(d, K, psd, psdmin1)
solution1
# ----------------------

# ----------------------
# D=2 (1.0)
# ----------------------
d <- 2 # aggregate treatment level
K <- 4 # number of distinct sub-treatments
sd <- generate_sd(d,K)
sdmin1 <- generate_sd(d-1,K)
sd
sdmin1

psd <- c(0,0,0,0,
         0,0,0.06666667,0.26666667,
         0,0.66666667) # length = nrow(sd), each element is the P(S=s_d|D=d).
psdmin1 <- c(0,0,0,1)

# Solve the optimization problem
solution2 <- solve_optimization(d, K, psd, psdmin1)
solution2
# ----------------------

# ----------------------
# D=3 (1.5)
# ----------------------
d <- 3 # aggregate treatment level
K <- 4 # number of distinct sub-treatments
sd <- generate_sd(d,K)
sdmin1 <- generate_sd(d-1,K)

# input weights for s_d's:
sd
psd <- integer(nrow(sd))
psd[1] <- S_counts$prob_S_givenD[6]   # 0.1250
psd[4] <- S_counts$prob_S_givenD[7]   # 0.1875
psd[10] <- S_counts$prob_S_givenD[8]  # 0.0625
psd[11] <- S_counts$prob_S_givenD[9]  # 0.0625
psd[13] <- S_counts$prob_S_givenD[10] # 0.1875
psd[15] <- S_counts$prob_S_givenD[11] # 0.0625
psd[20] <- S_counts$prob_S_givenD[12] # 0.3125

# input weights for s_{d-1}'s:
sdmin1
psdmin1 <- c(0,0,0,0,
             0,0,0.06666667,0.26666667,
             0,0.66666667) # length = nrow(sd), each element is the P(S=s_d|D=d).

# Solve the optimization problem
solution3 <- solve_optimization(d, K, psd, psdmin1)
solution3
# ----------------------

# ----------------------
# D=4 (2.0)
# ----------------------
d <- 4 # aggregate treatment level
K <- 4 # number of distinct sub-treatments
sd <- generate_sd(d,K)
sdmin1 <- generate_sd(d-1,K)

# input weights for s_{d}'s:
sd
psd <- integer(nrow(sd))
psd[1] <- S_counts$prob_S_givenD[13]   # 0.2
psd[5] <- S_counts$prob_S_givenD[14]   # 0.3
psd[15] <- S_counts$prob_S_givenD[15]  # 0.1
psd[19] <- S_counts$prob_S_givenD[16]  # 0.3
psd[35] <- S_counts$prob_S_givenD[17]  # 0.1

# input weights for s_{d-1}'s:
sdmin1
psdmin1 <- integer(nrow(sdmin1))
psdmin1[1] <- S_counts$prob_S_givenD[6]   # 0.1250
psdmin1[4] <- S_counts$prob_S_givenD[7]   # 0.1875
psdmin1[10] <- S_counts$prob_S_givenD[8]  # 0.0625
psdmin1[11] <- S_counts$prob_S_givenD[9]  # 0.0625
psdmin1[13] <- S_counts$prob_S_givenD[10] # 0.1875
psdmin1[15] <- S_counts$prob_S_givenD[11] # 0.0625
psdmin1[20] <- S_counts$prob_S_givenD[12] # 0.3125

# Solve the optimization problem
solution4 <- solve_optimization(d, K, psd, psdmin1)
solution4
# ----------------------

# ----------------------
# D=5 (2.5)
# ----------------------
d <- 5 # aggregate treatment level
K <- 4 # number of distinct sub-treatments
sd <- generate_sd(d,K)
sdmin1 <- generate_sd(d-1,K)

# input weights for s_{d}'s:
sd
psd <- integer(nrow(sd))
psd[1] <- S_counts$prob_S_givenD[18]   # 0.2
psd[22] <- S_counts$prob_S_givenD[19]  # 0.2
psd[26] <- S_counts$prob_S_givenD[20]  # 0.2
psd[27] <- S_counts$prob_S_givenD[21]  # 0.2
psd[40] <- S_counts$prob_S_givenD[22]  # 0.2

# input weights for s_{d-1}'s:
sdmin1
psdmin1 <- integer(nrow(sdmin1))
psdmin1[1] <- S_counts$prob_S_givenD[13]   # 0.2
psdmin1[5] <- S_counts$prob_S_givenD[14]   # 0.3
psdmin1[15] <- S_counts$prob_S_givenD[15]  # 0.1
psdmin1[19] <- S_counts$prob_S_givenD[16]  # 0.3
psdmin1[35] <- S_counts$prob_S_givenD[17]  # 0.1

# Solve the optimization problem
solution5 <- solve_optimization(d, K, psd, psdmin1)
solution5
# ----------------------

# ----------------------
# D=6 (3.0)
# ----------------------
d <- 6 # aggregate treatment level
K <- 4 # number of distinct sub-treatments
sd <- generate_sd(d,K)
sdmin1 <- generate_sd(d-1,K)

# input weights for s_{d}'s:
sd
psd <- integer(nrow(sd))
psd[65] <- S_counts$prob_S_givenD[23]   # 0.3333333
psd[82] <- S_counts$prob_S_givenD[24]   # 0.6666667


# input weights for s_{d-1}'s:
sdmin1
psdmin1 <- integer(nrow(sdmin1))
psdmin1[1] <- S_counts$prob_S_givenD[18]   # 0.2
psdmin1[22] <- S_counts$prob_S_givenD[19]  # 0.2
psdmin1[26] <- S_counts$prob_S_givenD[20]  # 0.2
psdmin1[27] <- S_counts$prob_S_givenD[21]  # 0.2
psdmin1[40] <- S_counts$prob_S_givenD[22]  # 0.2

# Solve the optimization problem
solution6 <- solve_optimization(d, K, psd, psdmin1)
solution6
# ----------------------

# ________________________________________________
# ________________________________________________
# Begin Analysis: 
# ________________________________________________
# ________________________________________________

# ------------------------------------------------
# [1] Any weight on incongruent comparisons? 
# ------------------------------------------------
sum(solution1$incongruent_weight) # no
sum(solution2$incongruent_weight) # no
sum(solution3$incongruent_weight) # yes
sum(solution4$incongruent_weight) # yes
sum(solution5$incongruent_weight) # yes
sum(solution6$incongruent_weight) # yes
# ------------------------------------------------

# ------------------------------------------------
# [2] Where/what are the optimally congruent weights?
# ------------------------------------------------

# D=1 (0.5)
solution1[solution1$weight>0 | solution1$incongruent_weight>0,] # congruent/incongruent
sum(solution1$incongruent_weight) # total incongruent weight: none

# D=2 (1.0)
solution2[solution2$weight>0 | solution2$incongruent_weight>0,] # congruent/incongruent
sum(solution2$incongruent_weight) # total incongruent weight: none

# D=3 (1.5)
solution3[solution3$weight>0 | solution3$incongruent_weight>0,] # congruent/incongruent
sum(solution3$incongruent_weight) # total incongruent weight: 0.375

# D=4 (2.0)
solution4[solution4$weight>0 | solution4$incongruent_weight>0,] # congruent/incongruent
sum(solution4$incongruent_weight) # total incongruent weight: 0.3375

# D=5 (2.5)
solution5[solution5$weight>0 | solution5$incongruent_weight>0,] # congruent/incongruent
sum(solution5$incongruent_weight) # total incongruent weight: 0.40

# D=6 (3.0)
solution6[solution6$weight>0 | solution6$incongruent_weight>0,] # congruent/incongruent
sum(solution6$incongruent_weight) # total incongruent weight: 0.40

results_algorthm <- rbind(solution1[solution1$weight>0 | solution1$incongruent_weight>0,],
                          solution2[solution2$weight>0 | solution2$incongruent_weight>0,],
                          solution3[solution3$weight>0 | solution3$incongruent_weight>0,],
                          solution4[solution4$weight>0 | solution4$incongruent_weight>0,],
                          solution5[solution5$weight>0 | solution5$incongruent_weight>0,],
                          solution6[solution6$weight>0 | solution6$incongruent_weight>0,])
dim(results_algorthm)
head(results_algorthm)
results_algorthm <- results_algorthm[,-c(1,2)] # remove indices.
dim(results_algorthm)
head(results_algorthm)

# create D-column for table: 
Dlevel <- c(rep(1,nrow(solution1[solution1$weight>0 | solution1$incongruent_weight>0,])),
            rep(2,nrow(solution2[solution2$weight>0 | solution2$incongruent_weight>0,])),
            rep(3,nrow(solution3[solution3$weight>0 | solution3$incongruent_weight>0,])),
            rep(4,nrow(solution4[solution4$weight>0 | solution4$incongruent_weight>0,])),
            rep(5,nrow(solution5[solution5$weight>0 | solution5$incongruent_weight>0,])),
            rep(6,nrow(solution6[solution6$weight>0 | solution6$incongruent_weight>0,])))
results_algo <- as.data.frame(cbind(Dlevel, results_algorthm))
dim(results_algo)
head(results_algo)

# Add delta_Y's and MATTilde weights: 
MATTtilde_wts.nu <- c(0.319444444, # D=0.5
                      0, 0.013888889, 0.055555556, 0.138888889,   # D=1.0
                      0, 1, 0, 1, 1, 0, 0, 0, 1) # D=1.5
delta_y <- c(-0.2189263814, # D=0.5
             0, 0.3880582556, -0.2274522006, -0.0183392495, # D=1.0
             0, 0.3398067355, 0, -0.0001286889, 0.7522802874, 0, 0, 0, 0.0608896747, # D=1.5
             0 # D=2.0
             )

dim(results_algo)
dim(results_only)
# -------------------------------

# -------------------------------
# Table: 
# -------------------------------
install.packages("xtable")
library(xtable)

# Suppose your data frame is called `df`
print(
  xtable(results_algo, caption = "My Table Caption", label = "tab:mytable",
         include.rownames = FALSE,   # drop row names if you like
         booktabs = TRUE,             # use \toprule/\midrule/\bottomrule
         digits = c(1,    # D-level
                    0,    # s_d
                    0,    # s_{d-1}
                    0,    # in M+
                    0,    # congruent weight
                    4,    # congruent weight
                    4     # incongruent weight
         )
  ))
xtable(results)
head(results_algo)
results_only <- as.data.frame(results[,-c(6,7)])
view(results_only)


#install.packages("huxtable")
#library(huxtable)
#ht <- as_hux(results_algo)
#ht <- set_caption(ht, "My Table Caption")
#ht <- set_header_labels(ht, col1 = "New Name", col2 = "Another")
#print_latex(ht)
# -------------------------------

# ________________________________________________
# End Analysis 
# ________________________________________________

# --------------------------------------------------
# for algoithm to get MATTilde weights and delta_Y:
# --------------------------------------------------

# (1.) Get delta_Y:
results_table <- as.data.frame(results)
head(results_table)
colnames(results_table)[3] <- "$s_{d-1}$" # relabels columns.
colnames(results_table)[4] <- "$s_d$" # relabels columns.
head(results_table)
results_table <- results_table[ , c(1,2,4,3,5:ncol(results_table))] # swap columns.
head(results_table)
results_table <- results_table[ ,-c(1,2)] # remove columns. 
head(results_table)
head(results_table)
results_table <- results_table[ ,-c(4,5)] # remove columns. 
head(results_table)
xtable(results_table,digits=c(1,0,0,3,3))
view(results_table)

# --------------------------------------------------

# ########################################################################################## #
# ########################################################################################## #
# ########################################################################################## #

