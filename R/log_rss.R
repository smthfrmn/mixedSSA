# suppressWarnings({
#   suppressPackageStartupMessages({
#     library(amt)
#     library(dplyr)
#     library(ggplot2)
#   })
# })
#
# #Load data
# data(goats, package = "ResourceSelection")
# #Change any SLOPE == 0 to 0.1
# goats$SLOPE[which(goats$SLOPE == 0)] <- 0.1
# #Transform covariates
# goats$ELEVATION_sc <- (goats$ELEVATION - mean(goats$ELEVATION))/sd(goats$ELEVATION)
# goats$SLOPE_sc <-  (goats$SLOPE - mean(goats$SLOPE))/sd(goats$SLOPE)
# goats$ELEVATION_log <- log(goats$ELEVATION)
# goats$SLOPE_log <- log(goats$SLOPE)
#
# #Visualize two covariates we'll use
# hist(goats$SLOPE)
#
# m1 <- fit_rsf(goats, STATUS ~ ELEVATION_sc + SLOPE_sc)
# m2 <- fit_rsf(goats, STATUS ~ ELEVATION_sc * SLOPE_sc)
# m3 <- fit_rsf(goats, STATUS ~ ELEVATION_sc + I(ELEVATION_sc^2) + SLOPE_sc)
# m4 <- fit_rsf(goats, STATUS ~ ELEVATION_sc * SLOPE_sc + I(ELEVATION_sc^2))
# m5 <- fit_rsf(goats, STATUS ~ ELEVATION_log)
# m6 <- fit_rsf(goats, STATUS ~ ELEVATION_log * SLOPE_sc)
# m7 <- fit_rsf(goats, STATUS ~ ELEVATION_log + SLOPE_log)
