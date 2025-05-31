library(data.table)
set.seed(100)

# Part 1: Hydrological Years and Catchment Selection

catchmentIDs <- paste0("C", 1:5)
years <- 2000:2002
months <- 1:12
data <- CJ(ID = catchmentIDs, YR = years, MNTH = months)

data[, PRCP := round(runif(.N, 0, 300), 1)]

data[, `:=`(
  OBS_RUN = round(PRCP * runif(.N, 0.2, 0.8), 1),
  PET = round(runif(.N, 10, 200), 1),
  SWE = round(runif(.N, 0, 100), 1)
)]

data[, HYR := fifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

cat("HYDROLOGICAL YEAR ASSIGNMENT CHECK:\n")
cat("These rows show how months are assigned to hydrological years (Oct-Dec months shift the year forward):\n\n")
print(data[MNTH %in% c(9, 10, 11, 12)][order(ID, YR, MNTH)])

runoffStats <- data[, .(
  total_PRCP = sum(PRCP, na.rm = TRUE),
  total_OBS_RUN = sum(OBS_RUN, na.rm = TRUE)
), by = ID]

runoffStats[, runoffCoeff := total_OBS_RUN / total_PRCP]

cat("\nRUNOFF COEFFICIENTS:\n")
cat("This table shows total rainfall (PRCP), total observed runoff, and their ratio (runoff coefficient) per catchment.\n")
cat("Runoff coefficient = how much rain becomes runoff (closer to 1 = more runoff, lower = more infiltration/evaporation).\n\n")
print(runoffStats)

runoffBreaks <- quantile(runoffStats$runoffCoeff, probs = seq(0, 1, 0.2), na.rm = TRUE)
runoffStats[, runoffClass := cut(runoffCoeff,
                                 breaks = runoffBreaks,
                                 labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                                 include.lowest = TRUE)]

chosenCatchments <- runoffStats[, .SD[sample(.N, 1)], by = runoffClass]

cat("\nSELECTED CATCHMENTS (ONE PER RUNOFF CLASS):\n")
cat("These catchments were randomly chosen, one from each runoff class (Very Low to Very High).\n\n")
print(chosenCatchments)

# Part 2: Water Balance and Snowmelt Contribution

catchmentData <- data[ID %in% chosenCatchments$ID]

monthlySum <- catchmentData[, .(
  mean_PRCP = mean(PRCP),
  mean_PET = mean(PET),
  mean_OBS_RUN = mean(OBS_RUN)
), by = .(HYR, ID, MNTH)]

monthlySum[, waterBalance := mean_PRCP - mean_PET]

deficitMonths <- monthlySum[waterBalance < 0]

annualSum <- catchmentData[, .(
  total_PRCP = sum(PRCP),
  total_PET = sum(PET),
  total_OBS_RUN = sum(OBS_RUN)
), by = .(HYR, ID)]

annualSum[, waterBalance := total_PRCP - total_PET]

cat("\nMONTHLY WATER BALANCE:\n")
cat("This preview shows average rainfall (PRCP), evapotranspiration (PET), and runoff for each month.\n")
cat("The 'waterBalance' column tells you if there's a surplus (>0) or deficit (<0) of water.\n\n")
print(head(monthlySum))

cat("\nANNUAL WATER BALANCE:\n")
cat("Annual totals for each catchment's rainfall, evapotranspiration, and runoff.\n")
cat("Positive water balance = more rain than PET. Negative = more PET than rain.\n\n")
print(head(annualSum))

cat("\nMONTHS IN WATER DEFICIT:\n")
cat("These are the months where evapotranspiration (PET) was greater than precipitation — meaning a dry period.\n\n")
print(deficitMonths)

snowWaterSum <- catchmentData[, .(
  mean_SWE = mean(SWE)
), by = .(HYR, ID, MNTH)]

maxSnowWater <- snowWaterSum[, .(
  max_SWE = max(mean_SWE)
), by = .(HYR, ID)]

springSnowData <- merge(
  snowWaterSum[MNTH %in% 3:5],
  maxSnowWater,
  by = c("HYR", "ID")
)

springSnowData[, snowmeltAmount := max_SWE - mean_SWE]

springMergedData <- merge(
  springSnowData,
  monthlySum[MNTH %in% 3:5],
  by = c("HYR", "ID", "MNTH")
)

snowmeltRunoffCor <- springMergedData[, .(
  correlation = cor(snowmeltAmount, mean_OBS_RUN, use = "complete.obs")
), by = ID]

cat("\nCORRELATION BETWEEN SNOWMELT AND RUNOFF (MARCH–MAY):\n")
cat("This table shows how strongly snowmelt and runoff are related for each catchment during spring months.\n")
cat("Correlation near +1 = strong positive link. Closer to 0 = weak/no link.\n\n")
print(snowmeltRunoffCor)

cat("\nINTERPRETATION OF SNOWMELT-RUNOFF CORRELATIONS:\n")
for (i in 1:nrow(snowmeltRunoffCor)) {
  rel <- ifelse(abs(snowmeltRunoffCor$correlation[i]) > 0.6, 
                "strong relationship", 
                "weaker relationship")
  msg <- sprintf("Catchment %s: Correlation = %.2f (%s)",
                 snowmeltRunoffCor$ID[i],
                 snowmeltRunoffCor$correlation[i],
                 rel)
  cat(msg, "\n")
}
