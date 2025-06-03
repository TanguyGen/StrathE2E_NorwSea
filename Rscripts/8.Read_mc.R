rm(list=ls()) #clear environment

library(tidyverse)
library(purrr)


# Define base path and inputs
base_dir <- "./temp/Norwegian_Basin_MA"
decades <- c("2010-2019", "2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069")
scenarios <- c("CNRM_SSP126", "CNRM_SSP370", "GFDL_SSP126", "GFDL_SSP370")

# Function to construct path and read CSV
read_flux_file <- function(decade, scenario) {
  scenario_folder <- gsub("_", "-", scenario)
  file_name <- paste0("CredInt_cumulative_wholeannualflux-baseline_", decade, "-", scenario_folder, ".csv")
  file_path <- file.path(base_dir, paste0(decade, "-", scenario_folder), "CredInt", file_name)
  
  if (file.exists(file_path)) {
    df<-read_csv(file_path, show_col_types = FALSE)
    df <- as.data.frame(df)
    rownames(df) <- df[[1]]
    df <- df[, -1]
    df
  } else {
    warning("Missing file: ", file_path)
    NULL
  }
}

flux_mc <- set_names(scenarios) %>%
 map(~ set_names(decades) %>%
        map(~ read_flux_file(.x, .y), scenario = .x))

flux_mc_with_catch <- map(flux_mc, function(scenario_data) {
  map(scenario_data, function(df) {
    landing_cols <- grep("_landings_live_weight$", colnames(df), value = TRUE)
    
    for (land_col in landing_cols) {
      prefix <- sub("_landings_live_weight$", "", land_col)
      discard_col <- paste0(prefix, "_discards")
      catch_col <- paste0(prefix, "_catch")
      
      df[catch_col] <- df[land_col] + df[discard_col]
    }
    df
  })
})

flux_mc_by_group <- map(flux_mc_with_catch, function(scenario_data) {
  map(scenario_data, function(df) {
    df%>%
      mutate(
        Prod_primary=Phytoplankton_net_primary_production+Macrophyte_gross_production,
        Prod_fish=Planktiv.fish_net_production+Dem.fish_net_production+ Mig.fish_net_production,
        Prod_tp=Pinniped_net_production+Bird_net_production+Cetacean_net_production,
        Prod_zoo=Omniv.zooplankton_net_production+Carniv.zooplankton_net_production,
        Prod_benthos=Benthos_susp/dep_net_production+Benthos_carn/scav_net_production
      )
  })
})

flux_mc_anomaly <- map(scenarios, function(scenario) {
  baseline <- flux_mc_by_group[[scenario]][["2010-2019"]]
  
  map(decades, function(decade) {
    df <- flux_mc_by_group[[scenario]][[decade]]
    
    if (is.null(df)) return(NULL)  # In case file is missing
    
    if (decade == "2010-2019") {
      return(df) 
    } else {
      df_ratio <- (df -baseline)/baseline*100
      return(df_ratio)
    }
  }) %>% set_names(decades)
}) %>% set_names(scenarios)

saveRDS(flux_mc,"./temp/flux_mc.rds")

CredIntMakeAnnualFluxResults <- function(annualFluxData, plotGraphs = FALSE,  credProbs= c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  
  getCredibleIntervals <- function(values, likelihoods, probs, varName = "", plot = FALSE) {
    data <- data.frame(Value = values, Likelihood = likelihoods)
    data <- na.omit(data)
    
    if (nrow(data) == 0) return(rep(NA, length(probs)))
    
    uniqueVals <- length(unique(data$Value))
    isConstant <- uniqueVals == 1
    isZero <- all(data$Value == 0)
    
    if (isConstant) return(rep(data$Value[1], length(probs)))
    if (isZero) return(rep(0, length(probs)))
    
    data <- data[order(data$Value), ]
    data$CumLikelihood <- cumsum(data$Likelihood)
    data$CumProb <- data$CumLikelihood / sum(data$Likelihood)
    
    if (plot) {
      plot(data$Value, data$CumProb, type = "l", ylim = c(0, 1),
           main = varName, xlab = "Simulated values", ylab = "Cumulative likelihood")
    }
    
    interpolation <- approxfun(data$CumProb, data$Value, rule = 2)
    return(interpolation(probs))
  }
  
  statLabels <- c("maxlik", "lowlimit", "lowquart", "median", "uppquart", "upplimit")
  nVars <- ncol(annualFluxData) - 2
  varNames <- names(annualFluxData)[3:(nVars + 2)]
  
  # Initialize result data frame
  resultMatrix <- matrix(0, nrow = length(statLabels), ncol = nVars)
  rownames(resultMatrix) <- statLabels
  colnames(resultMatrix) <- varNames
  
  # Max likelihood values from first row
  resultMatrix[1, ] <- as.numeric(annualFluxData[1, 3:(nVars + 2)])
  
  # Calculate credible intervals for each variable
  for (i in 1:nVars) {
    values <- as.numeric(annualFluxData[, i + 2])
    likelihoods <- as.numeric(annualFluxData[, 2])
    resultMatrix[2:6, i] <- getCredibleIntervals(
      values, likelihoods, credProbs,
      varName = varNames[i],
      plot = plotGraphs
    )
  }
  
  return(as.data.frame(resultMatrix))
}

flux_ci <- map(flux_mc_anomaly, function(scenario_list) {
  map(scenario_list, function(flux_df) {
    if (!is.null(flux_df)) {
      CredIntMakeAnnualFluxResults(flux_df)
    } else {
      NULL
    }
  })
})

saveRDS(flux_ci,"./temp/flux_ci.rds")

