rm(list=ls()) #clear environment

library(furrr)
library(purrr)
library(dplyr)
library(future)
library(tidyr)
library(StrathE2E2)
plan(multisession)

base_dir <- "./temp/Norwegian_Basin_MA"
decades <- c("2010-2019", "2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069")
scenarios <- c("CNRM_SSP126", "CNRM_SSP370", "GFDL_SSP126", "GFDL_SSP370")
gear_mult_values <- seq(0, 10, 0.2)  # You can add more as needed

# Load model variant names
model_files <- list.files("./Norwegian_Basin_MA")
model_variants <- list(
  CNRM_SSP126 = grep(".*CNRM-ssp126.*", model_files, value = TRUE),
  CNRM_SSP370 = grep(".*CNRM-ssp370.*", model_files, value = TRUE),
  GFDL_SSP126 = grep(".*GFDL-ssp126.*", model_files, value = TRUE),
  GFDL_SSP370 = grep(".*GFDL-ssp370.*", model_files, value = TRUE)
)

# Run all combinations
tictoc::tic()
mc_results <- future_map(scenarios, function(scenario) {
  variants <- model_variants[[scenario]]
  
  models <- map(variants,  function(variant) {
    e2e_read(
      model.name = "Norwegian_Basin_MA",
      model.variant = variant,
      models.path = ".",
      results.path = "./temp",
      model.ident = paste0("baseline_", variant)
    )
  }) %>% set_names(decades)
    
  montecarlo <- map(decades, function(decade) {
    
      map(gear_mult_values, function(mult) {
        
        if (mult==1){
          scenario_folder <- gsub("_", "-", scenario)
          file_name <- paste0("CredInt_processed_annualflux_whole-baseline_", decade, "-", scenario_folder, ".csv")
          file_path <- file.path(base_dir, paste0(decade, "-", scenario_folder), "CredInt", file_name)
          baseline=read.csv(file_path)
          data_pf=baseline
          data_df=baseline
        }else{
          model<-models[[decade]]
          model$data$fleet.model$HRscale_vector_multiplier[1]<-mult
            
          mc_pf<-e2e_run_mc(
            model,
            nyears = 10,
            baseline.mode = FALSE,
            use.example.baseparms = FALSE,
            baseparms.ident = paste0("baseline_",decade,"-", sub("_SSP", "-ssp", scenario)),
            begin.sample = 1,
            n_iter = 30,
            csv.output = FALSE,
            runtime.plot = FALSE,
            postprocess = TRUE
          )
          
          data_pf<-mc_pf[["CI_annual_fluxes"]][["whole"]]
          
          mc_df<-e2e_run_mc(
            model,
            nyears = 10,
            baseline.mode = FALSE,
            use.example.baseparms = FALSE,
            baseparms.ident = paste0("baseline_",decade,"-", sub("_SSP", "-ssp", scenario)),
            begin.sample = 1,
            n_iter = 30,
            csv.output = FALSE,
            runtime.plot = FALSE,
            postprocess = TRUE
          )
          
          data_df<-mc_df[["CI_annual_fluxes"]][["whole"]]
        }
      list_res<-list(pf=data_pf,df=data_df)
      file.path<-paste0("./temp/Norwegian_Basin_MA/",decade,"-", sub("_SSP", "-ssp", scenario),"/HR/HR",mult,".rds")
      saveRDS(list_res,file.path)
      }) %>% set_names(paste0("HR_", gear_mult_values))
      
    }) %>% set_names(decades)
    
    list(models = models, mc = montecarlo)
    
}, .options = furrr_options(seed = TRUE)) %>%
  set_names(scenarios)
tictoc::toc()

saveRDS(mc_results, "./temp/mc_msy.rds")
