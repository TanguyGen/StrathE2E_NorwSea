# Setup ------------------------------------------------------------------
rm(list = ls())
library(StrathE2E2)
library(future)
library(furrr)
library(purrr)
library(dplyr)
library(tictoc)
library(progressr)
handlers("progress")

plan(multisession)  # Use multicore on Linux/macOS or multisession on Windows

decades <- c("2010-2019", "2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069")
scenarios <- c("CNRM_SSP126", "CNRM_SSP370", "GFDL_SSP126", "GFDL_SSP370")

# Load model variant names -----------------------------------------------
model_files <- list.files("./Norwegian_Basin_MA")

model_variants <- list(
  CNRM_SSP126 = grep(".*CNRM-ssp126.*", model_files, value = TRUE),
  CNRM_SSP370 = grep(".*CNRM-ssp370.*", model_files, value = TRUE),
  GFDL_SSP126 = grep(".*GFDL-ssp126.*", model_files, value = TRUE),
  GFDL_SSP370 = grep(".*GFDL-ssp370.*", model_files, value = TRUE)
)



mc <- future_map(scenarios, function(scenario) {
  
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
  
  
  mc <- map(decades,~e2e_run_mc(
      models[[.x]],
      nyears = 50,
      baseline.mode = FALSE,
      use.example.baseparms = FALSE,
      baseparms.ident = paste0("baseline_2010-2019-",sub("_SSP", "-ssp", scenario)),
      begin.sample = 1,
      n_iter = 1000,
      csv.output = TRUE,
      runtime.plot = TRUE,
      postprocess = TRUE)
  ) %>% set_names(decades)
  
  list(models = models, mc = mc)
}, .options = furrr_options(seed = TRUE)) %>%
  set_names(scenarios)

# Extract results into separate lists ------------------------------------
list_models <- map(results, "models")
list_results <- map(results, "results")
list_mc <- map(results, "mc")

# Save -------------------------------------------------------------------
saveRDS(list_models, "./temp/list_models.rds")
saveRDS(list_results, "./temp/list_results.rds")
saveRDS(list_mc, "./temp/list_mc.rds")
