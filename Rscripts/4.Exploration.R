library(StrathE2E2)
rm(list=ls()) #clear environment
models_lists<-readRDS("./temp/list_models.rds")
results_lists<-readRDS("./temp/list_results.rds")


#CNRM
e2e_compare_obs(
  selection = "ANNUAL",
  model= models_lists$GFDL_SSP126$`2030-2039`,
  use.saved=TRUE,
  ci.data = TRUE
)

e2e_compare_obs(
  selection = "MONTHLY",
  model= models_lists$GFDL_SSP126$`2010-2019`,
  use.saved=TRUE,
  ci.data = TRUE
)


e2e_compare_runs_bar(
  selection = "AAM",
  model1 = models_lists$CNRM_SSP126$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$CNRM_SSP126$`2010-2019`,
  model2 = models_lists$CNRM_SSP370$`2010-2019`,
  use.saved2 = FALSE,
  results2=results_lists$CNRM_SSP370$`2010-2019`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)

#GFDL

e2e_compare_obs(
  selection = "ANNUAL",
  model= models_lists$GFDL_SSP126$`2010-2019`,
  results = results_lists$GFDL_SSP126$`2010-2019`
)

e2e_compare_obs(
  selection = "MONTHLY",
  model= models_lists$GFDL_SSP126$`2010-2019`,
  results = results_lists$GFDL_SSP126$`2010-2019`
)

e2e_compare_runs_bar(
  selection = "AAM",
  model1 = models_lists$GFDL_SSP126$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$GFDL_SSP126$`2010-2019`,
  model2 = models_lists$GFDL_SSP370$`2010-2019`,
  use.saved2 = FALSE,
  results2=results_lists$GFDL_SSP370$`2010-2019`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)

#Inter-model
e2e_compare_runs_bar(
  selection = "CATCH",
  model1 = models_lists$CNRM_SSP126$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$CNRM_SSP126$`2010-2019`,
  model2 = models_lists$GFDL_SSP126$`2010-2019`,
  use.saved2 = FALSE,
  results2=results_lists$GFDL_SSP126$`2010-2019`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-70),
  bpmax = (+70),
  maintitle = ""
)

e2e_compare_runs_bar(
  selection = "AAM",
  model1 = models_lists$CNRM_SSP370$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$CNRM_SSP370$`2010-2019`,
  model2 = models_lists$GFDL_SSP370$`2010-2019`,
  use.saved2 = FALSE,
  results2=results_lists$GFDL_SSP370$`2010-2019`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-70),
  bpmax = (+70),
  maintitle = ""
)


#CNRM



e2e_compare_runs_bar(
  selection = "AAM",
  model1 = models_lists$CNRM_SSP126$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$CNRM_SSP126$`2010-2019`,
  model2 = models_lists$CNRM_SSP126$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$CNRM_SSP126$`2060-2069`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)

e2e_compare_runs_bar(
  selection = "AAM",
  model1 = models_lists$CNRM_SSP370$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$CNRM_SSP370$`2010-2019`,
  model2 = models_lists$CNRM_SSP370$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$CNRM_SSP370$`2060-2069`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)



e2e_compare_runs_bar(
  selection = "CATCH",
  model1 = models_lists$CNRM_SSP126$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$CNRM_SSP126$`2010-2019`,
  model2 = models_lists$CNRM_SSP126$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$CNRM_SSP126$`2060-2069`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)
e2e_compare_runs_bar(
  selection = "CATCH",
  model1 = models_lists$CNRM_SSP370$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$CNRM_SSP370$`2010-2019`,
  model2 = models_lists$CNRM_SSP370$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$CNRM_SSP370$`2060-2069`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)

#GFDL

e2e_compare_runs_bar(
  selection = "AAM",
  model1 = models_lists$GFDL_SSP126$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$GFDL_SSP126$`2010-2019`,
  model2 = models_lists$GFDL_SSP126$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$GFDL_SSP126$`2060-2069`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)


e2e_compare_runs_bar(
  selection = "AAM",
  model1 = models_lists$GFDL_SSP370$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$GFDL_SSP370$`2010-2019`,
  model2 = models_lists$GFDL_SSP370$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$GFDL_SSP370$`2060-2069`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)


e2e_compare_runs_bar(
  selection = "CATCH",
  model1 = models_lists$GFDL_SSP126$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$GFDL_SSP126$`2010-2019`,
  model2 = models_lists$GFDL_SSP126$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$GFDL_SSP126$`2060-2069`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)

e2e_compare_runs_bar(
  selection = "CATCH",
  model1 = models_lists$GFDL_SSP370$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$GFDL_SSP370$`2010-2019`,
  model2 = models_lists$GFDL_SSP370$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$GFDL_SSP370$`2060-2069`,
  log.pc = "PC",
  zone = "W",
  bpmin = (-50),
  bpmax = (+50),
  maintitle = ""
)


e2e_compare_runs_box(
  selection = "MONTHLY",
  model1 = models_lists$GFDL_SSP126$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$GFDL_SSP126$`2010-2019`,
  model2 = models_lists$GFDL_SSP126$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$GFDL_SSP126$`2060-2069`
)

e2e_compare_runs_box(
  selection = "ANNUAL",
  model1 = models_lists$CNRM_SSP126$`2010-2019`,
  use.saved1 = FALSE,
  results1=results_lists$CNRM_SSP126$`2010-2019`,
  model2 = models_lists$CNRM_SSP126$`2060-2069`,
  use.saved2 = FALSE,
  results2=results_lists$CNRM_SSP126$`2060-2069`
)

e2e_plot_migration(
  models_lists$CNRM_SSP126$`2010-2019`,
  ci.data = FALSE,
  use.saved = FALSE,
  use.example = FALSE,
  results = results_lists$CNRM_SSP126$`2010-2019`
)

e2e_optimize_hr(
  models_lists$CNRM_SSP126$`2010-2019`,
  nyears = 10,
  n_iter = 5,
  start_temperature = 1,
  cooling = 0.975,
  quiet = TRUE,
  csv.output = TRUE,
  runtime.plot = TRUE
)

e2e_compare_runs_box(selection="ANNUAL", model1=models_lists$CNRM_SSP126$`2010-2019`, ci.data1=TRUE, use.saved1=TRUE,
                     model2=models_lists$CNRM_SSP126$`2020-2029`, ci.data2=TRUE,use.saved2=TRUE)



