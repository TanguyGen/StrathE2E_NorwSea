#install.packages("StrathE2E2", repos="https://www.marineresourcemodelling.maths.strath.ac.uk/sran/")

rm(list=ls())

library(StrathE2E2)

decades <- c("2010-2019", "2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069")
scenarios <- list("CNRM_SSP126", "CNRM_SSP370","GFDL_SSP126","GFDL_SSP370")
list_models <- list.files("./Celtic_Sea_MA") # Obtain the name of each models

CNRM126_models<-list_models[grep(".*CNRM-ssp126.*", list_models)]
CNRM370_models<-list_models[grep(".*CNRM-ssp370.*", list_models)] 
GFDL126_models<-list_models[grep(".*GFDL-ssp126.*", list_models)] 
GFDL370_models<-list_models[grep(".*GFDL-ssp370.*", list_models)] 

#CNRM126
CNRM126<-lapply(CNRM126_models,function(variant) {
  e2e_read(model.name = "Celtic_Sea_MA", 
           model.variant = variant, 
           models.path = ".")
})
names(CNRM126)<-decades
CNRM126_res<-lapply(CNRM126,function(variant) {
  e2e_run(variant,
          nyear=1)
})

#CNRM370
CNRM370<-lapply(CNRM370_models,function(variant) {
  e2e_read(model.name = "Celtic_Sea_MA", 
           model.variant = variant, 
           models.path = ".")
})
names(CNRM370)<-decades
CNRM370_res<-lapply(CNRM370,function(variant) {
  e2e_run(variant,
          nyear=1)
})

#GFDL126
GFDL126<-lapply(GFDL126_models,function(variant) {
  e2e_read(model.name = "Celtic_Sea_MA", 
           model.variant = variant, 
           models.path = ".")
})
names(GFDL126)<-decades
GFDL126_res<-lapply(GFDL126,function(variant) {
  e2e_run(variant,
          nyear=1)
})

#GFDL370
GFDL370<-lapply(GFDL370_models,function(variant) {
  e2e_read(model.name = "Celtic_Sea_MA", 
           model.variant = variant, 
           models.path = ".")
})
names(GFDL370)<-decades
GFDL370_res<-lapply(GFDL370,function(variant) {
  e2e_run(variant,
          nyear=1)
})

list_models<-list(CNRM126,CNRM370,GFDL126,GFDL370)
list_results<-list(CNRM126_res,CNRM370_res,GFDL126_res,GFDL370_res)

names(list_models)<-scenarios
names(list_results)<-scenarios
saveRDS(list_models, "./temp/list_models.rds")
saveRDS(list_results, "./temp/list_resultss.rds")
