#install.packages("StrathE2E2", repos="https://www.marineresourcemodelling.maths.strath.ac.uk/sran/")

rm(list=ls()) #clear environment

library(StrathE2E2)
decades <- c("2010-2019", "2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069") #list of decades used in the model
scenarios <- list("CNRM_SSP126", "CNRM_SSP370","GFDL_SSP126","GFDL_SSP370") #list of climate forcings-scenarios

list_models <- list.files("./Norwegian_Basin_MA") # Obtain the name of each models

#Get the names of each of the models for each of the scenarios
CNRM126_models<-list_models[grep(".*CNRM-ssp126.*", list_models)] 
CNRM370_models<-list_models[grep(".*CNRM-ssp370.*", list_models)] 
GFDL126_models<-list_models[grep(".*GFDL-ssp126.*", list_models)] 
GFDL370_models<-list_models[grep(".*GFDL-ssp370.*", list_models)] 

#CNRM126
#Create "Models" for each of the variants
CNRM126<-lapply(CNRM126_models,function(variant) {
  e2e_read(model.name = "Norwegian_Basin_MA", 
           model.variant = variant, 
 model.ident = paste0("baseline_", variant),
           models.path = ".",
           results.path = "./temp")
})
names(CNRM126)<-decades #name them by their corresponding decade
#Do a run for each of the models
CNRM126_res<-lapply(CNRM126,function(variant) {
  e2e_run(variant,
          nyear=50)
})

#CNRM370
#Create "Models" for each of the variants
CNRM370<-lapply(CNRM370_models,function(variant) {
  e2e_read(model.name = "Norwegian_Basin_MA", 
           model.variant = variant, 
           model.ident = paste0("baseline_", variant),
           models.path = ".",
           results.path = "./temp")
})
names(CNRM370)<-decades #name them by their corresponding decade
#Do a run for each of the models
CNRM370_res<-lapply(CNRM370,function(variant) {
  e2e_run(variant,
          nyear=50)
})

#GFDL126
#Create "Models" for each of the variants
GFDL126<-lapply(GFDL126_models,function(variant) {
  e2e_read(model.name = "Norwegian_Basin_MA", 
           model.variant = variant, 
           model.ident = paste0("baseline_", variant),
           models.path = ".",
           results.path = "./temp")
})
names(GFDL126)<-decades #name them by their corresponding decade
#Do a run for each of the models
GFDL126_res<-lapply(GFDL126,function(variant) {
  e2e_run(variant,
          nyear=50)
})

#GFDL370
#Create "Models" for each of the variants
GFDL370<-lapply(GFDL370_models,function(variant) {
  e2e_read(model.name = "Norwegian_Basin_MA", 
           model.variant = variant, 
           model.ident = paste0("baseline_", variant),
           models.path = ".",
           results.path = "./temp")
})
names(GFDL370)<-decades #name them by their corresponding decade
#Do a run for each of the models
GFDL370_res<-lapply(GFDL370,function(variant) {
  e2e_run(variant,
          nyear=50)
})

list_models<-list(CNRM126,CNRM370,GFDL126,GFDL370)#regroup all models into a list
list_results<-list(CNRM126_res,CNRM370_res,GFDL126_res,GFDL370_res)#regroup all results into a list

#name the element of the list by their associated scenario 
names(list_models)<-scenarios 
names(list_results)<-scenarios

#Save the lists
saveRDS(list_models, "./temp/list_models.rds")
saveRDS(list_results, "./temp/list_results.rds")
