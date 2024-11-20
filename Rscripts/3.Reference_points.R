rm(list=ls()) #clear environment

packages<- c("StrathE2E2", "dplyr", "ggplot2", "patchwork") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
models_lists<-readRDS("./temp/list_models.rds")

PFHRvector <- seq(0,5,by=0.1)  # set up the vector of harvest ratios

# Plot example data for one of the North Sea model versions internal to the package
model <- models_lists$CNRM_SSP126$`2010-2019`
pf_yield_data <-e2e_run_ycurve(model, selection="PLANKTIV", nyears=10, HRvector=PFHRvector)

e2e_plot_ycurve(model, selection="PLANKTIV", results=pf_yield_data,
                title="Planktivorous yield with baseline demersal fishing")

max(pf_yield_data$PlankFishland)
pf_yield_data

#Then make a plot of the dependence of cetaceans on planktivorous fish harvesting rate
par(mfrow=c(2,1))
par(mar=c(3.2,5,2,0.8))
ym<-1.1*max(pf_yield_data$Birdbiom)
plot(pf_yield_data$PlankFishHRmult,pf_yield_data$Birdbiom,ylim=c(0,ym),type="l",
     lwd=3,yaxt="n",xaxt="n",ann=FALSE)
abline(v=1,lty="dashed")
axis(side=1,las=1,cex.axis=0.9)
axis(side=2,las=1,cex.axis=0.9)
mtext("Planktiv. fish harvest ratio multiplier",cex=1,side=1,line=2)
mtext("Cetacean biomass",cex=1,side=2,line=3.5)
mtext(bquote("mMN.m"^-2),cex=0.7,side=3,line=-0.05,adj=-0.18)
ym<-1.1*max(pf_yield_data$Birddisc)
plot(pf_yield_data$PlankFishHRmult,pf_yield_data$Birddisc,ylim=c(0,ym),type="l",
     lwd=3,yaxt="n",xaxt="n",ann=FALSE)
abline(v=1,lty="dashed")
axis(side=1,las=1,cex.axis=0.9)
axis(side=2,las=1,cex.axis=0.9)
mtext("Planktiv. fish harvest ratio multiplier",cex=1,side=1,line=2)
mtext("Cetacean by-catch",cex=1,side=2,line=3.5)
mtext(bquote("mMN.m"^-2 ~ ".y"^-1),cex=0.7,side=3,line=-0.05,adj=-0.18)
