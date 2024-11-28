rm(list=ls()) #clear environment

packages<- c("StrathE2E2", "dplyr", "ggplot2", "patchwork","pbapply","tictoc") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
models_lists<-readRDS("./temp/list_models.rds")

HRvector <- seq(0,5,by=0.1)  # set up the vector of harvest ratios

tic()
fisheries_hr<-names(models_lists)%>%
  pblapply( function(scenario) { 
  # Access each scenario
  scenario_models <- models_lists[[scenario]] #for each of the scenarios
  
  # Loop through decades
  lapply(names(scenario_models), function(decade) {
    model<-scenario_models[[decade]]
    pf_yield_data <-e2e_run_ycurve(model, selection="PLANKTIV", nyears=10, HRvector=HRvector)
    pd_yield_data <-e2e_run_ycurve(model, selection="DEMERSAL", nyears=10, HRvector=HRvector)
    Planktibiom<-c(pf_yield_data$PlankFishbiom)
    Plankticatch<-c(pf_yield_data$PlankFishland+pf_yield_data$PlankFishdisc)
    Dembiom<-pd_yield_data$DemFishbiom
    Demcatch<-pd_yield_data$DemFishland+pd_yield_data$DemFishdisc
    # Create a data frame
    data.frame(
      Decade = decade,
      Planktibiom = Planktibiom,
      Plankticatch = Plankticatch,
      Dembiom = Dembiom,
      Demcatch = Demcatch,
      Scenario = scenario,
      Harvest_ratio=HRvector
    )
  }) %>% bind_rows() # Combine decades for the scenario
})
scenarios <- list("CNRM_SSP126", "CNRM_SSP370","GFDL_SSP126","GFDL_SSP370") #list of climate forcings-scenarios
names(fisheries_hr)<-scenarios
saveRDS(fisheries_hr,file="./temp/Fisheries_MSY.RDS")
toc()

MSY <- fisheries_hr$CNRM_SSP126%>%
  group_by(Decade)%>%
  mutate(MSY=max(Plankticatch),
         HR_max = Harvest_ratio[which.max(Plankticatch)])
sapply(fisheries_df$CNRM_SSP126$Plankticatch, max)

ggplot(fisheries_hr$CNRM_SSP126,aes(x = Harvest_ratio, y = Plankticatch, color = Decade, group = Decade)) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = MSY$MSY, color = Decade),linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept=MSY$HR_max,color=Decade),linetype = "dashed", size = 1)+
  labs(title = "Annual biomass of plantivorous fish\nover time for all scenarios",
       x = "",
       y = "Catch (mMN/m2/y)",
       color = "Decade") +
  scale_color_viridis_d()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5,margin=margin(0,0,40,0)),
        text = element_text(size = 26))



# Plot example data for one of the North Sea model versions internal to the package
model <- models_lists$CNRM_SSP126$`2010-2019`
pf_yield_data <-e2e_run_ycurve(model, selection="PLANKTIV", nyears=10, HRvector=HRvector)

e2e_plot_ycurve(model, selection="PLANKTIV", results=pf_yield_data,
                title="Planktivorous yield with baseline demersal fishing")

pd_yield_data <-e2e_run_ycurve(model, selection="DEMERSAL", nyears=10, HRvector=HRvector)
e2e_plot_ycurve(model, selection="DEMERSAL", results=pd_yield_data,
                title="Demersal yield with baseline demersal fishing")


#Then make a plot of the dependence of Cetaceans on planktivorous and demersal fish harvesting rate
# Define ymax for each plot
ymax_biom <- 1.1 * max(pf_yield_data$Cetaceanbiom)

# Plot for planktivorous fish
Cetaceanplankti <- ggplot(pf_yield_data, aes(x = PlankFishHRmult, y = Cetaceanbiom)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0, ymax_biom)) +
  labs(
    x = "Planktivorous fish harvest ratio multiplier",
    y = "Cetacean biomass"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )



ymax_biom_d <- 1.1 * max(pd_yield_data$Cetaceanbiom)

# Plot for Cetacean with demersal harvest ratio
Cetaceandemer <- ggplot(pd_yield_data, aes(x = DemFishHRmult, y = Cetaceanbiom)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0, ymax_biom_d)) +
  labs(
    x = "Demersal fish harvest ratio multiplier",
    y = "Cetacean by-catch"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )



#Then make a plot of the dependence of Cetaceans on planktivorous fish harvesting rate
# Define ymax for each plot
ymax_biom <- 1.1 * max(pf_yield_data$Cetaceanbiom)

# Plot for Cetacean biomass
Cetaceanplankti <- ggplot(pf_yield_data, aes(x = PlankFishHRmult, y = Cetaceanbiom)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0, ymax_biom)) +
  labs(
    x = "Planktivorous fish harvest ratio multiplier",
    y = "Cetacean biomass"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )



# Define ymax for each plot
ymax_biom_d <- 1.1 * max(pd_yield_data$Cetaceanbiom)

# Plot for Demersal fish
Cetaceandemer <- ggplot(pd_yield_data, aes(x = DemFishHRmult, y = Cetaceanbiom)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0, ymax_biom_d)) +
  labs(
    x = "Demersal fish harvest ratio multiplier",
    y = "Cetacean biomass"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )

Cetaceanplankti/Cetaceandemer


#Then make a plot of the dependence of Birds on planktivorous fish harvesting rate
# Define ymax for each plot
ymax_biom <- 1.1 * max(pf_yield_data$Birdbiom)

# Plot for Bird biomass
Birdplankti <- ggplot(pf_yield_data, aes(x = PlankFishHRmult, y = Birdbiom)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0, ymax_biom)) +
  labs(
    x = "Planktivorous fish harvest ratio multiplier",
    y = "Bird biomass"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )



# Define ymax for each plot
ymax_biom_d <- 1.1 * max(pd_yield_data$Birdbiom)

# Plot for Demersal fish
Birddemer <- ggplot(pd_yield_data, aes(x = DemFishHRmult, y = Birdbiom)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0, ymax_biom_d)) +
  labs(
    x = "Demersal fish harvest ratio multiplier",
    y = "Bird biomass"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )

Birdplankti/Birddemer

#Then make a plot of the dependence of Pinnipeds on planktivorous fish harvesting rate
# Define ymax for each plot
ymax_biom <- 1.1 * max(pf_yield_data$Pinnipedbiom)

# Plot for Pinniped biomass
pinnipedplankti <- ggplot(pf_yield_data, aes(x = PlankFishHRmult, y = Pinnipedbiom)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0, ymax_biom)) +
  labs(
    x = "Planktivorous fish harvest ratio multiplier",
    y = "Pinniped biomass"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )



# Define ymax for each plot
ymax_biom_d <- 1.1 * max(pd_yield_data$Pinnipedbiom)

# Plot for Demersal fish
pinnipeddemer <- ggplot(pd_yield_data, aes(x = DemFishHRmult, y = Pinnipedbiom)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0, ymax_biom_d)) +
  labs(
    x = "Demersal fish harvest ratio multiplier",
    y = "Pinniped biomass"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(5.5, 10, 5.5, 5.5)
  )

pinnipedplankti/pinnipeddemer
