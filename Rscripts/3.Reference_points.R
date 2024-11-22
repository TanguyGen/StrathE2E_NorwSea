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

pd_yield_data <-e2e_run_ycurve(model, selection="DEMERSAL", nyears=10, HRvector=PFHRvector)
e2e_plot_ycurve(model, selection="DEMERSAL", results=pd_yield_data,
                title="Demersal yield with baseline demersal fishing")

max(pf_yield_data$PlankFishland)
pf_yield_data

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
