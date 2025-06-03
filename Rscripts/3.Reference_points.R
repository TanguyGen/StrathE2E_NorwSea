rm(list=ls()) #clear environment

packages<- c("StrathE2E2", "dplyr","tidyr", "ggplot2", "patchwork","pbapply","tictoc") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
models_lists<-readRDS("./temp/list_models.rds")
decades <- c("2010-2019", "2020-2029", "2030-2039", "2040-2049", "2050-2059", "2060-2069")

# 
# tic()
# run_ycurve <- function(model, selection, hr_field, nyears = 10, threshold = 0.01) {
#   HR_min <- 0
#   HR_max <- 10
#   diff_highest_catches <- Inf
#   all_yield_data <- tibble()  # To store all intermediate yield data as a tibble
# 
#   
#   while (diff_highest_catches > threshold) {
#     # Generate HRvector with unique, ascending values
#     HRvector <- seq(HR_min, HR_max, length.out = 10)  # Ensure at least 5 values
#     
#     # Run the model
#     yield_data <- e2e_run_ycurve(model, selection = selection, nyears = nyears, HRvector = HRvector)
#     
#     # Convert to a tibble and calculate catches
#     yield_data <- yield_data%>%
#       mutate(
#       Harvest_Ratio = yield_data[[hr_field]],
#       Catches = c(yield_data$PlankFishland + yield_data$PlankFishdisc)  # For PLANKTIV
#     )
#     if (selection == "DEMERSAL") {
#       yield_data$Catches <- c(yield_data$DemFishland + yield_data$DemFishdisc)  # For DEMERSAL
#     }
#     
#     # Append the new data, removing duplicates, and sort by Harvest Ratio
#     all_yield_data <- all_yield_data %>%
#       bind_rows(yield_data) %>%  # Append new data
#       distinct(Harvest_Ratio, .keep_all = TRUE) %>%  # Remove duplicates based on Harvest Ratio
#       arrange(Harvest_Ratio)
#     
#     # Calculate catches and identify top harvest ratios
#     catches <- all_yield_data$Catches
#     hr_values <- all_yield_data$Harvest_Ratio
#     sorted_indices <- order(catches, decreasing = TRUE)
#     top_hr_values <- hr_values[sorted_indices[1:2]]
#     
#     # Update the difference between highest catches
#     diff_highest_catches <- catches[sorted_indices[1]] - catches[sorted_indices[2]]
#     diff_highest_hr<-
#     
#     # Adjust HR_min and HR_max to focus on the top region
#     if (diff_highest_catches > threshold) {
#       HR_min <- min(top_hr_values)
#       HR_max <- max(top_hr_values)
#     } else {
#       max_catch_hr <- hr_values[sorted_indices[1]]
#       HR_min <- 0.9 * max_catch_hr
#       HR_max <- 1.1 * max_catch_hr
#       HRvector <- seq(HR_min, HR_max, length.out = 10)  # Higher precision
#       yield_data_refined <- e2e_run_ycurve(model, selection = selection, nyears = nyears, HRvector = HRvector)
#       
#       # Add refined yield data and exit loop
#       all_yield_data <- bind_rows(all_yield_data, as_tibble(yield_data_refined)) %>%
#         distinct(across(hr_field), .keep_all = TRUE) %>%
#         arrange(across(hr_field))
#       break
#     }
#   }
#   
#   return(all_yield_data)  # Return all intermediate yield data as a tibble
# }
# 
# # Main execution loop
# fisheries_hr <- names(models_lists) %>%
#   pblapply(function(scenario) {
#     scenario_models <- models_lists[[scenario]]  # Access each scenario
#     
#     lapply(names(scenario_models), function(decade) {
#       model <- scenario_models[[decade]]
#       
#       # Run adjustments for planktivorous and demersal fish
#       pf_yield_data_all <- run_ycurve(
#         model, selection = "PLANKTIV", hr_field = "PlankFishHRmult"
#       )%>%  # Sort by Harvest Ratio
#         select(-Catches,-Harvest_Ratio)
#       df_yield_data_all <- run_ycurve(
#         model, selection = "DEMERSAL", hr_field = "DemFishHRmult"
#       )%>%  # Sort by Harvest Ratio
#         select(-Catches,-Harvest_Ratio)
#       
#       # Return results for the decade
#       list(
#         Decade = decade,
#         Scenario = scenario,
#         pf_Yield_Data_All = pf_yield_data_all,  # All PF yield data as tibble
#         df_Yield_Data_All = df_yield_data_all   # All PD yield data as tibble
#       )
#     }) %>% setNames(names(scenario_models)) # Name by decades
#   }) %>% setNames(names(models_lists)) # Name by scenarios

tic()
fisheries_hr<-names(models_lists)%>%
  pblapply( function(scenario,Step=0.2,nyears=50) {
    
    HRvector <- seq(0,10,by=Step)
    # Access each scenario
    scenario_models <- models_lists[[scenario]] #for each of the scenarios
    # Loop through decades
    lapply(names(scenario_models), function(decade) {
      model<-scenario_models[[decade]]
      pf_yield_data <-e2e_run_ycurve(model, selection="PLANKTIV", nyears=nyears, HRvector=HRvector)
      df_yield_data <-e2e_run_ycurve(model, selection="DEMERSAL", nyears=nyears, HRvector=HRvector)
      # Create a data frame
      list(
        Decade = decade,
        Scenario = scenario,
        pf_yield_data = pf_yield_data,  # All PF yield data as tibble
        df_yield_data = df_yield_data   # All PD yield data as tibble
      )
    }) %>% setNames(names(scenario_models)) # Name by decades
  }) %>% setNames(names(models_lists)) # Name by scenarios


 saveRDS(fisheries_hr,file="./temp/Fisheries_MSY.RDS")
toc() #1h51


# PLOT 1

#fisheries_hr<-readRDS("./temp/Fisheries_MSY.RDS")


# Prepare the data with MSY and HR for each scenario
All_pf <- bind_rows(lapply(fisheries_hr, function(scenario_data) {
  bind_rows(lapply(scenario_data, function(decade_data) {
    decade_data$pf_yield_data %>%
      mutate(
        Decade = decade_data$Decade,
        Catches = PlankFishland + PlankFishdisc,
        Harvest_ratio=PlankFishHRmult,
        Scenario = decade_data$Scenario
      )
  }))
}))

# Calculate MSY for each scenario and decade
msy_pf <- All_pf %>%
  group_by(Scenario, Decade) %>%
  summarise(
    MSY = max(Catches),
    HR_at_MSY = Harvest_ratio[which.max(Catches)],
    .groups = 'drop'
  )

All_pf <- All_pf %>%
  left_join(msy_pf, by = c("Scenario", "Decade"))


loess_predictions_pf <- All_pf %>%
  group_by(Scenario, Decade,HR_at_MSY) %>%
  summarise(
    grid = list(seq(min(Harvest_ratio), max(Harvest_ratio), length.out = 500)),
    loess_model = list(loess(Catches ~ Harvest_ratio))
  ) %>%
  rowwise() %>%
  mutate(
    prediction = list(tibble(
      Harvest_ratio = grid,
      Catches_pred = predict(loess_model, newdata = data.frame(Harvest_ratio = grid))
    ))
  ) %>%
  select(-grid, -loess_model) %>%
  unnest(prediction)

# Plot grid of histograms with MSY-based color
ggplot(data = All_pf, aes(x = Harvest_ratio, y = Catches, fill = HR_at_MSY)) +
  geom_point()+
  geom_ribbon(
    aes(x = Harvest_ratio, ymin = 0, ymax = Catches, group = interaction(Scenario, Decade),fill = HR_at_MSY),
    inherit.aes = FALSE,
    alpha = 0.6
  ) +
  scale_fill_gradientn(colors = c("red","purple","blue","darkblue"),
                       name="Harvest ratio at MSY",
                       guide = guide_colorbar(
                         alpha=0.6
                       )) +
  labs(
    title = "",
    x = "Harvest Ratio Multiplier",
    y = "Catches (mMN)"
  ) +
  geom_vline(xintercept=1, linetype="dashed", color = "black")+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 20),
    legend.title = element_text(size = 14,
                                margin = margin(r=5,b = 10)),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.5, "cm"),       # Increase the overall legend key size
    legend.key.height = unit(0.8, "cm"),     # Adjust legend key height
    legend.key.width = unit(2, "cm")         # Adjust legend key width
  ) +
  facet_grid(Scenario~Decade)  # Facet by Scenario and HR_at_MSY





#DEMERSAL



# Prepare the data with MSY and HR for each scenario
All_df <- bind_rows(lapply(fisheries_hr, function(scenario_data) {
  bind_rows(lapply(scenario_data, function(decade_data) {
    decade_data$df_yield_data %>%
      mutate(
        Decade = decade_data$Decade,
        Catches = DemFishland + DemFishdisc,
        Harvest_ratio=DemFishHRmult,
        Scenario = decade_data$Scenario
      )
  }))
}))

# Calculate MSY for each scenario and decade
msy_data <- All_df %>%
  group_by(Scenario, Decade) %>%
  summarise(
    MSY = max(Catches),
    HR_at_MSY = Harvest_ratio[which.max(Catches)],
    .groups = 'drop'
  )

# Add MSY information to all_decades_data
All_df <- All_df %>%
  left_join(msy_data, by = c("Scenario", "Decade"))

loess_predictions_df <- All_df %>%
  group_by(Scenario, Decade,HR_at_MSY) %>%
  summarise(
    grid = list(seq(min(Harvest_ratio), max(Harvest_ratio), length.out = 500)),
    loess_model = list(loess(Catches ~ Harvest_ratio))
  ) %>%
  rowwise() %>%
  mutate(
    prediction = list(tibble(
      Harvest_ratio = grid,
      Catches_pred = predict(loess_model, newdata = data.frame(Harvest_ratio = grid))
    ))
  ) %>%
  select(-grid, -loess_model) %>%
  unnest(prediction)

# Plot grid of histograms with MSY-based color
ggplot(data = All_df, aes(x = Harvest_ratio, y = Catches, fill = HR_at_MSY)) +
  geom_point()+
  geom_ribbon(
    aes(x = DemFishHRmult, ymin = 0, ymax = Catches, group = interaction(Scenario, Decade),fill=HR_at_MSY),
    inherit.aes = FALSE,
    alpha = 0.6
  ) +
  scale_fill_gradientn(colors = c("red","purple","blue","darkblue"), 
                       guide = guide_colorbar(alpha=0.6),
                       ) +
  labs(
    title = "Harvest ratio of demersal fish",
    x = "Harvest Ratio Multiplier",
    y = "Catches"
  ) +
  geom_vline(xintercept=1, linetype="dashed", color = "black")+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 20),
    legend.title = element_text(size = 14,
                                margin = margin(r=5,b = 10)),
    legend.text = element_text(size = 12),
    legend.key.size = unit(2, "cm"),       # Increase the overall legend key size
    legend.key.height = unit(0.8, "cm"),     # Adjust legend key height
    legend.key.width = unit(3, "cm")         # Adjust legend key width
  ) +
  facet_grid(Scenario ~ Decade)  # Facet by Scenario and HR_at_MSY



# PLOT 2


# Prepare your data with a Transparency variable (already done in Pf2)
Pf2 <- All_pf %>%
  group_by(Decade, Scenario) %>%
  mutate(
    Scenario_Decade=paste0(Scenario,"_",Decade),
    TenPercent = if_else(
      Catches < MSY * 0.95 | Catches > MSY * 1.05,
      FALSE,
      TRUE
    )
  )

ribbon_data <- Pf2 %>%
  group_by(Decade,Harvest_ratio ) %>%
  summarise(
    ymin = min(Catches),  # Minimum y-value for the ribbon
    ymax = max(Catches)   # Maximum y-value for the ribbon
  )

# Plot with smooth lines and dynamic transparency
ggplot(data = Pf2, aes(x = PlankFishHRmult, y = Catches)) +
  geom_point(aes(x=HR_at_MSY,y = MSY,type=as.factor(Scenario)),size=0.5)+
  # Filter and apply high transparency for one smooth line
  geom_line(stat="smooth",method = "loess",
    data = filter(Pf2, TenPercent), 
    aes(group=as.factor(Scenario_Decade)),
    alpha = 1, se = FALSE
  ) +
  geom_ribbon(data = ribbon_data, 
              aes(x = Harvest_ratio, ymin = ymin, ymax = ymax, fill = Decade), 
              alpha = 0.2, inherit.aes = FALSE) +
  ylim(c(0,max(Pf2$Catches)*1.1))+
  labs(
    title = "Harvest Ratio of Planktivorous Fish",
    x = "Harvest Ratio Multiplier",
    y = "Catches",
    color = "Scenario",
    linetype = "Decade"
  ) + 
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "bottom"
  )
 
# Prepare your data with a Transparency variable (already done in Pf2)
Df2 <- All_df %>%
  group_by(Decade, Scenario) %>%
  mutate(
    Scenario_Decade=paste0(Scenario,"_",Decade),
    TenPercent = if_else(
      Catches < MSY * 0.95 | Catches > MSY * 1.05,
      FALSE,
      TRUE
    )
  )

ribbon_data <- Df2 %>%
  group_by(Decade,Harvest_ratio ) %>%
  summarise(
    ymin = min(Catches),  # Minimum y-value for the ribbon
    ymax = max(Catches)   # Maximum y-value for the ribbon
  )

# Plot with smooth lines and dynamic transparency
ggplot(data = Df2, aes(x = DemFishHRmult, y = Catches)) +
  geom_point(aes(x=HR_at_MSY,y = MSY,type=as.factor(Scenario)),size=0.5)+
  # Filter and apply high transparency for one smooth line
  geom_line(stat="smooth",method = "loess",
            data = filter(Df2, TenPercent), 
            aes(group=as.factor(Scenario_Decade)),
            alpha = 1, se = FALSE
  ) +
  geom_ribbon(data = ribbon_data, 
              aes(x = Harvest_ratio, ymin = ymin, ymax = ymax, fill = Decade), 
              alpha = 0.2, inherit.aes = FALSE) +
  ylim(c(0,max(Df2$Catches)*1.1))+
  labs(
    title = "Harvest Ratio of Demersal Fish",
    x = "Harvest Ratio Multiplier",
    y = "Catches",
    color = "Scenario",
    linetype = "Decade"
  ) + 
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    legend.position = "bottom"
  )


#Plot 3
#fisheries_hr<-readRDS("./temp/Fisheries_MSY.RDS")


Scenario_colors <- c(
  "CNRM_SSP126" = "#1B9AAA",
  "CNRM_SSP370" = "#A0DDE6",
  "GFDL_SSP126" = "#D7263D",
  "GFDL_SSP370" = "#F26B6B"
)

Decade_colors <- c(
  "2010-2019" = "#073b4c",
  "2020-2029" = "#118ab2",
  "2030-2039" = "#06d6a0",
  "2040-2049" = "#ffd166",
  "2050-2059" = "#f78c6b",
  "2060-2069" = "#ef476f"
)

# Prepare the data with MSY and HR for each scenario
All_pf <- bind_rows(lapply(fisheries_hr, function(scenario_data) {
  bind_rows(lapply(scenario_data, function(decade_data) {
    decade_data$pf_yield_data %>%
      mutate(
        Decade = decade_data$Decade,
        Catches = PlankFishland + PlankFishdisc,
        Harvest_ratio=PlankFishHRmult,
        Scenario = decade_data$Scenario
      )
  }))
}))

# Calculate MSY for each scenario and decade
msy_pf <- All_pf %>%
  group_by(Scenario, Decade) %>%
  summarise(
    MSY = max(Catches),
    HR_at_MSY = Harvest_ratio[which.max(Catches)],
    .groups = 'drop'
  )

All_pf <- All_pf %>%
  left_join(msy_pf, by = c("Scenario", "Decade"))


loess_predictions_pf <- All_pf %>%
  group_by(Scenario, Decade,HR_at_MSY) %>%
  summarise(
    grid = list(seq(min(Harvest_ratio), max(Harvest_ratio), length.out = 500)),
    loess_model = list(loess(Catches ~ Harvest_ratio))
  ) %>%
  rowwise() %>%
  mutate(
    prediction = list(tibble(
      Harvest_ratio = grid,
      Catches_pred = predict(loess_model, newdata = data.frame(Harvest_ratio = grid))
    ))
  ) %>%
  select(-grid, -loess_model) %>%
  unnest(prediction)


All_pf <- All_pf %>%
  group_by(Decade, Scenario) %>%
  summarise(max_catch = max(Catches, na.rm = TRUE), .groups = "drop") %>%
  arrange(Decade, desc(max_catch)) %>%
  group_by(Decade) %>%
  mutate(scenario_rank = row_number()) %>%
  ungroup() %>%
  select(Decade, Scenario, scenario_rank) %>%
  right_join(All_pf, by = c("Decade", "Scenario")) %>%
  mutate(Scenario = forcats::fct_reorder2(Scenario, Decade, -scenario_rank))



# Plot grid of histograms with MSY-based color
p1<-ggplot(data = All_pf, aes(x = Harvest_ratio, y = Catches, fill = Scenario)) +
  geom_ribbon(
    aes(x = PlankFishHRmult, ymin = 0, ymax = Catches, group = interaction(Scenario, Decade),fill=Scenario),
    color="black",
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  scale_fill_manual(values = Scenario_colors,
                    breaks =c("CNRM_SSP126", "CNRM_SSP370", "GFDL_SSP126", "GFDL_SSP370"))+
  labs(
    title = "",
    x = "Harvest Ratio Multiplier",
    y = "Catches"
  ) +
  geom_vline(xintercept=1, linetype="dashed", color = "black")+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 20),
    legend.title = element_text(size = 14,
                                margin = margin(r=5,b = 10)),
    legend.text = element_text(size = 12),
    legend.key.size = unit(2, "cm"),       # Increase the overall legend key size
    legend.key.height = unit(0.8, "cm"),     # Adjust legend key height
    legend.key.width = unit(3, "cm")         # Adjust legend key width
  ) +
  facet_grid(~Decade)  # Facet by Scenario and HR_at_MSY





#DEMERSAL



# Prepare the data with MSY and HR for each scenario
All_df <- bind_rows(lapply(fisheries_hr, function(scenario_data) {
  bind_rows(lapply(scenario_data, function(decade_data) {
    decade_data$df_yield_data %>%
      mutate(
        Decade = decade_data$Decade,
        Catches = DemFishland + DemFishdisc,
        Harvest_ratio=DemFishHRmult,
        Scenario = decade_data$Scenario
      )
  }))
}))

# Calculate MSY for each scenario and decade
msy_data <- All_df %>%
  group_by(Scenario, Decade) %>%
  summarise(
    MSY = max(Catches),
    HR_at_MSY = Harvest_ratio[which.max(Catches)],
    .groups = 'drop'
  )

# Add MSY information to all_decades_data
All_df <- All_df %>%
  left_join(msy_data, by = c("Scenario", "Decade"))

loess_predictions_df <- All_df %>%
  group_by(Scenario, Decade,HR_at_MSY) %>%
  summarise(
    grid = list(seq(min(Harvest_ratio), max(Harvest_ratio), length.out = 500)),
    loess_model = list(loess(Catches ~ Harvest_ratio))
  ) %>%
  rowwise() %>%
  mutate(
    prediction = list(tibble(
      Harvest_ratio = grid,
      Catches_pred = predict(loess_model, newdata = data.frame(Harvest_ratio = grid))
    ))
  ) %>%
  select(-grid, -loess_model) %>%
  unnest(prediction)

All_df_ordered <- All_df %>%
  group_by(Decade, Scenario) %>%
  summarise(max_catch = max(Catches, na.rm = TRUE), .groups = "drop") %>%
  arrange(Decade, desc(max_catch)) %>%
  group_by(Decade) %>%
  mutate(scenario_rank = row_number()) %>%
  ungroup() %>%
  select(Decade, Scenario, scenario_rank) %>%
  right_join(All_df, by = c("Decade", "Scenario")) %>%
  mutate(Scenario = forcats::fct_reorder2(Scenario, Decade, -scenario_rank))

# Plot grid of histograms with MSY-based color

p2<-ggplot(data = All_df_ordered, aes(x = Harvest_ratio, y = Catches, fill = Scenario)) +
  geom_ribbon(
    aes(x = Harvest_ratio, ymin = 0, ymax = Catches, group = interaction(Decade,Scenario),fill=Scenario),
    color="black",
    inherit.aes = FALSE,
    alpha = 0.5
  ) +
  scale_fill_manual(values = Scenario_colors,
                    breaks =c("CNRM_SSP126", "CNRM_SSP370", "GFDL_SSP126", "GFDL_SSP370"))+
  labs(
    title = "",
    x = "Harvest Ratio Multiplier",
    y = "Catches"
  ) +
  geom_vline(xintercept=1, linetype="dashed", color = "black")+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 20),
    legend.title = element_text(size = 14,
                                margin = margin(r=5,b = 10)),
    legend.text = element_text(size = 12),
    legend.key.size = unit(2, "cm"),       # Increase the overall legend key size
    legend.key.height = unit(0.8, "cm"),     # Adjust legend key height
    legend.key.width = unit(3, "cm")         # Adjust legend key width
  ) +
  facet_grid(~Decade, scales = "free_x", drop = TRUE)  # Facet by Scenario and HR_at_MSY

p1/p2+ 
  plot_layout(guides = "collect")& theme(legend.position = "bottom")
