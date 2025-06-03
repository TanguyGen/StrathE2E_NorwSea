rm(list = ls()) #clear environment

packages <- c("StrathE2E2", "dplyr", "ggplot2", "tidyr","purrr") #list of packages
lapply(packages, library, character.only = TRUE)

#Get the results list from 1.
models_lists <- readRDS("./temp/list_models.rds")

decades <- c("2010-2019")
scenarios <- c("CNRM_SSP126", "CNRM_SSP370", "GFDL_SSP126", "GFDL_SSP370")
base_dir <- "./temp/Norwegian_Basin_MA"



name_map <- c(
  "Obs_TAPP" = "Total phyt.",
  "Obs_NP" = "New primary phyt.",
  "Obs_KelpP" = "Macrophyte carbon",
  "Obs_OmnizooP" = "Omniv.zooplankton",
  "Obs_CarnzooP" = "Carniv.zooplankton",
  "Obs_PFishP" = "Planktiv.fish",
  "Obs_DFishP" = "Demersal fish",
  "Obs_BensuspP" = "Susp/dep.benthos",
  "Obs_BencarnP" = "Carn/scav.benthos",
  "Obs_birdP" = "Seabirds",
  "Obs_sealP" = "Pinnipeds",
  "Obs_cetaP" = "Cetaceans",
  "Obs_maxbenthslar" = "Susp/dep.benthos larv",
  "Obs_maxbenthclar" = "Carn/scav.benthos larv",
  "Obs_Conpfishfish" = "Plank.fish by fish",
  "Obs_Condfishfish" = "Dem.fish by fish",
  "Obs_Conzoofish" = "Zooplankton by fish",
  "Obs_Conzoocarnz" = "Omniv.zoo by carniv.zoo.",
  "Obs_Conbenfish" = "Benthos by fish",
  "Obs_Contotal_bird" = "Total by birds",
  "Obs_Proppfishbird" = "Plank.fish in bird diet",
  "Obs_Propdfishbird" = "Dem.fish in bird diet",
  "Obs_Propmfishbird" = "Mig.fish in bird diet",
  "Obs_Propdiscbird" = "Disc. in bird diet",
  "Obs_Contotal_seal" = "Total by pinnipeds",
  "Obs_Proppfishseal" = "Plank.fish in pinn. diet",
  "Obs_Propdfishseal" = "Dem.fish in pinn. diet",
  "Obs_Propmfishseal" = "Mig.fish in pinn. diet",
  "Obs_Contotal_ceta" = "Total by cetaceans",
  "Obs_Proppfishceta" = "Plank.fish in cet. diet",
  "Obs_Propdfishceta" = "Dem.fish in cet. diet",
  "Obs_Propmfishceta" = "Mig.fish in cet. diet",
  "Obs_Pland_livewt" = "Plank.fish landings",
  "Obs_Dland_livewt" = "Dem.fish landings",
  "Obs_Mland_livewt" = "Mig.fish landings",
  "Obs_Bsland_livewt" = "Susp/dep.benthos landings",
  "Obs_Bcland_livewt" = "Carn/scav.benthos landings",
  "Obs_Zcland_livewt" = "Pel.invert. landings",
  "Obs_Kland_livewt" = "Macrop. harvest",
  "Obs_kelp_pb" = "Macrop. P/B",
  "Obs_benslar_pb" = "Susp/dep.benthos larv. P/B",
  "Obs_benclar_pb" = "Carn/scav.benthos larv. P/B",
  "Obs_bens_pb" = "Susp/dep.benthos P/B",
  "Obs_benc_pb" = "Carn/scav.benthos P/B",
  "Obs_omni_pb" = "Omniv.zooplankton P/B",
  "Obs_carn_pb" = "Carniv.zooplankton P/B",
  "Obs_fishplar_pb" = "Plank.fish larvae P/B",
  "Obs_fishdlar_pb" = "Dem.fish larvae P/B",
  "Obs_fishp_pb" = "Plank.fish P/B",
  "Obs_fishd_pb" = "Dem.fish P/B",
  "Obs_fishm_pb" = "Mig.fish P/B",
  "Obs_bird_pb" = "Bird P/B",
  "Obs_seal_pb" = "Pinniped P/B",
  "Obs_ceta_pb" = "Cetacean P/B",
  "Obs_exud_C_kelp" = "Prop. macrop. prod. exuded",
  "Obs_kelp_NC" = "Macrop. N/C ratio",
  "Obs_Denitrif" = "Denitrification",
  "Obs_Dfdiscardp" = "Dem.fish discard/catch",
  "Obs_s_x_ammonia" = "Sand porewater ammonia",
  "Obs_d_x_ammonia" = "Mud porewater ammonia",
  "Obs_s_x_nitrate" = "Sand porewater nitrate",
  "Obs_d_x_nitrate" = "Mud porewater nitrate",
  "Obs_s_x_TON" = "Sand %TON",
  "Obs_d_x_TON" = "Mud %TON",
  "Obs_NDJF_s_chl" = "Winter surf.chlorophyll",
  "Obs_MJJA_s_chl" = "Summer surf.chlorophyll",
  "Obs_NDJF_s_nitrate" = "Winter surf.nitrate",
  "Obs_MJJA_s_nitrate" = "Summer surf.nitrate",
  "Obs_NDJF_d_nitrate" = "Winter deep nitrate",
  "Obs_MJJA_d_nitrate" = "Summer deep nitrate",
  "Obs_NDJF_s_ammonia" = "Winter surf.ammonia",
  "Obs_MJJA_s_ammonia" = "Summer surf.ammonia",
  "Obs_NDJF_d_ammonia" = "Winter deep ammonia",
  "Obs_MJJA_d_ammonia" = "Summer deep ammonia",
  "Obs_carn_io_ratio" = "Carniv.zooplanton_io",
  "Obs_omni_io_ratio" = "Omniv.zooplankton_io",
  "Obs_phyt_io_ratio" = "Surf.phytoplankton_io",
  "Obs_nit_io_ratio" = "Surf.nitrate_io",
  "Obs_amm_io_ratio" = "Surf.ammonia_io",
  "Obs_pfish_io_ratio" = "Plank.fish_io",
  "Obs_dfish_io_ratio" = "Dem.fish_io",
  "Obs_birddisc" = "Bird by-catch",
  "Obs_sealdisc" = "Pinniped by-catch",
  "Obs_cetadisc" = "Cetacean by-catch",
  "Obs_kelp_beachcast" = "Macrop. beach-cast",
  "Obs_Ctland_livewt" = "Cetacean landings"
)
category_map <- list(
  Production = c(
    "Total phyt.",
    "New primary phyt.",
    "Macrophyte carbon",
    "Denitrification",
    "Omniv.zooplankton",
    "Carniv.zooplankton",
    "Susp/dep.benthos",
    "Carn/scav.benthos",
    "Planktiv.fish",
    "Demersal fish",
    "Seabirds",
    "Pinnipeds",
    "Cetaceans"
  ),
  
  Consumption = c(
    "Plank.fish by fish",
    "Dem.fish by fish",
    "Zooplankton by fish",
    "Omniv.zoo by carniv.zoo.",
    "Benthos by fish",
    "Total by birds",
    "Total by pinnipeds",
    "Total by cetaceans",
    "Plank.fish in bird diet",
    "Dem.fish in bird diet",
    "Mig.fish in bird diet",
    "Disc. in bird diet",
    "Plank.fish in pinn. diet",
    "Dem.fish in pinn. diet",
    "Mig.fish in pinn. diet",
    "Plank.fish in cet. diet",
    "Dem.fish in cet. diet",
    "Mig.fish in cet. diet"
  ),
  
  Catches = c(
    "Plank.fish landings",
    "Dem.fish landings",
    "Mig.fish landings",
    "Susp/dep.benthos landings",
    "Carn/scav.benthos landings",
    "Pel.invert. landings",
    "Macrop. harvest",
    "Cetacean landings",
    "Bird by-catch",
    "Pinniped by-catch",
    "Cetacean by-catch",
    "Dem.fish discard/catch"
  ),
  
  Annual_ratios = c(
    "Macrop. P/B",
    "Susp/dep.benthos larv. P/B",
    "Carn/scav.benthos larv. P/B",
    "Susp/dep.benthos P/B",
    "Carn/scav.benthos P/B",
    "Omniv.zooplankton P/B",
    "Carniv.zooplankton P/B",
    "Plank.fish larvae P/B",
    "Dem.fish larvae P/B",
    "Plank.fish P/B",
    "Dem.fish P/B",
    "Mig.fish P/B",
    "Bird P/B",
    "Pinniped P/B",
    "Cetacean P/B",
    "Prop. macrop. prod. exuded",
    "Macrop. N/C ratio",
    "Macrop. beach-cast"
  ),
  
  Concentrations = c(
    "Winter surf.chlorophyll",
    "Summer surf.chlorophyll",
    "Winter surf.nitrate",
    "Summer surf.nitrate",
    "Winter deep nitrate",
    "Summer deep nitrate",
    "Winter surf.ammonia",
    "Summer surf.ammonia",
    "Winter deep ammonia",
    "Summer deep ammonia",
    "Sand porewater nitrate",
    "Mud porewater nitrate",
    "Sand porewater ammonia",
    "Mud porewater ammonia",
    "Sand %TON",
    "Mud %TON"
  )
)
assign_categories <- function(df) {
  df$Category <- sapply(df$Name, function(name) {
    cat <- names(Filter(function(x)
      name %in% x, category_map))
    if (length(cat) == 0)
      NA
    else
      cat
  })
  df %>%
    drop_na(Category)
}



read_obs_file <- function(scenario) {
  decade = "2010-2019"
  scenario_folder <- gsub("_", "-", scenario)
  file_name <- paste0(
    "CredInt_processed_targetresults-baseline_",
    decade,
    "-",
    scenario_folder,
    ".csv"
  )
  file_path <- file.path(base_dir,
                         paste0(decade, "-", scenario_folder),
                         "CredInt",
                         file_name)
  
  if (file.exists(file_path)) {
    df <- read.csv(file_path)
    df <- as.data.frame(df)
    rownames(df) <- df[[1]]
    df <- df[, -1]
    colnames(df) <- name_map
    df["Name", ] <- colnames(df)
    df <- as.data.frame(t(df))
    df%>%
      drop_na()
  } else {
    warning("Missing file: ", file_path)
    NULL
  }
}
obs_mc <- set_names(scenarios) %>%
  map( ~ read_obs_file(.x))

obs_mc <- map(obs_mc, assign_categories)

read_target <- function(scenario) {
  decade = "2010-2019"
  obs_path <- paste0(
    "./Norwegian_Basin_MA/",
    decade,
    "-",
    gsub("_", "-", scenario),
    "/Target/",
    "annual_observed_NORWEGIAN_BASIN_",
    decade,
    ".csv"
  )
  if (file.exists(obs_path)) {
    annualtargetdata <- read.csv(obs_path)
    annualtargetdata <- as.data.frame(annualtargetdata)
    annualtargetdata[, 4] <- recode(annualtargetdata[, 4], !!!name_map)
    annualtargetdata
  } else {
    warning("Missing file: ", obs_path)
    NULL
  }
}

target <-  set_names(scenarios) %>%
  map( ~ read_target(.x))

target <- imap(target, function(df, name) {
  shared_names <- intersect(df$Name, rownames(obs_mc[[name]]))
  df[df$Name %in% shared_names, , drop = FALSE]
})


target <- map(target, assign_categories)
target <- map(target, ~ filter(.x, Use1_0 == 1))

obs_mc <- imap(obs_mc, ~ {
  target_names <- as.character(target[[.y]]$Name)
  .x[target_names, , drop = FALSE]
})


##Monthly

read_obs_file_monthly <- function(scenario) {
  decade = "2010-2019"
  scenario_folder <- gsub("_", "-", scenario)
  file_name <- paste0(
    "CredInt_processed_monthly_mass-baseline_",
    decade,
    "-",
    scenario_folder,
    ".csv"
  )
  file_path <- file.path(base_dir,
                         paste0(decade, "-", scenario_folder),
                         "CredInt",
                         file_name)
  
  if (file.exists(file_path)) {
    df <- read.csv(file_path)
    
    df <- as.data.frame(df)
    colnames(df)<- c("Name",month.name)
    df<- df%>%
      separate(Name, c("Name", "Quantile"), "-")
    df
  } else {
    warning("Missing file: ", file_path)
    NULL
  }
}
obs_mc_month <- set_names(scenarios) %>%
  map( ~ read_obs_file_monthly(.x)%>%
         mutate(Name=ifelse(Name=="surfchlmgm3","surface_chlorophyll",Name)))

read_target_monthly <- function(scenario) {
  decade = "2010-2019"
  obs_path <- paste0(
    "./Norwegian_Basin_MA/",
    decade,
    "-",
    gsub("_", "-", scenario),
    "/Target/",
    "monthly_observed_NORWEGIAN_BASIN_",
    decade,
    ".csv"
  )
  if (file.exists(obs_path)) {
    monthlytargetdata <- read.csv(obs_path)
    monthlytargetdata <- as.data.frame(monthlytargetdata)%>%
      drop_na(median)
    monthlytargetdata
  } else {
    warning("Missing file: ", obs_path)
    NULL
  }
}

target_month <-  set_names(scenarios) %>%
  map( ~ read_target_monthly(.x))

target_month <- imap(target_month, function(df, name) {
  shared_names <- intersect(df$Variable, obs_mc_month[[name]]$Name)
  df[df$Variable %in% shared_names, , drop = FALSE]
}) 

obs_mc_month <- imap(obs_mc_month, ~ {
  target_names <- as.character(target_month[[.y]]$Name)
  .x[target_names, , drop = FALSE]
})




plot_comparaison <- function(scenario) {
  model <- obs_mc[[scenario]]
  model$lowquart <- as.numeric(model$lowquart)
  model$uppquart <- as.numeric(model$uppquart)
  model$median <- as.numeric(model$median)
  model$lowlimit <- as.numeric(model$lowlimit)
  model$upplimit <- as.numeric(model$upplimit)
  
  obs <- target[[scenario]]
  
  # Generate quantiles from the normal distribution
  obs <- obs %>%
    mutate(
      q25  = qnorm(0.25,  mean = Annual_measure, sd = SD_of_measure),
      q50  = qnorm(0.50,  mean = Annual_measure, sd = SD_of_measure),
      q75  = qnorm(0.75,  mean = Annual_measure, sd = SD_of_measure),
      q975 = qnorm(0.975, mean = Annual_measure, sd = SD_of_measure),
      q025 = pmax(qnorm(0.025, mean = Annual_measure, sd = SD_of_measure),q50/1000)
    )
  # Convert Name to factor for consistent ordering
  all_names <- unique(c(obs$Name, model$Name))
  obs$Name <- factor(obs$Name, levels = all_names)
  model$Name <- factor(model$Name, levels = all_names)
  
  ggplot() +
    # Simulated boxplot from normal dist
    geom_boxplot(
      data = obs,
      aes(
        y = Name,
        xmiddle = log10(q50),
        xlower  = log10(q25),
        xupper  = log10(q75),
        xmin    = log10(q025),
        xmax    = log10(q975),
        group   = Name
      ),
      stat = "identity",
      width = 0.4,
      fill = "#FF6663",
      color = "#FF6663",
      alpha = 0.5
    ) +
    # Model boxplot
    geom_boxplot(
      data = model,
      aes(
        y = Name,
        xlower = log10(lowquart),
        xupper = log10(uppquart),
        xmiddle = log10(median),
        xmin = log10(lowlimit),
        xmax = log10(upplimit),
        group = Name
      ),
      stat = "identity",
      width = 0.6,
      color = "black",
      fill = "#CFCCD6",
      alpha = 0.5
    ) +
    facet_wrap(~ Category, scales = "free", ncol = 2) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 10),
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_rect(color = "black", linewidth = 0.8),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      panel.spacing = unit(1, "lines")
    ) +
    labs(x = "log10(Value)", y = "")
}



plot_comparaison("CNRM_SSP126")

ggsave("./Plots/cnrm_compare.pdf", width = 1866/90, height = 1080/90)

plot_comparaison("GFDL_SSP126")

ggsave("./Plots/gfdl_compare.pdf", width = 1866/90, height = 1080/90)
