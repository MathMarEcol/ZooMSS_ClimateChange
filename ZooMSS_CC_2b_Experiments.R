library(tidyverse)
library(yaImpute)

source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS","_LatestModel","20220315_TheMatrix2","")

runs <- c("Control", "FixedCarbon")

#### Load ZooMSS Matrix Data ####
enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20220315_TheMatrix2/Control/ClimateChange_Compiled_Distinct.rds") %>%
  mutate(cellID = 1:n())

nc <- read_rds(paste0(base_dir, "ClimateChange_Compiled.rds"))

models <- str_extract(unique(nc$Model), "[^-]+") # Extract model names

minb <- 1
maxb <- 158 # Max weight of 100 kg

for (r in 1:length(runs)){

  zoo <- read_rds(file.path(base_dir,runs[r],"Output",paste0("res_",runs[r],".RDS")))
  mdl <- read_rds(file.path(base_dir,runs[r],"Output",paste0("model_",runs[r],".RDS")))

  # These next two lines are a hack to keep the size selection working for the moment.
  # I will come up with a better solution soon
  mdl2 <- mdl
  mdl2$param$w <- mdl$param$w[minb:maxb]

  Bio <- fZooMSS_SpeciesBiomass(fZooMSS_ExtractSizeRange(zoo, minb, maxb), mdl2)

  Bio_df <- as_tibble(matrix(unlist(Bio), nrow=length(Bio), byrow=T), .name_repair = "unique") %>%
    rename_with(~mdl$param$Groups$Species) %>%
    mutate(cellID = 1:n()) %>% # Create a cellID
    left_join(enviro_data %>% dplyr::select(cellID, chlo, sst), by = "cellID") %>%
    rename(SST = sst, Chl = chlo) %>%
    mutate(Chl_log10 = log10(Chl))

  out <- ann(as.matrix(Bio_df[,c("SST", "Chl_log10")]),
             as.matrix(nc[,c("SST", "Chl_log10")]),
             k = 1, verbose = FALSE)

  nc2 <- nc %>%
    mutate(cellID = out$knnIndexDist[,1]) %>%
    left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID") %>%
    filter(!is.na(SST))

  for (m in 1:length(models)){

    nc_mdl <- nc2 %>%
      filter(str_detect(Model, models[m]))

    write_rds(nc_mdl, paste0(file.path(base_dir,"Biomass_"), "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))
    rm(nc_mdl)

  }

  rm(nc2, Bio, Bio_df, out)
}
