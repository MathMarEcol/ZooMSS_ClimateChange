library(tidyverse)
library(yaImpute)

source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")

enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
nc <- read_rds(paste0(base_dir, "ClimateChange_Compiled.rds"))

# Setup vector of models and runs
runs <- c("Control", "NoCarnivores", "NoOmnivores", "NoFilterFeeders", "OneZoo")

models <- str_extract(unique(nc$Model), "[^-]+") # Extract model names
exps <- unique(nc$Experiment)

minb <- 1
maxb <- 158 # Max weight of 100 kg.

for (r in 2:length(runs)){

  zoo <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/diets_",runs[r],".RDS")) # Run specific model output
  mdl <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/model_",runs[r],".RDS")) # Model details

  for (m in 1:length(models)){
    for (ex in 1:length(exps)){

      # Subset nc for model and experiment
      nc_mex <- nc %>%
        filter(str_detect(Model, models[m])) %>%
        filter(str_detect(Experiment, exps[ex]))

      # Get cellID for enviro data for the climate runs
      cellID <- ann(as.matrix(enviro_data[,c("sst", "chlo")]),
                    as.matrix(nc_mex[,c("SST", "Chl")]),
                    k = 1, verbose = FALSE)$knnIndexDist[,1]

      zoo_mex <- zoo[cellID]

      # This code takes too long to run for the climate change data.
      # For the moment I will save it only as a list.
      # profvis::profvis({out <- map_df(.x = zoo_mex, mdl = mdl,  .f = fZooMS_MakeDietTibble)})
      # out <- map_df(.x = zoo_mex, mdl = mdl,  .f = fZooMS_MakeDietTibble)

      write_rds(zoo_mex, file.path(base_dir, "Diets", runs[r], paste0("ClimateChange_withZooMSS_Diets_",exps[ex],"_", models[m],"_",runs[r],".rds")))

      # if (r == 1){
      #   write_rds(nc_mex, file.path(base_dir, "Diets", "EnviroData", paste0("EnviroData_Diets_",exps[ex],"_", models[m],".rds")))
      # }

      rm(zoo_mex, cellID, nc_mex)
    }
  }
  rm(zoo, mdl)
}
