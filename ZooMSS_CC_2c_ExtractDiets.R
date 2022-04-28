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
exps <- unique(nc$Experiment)

minb <- 1
maxb <- 158 # Max weight of 100 kg.

for (r in 1:length(runs)){

  zoo <- read_rds(file.path(base_dir,runs[r],"Output",paste0("diets_",runs[r],".RDS")))
  mdl <- read_rds(file.path(base_dir,runs[r],"Output",paste0("model_",runs[r],".RDS")))

  for (m in 1:length(models)){
    for (ex in 1:length(exps)){

      # Subset nc for model and experiment
      nc_mex <- nc %>%
        filter(str_detect(Model, models[m])) %>%
        filter(str_detect(Experiment, exps[ex])) %>%
        select("Lon","Lat","Date","SST","chlo","Model","Experiment","Chl_log10")

      # Get cellID for enviro data for the climate runs
      cellID <- ann(as.matrix(enviro_data[,c("sst", "chlo")]),
                    as.matrix(nc_mex[,c("SST", "chlo")]),
                    k = 1, verbose = FALSE)$knnIndexDist[,1]

      zoo_mex <- zoo[cellID]

      # This code takes too long to run for the climate change data.
      # For the moment I will save it only as a list.
      # profvis::profvis({out <- map_df(.x = zoo_mex, mdl = mdl,  .f = fZooMS_MakeDietTibble)})
      # out <- map_df(.x = zoo_mex, mdl = mdl,  .f = fZooMS_MakeDietTibble)

      write_rds(zoo_mex, paste0(file.path(base_dir,"Diets_"), "ClimateChange_Compiled_withZooMSS_",models[m],"_",exps[ex],"_",runs[r],".rds"))
      write_rds(nc_mex, paste0(file.path(base_dir,"DietInfo_"), "ClimateChange_Compiled_withZooMSS_",models[m],"_",exps[ex],"_",runs[r],".rds"))
      # if (r == 1){
      #   write_rds(nc_mex, file.path(base_dir, "Diets", "EnviroData", paste0("EnviroData_Diets_",exps[ex],"_", models[m],".rds")))
      # }

      rm(zoo_mex, cellID, nc_mex)
    }
  }
  rm(zoo, mdl)
}
