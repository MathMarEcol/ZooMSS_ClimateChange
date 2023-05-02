library(tidyverse)
library(yaImpute)

source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

base_dir <- file.path("~","Nextcloud","MME2Work-Q1216","ZooMSS","_LatestModel","20220925_TheMatrixESM")
runs <- c("Control", "FixedCarbon")

#### Load ZooMSS Matrix Data ####
# enviro_data <- read_rds(file.path(base_dir,"ClimateChange_Compiled_Distinct_TheMatrixESM.rds"))

nc <- read_rds(file.path(base_dir, "ClimateChange_Compiled_New.rds"))

models <- str_extract(unique(nc$Model), "[^-]+") # Extract model names
exps <- unique(nc$Experiment)

minb <- 1
maxb <- 158 # Max weight of 100 kg.


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
    mutate(FID = enviro_data$FID) %>% # Create a cellID
    left_join(enviro_data %>% dplyr::select(FID, sst, phyto_slope, phyto_int, phyto_max), by = "FID") %>%
    rename(cellID = FID) # %>%
    # rename(SST = sst, Chl = chlo) %>%
    # mutate(Chl_log10 = log10(Chl))

  # out <- ann(as.matrix(Bio_df[,c("SST", "Chl_log10")]),
  #            as.matrix(nc[,c("SST", "Chl_log10")]),
  #            k = 1, verbose = FALSE)

  nc2 <- nc %>%
    rename(cellID = idx) %>%
    left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID") %>%
    filter(!is.na(SST))

  for (m in 1:length(models)){

    nc_mdl <- nc2 %>%
      filter(str_detect(Model, models[m]))

    write_rds(nc_mdl, paste0(file.path(base_dir,"Biomass_"), "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))
    rm(nc_mdl)

  }

  rm(nc2, Bio, Bio_df)
}


## Now do diets


for (r in 1:length(runs)){

  zoo <- read_rds(file.path(base_dir,runs[r],"Output",paste0("diets_",runs[r],".RDS")))
  mdl <- read_rds(file.path(base_dir,runs[r],"Output",paste0("model_",runs[r],".RDS")))

  for (m in 1:length(models)){
    for (ex in 1:length(exps)){

      # Subset nc for model and experiment
      nc_mex <- nc %>%
        filter(str_detect(Model, models[m])) %>%
        filter(str_detect(Experiment, exps[ex])) %>%
        select("Lon","Lat","Date","SST","chlo","Model","Experiment","Chl_log10", "idx")

      # Get cellID for enviro data for the climate runs
      # cellID <- ann(as.matrix(enviro_data[,c("sst", "chlo")]),
      #               as.matrix(nc_mex[,c("SST", "chlo")]),
      #               k = 1, verbose = FALSE)$knnIndexDist[,1]

      zoo_mex <- zoo[nc_mex$idx]

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

