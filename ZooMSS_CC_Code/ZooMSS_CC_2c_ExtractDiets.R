library(tidyverse)
library(yaImpute)

source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

fZooMS_MakeDietTibble <- function(mat, mdl){
  suppressMessages(
    out <- as_tibble(mat, .name_repair = "unique") %>%
      rename_with(~c("Phyto_Small", "Phyto_Med", "Phyto_Large", mdl$param$Groups$Species)) %>%
      mutate(Predator = mdl$param$Groups$Species,
             cellID = row_number()) %>%
      pivot_longer(cols = Phyto_Small:Fish_Large, names_to = "Prey", values_to = "Diet")
  )
  return(out)
}

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")

runs <- c("Control", "NoCarnivores", "NoOmnivores", "NoFilterFeeders", "OneZoo")

#### Load ZooMSS Matrix Data ####
enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
nc <- read_rds(paste0(base_dir, "ClimateChange_Compiled.rds"))

models <- str_extract(unique(nc$Model), "[^-]+") # Extract model names

minb <- 1
maxb <- 158 # Max weight of 100 kg.

for (r in 1:1){#length(runs)){

  zoo <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/diets_",runs[r],".RDS"))
  mdl <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/model_",runs[r],".RDS"))

  cellID <- ann(as.matrix(enviro_data[,c("sst", "chlo")]),
             as.matrix(nc[,c("SST", "Chl_log10")]),
             k = 1, verbose = FALSE)$knnIndexDist[,1]

  zoo2 <- zoo[cellID]

  system.time(out2 <- map_df(.x = zoo2, mdl = mdl,  .f = fZooMS_MakeDietTibble))




  for (m in 1:length(models)){

    nc_mdl <- nc2 %>%
      filter(str_detect(Model, models[m]))

    # temp folder for the moment while RDM is down
    out_dir <- file.path("~","Dropbox","TempZooMSSData","")
    write_rds(nc_mdl, paste0(out_dir, "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))

    # write_rds(nc_mdl, paste0(base_dir, "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))
    rm(nc_mdl)

  }

  rm(nc2, Bio, Bio_df, out)
}













  ## Get the row matches


  ## Create a new list


  # Convert it to a long tibble


}
