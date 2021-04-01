library(tidyverse)
library(yaImpute)

source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

fZooMS_MakeDietTibble <- function(mat, mdl){
  suppressMessages(
    out <- as_tibble(mat, .name_repair = "unique") %>%
      rename_with(~c("Phyto_Small", "Phyto_Med", "Phyto_Large", mdl$param$Groups$Species)) %>%
      mutate(Predator = mdl$param$Groups$Species) %>%
      pivot_longer(cols = Phyto_Small:Fish_Large, names_to = "Prey", values_to = "Diet")
  )
  return(out)
}

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")

enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
nc <- read_rds(paste0(base_dir, "ClimateChange_Compiled.rds"))

# Setup vector of models and runs
runs <- c("Control", "NoCarnivores", "NoOmnivores", "NoFilterFeeders", "OneZoo")
models <- str_extract(unique(nc$Model), "[^-]+") # Extract model names
exps <- unique(nc$Experiment)

minb <- 1
maxb <- 158 # Max weight of 100 kg.

for (r in 1:1){#length(runs)){

  zoo <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/diets_",runs[r],".RDS")) # Run specific model output
  mdl <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/model_",runs[r],".RDS")) # Model details

  out <- map_df(.x = zoo, mdl = mdl,  .f = fZooMS_MakeDietTibble, .id="cellID")

  write_rds(out, "Test.rds")


  for (m in 1:length(models)){
    for (ex in 1:length(exps)){

      nc_mex <- nc %>%
        filter(str_detect(Model, models[m])) %>%
        filter(str_detect(Experiment, exps[ex]))

      # Get cellID for enviro data for the climate runs
      cellID <- ann(as.matrix(enviro_data[,c("sst", "chlo")]),
                    as.matrix(nc_mex[,c("SST", "Chl_log10")]),
                    k = 1, verbose = FALSE)$knnIndexDist[,1]




      zoo_mex <- zoo[cellID]




      write_rds(out, paste0(base_dir, "ClimateChange_withZooMSS_Diets_",exps[ex],"_", models[m],"_",runs[r],".rds"))

      rm(out, zoo_mex, cellID, nc_mex)
    }
  }
  rm(zoo, mdl)
}














  ## Get the row matches


  ## Create a new list


  # Convert it to a long tibble
    # These next two lines are a hack to keep the size selection working for the moment.
    # I will come up with a better solution soon
    mdl2 <- mdl
    mdl2$param$w <- mdl$param$w[minb:maxb]

    Bio <- fZooMSS_SpeciesBiomass(fZooMSS_ExtractSizeRange(zoo, minb, maxb), mdl2)

    Bio_df <- as_tibble(matrix(unlist(Bio), nrow=length(Bio), byrow=T), .name_repair = "unique") %>%
      rename_with(~mdl$param$Groups$Species) %>%
      mutate(cellID = 1:n()) %>% # Create a cellID
      left_join(dplyr::select(enviro_data, cellID, chlo, sst), by = "cellID") %>%
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

      # temp folder for the moment while RDM is down
      out_dir <- file.path("~","Dropbox","TempZooMSSData","")
      write_rds(nc_mdl, paste0(out_dir, "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))

      # write_rds(nc_mdl, paste0(base_dir, "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))
      rm(nc_mdl)

    }

    rm(nc2, Bio, Bio_df, out)



}
