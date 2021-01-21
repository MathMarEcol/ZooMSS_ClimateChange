library(tidyverse)
library(yaImpute)

source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")

runs <- c("Control", "NoCarnivores", "NoOmnivores", "NoFilterFeeders", "OneZoo")

#### Load ZooMSS Matrix Data ####
enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
nc <- read_rds(paste0(base_dir, "ClimateChange_Compiled.rds"))

models <- str_extract(unique(nc$Model), "[^-]+") # Extract model names

minb <- 1
maxb <- 158 # Max weight of 100 kg.

for (r in 1:length(runs)){

  zoo <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/res_",runs[r],".RDS"))
  mdl <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/model_",runs[r],".RDS"))

  Bio <- fZooMSS_SpeciesBiomass(fZooMSS_ExtractSizeRange(zoo, minb, maxb), mdl$param$w[minb:maxb])

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
    left_join(select(Bio_df, cellID, Flagellates:Fish_Large), by = "cellID")

  for (m in 1:length(models)){

    nc_mdl <- nc2 %>%
      filter(str_detect(Model, models[m]))

    write_rds(nc_mdl, paste0(base_dir, "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))
    rm(nc_mdl)

  }

  rm(nc2, Bio, Bio_df, out)
}
