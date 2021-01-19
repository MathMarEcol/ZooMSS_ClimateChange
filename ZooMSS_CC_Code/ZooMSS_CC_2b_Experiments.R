library(tidyverse)
library(yaImpute)

source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")

runs <- c("Control", "NoCarnivores", "NoOmnivores", "NoFilterFeeders", "OneZoo")

#### Load ZooMSS Matrix Data ####
enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
mdl <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/Control/Output/model_Control.RDS")
nc <- read_rds(paste0(base_dir, "ClimateChange_Compiled.rds"))

w <- mdl$param$w

minb <- 1
maxb <- 158 # Max weight of 100 kg.

for (r in 1:length(runs)){

  out <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/res_",runs[r],".RDS"))

  Bio <- fZooMSS_SpeciesBiomass(fZooMSS_ExtractSizeRange(out, minb, maxb), w) # Sum across all species, keeping size Bins

  Bio_df <- as_tibble(matrix(unlist(Bio), nrow=length(Bio), byrow=T)) %>%
    rename_with(~mdl$param$Groups$Species) %>%
    mutate(cellID = 1:n()) %>% # Create a cellID
    left_join(dplyr::select(enviro_data, cellID, chlo, sst), by = "cellID") %>%
    rename(SST = sst, Chl = chlo) %>%
    mutate(Chl_log10 = log10(Chl))

  out <- ann(as.matrix(Bio_df[,c("SST", "Chl_log10")]),
             as.matrix(nc[,c("SST", "Chl_log10")]),
             k = 1, verbose = FALSE)

  nc2 <- nc %>%
    mutate(cellID = out$knnIndexDist[,1],
           Flagellates = Bio_df$Flagellates[cellID],
           Ciliates = Bio_df$Ciliates[cellID],
           Larvaceans = Bio_df$Larvaceans[cellID],
           OmniCopepods = Bio_df$OmniCopepods[cellID],
           CarnCopepods = Bio_df$CarnCopepods[cellID],
           Euphausiids = Bio_df$Euphausiids[cellID],
           Chaetognaths = Bio_df$Chaetognaths[cellID],
           Salps = Bio_df$Salps[cellID],
           Jellyfish = Bio_df$Jellyfish[cellID],
           Fish_Small = Bio_df$Fish_Small[cellID],
           Fish_Med = Bio_df$Fish_Med[cellID],
           Fish_Large= Bio_df$Fish_Large[cellID])

  write_rds(nc2, paste0(base_dir, "ClimateChange_Compiled_withZooMSS_",runs[r],".rds"))

  rm(nc2, Bio, Bio_df, out)
}
