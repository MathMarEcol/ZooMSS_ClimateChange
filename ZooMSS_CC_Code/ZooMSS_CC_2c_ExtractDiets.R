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

for (r in 1:1){#length(runs)){

  zoo <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/diets_",runs[r],".RDS"))
  mdl <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/model_",runs[r],".RDS"))



}
