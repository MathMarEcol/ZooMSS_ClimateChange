library(tidyverse)
library(yaImpute)

source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

base_dir <- file.path("~","Nextcloud","MME2Work-Q1216","ZooMSS","_LatestModel","20220925_TheMatrixESM")
runs <- c("Control")

#### Load ZooMSS Matrix Data ####
# enviro_data <- read_rds(file.path(base_dir,"ClimateChange_Compiled_Distinct_TheMatrixESM.rds"))

nc <- read.csv(file.path(base_dir, "Control","ZooMSS_test1000.csv"))

minb <- 1
maxb <- 158 # Max weight of 100 kg.


zoo <- read_rds(file.path(base_dir,runs,"Output",paste0("res_",runs,".RDS")))
mdl <- read_rds(file.path(base_dir,runs,"Output",paste0("model_",runs,".RDS")))

# These next two lines are a hack to keep the size selection working for the moment.
# I will come up with a better solution soon
mdl2 <- mdl
mdl2$param$w <- mdl$param$w[minb:maxb]

Bio <- fZooMSS_SpeciesBiomass(fZooMSS_ExtractSizeRange(zoo, minb, maxb), mdl2)

Bio_df <- as_tibble(matrix(unlist(Bio), nrow=length(Bio), byrow=T), .name_repair = "unique") %>%
  rename_with(~mdl$param$Groups$Species) #%>%


write_rds(Bio_df, paste0(file.path(base_dir,"Biomass_"), "ClimateChange_Compiled_withZooMSS_","1000Test","_",runs,".rds"))


## Now do diets


zoo <- read_rds(file.path(base_dir,runs,"Output",paste0("diets_",runs,".RDS")))

write_rds(zoo, paste0(file.path(base_dir,"Diets_"), "ClimateChange_Compiled_withZooMSS_","1000Test","_",runs,".rds"))




