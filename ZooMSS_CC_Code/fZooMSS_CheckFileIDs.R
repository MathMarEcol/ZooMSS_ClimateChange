

library(tidyverse)
# Check the file name vs cellID

enviro <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
fil <- list.files("/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/Control/RawOutput/", full.names = TRUE)

fil <- list.files("/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/NoOmnivores/RawOutput/", full.names = TRUE)

fil <- list.files("/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/NoCarnivores/RawOutput/", full.names = TRUE)

enviro$chk_fileID <- NA
enviro$chk_cellID <- NA
enviro$chk_chlo <- NA
enviro$chk_sst <- NA

for (i in 1:length(fil)){
  out <- read_rds(fil[i])
  enviro$chk_fileID[i] = as.numeric(str_sub(fil[i], -10,-5))
  enviro$chk_cellID[i] = out$model$param$cellID
  enviro$chk_chlo[i] = out$model$param$chlo
  enviro$chk_sst[i] = out$model$param$sst
  rm(out)
}

plot(enviro$chk_fileID, enviro$chk_cellID)



x <- enviro$cellID[enviro$cellID != enviro$chk_cellID]
y <- enviro$cellID[enviro$chk_fileID != enviro$chk_cellID]

enviro <- enviro[x,]

# saveRDS(enviro, "~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto_MatrixErrors3.RDS")