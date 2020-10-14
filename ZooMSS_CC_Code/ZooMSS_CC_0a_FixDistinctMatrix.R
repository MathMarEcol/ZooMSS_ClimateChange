library(tidyverse)

old <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_old.RDS")

new <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix.RDS")
new$cellID <- 1:dim(new)[1]
new$ID_char <- sprintf("%06d",new$cellID)

# fil <- list.files("/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/NoCarnivores/RawOutput_old", full.names = TRUE)
fil <- list.files("/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/NoOmnivores/RawOutput_old", full.names = TRUE)

for (f in 1:length(fil)){
  dat <- read_rds(fil[f])

  sst <- dat$model$param$sst
  chl <- round(log10(dat$model$param$chlo),2)

  if (is.na(sst)==FALSE & is.na(chl)==FALSE){

    x = new$cellID[round(log10(new$chlo),2) == chl & new$sst == sst]

    if (length(x) != 1){
      print(paste0("Wrong x for f = ",f))
    } else{

      # new_fi <- str_replace(fil[f], "(RawOutput.*$)", paste0("RawOutput/20200917_TheMatrix_NC_",new$ID_char[x],".RDS"))
      new_fi <- str_replace(fil[f], "(RawOutput.*$)", paste0("RawOutput/20200917_TheMatrix_NO_",new$ID_char[x],".RDS"))

      saveRDS(dat, file = new_fi)
    }
    rm(x, new_fi, dat)
  }
}



## Now create the Control Matrix using the new matrix inputs

old <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200526_TheMatrix/enviro_Matrix.RDS")

new <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix.RDS")
new$cellID <- 1:dim(new)[1]
new$ID_char <- sprintf("%06d",new$cellID)

fil <- list.files("/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200526_TheMatrix/RawOutput", full.names = TRUE)

for (f in 1:length(fil)){
  dat <- read_rds(fil[f])

  sst <- dat$model$param$sst
  chl <- round(log10(dat$model$param$chlo),2)

  if (is.na(sst)==FALSE & is.na(chl)==FALSE){

    x = new$cellID[round(log10(new$chlo),2) == chl & new$sst == sst]

    if (length(x) != 1){
      # print(paste0("Wrong x for f = ",f))
    } else{
      # new_fi <- str_replace(fil[f], "(RawOutput.*$)", paste0("RawOutput/20200917_TheMatrix_NC_",new$ID_char[x],".RDS"))

       new_fi <- str_replace(fil[f], "(20200526_TheMatrix.*$)", paste0("20200917_CMIP_Matrix/Control/RawOutput/20200917_TheMatrix_Control_",new$ID_char[x],".RDS"))

       saveRDS(dat, file = new_fi)
    }
    rm(x, new_fi, dat, sst, chl)
  }
}
