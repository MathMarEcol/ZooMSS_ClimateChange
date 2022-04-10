library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

base_dir <- file.path("~","Nextcloud","MME1Data","ZooMSS_Climate_Change","merged","")
out_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")

ModelArray <- c("CESM2", "GFDL-ESM4", "IPSL-CM6A-LR",
                "MPI-ESM1-2-HR", "UKESM1-0-LL")
ExpArray <- c("historical", "ssp126", "ssp370", "ssp585")

for (m in 1:length(ModelArray)){
  for (e in 1:length(ExpArray)){

    ftos <- list.files(paste0(base_dir, "tos"), pattern = paste0(ModelArray[m],"_",ExpArray[e]), full.names = TRUE)
    fchl <- list.files(paste0(base_dir, "chl"), pattern = paste0(ModelArray[m],"_",ExpArray[e]), full.names = TRUE)

    ttos <- as.data.frame(stack(ftos), xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(cols = contains("X", ignore.case = FALSE), names_to = "Date", values_to = "SST")

    tchl <- as.data.frame(stack(fchl), xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(cols = contains("X", ignore.case = FALSE), names_to = "Date", values_to = "Chl") %>%
      dplyr::select(Chl)

    temp <- bind_cols(ttos, tchl) %>%
      add_column(Model = ModelArray[m],
                 Experiment = ExpArray[e])

    if (m == 1 & e == 1){
      df <- temp
    } else {
      df <- bind_rows(df, temp)
    }
    rm(temp, ttos, tchl, ftos, fchl)
  }
}


df <- df %>%
  mutate(Date = str_replace(Date, "X", ""),
         Date = ymd(Date),
         Year = year(Date)) %>%
  rename(Lon = x, Lat = y) %>%
  mutate(SST = round(SST, digits = 1),
         Chl = case_when(Model == "IPSL-CM6A-LR" ~ Chl * 1e3, # Convert to mg m-3
                         Model != "IPSL-CM6A-LR" ~ Chl * 1e6), # Convert to mg m-3
         Chl_log10 = log10(Chl),
         Chl_log10 = round(Chl_log10, digits = 2))


write_rds(df, paste0(out_dir,.Platform$file.sep,"ClimateChange_Compiled.rds")) # Save to RDM

