# Load and plot the CMIP model output to examine spatial and model variability in SST/Chl/NPP
# Jason Everett (UQ)
# 30th October 2020

library(tidyverse)
library(tidync)
library(raster)
library(sf)

base_dir <- "/Users/jason/Nextcloud/MME1Data/ZooMSS_Climate_Change/merged/"

ModelArray <- c("CESM2", "GFDL-ESM4", "IPSL-CM6A-LR",
                "MPI-ESM1-2-HR", "UKESM1-0-LL")
ExpArray <- c("historical")


# for (m in 1:length(ModelArray)){
#   for (e in 1:length(ExpArray)){

m <- 1
e <- 1

ftos <- list.files(paste0(base_dir, "tos"), pattern = paste0(ModelArray[m],"_",ExpArray[e]), full.names = TRUE)
    fchl <- list.files(paste0(base_dir, "chl"), pattern = paste0(ModelArray[m],"_",ExpArray[e]), full.names = TRUE)

    ttos <- stack(ftos)
    ttos <- mean(ttos)

    tchl <- stack(fchl)
    tchl <- mean(log10(tchl))
    tchl_sf <- st_as_sf(tchl)


    tchl <- as.data.frame(stack(fchl), xy = TRUE, na.rm = FALSE) %>%
      pivot_longer(cols = contains("X", ignore.case = FALSE), names_to = "Date", values_to = "Chl") %>%
      dplyr::select(Chl)

    temp <- bind_cols(ttos, tchl) %>%
      add_column(Model = ModelArray[m],
                 Experiment = ExpArray[e]) %>%
      mutate(SST = round(SST, digits = 1),
             Chl = case_when(Model=="IPSL-CM6A-LR" ~ Chl * 1e3, # Convert to mg m-3
                             Model!="IPSL-CM6A-LR" ~ Chl * 1e6), # Convert to mg m-3
             Chl_log10 = log10(Chl),
             Chl_log10 = round(Chl_log10, digits = 2))
    # %>%
    #   distinct(SST, Chl_log10, .keep_all = TRUE)

    if (m == 1 & e == 1){
      df <- temp
    } else {
      df <- bind_rows(df, temp)
      # %>%
      #   distinct(SST, Chl_log10, .keep_all = TRUE) # Only keep saving the
    }
    rm(temp, ttos, tchl)


    # }
# }


df <- df %>%
  rename("Lon" = x, "Lat" = y)

## Do the plotting for all the data

graphics.off()
x11(width = 6, height = 6)
ggplot(data = df, mapping = aes(x = SST, y = Chl_log10)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_bw()
ggsave("Figures/SSTChl_All.pdf")

graphics.off()
x11(width = 10, height = 6)
ggplot(data = df, mapping = aes(x = SST, y = Chl_log10)) +
  geom_hex() +
  scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_bw() +
  facet_wrap(facets = "Model", scales = "fixed")
ggsave("Figures/SSTChl_ModelFacet.pdf")


## Now check the distinct rows
ds <- df %>%
  distinct(SST, Chl_log10, .keep_all = TRUE)

graphics.off()
x11(width = 6, height = 6)
ggplot(data = ds, mapping = aes(x = SST, y = Chl_log10)) +
  geom_point() +
  # scale_fill_continuous(type = "viridis", trans = "log10") +
  theme_bw()
ggsave("Figures/SSTChl_Distinct.pdf")


# Now save environmental data space
enviro_data <- ds %>%
  mutate(Chl = 10^Chl_log10) %>%
  dplyr::select(c(SST, Chl)) %>%
  arrange(desc(Chl), desc(SST)) %>%
  filter(is.na(Chl)==FALSE) %>%
  filter(is.na(SST)==FALSE) %>%
  rename(chlo = Chl, sst = SST)

saveRDS(enviro_data, "~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix.RDS")


