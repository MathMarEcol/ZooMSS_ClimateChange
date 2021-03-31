# Load and plot the CMIP model output to examine spatial and model variability in SST/Chl/NPP
# Jason Everett (UQ)
# 30th October 2020

library(tidync)
library(raster)
library(sf)
library(rnaturalearth)
library(patchwork)
library(tidyverse)

base_dir <- "/Users/jason/Nextcloud/MME1Data/ZooMSS_Climate_Change/merged/"

ModelArray <- c("CESM2", "GFDL-ESM4", "IPSL-CM6A-LR",
                "MPI-ESM1-2-HR", "UKESM1-0-LL", "CanESM5-CanOE")

ModelArray2 <- c("CESM2", "GFDL-ESM4", "IPSL-CM6A-LR",
                 "MPI-ESM1-2-LR", "UKESM1-0-LL", "CanESM5-CanOE")

ExpArray <- c("historical")
chl_conv <- c(1e6, 1e6, 1e3, 1e6, 1e6)

latlonCRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
robCRS <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world <- ne_countries(scale = "medium", returnclass = "sf")
world_sf <- st_transform(world, crs = st_crs(robCRS)) # Convert to Mollweide

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_line(colour = "grey50", size = 0.2),
                         panel.background = element_rect(fill = "transparent", colour = NA),
                         panel.border = element_blank(),
                         plot.background = element_rect(fill = "transparent", colour = NA),
                         plot.title = element_text(hjust = 0.5),
                         plot.margin = unit(c(0,0,0,0), "mm"),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         # axis.title.y = element_blank(),
                         # legend.title = element_text(size = 6),
                         # legend.text = element_text(size = 6),
                         legend.position = "right",
                         # legend.direction = "horizontal",
                         legend.background = element_rect(fill = "transparent", colour = NA),
                         legend.key.height = unit(9, "mm"),
                         legend.key.width = unit(4, "mm")
                         # legend.position = c(0.5, -0.05),
))

cnt = 0
myplots = list()
for (m in 1:length(ModelArray)){
  for (e in 1:length(ExpArray)){

    cnt = cnt + 1
    ftos <- list.files(paste0(base_dir, "tos"), pattern = paste0(ModelArray[m],"_",ExpArray[e]), full.names = TRUE)
    ttos <- stack(ftos)
    ttos <- mean(ttos[[(length(names(ttos))-9):length(names(ttos))]])
    ttos_sf <- st_as_sf(rasterToPolygons(ttos, na.rm = FALSE))
    ttos_sf <- st_transform(ttos_sf, crs = st_crs(robCRS)) # Convert to Robinson Projection
    if (m == 1){
      ttos_sf_all <- ttos_sf %>%
        rename(!!ModelArray[m] := layer)

    }else{
      ttos_sf_all <- ttos_sf_all %>%
        mutate(!!str_replace(ModelArray[m],'-','_') := ttos_sf$layer)
    }


    myplots[[cnt]] <- ggplot() +
      geom_sf(data = ttos_sf, aes(fill = layer), colour = NA) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      scale_fill_gradientn(limits = c(-2, 32),
                           colours = rev(rainbow(5)),
                           na.value = "grey50",
                           oob = scales::squish,
                           guide = guide_colourbar(title = "SST (°C)",
                                                   title.position = "right",
                                                   title.hjust = 0.5,
                                                   title.theme = element_text(angle = 270, size = 10))) +
      theme_opts +
      scale_alpha(range = c(-0, 0.5)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(y = ModelArray[m]) +
      (if(m==1){ggtitle(expression("Sea Surface Temperature (2005-2014)"))}) +
      annotate("text", x = -1e6, y = -6e6, label = paste0("Mean = ", round(mean(ttos_sf$layer, na.rm = TRUE), 2)), colour = "black")


    cnt = cnt + 1
    fchl <- list.files(paste0(base_dir, "chl"), pattern = paste0(ModelArray[m],"_",ExpArray[e]), full.names = TRUE)
    tchl <- stack(fchl)
    tchl <- tchl[[(length(names(tchl))-9):length(names(tchl))]]
    tchl <- mean(log10(tchl*chl_conv[m]))
    tchl_sf <- st_as_sf(rasterToPolygons(tchl, na.rm = FALSE))
    tchl_sf <- st_transform(tchl_sf, crs = st_crs(robCRS)) # Convert to Robinson Projection

    print(10^max(tchl_sf$layer, na.rm = TRUE))
    if (m == 1){
      tchl_sf_all <- tchl_sf %>%
        rename(!!ModelArray[m] := layer)

    }else{
      tchl_sf_all <- tchl_sf_all %>%
        mutate(!!str_replace(ModelArray[m],'-','_') := tchl_sf$layer)
    }

    myplots[[cnt]] <- ggplot() +
      geom_sf(data = tchl_sf, aes(fill = layer), colour = NA) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      scale_fill_gradientn(limits = c(-1.5, 0.5),
                           # low = "blue",
                           # high = "red",
                           colours = rev(rainbow(5)),
                           na.value = "grey50",
                           # aesthetics = "fill",
                           oob = scales::squish,
                           guide = guide_colourbar(title = expression(paste("log"[10], "Chl. ",italic(a)," (mg m"^-3, ")")),
                                                   title.position = "right",
                                                   title.hjust = 0.5,
                                                   title.theme = element_text(angle = 270, size = 10))) +
      theme_opts +
      theme(axis.title.y = element_blank()) +
      scale_alpha(range = c(-0, 0.5)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      (if(m==1){ggtitle(expression(paste("Chlorophyll ",italic(a)," (2005-2014)")))}) +
      annotate("text", x = -1e6, y = -6e6, label = paste0("Mean = ", round(mean(10^tchl_sf$layer, na.rm = TRUE),2)), colour = "black")

    cnt = cnt + 1
    fnpp <- list.files(paste0(base_dir, "npp"), pattern = paste0(ModelArray2[m],"*"), full.names = TRUE)
    tnpp <- stack(fnpp)
    tnpp <- tnpp[[(length(names(tnpp))-9):length(names(tnpp))]]
    tnpp <- mean(log10(tnpp * 86400))
    tnpp_sf <- st_as_sf(rasterToPolygons(tnpp, na.rm = FALSE))
    tnpp_sf <- st_transform(tnpp_sf, crs = st_crs(robCRS)) # Convert to Robinson Projection

    if (m == 1){
      tnpp_sf_all <- tnpp_sf %>%
        rename(!!ModelArray[m] := layer)
    }else{
      tnpp_sf_all <- tnpp_sf_all %>%
        mutate(!!str_replace(ModelArray[m],'-','_') := tnpp_sf$layer)
    }

    myplots[[cnt]] <- ggplot() +
      geom_sf(data = tnpp_sf, aes(fill = layer), colour = NA) +
      geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
      scale_fill_gradientn(limits = c(-4.5, -2.5),
                           # low = "blue",
                           # high = "red",
                           colours = rev(rainbow(5)),
                           na.value = "grey50",
                           # aesthetics = "fill",
                           oob = scales::squish,
                           guide = guide_colourbar(title = expression(paste("log"[10], "NPP (mol m"^-3," d"^-1, ")")),
                                                   title.position = "right",
                                                   title.hjust = 0.5,
                                                   title.theme = element_text(angle = 270, size = 10))) +
      theme_opts +
      theme(axis.title.y = element_blank()) +
      scale_alpha(range = c(-0, 0.5)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      (if(m==1){ggtitle("Net Primary Production (2005-2014)")}) +
      annotate("text", x = -2e6, y = -6e6, label = paste0("Mean = ", format(round(mean(10^tnpp_sf$layer, na.rm = TRUE),4)), scientific = FALSE), colour = "black")

    cnt = cnt + 1
    scat <- tibble(Chl = tchl_sf$layer, NPP = tnpp_sf$layer)

    myplots[[cnt]] <- ggplot(data = scat, aes(x = Chl, y = NPP)) +
      geom_point() +
      scale_x_continuous(expand = c(0, 0), limits = c(-3, 1.5)) +
      scale_y_continuous(expand = c(0, 0), limits = c(-6, -1)) +
      theme(aspect.ratio=1) +
      labs(x = expression(paste("log"[10], "Chl. ",italic(a)," (mg m"^-3, ")")),
           y = expression(paste("log"[10], "NPP (mol m"^-3," d"^-1, ")"))) +
      (if(m==1){ggtitle("Chl v NPP (2005-2014)")})
  }
}


source("ZooMSS_CC_1d_AddSatellite.R")


myplots[[cnt + 1]] <- ggplot() +
  geom_sf(data = msst_sf, aes(fill = layer), colour = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  scale_fill_gradientn(limits = c(-2, 32),
                       colours = rev(rainbow(5)),
                       na.value = "grey50",
                       oob = scales::squish,
                       guide = guide_colourbar(title = "SST (°C)",
                                               title.position = "right",
                                               title.hjust = 0.5,
                                               title.theme = element_text(angle = 270, size = 10))) +
  theme_opts +
  scale_alpha(range = c(-0, 0.5)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = "Satellite") +
  annotate("text", x = -1e6, y = -6e6, label = paste0("Mean = ", round(mean(msst_sf$layer, na.rm = TRUE), 2)), colour = "black")




myplots[[cnt + 2]] <- ggplot() +
  geom_sf(data = mchl_sf, aes(fill = log10(layer)), colour = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  scale_fill_gradientn(limits = c(-1.5, 0.5),
                       # low = "blue",
                       # high = "red",
                       colours = rev(rainbow(5)),
                       na.value = "grey50",
                       # aesthetics = "fill",
                       oob = scales::squish,
                       guide = guide_colourbar(title = expression(paste("log"[10], "Chl. ",italic(a)," (mg m"^-3, ")")),
                                               title.position = "right",
                                               title.hjust = 0.5,
                                               title.theme = element_text(angle = 270, size = 10))) +
  theme_opts +
  theme(axis.title.y = element_blank()) +
  scale_alpha(range = c(-0, 0.5)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  annotate("text", x = -2e6, y = -6e6, label = paste0("Mean = ", round(mean(mchl_sf$layer, na.rm = TRUE),2)), colour = "black")



graphics.off()
x11(height = 14, width = 15)
wrap_plots(myplots, guides = "collect") + plot_layout(ncol = 4, widths = c(1, 1, 1, 1)) + plot_annotation(tag_levels = "A", tag_suffix = ")")
ggsave("Figures/ESM_Output.png", dpi = 300)
ggsave("Figures/ESM_Output.pdf")








library(tidyverse)
library(patchwork)
library(lubridate)

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")
reprocess <- TRUE

if (reprocess == TRUE){

    ## Create a summary df to hold the spatial means of all the data

  ModelArray <- c("CESM2", "GFDL", "IPSL",
                  "MPI", "UKESM1")

  ExpArray <- c("historical", "ssp126", "ssp370", "ssp585")

  for (m in 1:length(ModelArray)){

      dat <- read_rds(paste0(base_dir, "ClimateChange_Compiled_withZooMSS_",ModelArray[m],"_Control.rds"))

      temp <- dat %>%
        group_by(Date, Year, Experiment) %>%
        summarise(Chl = mean(Chl, na.rm = TRUE),
                  SST = mean(SST, na.rm = TRUE),) %>%
        ungroup() %>%
        arrange(Date) %>%
        mutate(Model = ModelArray[m])

      if (m == 1){
        datyr <- temp} else{
          datyr <- bind_rows(datyr, temp)
        }
      rm(temp, dat)
    }

#   write_rds(datyr, file.path("Output","ClimateChange_Compiled_withZooMSS_GlobalYr.rds"))
# } else{
#   datyr <- read_rds(file.path("Output","ClimateChange_Compiled_withZooMSS_GlobalYr.rds"))
}

datyr <- datyr %>%
  mutate(Chl_log10 = log10(Chl))


gg <- ggplot(data = datyr, aes(x = Date, y = Chl_log10, colour = Experiment)) +
  geom_line() +
  theme_bw() +
  facet_wrap(vars(Model), scales = "free_y", ncol = 1)
ggsave("Figures/Chl_TimeSeries.pdf")

