library(tidyverse)
library(patchwork)
library(lubridate)
library(raster)

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")
reprocess <- TRUE

rast <- raster(nrows=180, ncols=360)
area_df <- as.data.frame(rast, xy = TRUE, na.rm = FALSE) %>%
  dplyr::select(-layer) %>%
  bind_cols(as.data.frame(area(rast))) %>%
  rename(area = layer, Lon = x, Lat = y)



if (reprocess == TRUE){
  ## Create a summary df to hold the spatial means of all the data
  runs <- c("Control", "NoCarnivores", "NoOmnivores", "NoFilterFeeders", "OneZoo")
  models <- c("UKESM1", "MPI", "IPSL", "GFDL", "CESM2")

  # runs <- c("Control", "OneZoo")
  # models <- c("UKESM1")

  zoo <- c("Flagellates","Ciliates","Larvaceans","OmniCopepods","CarnCopepods","Euphausiids","Chaetognaths","Salps","Jellyfish","Zooplankton")


  for (m in 1:length(models)){
    for (r in 1:length(runs)){

      dat <- read_rds(paste0(base_dir, "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))

      temp <- dat %>%
        left_join(area_df, by = c("Lon", "Lat")) %>%
        group_by(Date, Year, Experiment) %>%
        summarise(across(Flagellates:Fish_Large, ~mean(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        arrange(Date) %>%
        mutate(Run = runs[r],
               Model = models[m],
               Zoop_Total = reduce(select(., starts_with(zoo)), `+`), # Sum all the zooplankton
               Fish_Total = Fish_Small + Fish_Med + Fish_Large,
               TCB = Zoop_Total + Fish_Total) %>%
        mutate(ZoopBiomass_Change = (Zoop_Total - mean(Zoop_Total[Year<1960], na.rm = TRUE))/mean(Zoop_Total[Year<1960], na.rm = TRUE) * 100,
               FishBiomass_Change = (Fish_Total - mean(Fish_Total[Year<1960], na.rm = TRUE))/mean(Fish_Total[Year<1960], na.rm = TRUE) * 100,
               TCB_Change = (TCB - mean(TCB[Year<1960], na.rm = TRUE))/mean(TCB[Year<1960], na.rm = TRUE) * 100)

      if (m == 1 & r == 1){
        datyr <- temp} else{
          datyr <- bind_rows(datyr, temp)
        }
      rm(temp, dat)
    }
  }

  write_rds(datyr, file.path("Output","ClimateChange_Compiled_withZooMSS_GlobalYr.rds"))
} else{
  datyr <- read_rds(file.path("Output","ClimateChange_Compiled_withZooMSS_GlobalYr.rds"))
}


fPlot_Experiment <- function(dat, var){
  # graphics.off()
  # x11(width = 16, height = 12)

  var <- enquo(var)
  (gg <- ggplot(data = dat, aes(x = Date, y = !!var, colour = Experiment)) +
      geom_line() +
      theme_bw() +
      facet_grid(vars(Run), vars(Model), scales = "free_y"))

  ggsave(file.path("Figures",paste0(rlang::quo_text(var),"_Biomass.pdf")), width = 20, height = 12)

  return(gg)
}


gg <- fPlot_Experiment(datyr, Zoop_Total)
gg <- fPlot_Experiment(datyr, Fish_Total)
gg <- fPlot_Experiment(datyr, TCB)

gg <- fPlot_Experiment(datyr, Fish_Small)
gg <- fPlot_Experiment(datyr, Fish_Med)
gg <- fPlot_Experiment(datyr, Fish_Large)

gg <- fPlot_Experiment(datyr, OmniCopepods)
gg <- fPlot_Experiment(datyr, CarnCopepods)
gg <- fPlot_Experiment(datyr, Euphausiids)
gg <- fPlot_Experiment(datyr, Chaetognaths)
gg <- fPlot_Experiment(datyr, Larvaceans)
gg <- fPlot_Experiment(datyr, Salps)
gg <- fPlot_Experiment(datyr, Jellyfish)

gg <- fPlot_Experiment(datyr, ZoopBiomass_Change)
gg <- fPlot_Experiment(datyr, FishBiomass_Change)
gg <- fPlot_Experiment(datyr, TCB_Change)

