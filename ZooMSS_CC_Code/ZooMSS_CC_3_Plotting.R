library(tidyverse)
library(patchwork)
library(lubridate)

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")
reprocess <- FALSE

if (reprocess == TRUE){
  ## Create a summary df to hold the spatial means of all the data
  runs <- c("Control", "NoCarnivores", "NoOmnivores", "NoFilterFeeders", "OneZoo")
  models <- c("UKESM1", "MPI", "IPSL", "GFDL", "CESM2")

  for (m in 1:length(models)){
    for (r in 1:length(runs)){

      dat <- read_rds(paste0(base_dir, "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))

      temp <- dat %>%
        group_by(Date, Year, Experiment) %>%
        summarise(across(Flagellates:Fish_Large, ~mean(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        arrange(Date) %>%
        mutate(Run = runs[r],
               Model = models[m],
               Fish_Total = Fish_Small + Fish_Med + Fish_Large) %>%
        mutate(FishBiomassChange = (Fish_Total - mean(Fish_Total[Year<1960], na.rm = TRUE))/mean(Fish_Total[Year<1960], na.rm = TRUE) * 100)

      if (m == 1 & r == 1){
        datyr <- temp} else{
          datyr <- bind_rows(datyr, temp)
        }
      rm(temp, dat)
    }
  }

  write_rds(datyr, "ClimateChange_Compiled_withZooMSS_GlobalYr.rds")
} else{
  datyr <- read_rds("ClimateChange_Compiled_withZooMSS_GlobalYr.rds")
}


graphics.off()
x11(width = 16, height = 12)
(ggB <- ggplot(data = datyr, aes(x = Date, y = Fish_Total, colour = Experiment)) +
    geom_line() +
    theme_bw() +
    facet_grid(vars(Run), vars(Model), scales = "free_y"))
ggsave("Figures/Fish_TotalBiomass.pdf")

graphics.off()
x11(width = 16, height = 12)
(ggFS <- ggplot(datyr, aes(x = Date, y = Fish_Small, colour = Experiment)) +
    geom_line() +
    theme_bw() +
    facet_grid(vars(Run), vars(Model), scales = "free_y"))
ggsave("Figures/Fish_SmallBiomass.pdf")

graphics.off()
x11(width = 16, height = 12)
(ggFL <- ggplot(datyr, aes(x = Date, y = Fish_Med, colour = Experiment)) +
    geom_line() +
    theme_bw() +
    facet_grid(vars(Run), vars(Model), scales = "free_y"))
ggsave("Figures/Fish_MedBiomass.pdf")

graphics.off()
x11(width = 16, height = 12)
(ggFL <- ggplot(datyr, aes(x = Date, y = Fish_Large, colour = Experiment)) +
    geom_line() +
    theme_bw() +
    facet_grid(vars(Run), vars(Model), scales = "free_y"))
ggsave("Figures/Fish_LargeBiomass.pdf")


graphics.off()
x11(width = 16, height = 12)
(ggOC <- ggplot(datyr, aes(x = Date, y = OmniCopepods, colour = Experiment)) +
    geom_line() +
    theme_bw() +
    facet_grid(vars(Run), vars(Model), scales = "free_y"))
ggsave("Figures/OmniCopepods_TotalBiomass.pdf")

graphics.off()
x11(width = 16, height = 12)
(ggCC <- ggplot(datyr, aes(x = Date, y = CarnCopepods, colour = Experiment)) +
    geom_line() +
    theme_bw() +
    facet_grid(vars(Run), vars(Model), scales = "free_y"))
ggsave("Figures/CarnCopepods_TotalBiomass.pdf")


graphics.off()
x11(width = 16, height = 12)
(ggBC <- ggplot(datyr, aes(x = Date, y = FishBiomassChange, colour = Experiment)) +
    geom_line() +
    theme_bw() +
    facet_grid(vars(Run), vars(Model), scales = "fixed"))
ggsave("Figures/Fish_BiomassChange.pdf")

