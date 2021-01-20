library(tidyverse)
library(patchwork)
library(lubridate)

base_dir <- file.path("~","Nextcloud","MME2Work","ZooMSS_ClimateChange","")

runs <- c("Control", "NoCarnivores", "NoOmnivores", "NoFilterFeeders", "OneZoo")
models <- c("UKESM1", "MPI", "IPSL", "GFDL", "CESM2")

# #### Load ZooMSS Matrix Data ####
# enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")

datyr <- tibble(Date = ymd(), Experiment = character(), Model = character(), Run = character(), Fish = numeric(), BiomassChange = numeric())

for (m in 1:length(models)){
  for (r in 1:length(runs)){

    dat <- read_rds(paste0(base_dir, "ClimateChange_Compiled_withZooMSS_",models[m],"_",runs[r],".rds"))

    temp <- dat %>%
      group_by(Date, Experiment) %>%
      summarise(Fish = mean(Fish_Small + Fish_Med + Fish_Large)) %>%
      ungroup() %>%
      arrange(Date) %>%
      mutate(Run = runs[r],
             Model = models[m])

    temp$BiomassChange = (temp$Fish - mean(temp$Fish[1:10], na.rm = TRUE))/mean(temp$Fish[1:10], na.rm = TRUE) * 100

    datyr <- bind_rows(datyr, temp)

    rm(temp, dat)
  }
}

write_rds(datyr, paste0(base_dir, "ClimateChange_Compiled_withZooMSS_GlobalYr.rds"))

graphics.off()
x11(width = 16, height = 12)
(ggB <- ggplot(datyr, aes(x = Date, y = Fish, colour = Experiment)) +
  geom_line() +
  theme_bw() +
  facet_grid(vars(Run), vars(Model), scales = "free_y"))
ggsave("Figures/Fish_TotalBiomass.pdf")

graphics.off()
x11(width = 16, height = 12)
(ggBC <- ggplot(datyr, aes(x = Date, y = BiomassChange, colour = Experiment)) +
  geom_line() +
  theme_bw() +
  facet_grid(vars(Run), vars(Model), scales = "fixed"))
ggsave("Figures/Fish_BiomassChange.pdf")

