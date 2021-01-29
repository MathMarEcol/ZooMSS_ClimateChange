# There seems to be a problem with the
source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

library(tidyverse)

## There seems to be a problem with the OneZoo data between the original matrix runs and the CMIP runs.

# First lets check OneZoo for the CMIP matrix.
m1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/OneZoo/Output/model_OneZoo.RDS")
e1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
zoo1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/OneZoo/Output/res_OneZoo.RDS") %>%
  fZooMSS_SpeciesBiomass(m1) %>%
  fZooMSS_Convert2Tibble(m1) %>%
  fZooMSS_AddEnviro(e1) %>%
  pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass") %>%
  mutate(Chl_log10 = round(Chl_log10,2),
         SST = round(SST,2)) %>%
  rename(BiomassPPMR1000 = Biomass)


# Then One Zoo for the global runs
m2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200221_OneZoo_Full_UNSW/Output/model_20200221_OneZoo_Full_UNSW.RDS")
e2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200221_OneZoo_Full_UNSW/envirofull_20200312.RDS")
zoo2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200221_OneZoo_Full_UNSW/Output/res_20200221_OneZoo_Full_UNSW.RDS") %>%
  fZooMSS_SpeciesBiomass(m2) %>%
  fZooMSS_Convert2Tibble(m2) %>%
  fZooMSS_AddEnviro(e2) %>%
  pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass") %>%
  mutate(Chl_log10 = round(Chl_log10,2),
         SST = round(SST,2)) %>%
  rename(BiomassPPMR100 = Biomass)

zoo_join <- left_join(zoo2, zoo1, by = c("SST", "Chl_log10", "Species")) %>%
  drop_na(BiomassPPMR1000)

# Plot the biomass difference
(gg1 <- ggplot(data = zoo_join, aes(x = log10(BiomassPPMR1000), y = log10(BiomassPPMR100))) +
  geom_point() +
  facet_wrap(vars(Species), scales = "free"))

(gg2 <- ggplot(data = zoo_join, aes(x = log10(BiomassPPMR1000), y = log10(BiomassPPMR100), colour = Species)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1))



(gg2 <- ggplot() +
  geom_point(data = zoo1, aes(x = Chl_log10, y = Biomass), colour = "blue") +
  facet_wrap(facets = vars(Species), scales = "free_y") +
  geom_point(data = zoo2, aes(x = Chl_log10, y = Biomass), colour = "red") +
  facet_wrap(facets = vars(Species), scales = "free_y"))


# Lets check No Control now
zoo1a <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/OneZoo/Output/res_OneZoo.RDS") %>%
  fZooMSS_SpeciesBiomass(m1) %>%
  fZooMSS_Convert2Tibble(m1) %>%
  fZooMSS_AddEnviro(e1) %>%
  group_by(cellID) %>%
  mutate(FishBiomassOne = Fish_Small + Fish_Med + Fish_Large) %>%
  ungroup() %>%
  select(-c(Flagellates:Fish_Large))


zoo3a <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/Control/Output/res_Control.RDS") %>%
  fZooMSS_SpeciesBiomass(m3) %>%
  fZooMSS_Convert2Tibble(m3) %>%
  fZooMSS_AddEnviro(e3) %>%
  group_by(cellID) %>%
  mutate(FishBiomassControl = Fish_Small + Fish_Med + Fish_Large) %>%
  ungroup() %>%
  select(-c(Flagellates:Fish_Large))

zoo_diff <- left_join(zoo1a, zoo3a, by = c("cellID"), keep = FALSE) %>%
  mutate(Fish_diff = (FishBiomassControl-FishBiomassOne)/FishBiomassControl)

ggplot(data = zoo_diff, aes(x = Chl_log10.x, y = Fish_diff)) +
  geom_point()





Hi @ric325 @ryanheneghan @patricksykes

Turns out I made a small error in one of the Single Zooplankton runs. I set one to PPMR = 100, and the other to PPMR = 1000.

To refresh your memory. We have two sets of runs for OneZoo for different papers.
1. The original global runs for 1638 cells which we are using for the High Impact Paper. This has a PPMR of 100.
2. The Climate Change matrix runs for Ryan which had 100K odd combinations of SST and Chl. This has a PPMR of 1000.

I remember talking to you all about 2) and we decided on 1000 because it "better represented the actual zooplankton PPMR". This run was completed in October. The original 1) run were done back in Feb 2020.

The problem is that I hope these papers will both come out this year, and they probably should tell the same story for the same sort of experiment. You can see below, that they each give different output.


