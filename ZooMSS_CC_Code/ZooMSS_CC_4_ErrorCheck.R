# There seems to be a problem with the
source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")
source("~/GitHub/ZooMSS/fZooMSS_CalculatePhytoParam.R")

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
  rename(BiomassCMIP = Biomass)


# Then One Zoo for the global runs
m2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20210207_OneZoo/Output/model_20210207_OneZoo.RDS")
e2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20201009_OneZoo/envirofull_20200317.RDS")
zoo2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20210207_OneZoo/Output/res_20210207_OneZoo.RDS") %>%
  fZooMSS_SpeciesBiomass(m2) %>%
  fZooMSS_Convert2Tibble(m2) %>%
  fZooMSS_AddEnviro(e2) %>%
  pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass") %>%
  mutate(Chl_log10 = round(Chl_log10,2),
         SST = round(SST,2)) %>%
  rename(BiomassGlobal = Biomass)

zoo1 <- zoo1 %>%
  mutate(Chl = round(Chl, 3))

zoo2 <- zoo2 %>%
  mutate(Chl = round(Chl, 3))

zoo_join <- left_join(zoo2, zoo1, by = c("SST", "Chl", "Species")) %>%
  drop_na(BiomassCMIP) %>%
  select(sort(tidyselect::peek_vars()))


# Plot the biomass difference
(gg1 <- ggplot(data = zoo_join, aes(x = log10(BiomassCMIP), y = log10(BiomassGlobal))) +
  geom_point() +
  facet_wrap(vars(Species), scales = "free"))

(gg2 <- ggplot(data = zoo_join, aes(x = log10(BiomassCMIP), y = log10(BiomassGlobal), colour = Species)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1))



(gg3 <- ggplot() +
  geom_point(data = zoo1, aes(x = Chl_log10, y = BiomassCMIP), colour = "blue") +
  facet_wrap(facets = vars(Species), scales = "free_y") +
  geom_point(data = zoo2, aes(x = Chl_log10, y = BiomassGlobal), colour = "red") +
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


