# There seems to be a problem with the
source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")

library(tidyverse)


## There seems to be a problem with the OneZoo data between the original matrix runs and the CMIP runs.

# First lets check OneZoo. Then I'll check some of the other groups.

m1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/OneZoo/Output/model_OneZoo.RDS")
e1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
zoo1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/OneZoo/Output/res_OneZoo.RDS") %>%
  fZooMSS_SpeciesBiomass(m1) %>%
  fZooMSS_Convert2Tibble(m1) %>%
  fZooMSS_AddEnviro(e1) %>%
  pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass")

m2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200221_OneZoo_Full_UNSW/Output/ModelParameters.RDS")
e2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200221_OneZoo_Full_UNSW/envirofull_20200312.RDS")
zoo2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200221_OneZoo_Full_UNSW/Output/res_20200221_OneZoo_Full_UNSW.RDS") %>%
  fZooMSS_SpeciesBiomass(m1) %>%
  fZooMSS_Convert2Tibble(m1) %>%
  fZooMSS_AddEnviro(e2) %>%
  pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass")

(gg <- ggplot() +
  geom_point(data = zoo1, aes(x = Chl_log10, y = Biomass), colour = "blue") +
  facet_wrap(facets = vars(Species), scales = "free_y") +
  geom_point(data = zoo2, aes(x = Chl_log10, y = Biomass), colour = "red") +
  facet_wrap(facets = vars(Species), scales = "free_y"))


# Lets check No Control now
m1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/Control/Output/model_Control.RDS")
e1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
zoo1 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/Control/Output/res_Control.RDS") %>%
  fZooMSS_SpeciesBiomass(m1) %>%
  fZooMSS_Convert2Tibble(m1) %>%
  fZooMSS_AddEnviro(e1) %>%
  pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass")

m2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200212_Control_Full_UNSW/Output/ModelParameters.RDS")
e2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200212_Control_Full_UNSW/envirofull_20200312.RDS")
zoo2 <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200212_Control_Full_UNSW/Output/res_20200212_Control_Full_UNSW.RDS") %>%
  fZooMSS_SpeciesBiomass(m1) %>%
  fZooMSS_Convert2Tibble(m1) %>%
  fZooMSS_AddEnviro(e2) %>%
  pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass")

(gg <- ggplot() +
    geom_point(data = zoo1, aes(x = Chl_log10, y = Biomass), colour = "blue") +
    facet_wrap(facets = vars(Species), scales = "free_y") +
    geom_point(data = zoo2, aes(x = Chl_log10, y = Biomass), colour = "red") +
    facet_wrap(facets = vars(Species), scales = "free_y"))


zoo1$Matrix <- "CMIP"
zoo2$Matrix <- "FoodWeb"
zoo_join <- bind_rows(zoo1, zoo2)

gg <- ggplot() +
    geom_point(data = zoo1, aes(x = Chl_log10, y = SST, colour = Biomass)) +
    facet_grid(vars(Species), vars(Matrix))

gg

## They all look different. It could be an SST issue....

(gg <- ggplot() +
    geom_point(data = zoo1, aes(x = SST, y = Biomass), colour = "blue") +
    facet_wrap(facets = vars(Species), scales = "free_y") +
    geom_point(data = zoo2, aes(x = SST, y = Biomass), colour = "red") +
    facet_wrap(facets = vars(Species), scales = "free_y"))




f1 <- zoo1 %>%
  filter(SST >= 14.95 & SST <= 15)

f2 <- zoo2 %>%
  filter(SST >= 14.9 & SST <= 15)




