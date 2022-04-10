
# Plot Matrix Biomass
source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")
library(tidyverse)
library(patchwork)

enviro <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
mdl <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/Control/Output/model_Control.RDS")
Zoo <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/Control/Output/res_Control.RDS") %>%
  fZooMSS_SpeciesBiomass(mdl) %>%
  fZooMSS_Convert2Tibble(mdl) %>%
  fZooMSS_AddEnviro(enviro) %>%
  pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass") %>%
  mutate(Chl_log10 = round(Chl_log10,2),
         SST = round(SST,2))


plot_list <- list()
sp <- mdl$param$Groups$Species

for (i in 1:length(sp)){
  plot_list[[i]] <- Zoo %>%
    filter(Species == sp[i]) %>%
    ggplot(aes(x = SST, y = Chl_log10, colour = Biomass, fill = Biomass)) +
    geom_tile() +
    scale_fill_distiller(palette = "Spectral") +
    scale_colour_distiller(palette = "Spectral") +
    # scale_color_gradient2(low = "blue", high = "red", limits = c(-100, 100), oob = scales::squish) +
    guides(colour = FALSE)
}


x11(width = 24, height = 16)
wrap_plots(plot_list, ncol = 3)
ggsave("Figures/MatrixSpeciesBiomass.png", dpi = 300)






# Now plot change relative to Control
runs <- c("Control", "NoCarnivores", "NoOmnivores", "NoFilterFeeders", "OneZoo")
r <- 1

enviro <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/enviro_CMIP_Matrix_wPhyto.RDS")
mdl <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/model_",runs[r],".RDS"))

Zoo <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/res_",runs[r],".RDS")) %>%
  fZooMSS_SpeciesBiomass(mdl) %>%
  fZooMSS_Convert2Tibble(mdl) %>%
  fZooMSS_AddEnviro(enviro) %>%
  pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass") %>%
  mutate(Chl_log10 = round(Chl_log10,2),
         SST = round(SST,2)) %>%
  rename(Control = Biomass)

ControlOutput <- Zoo

for (r in 2:length(runs)){
  mdl <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/model_",runs[r],".RDS"))
  temp <- read_rds(paste0("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200917_CMIP_Matrix/",runs[r],"/Output/res_",runs[r],".RDS")) %>%
    fZooMSS_SpeciesBiomass(mdl) %>%
    fZooMSS_Convert2Tibble(mdl) %>%
    fZooMSS_AddEnviro(enviro) %>%
    pivot_longer(cols = Flagellates:Fish_Large, names_to = "Species", values_to = "Biomass") %>%
    mutate(Chl_log10 = round(Chl_log10,2),
           SST = round(SST,2))

  Zoo <- temp %>%
    right_join(Zoo, by = c("cellID", "Species", "SST", "Chl_log10", "Chl")) %>%
    rename(!!runs[r] := Biomass)
}


Zoo <- Zoo %>%
  mutate(Diff_NoCarnivores = ((NoCarnivores - Control)/Control) * 100,
         Diff_NoOmnivores = ((NoOmnivores - Control)/Control) * 100,
         Diff_NoFilterFeeders = ((NoFilterFeeders - Control)/Control) * 100,
         Diff_OneZoo = ((OneZoo - Control)/Control) * 100) %>%
  pivot_longer(cols = starts_with("Diff_"), names_to = "Group", values_to = "DiffBiomass") %>%
  mutate(Group = as.factor(Group),
         Species = as.factor(Species)) %>%
  filter(!is.na(DiffBiomass))


gg <- ggplot(data = Zoo, aes(x = SST, y = Chl_log10, colour = DiffBiomass, fill = DiffBiomass)) +
  geom_tile() +
  scale_color_gradient2(low = "blue", high = "red", limits = c(-100, 100), oob = scales::squish) +
  guides(fill=FALSE) +
  facet_grid(rows = vars(Group), cols = vars(Species))

x11(width = 24, height = 16)
gg
ggsave("Figures/Facet_MatrixBiomassChange.png", dpi = 300)



