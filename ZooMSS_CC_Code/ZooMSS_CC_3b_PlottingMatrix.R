
# Plot change relative to Control
source("~/GitHub/ZooMSS/fZooMSS_Xtras.R")
library(tidyverse)

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


gg <- ggplot(data = Zoo, aes(x = SST, y = Chl, colour = DiffBiomass, fill = DiffBiomass)) +
  geom_tile() +
  scale_color_gradient2(low = "blue", high = "red", limits = c(-100, 100), oob = scales::squish) +
  guides(fill=FALSE) +
  facet_grid(rows = vars(Group), cols = vars(Species))

x11(width = 24, height = 16)
gg
ggsave("Figures/Facet_MatrixBiomassChange.png", dpi = 300)



gg <- ggplot(data = Zoo, aes(x = SST, y = Chl_log10, colour = DiffBiomass, fill = DiffBiomass)) +
  geom_tile() +
  scale_color_gradient2(low = "blue", high = "red", limits = c(-100, 100), oob = scales::squish) +
  guides(fill=FALSE) +
  facet_grid(rows = vars(Group), cols = vars(Species))

x11(width = 24, height = 16)
gg
ggsave("Figures/Facet_MatrixBiomassChange_log10.png", dpi = 300)




Zoo2 <- Zoo %>%
  filter(Group %in% "Diff_NoOmnivores") %>%
  # filter(Species %in% "Jellyfish") %>%
  filter(SST == 20)

ggplot(data = Zoo2, aes(x = Chl_log10, y = DiffBiomass, colour = Species)) +
  geom_line() +
  ylim(c(-100, 100))


ggplot(data = Zoo2, aes(x = Chl_log10, y = log10(Control), colour = Species)) +
  geom_line()
# +
  # ylim(c(-100, 100))

ggplot(data = Zoo2, aes(x = Chl, y = log10(Control), colour = Species)) +
  geom_line()


enviro2 <- enviro %>%
  filter(sst == 20)

ggplot(data = enviro2, aes(x = log10(chlo), y = phyto_slope)) + geom_line()
ggplot(data = enviro2, aes(x = log10(chlo), y = phyto_int)) + geom_line()

ggplot(data = enviro2, aes(x = log10(chlo), y = pico_biom)) + geom_line()
ggplot(data = enviro2, aes(x = log10(chlo), y = nano_biom)) + geom_line()
ggplot(data = enviro2, aes(x = log10(chlo), y = micro_biom)) + geom_line()

ggplot(data = enviro2, aes(x = log10(chlo), y = pico_biom + nano_biom + micro_biom)) + geom_line()
