
library(tidyverse)

datn <- read_rds("/Users/jason/Nextcloud/MME2Work/ZooMSS/_LatestModel/20220315_TheMatrix2/Biomass_ClimateChange_Compiled_withZooMSS_IPSL_Control.rds")

dato <- read_rds("/Users/jason/Downloads/ClimateChange_Compiled_withZooMSS_IPSL_Control.rds")

var_names <- c("SST", "Chl", "Flagellates", "Ciliates", "Larvaceans", "OmniCopepods", "CarnCopepods", "Euphausiids",
               "Chaetognaths", "Salps", "Jellyfish", "Fish_Small", "Fish_Med", "Fish_Large")

datos <- dato %>%
  mutate(Date = lubridate::year(Date)) %>%
  group_by(Date, Experiment) %>%
  summarise(across(all_of(var_names), mean, na.rm = TRUE)) %>%
  select(Date, Experiment, all_of(var_names)) %>%
  pivot_longer(cols = all_of(var_names), names_to = "Species", values_to = "Biomass") %>%
  mutate(Version = "Original")

datns <- datn %>%
  rename(Chl = chlo) %>%
  group_by(Date, Experiment) %>%
  summarise(across(all_of(var_names), mean, na.rm = TRUE)) %>%
  select(Date, Experiment, all_of(var_names)) %>%
  pivot_longer(cols = all_of(var_names), names_to = "Species", values_to = "Biomass") %>%
  mutate(Version = "New")

dat2 <- bind_rows(datos, datns)

dat2$Version <- factor(dat2$Version, levels = c("Original", "New"))

ggplot() +
  geom_line(data = dat2, aes(x = Date, y = Biomass, colour = Experiment)) +
  facet_wrap(facets = c("Species", "Version"), scales = "free", ncol = 4, labeller = label_wrap_gen(multi_line=FALSE)) #+
  # theme(strip.background = element_blank(), strip.placement = "outside")



# dat3 <- dat2 %>%
#   filter(Experiment == "historical" & Species == "Chaetognaths")



ggplot() +
  geom_line(data = datos, aes(x = Date, y = Larvaceans, colour = Experiment)) +
  geom_line(data = datns, aes(x = Date, y = Larvaceans, colour = Experiment))


