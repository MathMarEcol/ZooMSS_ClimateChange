library(tidyverse)
library(yaImpute)

source("fZooMSS_Xtras.R")

base_dir <- paste0("~",.Platform$file.sep,
                  "Nextcloud",.Platform$file.sep,
                  "MME2Work",.Platform$file.sep,
                  "FishMIP",.Platform$file.sep,
                  "Phase1",.Platform$file.sep)

# TOTAL consumer biomass density, tcb, g m-2, 1° grid, monthly, all consumers (trophic level >1, vertebrates and invertebrates)
# TOTAL consumer biomass density in log10 weight bins, tcblog10, g m-2, 1° grid, monthly, 6 size bins, If the model is size-structured, please provide biomass in equal log 10 g C weight bins (1g, 10g, 100g, 1kg, 10kg, 100kg)
#
# TOTAL pelagic biomass density, tpb, g m-2, 1° grid, monthly, all pelagic consumers (trophic level >1, vertebrates and invertebrates)
# Biomass density of small pelagics <30cm, bp30cm, g m-2, 1° grid, monthly, if a pelagic species and L infinity is <30 cm, include in this variable
# Biomass density of medium pelagics >=30cm and <90cm, bp30to90cm, g m-2, 1° grid, monthly, if a pelagic species and L infinity is >=30 cm and <90cm, include in this variable
# Biomass density of large pelagics >=90cm, bp90cm, g m-2, 1° grid, monthly, if a pelagic species and L infinity is >=90cm, include in this variable

#### Conversions for Length to Weight ####
#convert FishMIP length thresholds to weight thresholds, using (wet weight)=0.01(length)^3
weight30 <- 10^round(log10(30^(1/3)*100),1) # using Weight (g) = 0.01 * length^3, length is in cm
weight90 <- 10^round(log10(90^(1/3)*100),1)

#### Mixed Layer Depth ####
MLD <- 60

#### Load ZooMSS Matrix Data ####
enviro_data <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200526_TheMatrix/enviro_Matrix.RDS")
res <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200526_TheMatrix/Output/res_20200526_TheMatrix.RDS" )
temp <- read_rds("~/Nextcloud/MME2Work/ZooMSS/_LatestModel/20200526_TheMatrix/20200526_TheMatrix_000001.RDS")


w <- temp$model$param$w
w_lim <- 10^c(-1, 0, 1, 2, 3, 4, 5)

Bio <- fZooMSS_Biomass(res, w)
BioSumSp <- fZooMSS_SumSpecies(Bio) # Sum the species
rm(res, Bio)

Bio_df <- as_tibble(matrix(unlist(BioSumSp), nrow=length(BioSumSp), byrow=T)) %>%
  mutate(cellID = 1:n()) %>% # Create a cellID
  pivot_longer(cols = starts_with("V"), names_to = "SizeClass", values_to = "Biomass") %>%
  mutate(Biomass = Biomass * MLD, # Convert biomass to m-2 by * MLD
         Weight = rep(w, times = length(BioSumSp))) %>% # Make sure weight class is on every row
  # select(-SizeClass) %>%
  filter(Weight <= 100001) %>% # Remove very large stuff (100 kg)
  # mutate(BiomassC = Biomass * 0.1) %>% # convert to carbon biomass
  add_column(tcb = 1, tcblog10_0 = 0, tcblog10_1 = 0, tcblog10_2 = 0,
             tcblog10_3 = 0, tcblog10_4 = 0, tcblog10_5 = 0, tpb = 1,
             bp30cm = 0, bp30to90cm = 0, bp90cm = 0) %>% # Create column of ones
  mutate(tcb = tcb * Biomass, # All consumers is simply biomass
         tpb = tpb * Biomass, # All pelagic consumers is simply biomass
         tcblog10_0 = replace(tcblog10_0, Weight >= w_lim[1] & Weight < w_lim[2], 1), # Replace 1 with zero for rows outside weight range
         tcblog10_1 = replace(tcblog10_1, Weight >= w_lim[2] & Weight < w_lim[3], 1), # Replace 1 with zero for rows outside weight range
         tcblog10_2 = replace(tcblog10_2, Weight >= w_lim[3] & Weight < w_lim[4], 1), # Replace 1 with zero for rows outside weight range
         tcblog10_3 = replace(tcblog10_3, Weight >= w_lim[4] & Weight < w_lim[5], 1), # Replace 1 with zero for rows outside weight range
         tcblog10_4 = replace(tcblog10_4, Weight >= w_lim[5] & Weight < w_lim[6], 1), # Replace 1 with zero for rows outside weight range
         tcblog10_5 = replace(tcblog10_5, Weight >= w_lim[6] & Weight < w_lim[7], 1), # Replace 1 with zero for rows outside weight range
         bp30cm = replace(bp30cm, Weight < weight30, 1), # Replace 1 with zero for rows outside weight range
         bp30to90cm = replace(bp30to90cm, Weight >= weight30 & Weight < weight90, 1), # Replace 1 with zero for rows outside weight range
         bp90cm = replace(bp90cm, Weight >= weight90, 1), # Replace 1 with zero for rows outside weight range
         tcblog10_0 = tcblog10_0 * Biomass, # Multiply weight class switch (0,1) by Biomass
         tcblog10_1 = tcblog10_1 * Biomass, # Multiply weight class switch (0,1) by Biomass
         tcblog10_2 = tcblog10_2 * Biomass, # Multiply weight class switch (0,1) by Biomass
         tcblog10_3 = tcblog10_3 * Biomass, # Multiply weight class switch (0,1) by Biomass
         tcblog10_4 = tcblog10_4 * Biomass, # Multiply weight class switch (0,1) by Biomass
         tcblog10_5 = tcblog10_5 * Biomass, # Multiply weight class switch (0,1) by Biomass
         bp30cm = bp30cm * Biomass, # Multiply weight class switch (0,1) by Biomass
         bp30to90cm = bp30to90cm * Biomass, # Multiply weight class switch (0,1) by Biomass
         bp90cm = bp90cm * Biomass)  # Multiply weight class switch (0,1) by Biomass

BioSum_df <- Bio_df %>%
  group_by(cellID) %>%
  summarise(tcb = sum(tcb),
            tcblog10_0 = sum(tcblog10_0),
            tcblog10_1 = sum(tcblog10_1),
            tcblog10_2 = sum(tcblog10_2),
            tcblog10_3 = sum(tcblog10_3),
            tcblog10_4 = sum(tcblog10_4),
            tcblog10_5 = sum(tcblog10_5),
            tpb = sum(tpb),
            bp30cm = sum(bp30cm),
            bp30to90cm = sum(bp30to90cm),
            bp90cm = sum(bp90cm),
            .groups = "keep") %>%
  ungroup() %>%
  left_join(select(enviro_data, cellID, chlo, sst), by = "cellID") %>%
  rename(SST = sst) %>%
  mutate(Chl_log10 = log10(chlo))

rm(BioSumSp, Bio_df, enviro_data)

files = list.files(path = paste0(base_dir, "Input"), pattern = "*.rds", full.names = FALSE)

for (f in 1:length(files)){

  nc <- read_rds(paste0(base_dir, "Input", .Platform$file.sep, files[f]))
  out <- ann(as.matrix(BioSum_df[,c("SST", "Chl_log10")]),
             as.matrix(nc[,c("SST", "Chl_log10")]),
             k = 1, verbose = FALSE)

  nc <- nc %>%
    mutate(cellID = out$knnIndexDist[,1],
           EuclideanDist = out$knnIndexDist[,2],
           tcb = BioSum_df$tcb[cellID],
           tpb = BioSum_df$tpb[cellID],
           tcblog10_0 = BioSum_df$tcblog10_0[cellID],
           tcblog10_1 = BioSum_df$tcblog10_1[cellID],
           tcblog10_2 = BioSum_df$tcblog10_2[cellID],
           tcblog10_3 = BioSum_df$tcblog10_3[cellID],
           tcblog10_4 = BioSum_df$tcblog10_4[cellID],
           tcblog10_5 = BioSum_df$tcblog10_5[cellID],
           bp30cm = BioSum_df$bp30cm[cellID],
           bp30to90cm = BioSum_df$bp30to90cm[cellID],
           bp90cm = BioSum_df$bp90cm[cellID],
           Chl_log10_ZooMSS = BioSum_df$Chl_log10[cellID],
           SST_ZooMSS = BioSum_df$SST[cellID])

  write_rds(nc, paste0(base_dir, "Output", .Platform$file.sep, str_replace(files[f],".rds", "_withZooMSS.rds"))) # Save to RDM
  rm(nc, out)
}

