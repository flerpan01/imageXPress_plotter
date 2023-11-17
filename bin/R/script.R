# The script looks for ImageXPress [Molecular Devices] outputs (excel) to 
# generate plots

# Pipeline:
# 
# calculate the mean per individual
# calculate the red / green ratio based on the means
# add DSMO to x-axis plots
# calculate the ratio between the red and the green channel of a X scan
# dataset is composed of excel file with 4 tabs
# orders of concentrations: control + increasing conc.
# facelift plots, theme_pubr()
# output csv file with calculated means for ratios

# title: Mitochondrial health assay in D.Magna using JC-1 fluorescence dye

# == setup ================================================================== #

# set working directory. Should contain `imgxexpress_output` folder
#setwd()

library(data.table)
library(tidyverse)
library(openxlsx)
library(cowplot)
library(ggpubr)
library(ggridges)
library(reshape2)

source("bin/R/functions.R")


# ========================== variables ======================================= #

files <- list.files("imgxpress_files")

for (file in files){
  print(paste("running:", file))
  
  data <- extract_dye_ch(file)
  p <- generate_plot(data, base_size = 12)
  save_pdf(p)

  # save values as csv as well
  # include mean, sd, rel. sd
  values <- data$ratios_mean %>%
    group_by(conc) %>%
    summarise(
      avg = mean(ratio, na.rm = T),
      std_dev = sd(ratio, na.rm = T),
      rel_std_dev = std_dev / avg * 100
    )

  filename <- sub(".xlsx", "_values.csv", file)
  filename <- file.path("data", filename)
  fwrite(values, file = filename)
}
