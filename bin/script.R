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

source("code/functions.R")


# = code ===================================================================== #

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

# = second part ============================================================== #

# combine experimental replicas, the 3 individual csv files 
# extract `avg` (name them `R_G`), calculate R_G_norm (value / R_G[0])
# import file containing "alive/total" data for each experiment

#immobilisation <- read.xlsx("misc/JC-1_immobilisation_data.xlsx")[, 2:5]
#immobilisation$compound <- sapply(immobilisation$Experiment, function(x){
#  paste(strsplit(x, "-")[[1]][1:3], collapse = "-")
#})
#immobilisation$experiment <- sapply(immobilisation$Experiment, function(x){
#  strsplit(x, "-")[[1]][4]
#})
#fwrite(immobilisation[, 2:6], file = "data/JC1_immobilisation_data.csv")

immobil <- read.csv("data/JC1_immobilisation_data.csv")


files <- list.files(path = "data", pattern = "values")

# FIX, extract_compound_names function, used in 3 places below, boilerplate...

extract_compound_names <- function(files){
  out <- sapply(files, function(file){
    part1 <- strsplit(file, "-")[[1]][5:7]
    part2 <- strsplit(part1[3], "_")[[1]]

    paste0(part1[1], "-", part1[2], "-", part2[1])
  })
  unique(out)
}

compounds <- extract_compound_names(files)

for (compound in compounds){
  file <- files[grepl(compound, files)]

  dat <- lapply(file, function(x){
    out <- fread(file.path("data", x))
    name <- strsplit(x, "-")[[1]][7]
    name <- strsplit(name, "_")[[1]][2]
    
    out$compound <- extract_compound_names(x)

    out$experiment <- name
    out$id <- nchar(name)

    out$conc <- as.numeric(out$conc)
    out <- out[!is.na(out$conc), ]

    # calculate normalised values of "avg" column
    out$R_G_norm <- out$avg / out$avg[1]
    colnames(out)[2] <- "R_G"

    return(out)
  })

  dat <- Reduce(function(x,y) rbind(x,y), dat) %>% data.frame
  
  dat <- merge(dat, immobil)
  dat <- dat[order(dat$conc, dat$id), 
    c("conc", "R_G", "R_G_norm", "alive", "total", "compound", "experiment")]
  rownames(dat) <- NULL

  filename <- extract_compound_names(file)
  filename <- paste0(filename, "_part2.csv") # quick'n'dirty

  write.csv(dat, file = file.path("data", filename), row.names = FALSE)
}