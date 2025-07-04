# This repository has moved

This repository has been moved! You can now find it here:

### https://github.com/andreyhgl/image-based-analysis

// Andrey 2025-06-18

---

# Project summary

The aim of this project was to develop an image-based analysis method for toxicological effects in Daphnia magna after exposure to industrial chemicals and/or environmental samples. Molecular staining with JC-1 combined with fluorescence microscopy was used to measure adverse effects on mitochondrial health. The effects were measured based on light intensity data extracted from fluorescence images.  Fast generation of toxicological data is a main goal in this study. Therefore image acquisition was done with an automated multipoint confocal high-content imaging system suitable for experiments in multi-well plates. To have an efficient analysis of intensity data, this R workflow was developed to process the microscope output, perform basic statistical analysis and generate figures to get a fast and comprehensive overview about the data

>Code was used in this paper: [Automated Image-based Fluorescence Screening of Mitochondrial Membrane Potential in Daphnia magna: An advanced ecotoxicological testing tool](https://doi.org/10.1021/acs.est.4c02897)

## Project structure

Three example files are found in `imgxpress_files` folder to test the code on. `bin` contains the functions and code needed to produce the plots.

```
project/
|-- bin/
|   |-- dose_response.R
|   |-- functions.R
|   `-- script.R
|-- data/
|-- doc/
|   `-- 24h_TCS.xlsx
|-- img/
|-- imgxpress_files/
|   |-- 2023-06-02-DM-JC1-CCCP-2h_I.xlsx
|   |-- 2023-06-02-DM-JC1-CCCP-2h_II.xlsx
|   `-- 2023-06-02-DM-JC1-CCCP-2h_III.xlsx
`-- README.md
```


## Code explanation

The imageXPress output files comes as excel and should contain 4 tabs: (1) "red", (2) "green", (3) "Segmentation" and (4) "Samples".
Tabs 1-3 are autogenerated by the imageXpress machine, tab 4 is manual (written during loading of the machine) and contains info about sample concentrations.

+ red = red channel
+ green = green channel
+ Segmentation = masking layer
+ Samples = sample metadata w/ concentrations

The `scripts.R` loads any files found in `imgxpress_files/` and produces for each file a figure with three plots (A) sample averages plotted against concentrations, (B) ratio between red / green channel (averages), and (C) distribution plots of each concentrations (`img/file.pdf`). Additionally, a csv with values for plot B is produced (`data/file.csv`).

## Code explanation: dose-response modeling
The results from the previous code are summarized in a new file containing JC-1 intensities and dead/leave evaluation of all 3 independent experiments.

The drc package is used to model dose-response relationships for both endpoints (immobilization and JC-1 signal). 
[Reference: Ritz, C.; Baty, F.; Streibig, J. C.; Gerhard, D. Dose-Response Analysis Using R. PLOS ONE 2015, 10 (12), e0146021.](https://doi.org/10.1371/journal.pone.0146021).

The file contains also the code used to create the plots in the publication: [Abele et. al 2024](https://doi.org/10.1021/acs.est.4c02897). A modification of the code obtained from: [ggplot with 2 y-axes on each side and different scales](https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales).
