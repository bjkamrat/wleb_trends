---
editor_options: 
  markdown: 
    wrap: 72
---

# README

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

## Project Description

This project provides the data, analysis, and reports for the
investigation into the effectiveness of nutrient

management regulations in the western lake erie basin (WLEB).

## Folder Structure

The project comprises of five main folders in which you would store
certain files.

### data/

This folder contains data files like .csv, .xls.

-   Data collected for this project included water quality, flow, and
    soil temperature data

In this folder:

-   Raw

    -   Downloaded data were stored in the data/raw folder.

    -   WQ data were downloaded from <https://ncwqr-data.org/> for each
        pollutant of interest.

        -   Each pollutant has its own folder.

    -   weather/soil data was obtained from the Ohio State University
        Northwest Agricultural Research station
        (<https://weather.cfaes.osu.edu//stationinfo.asp?id=9)>

        -   Saved as weatherdata_nw.csv.

    -   County livestock data was obtained from the NASS survey

        -   Saved as hogs_cattle_county.xlsx file.

-   wrtds

    -   contains inputs and outputs to and from wrtds models for each
        site and pollutant

    -   in the inputs, the gen_dat.xlsx contains the general data for
        each site and which was used in for loops throughout the
        analysis

-   process

    -   contains the processed results from data analysis.

### R/

This folder should contain R script files.

In this folder:

-   The analysis folder includes analysis scripts
    -   These analyses folder are label by number in order of run
-   The funcs folder includes functions
-   The 2Q folder includes the split flow analysis developed by Qian
    Zhang (UMCES/CBPO) and James S. Webber (USGS) - not used in
    manuscript
    -   The FN_2Q approach is formally documented in the following
        journal publication: Zhang, Q., J.S. Webber, D.L. Moyer, and
        J.G. Chanat, 2021, An approach for decomposing estimated river
        water-quality trends into different flow classes, Science of the
        Total Environment 755: 143562, doi:
        10.1016/j.scitotenv.2020.143562.
-   exploratory folder
    -   This includes what are essential random or trial scripts used to
        test ideas for analysis.
    -   Labeled by date of effort.

### Results/

-   Includes numerous figures, some used in manuscript.

### 

### docs/

This folder contains documents, including draft manuscripts of, of the
analysis.
