Prepared on 11/08/2021
Readme file for users to run the FN_2Q approach for decomposing Weighted Regression on Time Discharge and Season (WRTDS) flow-normalized (FN) flux estimates into low-flow and high-flow components.
by James S Webber and Qian Zhang

This model data release contains the Estby2Flows function for decomposing WRTDS FN concentrations and fluxes into low-flow and high-flow components. The approach is formally documented in the following journal manuscript: Zhang, Q., Webber, J.S., Moyer, D.L., and Chanat, J.G., 2020, An approach for decomposing river water-quality trends into different flow classes: Science of The Total Environment.

This model data release was updated in November 2021 to resolve an error in the original R script (posted September 2020, available from author). In the original script, the main Estby2Flows function did not partition the high flow component correctly when users selected a cutoff value that differs from the default of 0.5.

Prior to running this function, users will have to install R on their computer.  R software can be downloaded from the following location https://www.r-project.org/.  The version of R that was used to develop and run the function was R 3.6.0.

The files provided are as follows:
(1) WRTDS_FN_2Q_Readme_v1.1.txt - Provides general instructions to run the FN_2Q function
(2) WRTDS_FN_2Q_v1.1.R - Script used to run Estby2Flows and associated functions.	  

Steps to run the function:
(1) Download the WRTDS_FN_2Q.R file
(2) Begin an R or RStudio session
(3) Create a new or load an existing WRTDS workspace (called an eList) into the R or RStudio session. To obtain an "eList" for a specific data set, the standard WRTDS method needs to be run based on a record of water-quality samples and a record of daily flow discharges.  For WRTDS workflow and further information, refer to the following two resources:
EGRET website: http://usgs-r.github.io/EGRET/index.html
EGRET manual: https://pubs.usgs.gov/tm/04/a10/
(4) If not already done in step 3, install and load the EGRET package.
(5) Run the entire WRTDS_FN_2Q.R script in R or RStudio to load the main Estby2Flows function into R workspace. The script will also load:
	(a) Functions to aggregate daily values into monthly and annual estimates (annual.est, annualWY.est, and monthly.est)
	(b) Functions to generate and report uncertainties on FN and FN_2Q estimates (FN.2Q.wBT and FN.2Q.wBT.report)
(6) Generate the daily output by running the Estby2Flows function. Assign an object to store the daily values.
(7) If desired, aggregate the daily values to monthly or annual estimates by running the monthly.est, annual.est, or annualWT.est functions.
(8) If desired, generate uncertainity estimates by running FN.2Q.wBT and FN.2Q.wBT.report