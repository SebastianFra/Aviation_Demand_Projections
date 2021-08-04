#required packages
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
install.packages("madrat")
install.packages("rmndt")
install.packages("data.table")
require(madrat)
require(data.table)
require(rmndt)
# local folder
########### IMPORTANT ###################
#please insert here the path to you local folder
local_folder= "ADD/YOUR/LOCAL/PATH/HERE"
########### IMPORTANT ###################
setwd(local_folder)
data_folder = file.path(local_folder,"data_folder")
#load functions
source(file.path(local_folder,"/R/GenerateAviationDemand.R"))
source(file.path(local_folder,"/R/demand_regression.R"))
#COVID
start_time <- Sys.time()
GenerateAviationDemand(data_folder = data_folder,
                 REMIND_scenario="SSP1", saveRDS=TRUE, COVID_scenario = TRUE)
GenerateAviationDemand(data_folder = data_folder,
                       REMIND_scenario="SSP2", saveRDS=TRUE, COVID_scenario = TRUE)
GenerateAviationDemand(data_folder = data_folder,
                       REMIND_scenario="SSP5", saveRDS=TRUE, COVID_scenario = TRUE)

#noCOVID
GenerateAviationDemand(data_folder = data_folder,
                       REMIND_scenario="SSP1", saveRDS=TRUE, COVID_scenario = FALSE)
GenerateAviationDemand(data_folder = data_folder,
                       REMIND_scenario="SSP2", saveRDS=TRUE, COVID_scenario = FALSE)
GenerateAviationDemand(data_folder = data_folder,
                       REMIND_scenario="SSP5", saveRDS=TRUE, COVID_scenario = FALSE)

end_time <- Sys.time()
end_time - start_time
