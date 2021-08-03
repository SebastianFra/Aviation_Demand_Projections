#' Generate Aviation Demand Transport Input Data for the REMIND model.
#' @author Sebastian Franz

GenerateAviationDemand <- function(data_folder,REMIND_scenario="SSP2", COVID_scenario=TRUE, saveRDS=FALSE){
  ## load data
  print("-- load data")
  tech_output = readRDS(file =file.path(data_folder,"tech_output.rds"))
  #SSP differentiation
  if (REMIND_scenario == "SSP1") {
    GDP_country= readRDS(file =file.path(data_folder,"GDP_country_SSP1.rds"))
    POP_country=readRDS(file =file.path(data_folder,"POP_country_SPP1.rds"))
    GDP_POP=readRDS(file =file.path(data_folder,"GDP_POP_SSP1.rds"))
    price_baseline=readRDS(file =file.path(data_folder,"price_baseline_SPP1.rds"))
  }else if (REMIND_scenario == "SSP2") {
    GDP_country= readRDS(file =file.path(data_folder,"GDP_country_SSP2.rds"))
    POP_country=readRDS(file =file.path(data_folder,"POP_country_SPP2.rds"))
    GDP_POP=readRDS(file =file.path(data_folder,"GDP_POP_SSP2.rds"))
    price_baseline=readRDS(file =file.path(data_folder,"price_baseline_SPP2.rds"))
  }else if (REMIND_scenario == "SSP5") {
    GDP_country= readRDS(file =file.path(data_folder,"GDP_country_SSP5.rds"))
    POP_country=readRDS(file =file.path(data_folder,"POP_country_SPP5.rds"))
    GDP_POP=readRDS(file =file.path(data_folder,"GDP_POP_SSP5.rds"))
    price_baseline=readRDS(file =file.path(data_folder,"price_baseline_SPP5.rds"))
  }else{}
  ICCT_data_I =readRDS(file =file.path(data_folder,"ICCT_data_I.rds"))
  ICCT_data_D = readRDS(file =file.path(data_folder,"ICCT_data_D.rds"))
  REMIND2ISO_MAPPING_adj=readRDS(file =file.path(data_folder,"REMIND2ISO_MAPPING_adj.rds"))
  REMIND2ISO_MAPPING =readRDS(file =file.path(data_folder,"REMIND2ISO_MAPPING.rds"))

  ## regression demand calculation
  print("-- performing demand regression")
  demand_reg = demand_regression(tech_output = tech_output,
                              price_baseline = price_baseline,
                              GDP_country = GDP_country,
                              POP_country =POP_country,
                              GDP_POP = GDP_POP,
                              REMIND_scenario = REMIND_scenario,
                              ICCT_data_I = ICCT_data_I,
                              ICCT_data_D = ICCT_data_D,
                              REMIND2ISO_MAPPING_adj = REMIND2ISO_MAPPING_adj,
                              REMIND2ISO_MAPPING = REMIND2ISO_MAPPING,
                              COVID_scenario= COVID_scenario)
  #RPK Treshold & Decay
  write.csv2(demand_reg$Threshold_data, file.path(data_folder,"Threshold_data.csv"))
  saveRDS(demand_reg[["D_star"]], file.path(data_folder,(paste0("aviation_demand_",REMIND_scenario,"_COVID=",COVID_scenario, ".rds"))))
  saveRDS(demand_reg[["elasticities"]], file.path(data_folder,(paste0("elasticities_",REMIND_scenario,"_COVID=",COVID_scenario, ".rds"))))
}

