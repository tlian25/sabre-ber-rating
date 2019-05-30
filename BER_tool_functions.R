# BER_tool_functions.R
# TXL

# A file of useful tools for calculating BER project
library(data.table)
library(tidyverse)
library(gridExtra)
library(multiwayvcov)

# Make sure folder is correct
#folder_name = "/Users/txl/TXL_R/Final Data/"
folder_name = "~/Final Data/"

#---------------------------------------------------------------
# Imports data for a given season
import_season_data = function(year) {
  print(paste0("Importing Data for Season: ", as.character(year)))
  dt = fread(paste0(folder_name, as.character(year), "dataClean.csv"))
  
  # Make position_before and position_after factors
  dt$position_before = as.factor(dt$position_before)
  dt$position_after = as.factor(dt$position_after)

  # Make league a factor
  dt$league = as.factor(dt$league)
  
  # Transform park_factor to be a percentage above or below 100%
  dt$park_factor = dt$park_factor - 100
  
  # Turn FIP into numeric
  # NOTE: We have some NA FIPs
  # Cap fip at 16. 
  dt$FIP = as.numeric(dt$FIP)
  dt[FIP > 16]$FIP = 16
  
  # Calculate max_runs possible scored
  dt$max_runs = as.numeric(dt$first_runner != "") + 
    as.numeric(dt$second_runner != "") +
    as.numeric(dt$third_runner != "") + 1
  
  return(dt)
}

#---------------------------------------------------------------
# Calculate BER for the league model
# Runs regression from a loaded data.table
# Return: regression model object
# Return: value_list for state
calculate_BER_value_function_league = function(dt) {
  print("Calculating regression model...")
  reg = lm(runs_scored_EOI ~ position_before + position_before:league - 1,
           data = dt)
  summary(reg)
  
  # Cluster SEs on half-innings
  print("Clustering on half-innings...")
  clust = cluster.vcov(reg, cluster = dt$Half_Inning)
  coef.se = data.table(state = names(reg$coefficients),
                       coef = reg$coefficients, 
                       se = coef(summary(reg))[, "Std. Error"],
                       se.clust = sqrt(diag(clust)))
  
  vl = reg$coefficients 
  names(vl) = names(reg$coefficients) %>%
    sapply(function(s) gsub("position_before", "", s)) %>%
    sapply(function(s) gsub(":league", "", s)) %>%
    sapply(function(s) if_else(grepl("*NL", s), s, 
                               paste0(s, "AL")))
  
  # Add base states to NL adjustments for total NL effect
  vl[25:48] = vl[1:24] + vl[25:48]
  # Add values of 0 for end inning states
  vl = c(vl, "end inningAL" = 0, "end inningNL" = 0)
  
  return(list("reg" = reg, "vl" = vl, "coef.se" = coef.se))
  
  
}

#---------------------------------------------------------------
# Calculate BER for players in a given season
# Takes a data.table of players and a value_list of BER value functions
calculate_BER_league = function(dt, vl) {
  print("Calculating a BER_dt from a given value function list...")
  # Create a condensed data.table with BER calculations
  BER_dt = dt[, c("res_batter", "PositionBefore_League", "PositionAfter_League",
                  "runs_scored", "max_runs", "runs_scored_EOI", "outs", "league")]
  
  # Calculate values of PositionBefore and PositionAfter
  BER_dt$value_pb = sapply(BER_dt$PositionBefore_League, 
                           function(pb) vl[pb])
  BER_dt$value_pa = sapply(BER_dt$PositionAfter_League,
                           function(pa) vl[pa])
  # Calculate  value_max
  BER_dt$value_max = BER_dt$max_runs - BER_dt$value_pb + 
    sapply(BER_dt$PositionBefore_League,
           function(pb) switch(as.numeric(substr(pb, 3, 3)) + 1,
                               vl[paste0("['0,0,0,0']", substr(pb, 12, 13))],
                               vl[paste0("['1,0,0,0']", substr(pb, 12, 13))],
                               vl[paste0("['2,0,0,0']", substr(pb, 12, 13))]))
  
  # Calculate value_created
  BER_dt$value_created = BER_dt$runs_scored + BER_dt$value_pa - BER_dt$value_pb
  # Calculate BER
  BER_dt$BER = BER_dt$value_created / BER_dt$value_max
  
  return(BER_dt)
}



#---------------------------------------------------------------
# Calculate BER for the league + park_factor + fip model
# Runs regression from a loaded data.table
# Return: regression model object
# Return: value_list for state
calculate_BER_value_function_league_pf_fip = function(dt) {
  print("Calculating regression model...")
  reg = lm(runs_scored_EOI ~ position_before + position_before:league +
             position_before:park_factor + position_before:FIP - 1,
           data = dt)
  
  # Cluster SEs on half-innings
  print("Clustering on half-innings...")
  clust = cluster.vcov(reg, cluster = dt$Half_Inning)
  coef.se = data.table(state = names(reg$coefficients),
                       coef = reg$coefficients, 
                       se = coef(summary(reg))[, "Std. Error"],
                       se.clust = sqrt(diag(clust)))
  
  summary(reg)
  vl = reg$coefficients 
  names(vl) = names(reg$coefficients) %>%
    sapply(function(s) gsub("position_before", "", s)) %>%
    sapply(function(s) gsub(":league", "", s)) %>%
    sapply(function(s) if_else(grepl("*]$", s), paste0(s, "AL"), s))
  
  # Add base states to NL adjustments for total NL effect
  vl[25:48] = vl[1:24] + vl[25:48]
  # Add values of 0 for end inning states
  vl = c(vl, "end inningAL" = 0, "end inningNL" = 0)
  
  return(list("reg" = reg, "vl" = vl, "coef.se" = coef.se))
}

#---------------------------------------------------------------
# Calculate BER for players in a given season
# Takes a data.table of players and a value_list of BER value functions
calculate_BER_league_pf_fip = function(dt, vl) {
  print("Calculating a BER_dt from a given value function list...")
  # Create a condensed data.table with BER calculations
  BER_dt = dt[, c("res_batter", "position_before","position_after", 
                  "PositionBefore_League", "PositionAfter_League",
                  "runs_scored", "max_runs", "runs_scored_EOI", "outs", "league", 
                  "park_factor", "FIP")]
  
  # Calculate values of PositionBefore and PositionAfter
  BER_dt$value_pb = sapply(BER_dt$PositionBefore_League, 
                           function(pb) vl[pb])
  BER_dt$value_pa = sapply(BER_dt$PositionAfter_League,
                           function(pa) vl[pa])
  
  # Park_factor for position_before
  BER_dt$value_PF_pb = BER_dt$park_factor * 
    sapply(BER_dt$position_before, function(pb) vl[paste0(pb, ":park_factor")]) 
  
  # Park_factor for position_after
  BER_dt$value_PF_pa = BER_dt$park_factor * 
    sapply(BER_dt$position_after, function(pa) vl[paste0(pa, ":park_factor")]) 
  
  # FIP for position_before
  BER_dt$value_FIP_pb = BER_dt$FIP * 
    sapply(BER_dt$position_before, function(pb) vl[paste0(pb, ":FIP")])
  
  # FIP for position_after
  BER_dt$value_FIP_pa = BER_dt$FIP * 
    sapply(BER_dt$position_after, function(pa) vl[paste0(pa, ":FIP")])
  
  # Calculate value_max
  BER_dt$value_max = BER_dt$max_runs - BER_dt$value_pb -
    BER_dt$value_PF_pb - BER_dt$value_FIP_pb +
    sapply(BER_dt$PositionBefore_League,
           function(pb) switch(as.numeric(substr(pb, 3, 3)) + 1,
                               vl[paste0("['0,0,0,0']", substr(pb, 12, 13))],
                               vl[paste0("['1,0,0,0']", substr(pb, 12, 13))],
                               vl[paste0("['2,0,0,0']", substr(pb, 12, 13))])) + 
    sapply(BER_dt$position_before,
           function(pb) switch(as.numeric(substr(pb, 3, 3)) + 1, 
                               vl["['0,0,0,0']:park_factor"],
                               vl["['1,0,0,0']:park_factor"],
                               vl["['2,0,0,0']:park_factor"])) * BER_dt$park_factor +
    sapply(BER_dt$position_before,
           function(pb) switch(as.numeric(substr(pb, 3, 3)) + 1, 
                               vl["['0,0,0,0']:FIP"],
                               vl["['1,0,0,0']:FIP"],
                               vl["['2,0,0,0']:FIP"])) * BER_dt$FIP
  
  # Calculate value_created
  BER_dt$value_created = BER_dt$runs_scored + 
    BER_dt$value_pa - BER_dt$value_pb + 
    BER_dt$value_PF_pa - BER_dt$value_PF_pb + 
    BER_dt$value_FIP_pa - BER_dt$value_FIP_pb
  
  # Calculate BER
  BER_dt$BER = BER_dt$value_created / BER_dt$value_max
  
  return(BER_dt)
}



#---------------------------------------------------------------
# Create player table from a given BER_dt
player_name_dict = fread(paste0(folder_name, "Dictionary for Player Names.csv"),
                       col.names = c("res_batter", "player_name"))[order(res_batter)]

create_player_table = function(BER_dt) {
  print("Creating BER player table from a BER_dt...")
  player_list = sort(unique(BER_dt$res_batter))
  PA_count_table = count(BER_dt, res_batter)
  BER = BER_dt %>% group_by(res_batter) %>% summarize(BER_mean = mean(BER))
  RE24 = BER_dt %>% group_by(res_batter) %>% summarize(RE24_sum = sum(value_created))
  player_table = data.table(res_batter = player_name_dict[res_batter %in% 
                                                            player_list]$res_batter,
                            player_name = player_name_dict[res_batter %in%
                                                             player_list]$player_name,
                            PA_count = PA_count_table$n,
                            BER = BER$BER_mean,
                            RE24 = RE24$RE24_sum)
  
  return(player_table)
  
}
