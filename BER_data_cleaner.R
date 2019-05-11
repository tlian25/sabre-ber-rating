## BER DATA CLEANER
# A quick and dirty script to clean data and aggregate into one file. 

### Packages
library(data.table)
library(tidyverse)

# Neat Print - nprint() - a function to make printing and viewing more concise
nprint = function(dt) {
  columns = c("game_id", "batting_team", "res_batter", 
              "position_before", "position_after", 
              "runs_scored", "runs_scored_EOI")
  dt[, ..columns] %>% print()
}

# Create column names to fit to CSV imports
column_names = c("game_id", "visiting_team", "inning", "batting_team_raw",
                 "outs", "balls", "strikes", "vis_score", "home_score",
                 "res_batter", "res_batter_hand", "res_pitcher", "res_pitcher_hand",
                 "1b_runner", "2b_runner", "3b_runner", "event_text",
                 "leadoff_flag", "pinchhit_flag", "defensive_position", 
                 "lineup_posiiton", "event_type", "batter_event_flag",
                 "ab_flag", "hit_value", "SH_flag", "SF_flag", "outs_on_play",
                 "RBI_on_play", "wild_pitch_flag", "passed_ball_flag",
                 "num_errors", "batter_dest", "1b_dest", "2b_dest",
                 "3b_dest", "position_before", "runs_scored", "position_after", 
                 "runs_scored_EOI", "batting_team", "year", "date", "temperature",
                 "stadium", "ABvsPitcher", "Rtot", "DefEff", 
                 "1b_speed", "2b_speed", "3b_speed", "FIP", "park_factor")

# Make sure you have the right file location on your computer
# Create a list of file names to read
folder_name = "/Users/txl/TXL_R/Final Data"
fl = list.files(folder_name, pattern = "*dataClean.csv", full.names=T) 

# Loop through the files and put each season as a data.table in a list of data.tables
dat_list =  list()
i = 1
for (f in fl) {
  print("Reading file:")
  print(f)
  season_dat = fread(f, col.names = column_names)
  dat_list[[i]] = season_dat
  i = i + 1
}

# Bind all data.tables together in an aggregate
dt = rbindlist(dat_list, use.names = T)

# Write CSV of all seasons combined for future reference
write.csv(dt, "datClean_allseasons.csv")

#########################################################

## Regression Work
## Now on to creating value functions 

# For now, just read 2018 seasons data since all data is too big
dt = fread(paste(folder_name, "/2018dataClean_BER.csv", sep = ""))

#Make position_before and position_after factors
dt$position_before = as.factor(dt$position_before)
dt$position_after = as.factor(dt$position_after)

# Model 1: runs_scored_EOI ~ position_before - 1
# Note: I take out the intercept
m1 = lm(runs_scored_EOI ~ position_before - 1, data = dt)
summary(m1)

# Model 2: runs_scored_EOI ~ position_before + park_factor - 1
# Iteration 1: full interaction
m2_i1 = lm(runs_scored_EOI ~ position_before * park_factor - 1, 
        data = dt)
summary(m2_i1)


# Iteration 2: without main park_factor effects
m2_i2 = lm(runs_scored_EOI ~ position_before + position_before:park_factor - 1, 
           data = dt)
summary(m2_i2)

# Iteration 3



# Model 3: runs_scored_EOI ~ position_before + FIP - 1
m3 = lm(runs_scored_EOI ~ position_before + FIP - 1, data = dt)
summary(m3)

# Model 4: runs_scored_EOI ~ position_before 

# Create value table
# To calculate the max possible value, I calculate the maximum number of runs possible with # of bases + 1 (for the batter) + the state left with the same number of outs and no one on base. 

state = sort(unique(dt$position_before))
ev_m1 = m1$coefficients
ev_m2 = m2$coefficients
ev_m3 = m3$coefficients

value_table = data.table(state, ev_m1, ev_m2, ev_m3)



## Explore park_factor
parks_dt = unique(dt[, c("stadium", "park_factor")])
# Bargraph
ggplot(parks_dt, aes(x = stadium, y = park_factor)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("Park Factors")
  
## Explore FIP
fip_dt = unique(dt[, c("res_pitcher", "FIP")])
fip_dt$FIP = as.numeric(fip_dt$FIP)
# Histogram
ggplot(fip_dt, aes(x = FIP)) + 
  geom_histogram() + 
  ggtitle("Histogram of FIP")




############################################################

###### Calculate BER with regression models above

# Maximum Possible Value
# To calculate the max possible value, I calculate the maximum number of runs possible with # of bases + 1 (for the batter) + the state left with the same number of outs and no one on base. 
max_possible = c()
for (s in state) {
  str = strsplit(as.character(s), split = "")[[1]]
  # max number of runs (bases + batter)
  max = as.integer(str[5]) + as.integer(str[7]) + as.integer(str[9]) + 1
  # state of field after a home run
  after = switch(as.integer(str[3])+1, "['0,0,0,0']", "['1,0,0,0']", "['2,0,0,0']")
  # value of that state of field
  after_value = value_table[state == as.factor(after)]$ev
  # add max and after_value
  max_possible = cbind(max_possible, max + after_value)
}
value_table[, max := as.vector(max_possible)]

# Export to CSV
write.csv(value_table, "value_table.csv")















##############################################

# Season by Season value table 
value_table = fread("value_table.csv")[, V1:=NULL]

# Create a list of file names to read
folder_name = "/Users/txl/Google Drive/Sports Analytics/Final Data"
fl = list.files(folder_name, pattern = "*dataClean.csv", full.names=T) 

for (f in fl) {
  print("Reading file:")
  print(f)
  dat = fread(f, col.names = column_names)
  
  #Regression: runs_scored_EOI ~ position_before - 1
  m = lm(runs_scored_EOI ~ position_before - 1 , data = dat)
  value_table[, paste("ev_", substr(f, 53, 56), sep="") := m$coefficients]
  
}

write.csv(value_table, "value_table.csv")

# Look for patterns and persistence year to year in value functions
# Transpose value table so that rows are states and columns are years
vt = fread("value_table.csv")[, V1:= NULL]
tr_vt = dcast(melt(vt, id.vars = "state"), variable~state)[-(1:2), ]
tr_vt$year = sapply(tr_vt$variable, function(c) as.numeric(substr(c, 4, 7)))

### Line graph across time
# For each state, plot the EV from season to season across time
melted = melt(tr_vt[, -1], id.vars = "year") %>% setnames(old = "variable", new = "state")
ggplot(melted, aes(x = year, y = value, colour = state)) + geom_line()

### Boxplot across states
# For each state, create a boxplot of range of values
melted = melt(vt, id.vars = "state")[variable != "max"]
ggplot(melted, aes(x = state, y = value)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))


### Standardize across time to be mean 0

#Standardize our value table by dividing by the mean across time for each state.
tr_vt_std = sweep(tr_vt[, -1], 2, colMeans(tr_vt[,-1]), `-`) %>% data.table()
tr_vt_std = tr_vt_std[, year := tr_vt$year]

### Line graph across time (Mean-zeroed)
# For each state, plot the Standardized EV from season to season across time
melted = melt(tr_vt_std[, -1], id.vars = "year") %>% setnames(old = "variable", new = "state")
ggplot(melted, aes(x = year, y = value, colour = state)) + geom_line()


vt_std = sweep(vt[, -(1:3)], 1, rowMeans(vt[, -(1:3)]), `-`) %>% data.table()
vt_std = vt_std[, state := vt$state]

### Boxplot across states (Mean-zeroed)
# For each state, create a boxplot of range of values
melted = melt(vt_std, id.vars = "state")[variable != "max"]
ggplot(melted, aes(x = state, y = value)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))


### Fully Normalized Data

# Fully normalized with mean and variance
tr_vt_norm = scale(tr_vt[, -c("year", "variable")]) %>% 
  data.table()

tr_vt_norm = tr_vt_norm[, year := tr_vt$year]
vt_norm = dcast(melt(tr_vt_norm, id.vars = "year"), variable~year) %>% 
  setnames(old = "variable", new = "state")


### Boxplot across time (Normalized)
melted = melt(vt_norm, id.vars = "state")[variable != "max"]
ggplot(melted, aes(x = state, y = value)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90))


### Note: The plot seems to be well distributed across time and persistence seems to hold across years. 
# Need to decide which subset of data to use since the whole 40 seasons is too large. Or we could run season by season and take averages. But this method would cause issues in error estimation.








