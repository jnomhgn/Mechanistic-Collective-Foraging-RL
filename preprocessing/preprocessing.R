#### Setup #### 
setwd("/mnt/home/marienhagen/Users/MarienhagenJonathan/rlforaging")
datadir = "data/raw"
resultsdir = "data/processed"

library(stringr)
library(dplyr)
library(tidyr)

# Functions
#Determine trial number (1-36) from file names
trial_number <- function(x){ 
  as.numeric(str_match(x, "trial_\\s*(.*?)\\s*.csv")[,2])
}

#Determine session number (1-18) from file names
session_number <- function(x){ 
  as.numeric(str_match(x, "(?<=session).*"))
}

#### Data Processing ####

# Create wide format dataset from log files
#First, load all data and create combined data frame

#Get all file names
files <- list.files(path = paste(datadir, "data", sep = "/"), pattern = "session")

counter <- 1
for (file in files) {
  
  session <- session_number(file)
  trial_list <- read.csv2(paste(datadir, paste0("sessions/session",session,".csv"), sep = "/"))
  
  trials <- list.files(path = paste(datadir, paste0("data/",file), sep = "/"), pattern = "trial")
  
  for (trial in trials) {
    print(counter)
    
    trial_n <- trial_number(trial) 
    
    duration <- as.numeric(trial_list$duration[trial_n])
    left  <- as.numeric(as.character(trial_list$leftPondChance[trial_n]))
    right <- as.numeric(as.character(trial_list$rightPondChance[trial_n]))
    
    dat_new <- read.csv2(paste(datadir, paste0("data/", file,"/",trial), sep = "/")  )
    dat_new$session <- session
    dat_new$trial <- trial_n
    dat_new$duration <- duration
    dat_new$cond <- ifelse(is.na(trial_list$catchesObserved[trial_n]), 1, ifelse(trial_list$catchesObserved[trial_n] == FALSE, 2, 3))
    dat_new$better <- ifelse(left > right, 1, 2)
    dat_new$max <- round(max(c(left, right)), digits = 2)
    dat_new$ratio <- round(min(c(left, right))/max(c(left, right)), digits = 2)
    
    if (counter == 1){
      dat <- dat_new
    } else {
      dat <- rbind(dat, dat_new)  
    }
    counter <- counter + 1
  }
}

#Order dataset
dat <- dat[order(dat$session, dat$trial),]

# Save in wide format
write.csv(dat, file = paste(resultsdir, 'data_wide.csv', sep = "/"))

# Create long format dataset
# Note: As far as I can tell, catches and switches are logged separately even
# for the same time step. This means that once you drop the event type, there may
# be some duplicate rows. (Those cases where a time step has a catch- and switch-
# log for some participant, but the participant at hand didn't catch or switch.)
dat.prelim = dat %>% mutate(row.num = 1:nrow(.)) %>% # add row number to provide uniqueness duplicate rows
  group_by(session, trial, time) %>%
  pivot_longer(2:11, names_to = 'player.metric', values_to = 'player.metric.val') %>%
  mutate(player.metric = substr(player.metric, 9, 30)) %>%
  separate(player.metric, into = c('player', 'metric'), sep = 1) %>%
  mutate(metric = substr(metric, 2, 30)) %>%
  pivot_wider(names_from = metric, values_from = player.metric.val) %>%
  rename('pos' = FishingPosition)

if(nrow(dat.prelim) == nrow(dat) * 5){  # check pivoting was correct
  dat = dat.prelim %>% select(-c(X, event_type, row.num)) %>% distinct()
}else{
  warning('Something may have gone wrong when pivoting to wide format')
}

#Compute additional info 

# Without grouping
dat = dat %>% 
  mutate(lake = ifelse(pos %in% c(0:4), 1, 2)) %>% # Determine lake
  mutate(correct = ifelse(lake != better, 0, 1)) # Determine if in correct lake

# With grouping
dat = dat %>%
  group_by(session, player) %>% arrange(trial, time) %>%
  mutate(catch = ifelse(trial == 1 & time == 0, score, score - lag(score))) %>% # Determine catches per time step from score that is carried across trials
  group_by(session, player, trial) %>% arrange(time) %>%
  mutate(entry = ifelse(time == 0, 0, ifelse(lake == lag(lake), 0, 1))) # Determine when individuals enter a new lake (entry because this is the matching timestamp)

# Determine patch residence times. When entering a new lake, compute difference
# in time to when entering the previous lake, and associate it with previous
# lake
# prt = dat %>%
#   group_by(session, trial, player) %>% arrange(time) %>%
#   mutate(switch.time = ifelse(time == 0 | time == max(time), time, 
#                               ifelse(lead(entry == 1), lead(time), NA))) %>%
#   drop_na(switch.time) %>%
#   mutate(prt = switch.time - lag(switch.time)) %>%
#   select(-switch.time)
# 
# dat = merge(dat, prt, by = intersect(names(dat), names(prt)), all = T)

# Save
write.csv(dat, file = paste(resultsdir, 'data_long.csv', sep = "/"))

# Conditions: 1 <-  catchesObserved == NA, 2 <-  catchesObserved == FALSE, 3 <-  catchesObserved == TRUE
# Better: 1 <- left > right, 2 <- left <= right
# max: highest p
# ratio: smaller yielding / larger yielding

# Create time series with 1 second resolution 
# Uniquely assign events to time steps

# Drop rows where nothing happens (no catch, no switch)
# Since a row is logged for all participants as soon as an event happens for
# at least one of the them, there may be rows in the data frame where nothing happens
# for certain players.
dat = dat %>% filter(time == 0 | catch == 1 | entry == 1)

# Note: Catches and switches are logged separately.
any(which(dat$entry == 1 & dat$catch == 1))

# Thus, when we transform the time in milliseconds to time in seconds there may
# be two events, a catch and a switch, occurring at the same time step. We can
# check how often this is the case by looking at how many different lakes players
# occupied during a time step
dat %>% 
  mutate(time.rounded = round(time / 1000)) %>%
  group_by(session, trial, player, time.rounded) %>%
  distinct(lake) %>% 
  count() %>% 
  filter(n > 1) %>%
  print(n = 100)

# Put this info into the data frame
dat = dat %>% 
  mutate(time.rounded = round(time / 1000)) %>%
  group_by(session, trial, player, time.rounded) %>%
  add_count(name = 'events.per.time') %>% ungroup() %>%
  arrange(session, trial, player, time)

any(dat$events.per.time > 1)
any(dat$events.per.time > 2)

# Since individuals can't switch back within a second after switching, if there are
# two events for a given time step, there are two possible scenarios:
# Either individuals switch into a new lake and then immediately get a reward.
# Or individuals get a reward and then immediately switch into a new lake.

# Handle these cases
dat$to.delete = FALSE # rows (not) to delete

for(i in 1:(nrow(dat) - 1)){
  
  if(dat[i, 'events.per.time'] == 2 & dat[i + 1, 'events.per.time'] == 2){ # Look at the first of the two events occurring at the same timestep
    
    if(dat[i, 'entry'] == 1){ # If switch, then catch 
      dat[i+1, 'entry'] = 1 # Move switch to the immediately following catch
      dat[i, 'to.delete'] = T # remove row
    }else if(dat[i, 'entry'] == 0){ # If catch, then switch
      if(dat[i+2, 'time.rounded'] - dat[i+1, 'time.rounded'] <= 1){ # if next time step is immediate, move switch forward in time
        dat[i+2, 'entry'] = 1
        dat[i+1, 'to.delete'] = T
      }else if(dat[i+2, 'time.rounded'] - dat[i+1, 'time.rounded'] > 1){ # if larger than 1 second difference, increment time step
        dat[i+1, 'time.rounded'] = dat[i+1, 'time.rounded'] + 1
      }
    }
    
    
    
  }
  
}

# Remove rows as specified
dat = dat %>% filter(to.delete==F)

# Check again for multiple events
dat %>%
  group_by(session, trial, player, time.rounded) %>%
  add_count(name = 'events.per.time') %>% ungroup() %>%
  arrange(session, trial, player, time) %>%
  filter(events.per.time > 1)

# Remove the glitch where there were three events per time step
dat = dat[-which(dat$session == 5 & dat$trial == 7 & dat$player == 1 & dat$time.rounded == 64 & dat$catch == 0), ] 


# Create Discrete Time Time-Series 
dat = dat %>% 
  mutate(ratio = as.character(ratio)) %>% mutate(max = as.character(max))

dat.discrete = dat %>% 
  group_by(session, trial, player) %>%
  expand(time.rounded = full_seq(0:unique(duration), 1)) %>%
  left_join(., dat, by = join_by(session, trial, player, time.rounded)) %>%
  fill(c(duration, cond, better, max, ratio), .direction = 'downup') %>% # Fill in conditions
  fill(c(pos, lake), .direction = 'down') %>% # Fill in the position and lake
  mutate(catch = ifelse(is.na(catch), 0, catch)) %>% # Fill in catches
  mutate(entry = ifelse(is.na(entry), 0, entry)) %>% # Fill in entry variable
  mutate(correct = ifelse(lake == better, 1, 0)) %>% # Fill in correctness of position
  select(c(session, trial, player, time.rounded, duration, cond, better, max, ratio, lake, correct, catch, entry)) %>%
  arrange(session, trial, player, time.rounded)

# Factorise experimental conditions
dat.discrete = dat.discrete %>% 
  mutate(social.fac = cond) %>%
  mutate(ratio.fac = ifelse(ratio == "0.5", 1,
                            ifelse(ratio == "0.65", 2, 
                                   ifelse(ratio == "0.8", 3, 
                                          4)))) %>%
  mutate(max.fac = ifelse(max == "0.5", 1,
                          ifelse(max == "0.7", 2, 3)))

# Save discrete timeseries
write.csv(dat.discrete, file = paste(resultsdir, 'data_discrete_1s.csv', sep = "/"))
