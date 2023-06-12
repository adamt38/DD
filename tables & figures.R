source('libraries.R')

#-----Summary statistics-----:
## Gym
load('Data/gym_data.RData')
demo    = data.table(read.csv('Data/gym_demo_auc.csv'))
su_gym  = gym_data[,.(mean_att = mean(attended),
                           N_obs = .N,
                           mean_time_lag = sum(attended*time_lag, na.rm = TRUE)/sum(attended)), short_p_id]
su_gym_demo = demo[,.(gender = gender[1],
                      age = min(age)), short_p_id]
su_gym   = merge(su_gym, su_gym_demo, by = c('short_p_id'))
su_gym[,female:=as.numeric(gender=='F')]
su_gym   = data.table(su_gym[,-c("short_p_id", "gender")][,.(Mean   = unlist(lapply(.SD, function(x) mean(x, na.rm = TRUE))),
                             SD     = unlist(lapply(.SD, function(x) sd(x, na.rm = TRUE))),
                             Q25    = unlist(lapply(.SD, function(x) as.numeric(quantile(x, probs = 0.25, na.rm = TRUE)))),
                             Median = unlist(lapply(.SD, function(x) median(x, na.rm = TRUE))),
                             Q75    = unlist(lapply(.SD, function(x) as.numeric(quantile(x, probs = 0.75, na.rm = TRUE)))))])
su_gym[,Variable:=c('Avg. daily attendance', 'Number of days observed', 'Avg. days between gym visits', 'Age', 'Female')]
write.csv(su_gym[,c(6,1:5)], file = 'tables & figures/su_gym.csv')
stargazer(su_gym[c(4,5,1,2,3),c(6,1:5)], rownames = FALSE, summary = FALSE, digits = 2)
## Hand:
load('Data/hand_data.RData')
su_shift = data[entry_indicator == 1,.(epi_per_shift = no_epi[1],
                                         shift_length = shift_length[1],
                                         avg_switch_room = mean(time_since_last_opp),
                                         time_off = mean(time_off)), list(hospital_tag, shift7id)]
su_shift = su_shift[,.(epi_per_shift = mean(epi_per_shift),
                         shift_length = mean(shift_length),
                         avg_switch_room = mean(avg_switch_room),
                         time_off = mean(time_off)/60), hospital_tag]
su_id    = data[,.(avg_compliance = mean(compliant), 
                     total_shifts = max(shift7id),
                     total_locations = uniqueN(location),
                     total_hospitals = uniqueN(unit_id),
                     avg_epilength = mean(epilength),
                     patient_enc   = mean(patient_encounter),
                     experience = experience[1]), hospital_tag]
su_table = merge(su_id, su_shift, by = c("hospital_tag"))
su_all   = su_table
su_table[,shift_length:=as.numeric(shift_length)]
su_table = su_table[,-c("hospital_tag")][,.(Mean = lapply(.SD, function(x) round(mean(x), 2)),
                                              SD = lapply(.SD, function(x) round(sd(x), 2)),
                                              # Q10 = lapply(.SD, function(x) round(as.numeric(quantile(x, probs = 0.1)), 3)),
                                              Q25 = lapply(.SD, function(x) round(as.numeric(quantile(x, probs = 0.25)), 2)),
                                              Median = lapply(.SD, function(x) round(median(x), 2)),
                                              Q75 = lapply(.SD, function(x) round(as.numeric(quantile(x, probs = 0.75)), 2))
                                              # Q90 = lapply(.SD, function(x) round(as.numeric(quantile(x, probs = 0.9)), 3)),
                                              # Min = lapply(.SD, function(x) round(min(x), 3)), Max = lapply(.SD, function(x) round(max(x), 3)))
  )]
su_table[,Variable:=c("Compliance", "Total number of shifts", "Number of visited rooms", "Number of hospital units", "Avg. episode length (mins)",
                        "Avg. freq. of patient encounter", "Days worked at badge date", "Avg. number of episodes per shift", "Avg. shift length (mins)",
                        "Avg time between episodes (mins)", "Avg time off between shifts (hours)")]
su_table = su_table[,c(6,1:5)]
#-----Important variables tables-----: 
## Gym: ## TABLE S2
load('Data/gym_coef.RData')
imp_coef_gym     = coef_gym[,c(2:26)][,lapply(.SD, function(x) round(mean(abs(x)), 2))]                                                     
median_coef_gym  = coef_gym[,c(2:26)][,lapply(.SD, function(x) round(median(x), 2))]                                                 
Q25_coef_gym     = coef_gym[,c(2:26)][,lapply(.SD, function(x) round(quantile(x, probs = 0.25), 2))]                                                 
Q75_coef_gym     = coef_gym[,c(2:26)][,lapply(.SD, function(x) round(quantile(x, probs = 0.75), 2))]                                                 
zero_coef_gym    = coef_gym[,c(2:26)][,lapply(.SD, function(x) round(100*mean(as.numeric(x==0)), 0))]
pos_coef_gym     = coef_gym[,c(2:26)][,lapply(.SD, function(x) round(100*mean(as.numeric(x>0)), 0))]
neg_coef_gym     = coef_gym[,c(2:26)][,lapply(.SD, function(x) round(100*mean(as.numeric(x<0)), 0))]
var_table_gym    = cbind(t(imp_coef_gym), t(Q25_coef_gym), t(median_coef_gym),  t(Q75_coef_gym),
                          t(zero_coef_gym), t(pos_coef_gym), t(neg_coef_gym))
match_names_gym  = data.table(
  original = rownames(var_table_gym),
  new = c("Streak", "Streak^2", "Day-of-week streak", "(Day-of-week streak)^2",
          "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November",
          "December", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Time lag", "Attendance last 7 days", "Time lag^2")
)
rownames(var_table_gym) = match_names_gym$new
var_table_gym           = cbind(var_table_gym, abs(var_table_gym[,6]- var_table_gym[,7]))
colnames(var_table_gym) = c('Importance', 'Q1', 'Median', 'Q3', '% zero', '% positive', '% negative', 'Homogeneity index')
var_table_gym   = data.table(var_table_gym)
var_table_gym[,Variable:=match_names_gym$new]
var_table_gym   = var_table_gym[,c(9, 1:8)]
stargazer(var_table_gym[order(-Importance)], summary = FALSE, rownames = FALSE, digits = 2)
## Hand: ## TABLE S3
load('Data/hand_coef.RData')
imp_coef_hand     = coef_hand[,c(2:45)][,lapply(.SD, function(x) round(mean(abs(x)), 2))]                                                     
median_coef_hand  = coef_hand[,c(2:45)][,lapply(.SD, function(x) round(median(x), 2))]                                                 
Q25_coef_hand     = coef_hand[,c(2:45)][,lapply(.SD, function(x) round(quantile(x, probs = 0.44), 2))]                                                 
Q75_coef_hand     = coef_hand[,c(2:45)][,lapply(.SD, function(x) round(quantile(x, probs = 0.75), 2))]                                                 
zero_coef_hand    = coef_hand[,c(2:45)][,lapply(.SD, function(x) round(100*mean(as.numeric(x==0)), 0))]
pos_coef_hand     = coef_hand[,c(2:45)][,lapply(.SD, function(x) round(100*mean(as.numeric(x>0)), 0))]
neg_coef_hand     = coef_hand[,c(2:45)][,lapply(.SD, function(x) round(100*mean(as.numeric(x<0)), 0))]
var_table_hand    = cbind(t(imp_coef_hand), t(Q25_coef_hand), t(median_coef_hand),  t(Q75_coef_hand),
                          t(zero_coef_hand), t(pos_coef_hand), t(neg_coef_hand))

hand_rownames     = data.table(code_names = rownames(hand_lasso[[1]]$coef_scaled)[-1],
  real_names = c("Rooms visited since shift start", "Time at work", "Compliance last opp.", "Time since last opp.", "Time since last compliance",
                        "Time since last compliance^2",
                        "Frequency of patient encounter", "Compliance within episode", "12am-6am", "6am-12pm", "12pm-6pm", "6pm-12am",
                        "Prev. unit compliance", "Unit frequency", "Prev. day-of-week compliance", "Day-of-week frequency",
                        "Prev. room compliance", "Room frequency", "Room compliance of others", "Days since start", "Entry indicator", "Time since last opp.^2", 
                        "Time off", "Time off^2", "Streak", "Compliance last shift",
                        "February", "March", "April", "May", "June", "July", "August", "September", "October", "November",
                        "December", "Time at work x Patient encounter", "Time at work x 6am-12pm", "Time at work x 12pm-6pm",
                        "Time at work x 6pm-12am", "Compliance last opp. x Time since last opp", "Compliance last opp. x Entry indicator", "Compliance last opp. x Time since last opp^2")
)
match_names_hand = hand_rownames[order(code_names)]
rownames(var_table_hand) = match_names_hand$real_names
var_table_hand           = cbind(var_table_hand, abs(var_table_hand[,6]- var_table_hand[,7]))
colnames(var_table_hand) = c('Importance', 'Q25', 'Median', 'Q75', '% zero', '% positive', '% negative', 'Homogeneity index')
var_table_hand   = data.table(var_table_hand)
var_table_hand[,Variable:=match_names_hand$real_names]
var_table_hand   = var_table_hand[,c(9, 1:8)]
stargazer(var_table_hand[order(-Importance)], summary = FALSE, rownames = FALSE, digits = 2)

#-----AUC vs freq------: ## FIGURE S2
ggplot(coef_gym) + geom_point(aes(x = auc_holdout, y = att_rate), colour = 'black', fill = 'salmon', pch = 21) + theme_bw() +
  labs(x = 'Holdout AUC', y = 'Frequency') ## save pdf 4x4
ggplot(coef_hand) + geom_point(aes(x = auc_holdout, y = compliant_rate), colour = 'black', fill = 'salmon', pch = 21) + theme_bw() +
  labs(x = 'Holdout AUC', y = 'Frequency') ## save pdf 4x4

#-----Summary of Asymptotic Curves----- ## TABLE S8
stargazer(cbind(speed_habit_gym, speed_habit_hand[,-1]), 
          summary = FALSE, rownames = FALSE)