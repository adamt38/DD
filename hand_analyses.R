#-----Load necessary libraries and data-----:
source('libraries.R')
source('hand_functions.R')
load('Data/hand_data.RData')
##NOTE: change those to the appropriate directories

p_list    = as.list(unique(hand_data$hospital_tag))  ## Create list of individuals

#======Individual LASSO models======:
## hand
hand_cvlasso = lapply(p_list, function(p) 
  tryCatch(hand_indiv_lasso(data = hand_data[hospital_tag == p], normalize = TRUE),
           error = function(e) NULL))
## Remove untrainable individuals
hand_lasso_true = lapply(hand_cvlasso, function(u) !is.null(u))   
hand_lasso_true = hand_cvlasso[which(unlist(hand_lasso_true))]
# save(hand_lasso_true, file = 'hand_lasso.RData')

#======Calculate AUC sequences for each decile======:
load('Data/hand_coef.RData')
N_hand = N_hand[hospital_tag %in% coef_hand$hospital_tag]
N_hand[,decile_N:=ntile(N_hand$N, 10)]
hand_data = merge(hand_data, N_hand[,c('hospital_tag', 'decile_N')], by = c('hospital_tag'))

## Calculate AUC sequences and perform exponential fit
aucs_hand      = lapply(as.list(c(1:10)), function(dec)
  hand_indiv_auc_seq(dec))
aucs_hand      = lapply(aucs_hand, function(u) c(.5, .5, u))
hand_exp_fit   = lapply(aucs_hand, auc_exp_fit, increment = 2)

speed_habit_hand = data.table(deciles = c(1:10),
                             a = unlist(lapply(hand_exp_fit, function(u) u$a)),
                             b = unlist(lapply(hand_exp_fit, function(u) u$b)),
                             c = unlist(lapply(hand_exp_fit, function(u) u$c)),
                             Rsq = unlist(lapply(hand_exp_fit, function(u) u$Rsq)))
speed_habit_hand[,tstar:=-log(a/(20*b))/c]

## Create data to plot habit formation curves
plot_habit_hand = data.table(
  auc     = unlist(aucs_hand),
  deciles = rep(c(1:10), unlist(lapply(aucs_hand, function(u) length(u))))
)
plot_habit_hand = merge(plot_habit_hand, speed_habit_hand, by = c('deciles'))
plot_habit_hand[,t:=2*c(1:.N), deciles]
plot_habit_hand[,fit:=a-b*exp(-c*t)]
plot_habit_hand[,tstar:=-log(a/(20*b))/c]

### plot curves for all deciles
ggplot(plot_habit_hand, aes(x = t)) + geom_point(aes(y = auc)) + 
  geom_line(aes(y = fit, colour = 'Fitted exp. curve'), size = 1.2) +
  theme_bw() + labs(x = 'Days since first hand visit', y = 'AUC') +
  geom_vline(aes(xintercept = tstar, colour = 'Time to habit formation')) +
  facet_wrap(deciles~., nrow = 5) +
  scale_colour_manual('', values = c('salmon', 'blue')) +
  scale_x_discrete(limits = c(0,200,500,1000,2000, 4000)) +
  xlim(c(0,2000)) +
  theme(legend.position = 'bottom')

### plot curves for only 2nd and 9th deciles  ## FIGURE 3
plot_29 = rbind(plot_habit_hand[deciles==2],
                plot_habit_hand[deciles==9])
plot_29[,dec_lab:='2nd decile']
plot_29[deciles==9,dec_lab:='9th decile']
ggplot(plot_29, aes(x = t)) + geom_point(aes(y = auc)) + 
  geom_line(aes(y = fit, colour = 'Fitted exp. curve'), size = 1.2) +
  theme_bw() + labs(x = 'Days since first hand visit', y = 'AUC') +
  geom_vline(aes(xintercept = tstar, colour = 'Time to habit formation')) +
  facet_grid(.~dec_lab, scales = 'free_x') +
  theme(legend.position = 'bottom') +
  scale_colour_manual('', values = c('salmon', 'blue')) ## save 3x6 landscape

stargazer(cbind(speed_habit_hand, speed_habit_hand[,-1]), 
          summary = FALSE, rownames = FALSE)


#-----Check stability of correlated coefficients-----: ## TABLE S6
cor1 = hand_coef_cor('experience', 'lag_shift_compliance')
cor2 = hand_coef_cor('epi_id', 'lag_shift_compliance')
cor3 = hand_coef_cor('opposite_compliant', 'lag_compliant')

#-----Different LASSO for odd and even samples------   ## FIG S1 & TABLE S7
hand_data[,s:=shift7id%%2, hospital_tag]
hand_data_0 = hand_data[s==0]
hand_data_1 = hand_data[s==1]
rm(hand_data)
p_list      = coef_hand$hospital_tag

hand_cvlasso_0 = lapply(p_list, 
                        function(p) tryCatch(hand_indiv_lasso(data = hand_data_0[hospital_tag == p], type = 'all'), 
                                             error = function(e) NULL))
hand_cvlasso_0 = lapply(p_list, function(p) 
  tryCatch(hand_indiv_cvlasso(data = hand_data_0[hospital_tag == p], normalize = TRUE),
           error = function(e) NULL))
hand_lasso0_true = lapply(hand_cvlasso_0, function(u) !is.null(u))   
hand_lasso0_true = hand_cvlasso_0[which(unlist(hand_lasso0_true))]
#save(hand_lass0_true, file = 'hand_lasso0.RData')


hand_cvlasso_1 = lapply(p_list, 
                        function(p) tryCatch(hand_indiv_lasso(data = hand_data_1[hospital_tag == p], type = 'all'), 
                                             error = function(e) NULL))
hand_lasso1_true = lapply(hand_cvlasso_1, function(u) !is.null(u))   
hand_lasso1_true = hand_cvlasso_1[which(unlist(hand_lasso1_true))]
#save(hand_lass1_true, file = 'hand_lasso1.RData')

#-----New reward insensitivity analyses (07/2022)-----:

last_ep = hand_data[,c(1, 2, 6, 9, 11, 18, 24, 26:38, 40, 41, 42, 43, 45)]
rm(hand_data)
last_ep = merge(last_ep, N_hand[,c('hospital_tag', 'decile_N')], by =c('hospital_tag'))
setnames(last_ep, 'decile_N', 'deciles')
last_ep = merge(last_ep, speed_habit_hand[,c('deciles', 'tstar')], 
                by = c('deciles'))
last_ep[,post_habit:=as.numeric(shift7id>tstar)]
last_ep[,pre_habit:=as.numeric(shift7id<=tstar)]

hand_rr_reg = felm(compliant~epi_id  + time_elapsed + lag_compliant + time_since_last_opp + time_since_last_compliant + I(time_since_last_compliant^2) +
                     patient_encounter*time_elapsed + opposite_compliant + factor(hour_slot) + time_elapsed:factor(hour_slot) + 
                     prev_unit_freq + unit_prev_freq + prev_dow_freq + dow_prev_freq + 
                     prev_loc_freq + loc_prev_freq +  others_mean_in_loc + experience +
                     lag_compliant:time_since_last_opp + entry_indicator*lag_compliant + 
                     I(time_since_last_opp^2) + lag_compliant:I(time_since_last_opp^2) +
                     time_off + I(time_off^2) + streak +
                     lag_shift_compliance + as.factor(month)  +
                     is_last_epi + 
                     is_last_epi*post_habit|hospital_tag|0|hospital_tag, 
                   data = last_ep)
stargazer(hand_rr_reg) ## TABLE S11