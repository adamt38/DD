#-----Load necessary libraries and data-----:
source('libraries.R')
source('gym_functions.R')
load('Data/gym_data.RData') 
##NOTE: change those to the appropriate directories

p_list    = as.list(unique(gym_data$short_p_id))  ## Create list of individuals

#======Individual LASSO models======:
## GYM
gym_cvlasso = lapply(p_list, function(p) 
  tryCatch(gym_indiv_cvlasso(data = gym_data[short_p_id == p], normalize = TRUE),
           error = function(e) NULL))
## Remove untrainable individuals
gym_lasso_true = lapply(gym_cvlasso, function(u) !is.null(u))   
gym_lasso_true = gym_cvlasso[which(unlist(gym_lasso_true))]
# save(gym_lasso_true, file = 'gym_lasso.RData')

#======Calculate AUC sequences for each decile======:
load('Data/gym_coef.RData')
N_gym = N_gym[short_p_id %in% coef_gym$short_p_id]
N_gym[,decile_N:=ntile(N_gym$N, 10)]
gym_data = merge(gym_data, N_gym[,c('short_p_id', 'decile_N')], by = c('short_p_id'))

## Calculate AUC sequences and perform exponential fit
aucs_gym      = lapply(as.list(c(1:10)), function(dec)
  gym_indiv_auc_seq(dec))
aucs_gym      = lapply(aucs_gym, function(u) c(.5, .5, u))
gym_exp_fit   = lapply(aucs_gym, auc_exp_fit, increment = 14)

speed_habit_gym = data.table(deciles = c(1:10),
                             a = unlist(lapply(gym_exp_fit, function(u) u$a)),
                             b = unlist(lapply(gym_exp_fit, function(u) u$b)),
                             c = unlist(lapply(gym_exp_fit, function(u) u$c)),
                             Rsq = unlist(lapply(gym_exp_fit, function(u) u$Rsq)))
speed_habit_gym[,tstar:=-log(a/(20*b))/c]

## Create data to plot habit formation curves
plot_habit_gym = data.table(
  auc = unlist(aucs_gym),
  deciles = rep(c(1:10), unlist(lapply(aucs_gym, function(u) length(u))))
)
plot_habit_gym = merge(plot_habit_gym, speed_habit_gym, by = c('deciles'))
plot_habit_gym[,t:=14*c(1:.N), deciles]
plot_habit_gym[,fit:=a-b*exp(-c*t)]
plot_habit_gym[,tstar:=-log(a/(20*b))/c]

### plot curves for all deciles
ggplot(plot_habit_gym, aes(x = t)) + geom_point(aes(y = auc)) + 
  geom_line(aes(y = fit, colour = 'Fitted exp. curve'), size = 1.2) +
  theme_bw() + labs(x = 'Days since first gym visit', y = 'AUC') +
  geom_vline(aes(xintercept = tstar, colour = 'Time to habit formation')) +
  facet_wrap(deciles~., nrow = 5) +
  scale_colour_manual('', values = c('salmon', 'blue')) +
  scale_x_discrete(limits = c(0,200,500,1000,2000, 4000)) +
  xlim(c(0,2000)) +
  theme(legend.position = 'bottom')

### plot curves for only 2nd and 9th deciles  ## FIGURE 2
plot_29 = rbind(plot_habit_gym[deciles==2],
                plot_habit_gym[deciles==9])
plot_29[,dec_lab:='2nd decile']
plot_29[deciles==9,dec_lab:='9th decile']
ggplot(plot_29, aes(x = t)) + geom_point(aes(y = auc)) + 
  geom_line(aes(y = fit, colour = 'Fitted exp. curve'), size = 1.2) +
  theme_bw() + labs(x = 'Days since first gym visit', y = 'AUC') +
  geom_vline(aes(xintercept = tstar, colour = 'Time to habit formation')) +
  facet_grid(.~dec_lab, scales = 'free_x') +
  theme(legend.position = 'bottom') +
  scale_colour_manual('', values = c('salmon', 'blue')) ## save 3x6 landscape

#-----Check stability of correlated coefficients-----: ## TABLE S5
cor1 = gym_coef_cor('time_lag', 'last7days_attendance')
cor2 = gym_coef_cor('streak', 'last7days_attendance')

#-----Different LASSO for odd and even samples------   ## FIG S1 & TABLE S7
gym_data[,t:=1:.N,short_p_id]
gym_data[,s:=t%%2, short_p_id]
gym_data_0 = gym_data[s==0]
gym_data_1 = gym_data[s==1]
rm(gym_data)
p_list      = coef_gym$short_p_id

gym_cvlasso_0 = lapply(p_list, 
                       function(p) tryCatch(gym_indiv_lasso(data = gym_data_0[short_p_id == p], type = 'all'), 
                                            error = function(e) NULL))
gym_cvlasso_0 = lapply(p_list, function(p) 
  tryCatch(gym_indiv_cvlasso(data = gym_data_0[short_p_id == p], normalize = TRUE),
           error = function(e) NULL))
gym_lasso0_true = lapply(gym_cvlasso_0, function(u) !is.null(u))   
gym_lasso0_true = gym_cvlasso_0[which(unlist(gym_lasso0_true))]
#save(gym_lass0_true, file = 'gym_lasso0.RData')


gym_cvlasso_1 = lapply(p_list, 
                       function(p) tryCatch(gym_indiv_lasso(data = gym_data_1[short_p_id == p], type = 'all'), 
                                            error = function(e) NULL))
gym_lasso1_true = lapply(gym_cvlasso_1, function(u) !is.null(u))   
gym_lasso1_true = gym_cvlasso_1[which(unlist(gym_lasso1_true))]
#save(gym_lass1_true, file = 'gym_lasso1.RData')


#-----New reward insensitivity analyses (07/2022)-----:
load('weather.RData') #load gym data with weather
weather = merge(weather, N_gym, by = c('short_p_id'))
setnames(weather, 'decile_N', 'deciles')
weather = merge(weather, speed_habit_gym[,c('deciles', 'tstar')], 
                by = c('deciles'))
weather[,time:=1:.N, short_p_id]
weather[,pre_habit:=as.numeric(time<=tstar)]
weather[,post_habit:=as.numeric(time>tstar)]

gym_rr_reg = felm(attended ~  1 + streak + 
                I(streak^2) + 
                streak_dow + 
                I(streak_dow^2) + 
                factor(month) +
                factor(dow) + time_lag  + last7days_attendance + 
                good_weather + good_weather*post_habit + 
                bad_weather + bad_weather*post_habit +
                I(time_lag^2)|short_p_id|0|short_p_id, 
              data = weather)
stargazer(gym_rr_reg) ## TABLE S10

#-----StepUp Analysis----- 
stepup_dt = data.table(read.csv(
  '../pptdata.csv' # change to appropriate directory
))
load('gym_coef.RData')
load('gym_participant_id.RData')
su_att_data = stepup_dt[week>=-4]
su_att_data[,intervention:=as.numeric(week %in% c(1,2,3,4))]
su_att_data = su_att_data[week <=4]
su_att_data = su_att_data[,.(mean_attended = mean(visits)), list(participant_id, intervention)]
su_att_data = merge(su_att_data, p_id[,c('participant_id', 'short_p_id')], by = c('participant_id'))
su_att_data = su_att_data[,-c('participant_id')]
su_att_data = dcast(su_att_data, short_p_id ~ intervention, value.var = 'mean_attended')
colnames(su_att_data) = c('short_p_id', 'pre_int', 'int')
su_att_data[is.na(pre_int), pre_int:=0]
su_att_data[,diff:=int-pre_int]
su_att_data = merge(su_att_data, coef_gym[,c('short_p_id', 'auc_holdout')])
cor(su_att_data$diff, su_att_data$auc_holdout)
## FIGURE S5a
plot(su_att_data$auc_holdout, su_att_data$diff, pch =20, xlab = 'Pre-intervention AUC', 
     ylab = 'Difference in gym visits between intervention and pre-intervention')
## TABLE S13
summary(lm(diff ~ auc_holdout, data =su_att_data))
summary(lm(abs(diff) ~ auc_holdout, data =su_att_data))
summary(glm(I(as.numeric(diff>0)) ~ auc_holdout, data =su_att_data, family = binomial(link = 'logit')))
su_att_data[,positive_ic:=as.numeric(diff>0)]
pos_ic_reg = glm(I(as.numeric(diff>0)) ~ auc_holdout, data =su_att_data, family = binomial(link = 'logit'))

## correlations reported in Sec 4.2
cor.test(su_att_data$diff, su_att_data$auc_holdout)
cor.test(abs(su_att_data$diff), su_att_data$auc_holdout)
cor.test(su_att_data$positive_ic, su_att_data$auc_holdout)

## plot relationship between AUC and P(positive IC)
ggplot(su_att_data, aes(x = auc_holdout)) + geom_histogram(aes(fill = factor(positive_ic)), alpha = 0.5, position = 'identity') + 
  theme_bw() + scale_fill_manual('', values = c('steelblue', 'salmon'), labels = c('Non-positive IC', 'Positive IC')) +
  labs(x = 'AUC', y = '')
su_att_data[,pred_pos_ic:=predict(pos_ic_reg, type = 'response')]
su_att_data[,pred_ic:=predict(pos_ic_reg)]

##FIGURE S5b
ggplot(su_att_data, aes(x = auc_holdout, y = positive_ic)) + theme_bw() +  
  geom_smooth(method = 'lm', se = TRUE, color = 'red') +
  labs(x = 'AUC', y = 'Predicted probability of positive IC')
