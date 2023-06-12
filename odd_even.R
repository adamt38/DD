#-----Compare coefficients between subsample
# Gym
load('gym_cvlasso0.RData')
load('gym_cvlasso1.RData')
coef_gym0      = lapply(gym_lasso0_true, function(u) matrix(u$coef_scaled))
coef_gym0      = do.call('cbind', coef_gym0)
coef_gym0      = t(coef_gym0)
coef_gym0      = coef_gym0[,-c(1)]
coef_gym0_f    = coef_gym0  # full coefficients
## replace coefficients with indicator of nonzero
coef_gym0      = apply(coef_gym0, 2, function(u) as.numeric(u!=0))
coef_gym0      = data.table(coef_gym0)
names(coef_gym0) = rownames(gym_lasso0_true[[1]]$best_coef)[-c(1)]
coef_gym0[,short_p_id:=unlist(lapply(gym_lasso0_true, function(u) u$short_p_id))]

coef_gym1      = lapply(gym_lasso1_true, function(u) matrix(u$coef_scaled))
coef_gym1      = do.call('cbind', coef_gym1)
coef_gym1      = t(coef_gym1)
coef_gym1      = coef_gym1[,-c(1)]
coef_gym1_f    = coef_gym1  # full coefficients
## replace coefficients with indicator of nonzero
coef_gym1      = apply(coef_gym1, 2, function(u) as.numeric(u!=0))
coef_gym1      = data.table(coef_gym1)
names(coef_gym1) = rownames(gym_lasso1_true[[1]]$best_coef)[-c(1)]
coef_gym1[,short_p_id:=unlist(lapply(gym_lasso1_true, function(u) u$short_p_id))]

## remove individuals with missing model for either 0 or 1
coef_gym0      = coef_gym0[short_p_id %in% coef_gym1$short_p_id]
coef_gym1      = coef_gym1[short_p_id %in% coef_gym0$short_p_id]
## check overlap

matches_11 = coef_gym0[,-c('short_p_id')]*coef_gym1[,-c('short_p_id')]
matches_00 = (1-coef_gym0[,-c('short_p_id')])*(1-coef_gym1[,-c('short_p_id')])
matches    = matches_00 + matches_11
matches    = apply(matches, 1, mean)
# compare with main model
coef_gym_c = coef_gym[short_p_id %in% coef_gym0$short_p_id]
coef_gym_c = coef_gym_c[,c(1:26)]
coef_gym_c = as.matrix(coef_gym_c)
coef_gym_c[,c(2:26)] = apply(coef_gym_c[,c(2:26)] %>% as.matrix, 2, function(u) as.numeric(u!=0))
coef_gym_c = data.table(coef_gym_c)
coef_gym_c = coef_gym_c[,c(2:26,1)]

matches_11_1 = coef_gym_c[,-c('short_p_id')]*coef_gym1[,-c('short_p_id')]
matches_00_1 = (1-coef_gym_c[,-c('short_p_id')])*(1-coef_gym1[,-c('short_p_id')])
matches_1    = matches_00_1 + matches_11_1
matches_1    = apply(matches_1, 1, mean)

matches_11_0 = coef_gym_c[,-c('short_p_id')]*coef_gym0[,-c('short_p_id')]
matches_00_0 = (1-coef_gym_c[,-c('short_p_id')])*(1-coef_gym0[,-c('short_p_id')])
matches_0    = matches_00_0 + matches_11_0
matches_0    = apply(matches_0, 1, mean)
sum_vec = function(vec, nsmall = 0){
  c(mean(vec) %>% round(nsmall) %>% format(nsmall = nsmall),
    quantile(vec, probs = .25)%>% round(nsmall) %>% format(nsmall = nsmall),
    median(vec) %>% round(nsmall) %>% format(nsmall = nsmall),
    quantile(vec, probs = .75)%>% round(nsmall) %>% format(nsmall = nsmall)
  )
}
summary_match = t(data.table(match01 = sum_vec(100*matches, nsmall = 0),
                             match0f = sum_vec(100*matches_0, nsmall = 0),
                             match1f = sum_vec(100*matches_1, nsmall = 0)))
stargazer(summary_match, summary = FALSE) ## table S6
# correlation between coefficieints
## get correlation matrices, INCLUDING THE INTERCEPTS
coef_gym0_f    = lapply(gym_lasso0_true, function(u) matrix(u$coef_scaled))
coef_gym0_f    = do.call('cbind', coef_gym0_f)
coef_gym0_f    = t(coef_gym0_f)
coef_gym0_f    = data.table(coef_gym0_f)
names(coef_gym0_f) = rownames(gym_lasso0_true[[1]]$best_coef)
coef_gym0_f[,short_p_id:=unlist(lapply(gym_lasso0_true, function(u) u$short_p_id))]

coef_gym1_f    = lapply(gym_lasso1_true, function(u) matrix(u$coef_scaled))
coef_gym1_f    = do.call('cbind', coef_gym1_f)
coef_gym1_f    = t(coef_gym1_f)
coef_gym1_f    = data.table(coef_gym1_f)
names(coef_gym1_f) = rownames(gym_lasso1_true[[1]]$best_coef)
coef_gym1_f[,short_p_id:=unlist(lapply(gym_lasso1_true, function(u) u$short_p_id))]

coef_gym0_f    = coef_gym0_f[short_p_id %in% coef_gym0$short_p_id]
coef_gym1_f    = coef_gym1_f[short_p_id %in% coef_gym1$short_p_id]

coef_01_f      = cbind(coef_gym0_f[,-c('short_p_id')],
                       coef_gym1_f[,-c('short_p_id')]) %>% as.matrix()
cor_coef_01    = apply(coef_01_f, 1, function(u) cor(u[1:26], u[27:52]))
hist(cor_coef_01)
## FIGURE S1a
ggplot(data.table(corr = cor_coef_01)) + geom_histogram(aes(x = corr), bins = 50, fill = 'salmon', colour = 'black') +
  theme_bw() + labs(x = 'Correlation', y = 'Frequency') 

## data size for sample
dt_size = data.table(n = unlist(lapply(gym_lasso0_true, function(u) length(u$insample_pred))),
                     short_p_id = unlist(lapply(gym_lasso0_true, function(u) u$short_p_id)))
dt_size = dt_size[short_p_id %in% coef_gym0$short_p_id]
dt_size[,match_perc:=matches]
dt_size[,match0_perc:=matches_0]
dt_size[,match1_perc:=matches_1]
dt_size[,med_match:=as.numeric(match_perc>=median(match_perc))]
dt_size[,med_match0:=as.numeric(match0_perc>=median(match0_perc))]
dt_size[,med_match1:=as.numeric(match1_perc>=median(match1_perc))]
dt_size[,mean(n), med_match]
dt_size[,mean(n), med_match0]
dt_size[,mean(n), med_match1]


# hand-washing
#-----Compare coefficients between subsample
# hand
load('hand_cvlasso0.RData')
load('hand_cvlasso1.RData')
coef_hand0      = lapply(hand_lasso0_true, function(u) reform_coef(u$coef_scaled, model = hand_lasso0_true[[1]]))
coef_hand0      = do.call('cbind', coef_hand0)
coef_hand0      = t(coef_hand0)
coef_hand0_f    = coef_hand0  # full coefficients
coef_hand0      = coef_hand0[,-c(1)]

## replace coefficients with indicator of nonzero
coef_hand0      = apply(coef_hand0, 2, function(u) as.numeric(u!=0))
coef_hand0      = data.table(coef_hand0)
names(coef_hand0) = rownames(hand_lasso0_true[[1]]$best_coef)[-c(1)]
coef_hand0[,hospital_tag:=unlist(lapply(hand_lasso0_true, function(u) u$hospital_tag))]

coef_hand1      = lapply(hand_lasso1_true, function(u) reform_coef(u$coef_scaled, model = hand_lasso1_true[[1]]))
coef_hand1      = do.call('cbind', coef_hand1)
coef_hand1      = t(coef_hand1)
coef_hand1_f    = coef_hand1  # full coefficients
coef_hand1      = coef_hand1[,-c(1)]

## replace coefficients with indicator of nonzero
coef_hand1      = apply(coef_hand1, 2, function(u) as.numeric(u!=0))
coef_hand1      = data.table(coef_hand1)
names(coef_hand1) = rownames(hand_lasso1_true[[1]]$best_coef)[-c(1)]
coef_hand1[,hospital_tag:=unlist(lapply(hand_lasso1_true, function(u) u$hospital_tag))]

## remove individuals with missing model for either 0 or 1
coef_hand0      = coef_hand0[hospital_tag %in% coef_hand1$hospital_tag]
coef_hand1      = coef_hand1[hospital_tag %in% coef_hand0$hospital_tag]
## check overlap

matches_11 = coef_hand0[,-c('hospital_tag')]*coef_hand1[,-c('hospital_tag')]
matches_00 = (1-coef_hand0[,-c('hospital_tag')])*(1-coef_hand1[,-c('hospital_tag')])
matches    = matches_00 + matches_11
matches    = apply(matches, 1, mean)
# compare with main model
coef_hand_c = coef_hand[hospital_tag %in% coef_hand0$hospital_tag]
coef_hand_c = coef_hand_c[,c(1:45)]
coef_hand_c = as.matrix(coef_hand_c)
coef_hand_c[,c(2:45)] = apply(coef_hand_c[,c(2:45)] %>% as.matrix, 2, function(u) as.numeric(u!=0))
coef_hand_c = data.table(coef_hand_c)
coef_hand_c = coef_hand_c[,c(2:45,1)]

matches_11_1 = coef_hand_c[,-c('hospital_tag')]*coef_hand1[,-c('hospital_tag')]
matches_00_1 = (1-coef_hand_c[,-c('hospital_tag')])*(1-coef_hand1[,-c('hospital_tag')])
matches_1    = matches_00_1 + matches_11_1
matches_1    = apply(matches_1, 1, mean)

matches_11_0 = coef_hand_c[,-c('hospital_tag')]*coef_hand0[,-c('hospital_tag')]
matches_00_0 = (1-coef_hand_c[,-c('hospital_tag')])*(1-coef_hand0[,-c('hospital_tag')])
matches_0    = matches_00_0 + matches_11_0
matches_0    = apply(matches_0, 1, mean)
sum_vec = function(vec, nsmall = 0){
  c(mean(vec) %>% round(nsmall) %>% format(nsmall = nsmall),
    quantile(vec, probs = .25)%>% round(nsmall) %>% format(nsmall = nsmall),
    median(vec) %>% round(nsmall) %>% format(nsmall = nsmall),
    quantile(vec, probs = .75)%>% round(nsmall) %>% format(nsmall = nsmall)
  )
}
summary_match = t(data.table(match01 = sum_vec(100*matches, nsmall = 0),
                             match0f = sum_vec(100*matches_0, nsmall = 0),
                             match1f = sum_vec(100*matches_1, nsmall = 0)))
stargazer(summary_match, summary = FALSE)
# correlation between coefficieints
## get correlation matrices, INCLUDING THE INTERCEPTS
coef_hand0_f    = data.table(coef_hand0_f)
names(coef_hand0_f) = rownames(hand_lasso0_true[[1]]$best_coef)
coef_hand0_f[,hospital_tag:=unlist(lapply(hand_lasso0_true, function(u) u$hospital_tag))]

coef_hand1_f    = data.table(coef_hand1_f)
names(coef_hand1_f) = rownames(hand_lasso1_true[[1]]$best_coef)
coef_hand1_f[,hospital_tag:=unlist(lapply(hand_lasso1_true, function(u) u$hospital_tag))]

coef_hand0_f    = coef_hand0_f[hospital_tag %in% coef_hand0$hospital_tag]
coef_hand1_f    = coef_hand1_f[hospital_tag %in% coef_hand1$hospital_tag]

coef_01_f      = cbind(coef_hand0_f[,-c('hospital_tag')],
                       coef_hand1_f[,-c('hospital_tag')]) %>% as.matrix()
cor_coef_01    = apply(coef_01_f, 1, function(u) cor(u[1:45], u[46:90]))
hist(cor_coef_01)

## FIGURE S1b
ggplot(data.table(corr = cor_coef_01)) + geom_histogram(aes(x = corr), bins = 50, fill = 'salmon', colour = 'black') +
  theme_bw() + labs(x = 'Correlation', y = 'Frequency')

## data size for sample
dt_size = data.table(n = unlist(lapply(hand_lasso0_true, function(u) length(u$insample_pred))),
                     hospital_tag = unlist(lapply(hand_lasso0_true, function(u) u$hospital_tag)))
dt_size = dt_size[hospital_tag %in% coef_hand0$hospital_tag]
dt_size[,match_perc:=matches]
dt_size[,match0_perc:=matches_0]
dt_size[,match1_perc:=matches_1]
dt_size[,med_match:=as.numeric(match_perc>=median(match_perc))]
dt_size[,med_match0:=as.numeric(match0_perc>=median(match0_perc))]
dt_size[,med_match1:=as.numeric(match1_perc>=median(match1_perc))]
dt_size[,mean(n), med_match]
dt_size[,mean(n), med_match0]
dt_size[,mean(n), med_match1]
