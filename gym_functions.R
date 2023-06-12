#-------------------------------------------------------------
#-------------------------INDIVIDUAL LASSO--------------------
#-------------------------------------------------------------
gym_indiv_cvlasso = function(data, normalize = TRUE){
  ## THIS FUNCTION TAKES AN INDIVIDUAL TIME SERIES DATA, SPLIT THE DATA INTO TRAIN AND TEST SET AND FIT THE LASSO MODEL TO THE TRAIN SET
  ##-----INPUT-----
  # data          : individual time series (include both train and test data)
  # normalize     : whether to scale the LASSO coefficients (default is TRUE)

  ##-----OUTPUT-----
  # full_model    : full LASSO model, can be used to obtain prediction, calculate AUC in a test data
  # best_lambda   : the best lambda obtained from cross-validation procedure
  # best_coef     : LASSO coefficients using best_lambda
  # auc_holdout   : AUC on the test set
  # auc_train     : AUC on the train set
  # holdout_y     : vector of outcome variables on the test set
  # holdout_pred  : vector of predictions made on the test set
  # insample_pred : vector of predictions made on the train set
  # short_p_id    : ID of the corresponding individual
  # X_full        : full matrix of covariates in the train data
  # X_holdout     : full matrix of covariates in the test data
  data          = data[!is.na(time_lag)]
  holdout_data  = data[folds == 6]
  train_data    = data[folds <  6]
  X = model.matrix(attended ~ streak + 
                     I(streak^2) + 
                     streak_dow + 
                     I(streak_dow^2) + 
                     factor(month) +
                     factor(dow) + time_lag  + last7days_attendance +
                     I(time_lag^2) - 1, 
                   data = data)
  X_full    = X[data[folds < 6, which = TRUE],]
  X_holdout = X[data[folds == 6, which = TRUE],]
  full_model     = cv.glmnet(x = X_full, y = train_data$attended, type.measure = 'auc', nfolds = 5, foldid = train_data$folds,
                             family = 'binomial', standardize = normalize)
  best_lambda   = full_model$lambda.min
  best_coef     = coef(full_model, s = 'lambda.min')
  insample_pred = predict(full_model, newx = X_full, s = best_lambda, type = "response")
  pred_holdout  = predict(full_model, newx = X_holdout, s = best_lambda, type = "response")
  roc_train     = roc(response = train_data$attended, predictor = c(insample_pred), plot = FALSE,
                      quiet = TRUE)
  roc_holdout   = roc(response = holdout_data$attended, predictor = c(pred_holdout), plot = FALSE,
                      quiet = TRUE)
  sdd           = c(1,apply(X_full, 2, sd))
  coef_scaled   = best_coef*sdd
  return(
    list("full_model" = full_model, "best_lambda" = best_lambda, "best_coef" = best_coef,
         "coef_scaled" = coef_scaled,
         "auc_holdout" = roc_holdout$auc[1],
         "auc_train" = roc_train$auc[1],
         "holdout_y" = holdout_data$attended, "holdout_pred" = c(pred_holdout),
         "insample_pred" = c(insample_pred), 
         "short_p_id"   = data$short_p_id[1])
  )
}

#------------------------------------------------------------------------------
#-------------------------CALCULATE INDIVIDUAL AUC SEQUENCE--------------------
#------------------------------------------------------------------------------
gym_indiv_auc_seq = function(decile){
  ## THIS FUNCTION TAKES AN OUTPUT OF THE gym_indiv_lasso FUNCTION AND RETURN THE AUC SEQUENCE OF THE CORRESPONDING INDIVIDUAL
  ##-----INPUT-----
  # decile         : decile of data size N
  ##-----OUTPUT-----
  # A sequence of AUC (2-week windows) for the corresponding individual
  dt = gym_data[decile_N==decile & !is.na(time_lag)]
  dt[,t:=1:.N,short_p_id]
  date_seq = seq(14, max(dt$t), 14)
  auc_seq = NA
  X = model.matrix(attended ~ streak + 
                     I(streak^2) + 
                     streak_dow + 
                     I(streak_dow^2) + 
                     factor(month) +
                     factor(dow) + time_lag  + last7days_attendance +
                     I(time_lag^2) - 1, 
                   data = dt)
  for(i in 1:(length(date_seq)-2)){
    train = dt[t %in% c(i, i+1)]
    test  = dt[t == i+2]
    X_full= X[dt[t %in% c(i, i+1), which = TRUE],]
    X_holdout = X[dt[t == i+2, which = TRUE],]
    full_model= cv.glmnet(x = X_full, y = train$attended, 
                          type.measure = 'auc', nfolds = 5,
                          family = 'binomial', standardize = TRUE)
    pred_holdout  = predict(full_model, 
                            newx = X_holdout, s = full_model$lambda.min, 
                            type = "response")
    roc_holdout   = roc(response = 
                          test$attended, predictor = c(pred_holdout), plot = FALSE,
                        quiet = TRUE)
    auc_seq = c(auc_seq, roc_holdout$auc)
  }
  return(auc_seq[-1])
}
## above is AUC of grouped individuals based on deciles of N

#------------------------------------------------------------------------------
#-------------------------EXPONENTIAL CURVE FITTING----------------------------
#------------------------------------------------------------------------------
auc_exp_fit = function(chunk, increment = 14){
  ## THIS FUNCTION TAKES A SEQUENCE OF NUMBERS (e.g. SEQUENCE OF AUC RETURNED BY THE individual_auc_seq FUNCTION) AND FIT AN EXPONENTIAL CURVE OF THE FORM a+bexp(-ct)
  ##-----INPUT-----
  # chunk         : sequence of numbers
  # increment     : incremental windows (14 for gym, 2 for hand)
  ##-----OUTPUT-----
  # a,b,c         : parameters of the exponential curve 
  # fit           : fitted values as predicted by the exponential curve
  # Rsq           : R-squared of the fit
  T_grid = seq(increment, increment*length(chunk), increment)             
  init   = list(a = 0.8, b = 0.01, c = 0.002)
  fit    = NULL
  i=1
  while(is.null(fit) & i<500){
    fit    = tryCatch(nlsLM(chunk~a-b*exp(-c*T_grid), start = init, 
                            lower = c(0, 0, 0),
                            upper = c(Inf, Inf, Inf),
                            control = list(maxiter = 1000)), error = function(e) NULL)
    init   = lapply(init, function(u) u*1.1)
    i      = i+1
  }
  return(list(
    "a" = coef(fit)[1],
    "b" = coef(fit)[2],
    "c" = coef(fit)[3],
    "fit" = fit$m$fitted(),
    "Rsq" = 1-sum(fit$m$resid()^2)/((length(chunk)-1)*var(chunk, na.rm = TRUE))
  ))
}
#-----------------------------------------------------------------------------
#-----------------CHECKING SELECTION ON CORRELATED VARS------------------------
#------------------------------------------------------------------------------
gym_coef_cor = function(var1, var2){
  ## THIS FUNCTION PERFORM KS TEST BETWEEN DISTRIBUTION OF INDIVIDUAL LASSO COEFFICIENTS WHEN RANDOMLY SPLIT THE SAMPLE IN HALF
  ##-----INPUT-----
  # Var1, var2: the 2 variable names to compute correlations
  ##-----OUTPUT-----
  # Table with frequency of being chosen across the 2 variables, broken down by high & low correlation
  cor_2vars = lapply(p_list, function(u)
    cor(gym_data[short_p_id==u, ..var1], 
        gym_data[short_p_id==u, ..var2],
        use = 'complete.obs'))
  cor_2vars = unlist(cor_2vars)
  med       = median(abs(cor_2vars))
  qu        = quantile(abs(cor_2vars), probs = c(.25, .5, .75))
  low_cor   = which(abs(cor_2vars)<=med) 
  coef_low  = coef_gym[short_p_id %in% p_list[low_cor], c(..var1, ..var2)]
  coef_high = coef_gym[!(short_p_id %in% p_list[low_cor]), c(..var1, ..var2)]
  coef_low  = apply(coef_low, 2, function(u) as.numeric(u!=0)) %>% data.table()
  coef_high = apply(coef_high, 2, function(u) as.numeric(u!=0)) %>% data.table()
  out_low   = coef_low[,round(100*.N/nrow(coef_low)),c(var1, var2)]
  out_high  = coef_high[,round(100*.N/nrow(coef_high)),c(var1, var2)]
  out       = cbind(out_low, out_high$V1)
  names(out)[c(3,4)] = c('Low cor.', 'High cor.')
  return(list("out" = out,
              "median" = med,
              "qu" = qu))
}
