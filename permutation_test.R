permutation_test <- function(data_clf, fit_objects, n_perm){
  
  metric_list <- fit_objects$metric_list
  
  permuted_performance_list <- lapply(1:n_perm, function(p){
    
    print(p)
    
    data_clf_perm <- data_clf
    set.seed(p)
    data_clf_perm$outcome <- sample(data_clf_perm$outcome, replace = FALSE)
    
    # resample
    set.seed(p)
    folds_perm <- vfold_cv(data_clf_perm, v=n_inner, repeats = n_outer, strata = outcome)
    
    # Model specification
    rf_mod_perm <- rand_forest(mode='regression', trees = 1000) %>%
      set_engine('ranger', importance = 'permutation')
    
    rec_perm <- 
      recipe(outcome ~ ., data = data_clf_perm) %>%
      step_impute_median(all_numeric_predictors())
    
    rf_workflow_perm <- 
      workflow() %>% 
      add_recipe(rec_perm) %>% 
      add_model(rf_mod_perm)
    
    set.seed(p)
    rf_fit_perm <- rf_workflow_perm %>%
      fit_resamples(folds_perm, metrics = metric_list)
    
    permutation_metrics <- collect_metrics(rf_fit_perm)
    
    return(permutation_metrics)
    
  })  
  
  # aggregate permutation distributions for all metrics
  permuted_performance_df <- do.call(rbind, permuted_performance_list)
  permuted_performance_by_metric <- split(permuted_performance_df, permuted_performance_df$.metric)
  
  # function to plot permutation distribution versus observed value
  plot_permutation <- function(perm_dist, observed){
    hist_data <- c(perm_dist, observed)
    hist(perm_dist, xlim = range(hist_data), col = 'lightblue')
    abline(v = observed, lty=2, col = 'red')
  }
  
  # get original model fit
  rf_fit <- fit_objects$rf_fit
  
  # permutation test: R^2 traditional
  mean_rsq_trad_observed <- collect_metrics(rf_fit) %>%
    filter(.metric=='rsq_trad') %>%
    select(mean)
  
  p_rsq_trad <- ( sum(permuted_performance_by_metric$rsq_trad$mean >= mean_rsq_trad_observed$mean) + 1) / (n_perm + 1)
  
  print(sprintf("p-value (RSQ Traditional: %s)", p_rsq_trad))
  
  plot_permutation(perm_dist = permuted_performance_by_metric$rsq_trad$mean, 
                   observed = mean_rsq_trad_observed$mean)
  
  # permutation test: R^2 
  mean_rsq_observed <- collect_metrics(rf_fit) %>%
    filter(.metric=='rsq') %>%
    select(mean)
  
  p_rsq <- ( sum(permuted_performance_by_metric$rsq$mean >= mean_rsq_observed$mean) + 1) / (n_perm + 1)
  
  print(sprintf("p-value (RSQ: %s)", p_rsq))
  
  plot_permutation(perm_dist = permuted_performance_by_metric$rsq$mean, 
                   observed = mean_rsq_observed$mean)
  
  # permutation test: RMSE
  mean_rmse_observed <- collect_metrics(rf_fit) %>%
    filter(.metric=='rmse') %>%
    select(mean)
  
  p_rmse <- ( sum(permuted_performance_by_metric$rmse$mean <= mean_rmse_observed$mean) + 1) / (n_perm + 1)
  
  print(sprintf("p-value (RMSE: %s)", p_rmse))
  
  plot_permutation(perm_dist = permuted_performance_by_metric$rmse$mean, 
                   observed = mean_rmse_observed$mean)
  
  p_values_df <- data.frame(metrics = c('rsq_trad', 'rsq', 'rmse'), 
                         p=c(p_rsq_trad, p_rsq, p_rmse))
  
  return(p_values_df)
  
}