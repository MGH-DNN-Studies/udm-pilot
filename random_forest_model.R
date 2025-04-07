random_forest_model <- function(data_clf, n_inner, n_outer){
  
  # resample data
  set.seed(123)
  folds <- vfold_cv(data_clf, v=n_inner, repeats = n_outer, strata = outcome)
  
  # Model specification
  rf_mod <- rand_forest(mode='regression', trees = 1000) %>%
    set_engine('ranger', importance = 'permutation')
  
  # model recipe with median imputation
  rec <- 
    recipe(outcome ~ ., data = data_clf) %>%
    step_impute_median(all_numeric_predictors())
  
  # set workflow
  rf_workflow <- 
    workflow() %>% 
    add_recipe(rec) %>% 
    add_model(rf_mod)
  
  # set model control
  ctrl <- control_grid(verbose = FALSE, save_pred = TRUE, save_workflow = TRUE)
  
  # evaluation metrics
  metric_list <- metric_set(rsq_trad, rsq, rmse)
  
  # fit model to resampled data
  set.seed(123)
  rf_fit <- rf_workflow %>%
    fit_resamples(folds, metrics = metric_list, control = ctrl)
  
  rf_objects <- list(
    rf_fit=rf_fit,
    rf_workflow=rf_workflow,
    metric_list=metric_list
  )
  
  return(rf_objects)
  
}


