# To do:
# - modify to take variable list of pdp_variables
# - return plots as single object

shap_plots <- function(fit_objects, data_clf, pdp_variables){
  
  rf_fit <- fit_objects$rf_fit
  rf_workflow <- fit_objects$rf_workflow
  
  ## SHAP explanations
  best_params <- select_best(rf_fit)
  final_model <- finalize_workflow(rf_workflow, best_params) %>%
    fit(data_clf)
  
  # SHAP interpretation
  shap_data <- data_clf %>% select(-outcome)
  shap_explainer <- kernelshap(final_model, X = shap_data, bg_X = shap_data)
  
  # visualize SHAP values
  sv <- shapviz(shap_explainer, X = shap_data)
  sv_importance(sv, kind = "bee")
  
  # dependence plots for select variables: can make this a loop to print out pdfs for arbitrary number of features later
  sv_dependence(sv, v = pdp_variables[1], X = shap_data, color_var = NULL)
  sv_dependence(sv, v = pdp_variables[2], X = shap_data, color_var = NULL)
  sv_dependence(sv, v = pdp_variables[3], X = shap_data, color_var = NULL)
  
}