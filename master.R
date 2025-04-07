# load packages
require(dplyr)
require(janitor)
require(fastDummies)
require(missMethods)
require(tidymodels)
require(parsnip)
require(yardstick)
require(vip)
require(ggplot2)
require(treeshap)
require(shapviz)
require(kernelshap)

# set outcome
outcome <- 'bdi_score_fu'

# set parameters
n_inner <- 10
n_outer <- 10
n_perm <- 50 # 50 for piloting; set to 1000 or more for later

# set paths
path_data <- '/path/to/CollaborationCHUM_MGH/'
path_code <- '/path/to/code/'

# source functions
source(paste0(path_code, 'format_data.R'))
source(paste0(path_code, 'random_forest_model.R'))
source(paste0(path_code, 'shap_plots.R'))
source(paste0(path_code, 'permutation_test.R'))

# format data
data_clf <- format_data(path_data = path_data, outcome=outcome)

# train/test model
fit_objects <- random_forest_model(data_clf = data_clf, n_inner = n_inner, n_outer = n_outer)

# print model performance
collect_metrics(fit_objects$rf_fit)

# SHAP interpretations
shap_plots(fit_objects = fit_objects, data_clf = data_clf, pdp_variables = c('qfs_vci_2_bl', 'bai_21_bl', 'age'))

# permutation test
p_values_df <- permutation_test(data_clf = data_clf, fit_objects = fit_objects, n_perm = n_perm)
p_values_df                            





