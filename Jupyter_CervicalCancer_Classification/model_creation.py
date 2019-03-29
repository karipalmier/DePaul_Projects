import pandas as pd
import numpy as np
import time
from imblearn.over_sampling import SMOTE
from sklearn.model_selection import cross_validate
from sklearn import metrics
from feature_selection import low_var_filter, model_wrapper_select, stepwise_recur_select 
from feature_selection import univariate_chisq_select, mutual_info_select
from sklearn.model_selection import GridSearchCV
from imblearn.under_sampling import RandomUnderSampler

import warnings, sklearn.exceptions
warnings.filterwarnings("ignore", category=sklearn.exceptions.ConvergenceWarning)
warnings.filterwarnings("ignore", category=sklearn.exceptions.DataConversionWarning)

#############################################################################

def create_under_data(train_data, train_target, rand_st, sample_strat):

    x_col_names = list(train_data.columns.values)
    y_col_names = list(train_target.columns.values)

    print('Number of Original Target 0 Value: {}'.format(sum(train_target[y_col_names[0]] == 0)))
    print('Number of Original Target 1 Value: {}'.format(sum(train_target[y_col_names[0]] == 1)))
    print('\n')
    
    rus = RandomUnderSampler(random_state = rand_st, sampling_strategy = sample_strat)
    train_data_rus_np, train_target_rus_np = rus.fit_resample(train_data, train_target)

    train_data_rus = pd.DataFrame(train_data_rus_np, columns = x_col_names)
    train_rus_indices = train_data_rus.index.values
    train_target_rus = pd.DataFrame(train_target_rus_np, index = train_rus_indices, 
                                    columns = y_col_names)

    print('Number of Random Under Sample Target 0 Value: {}'.format(sum(train_target_rus[y_col_names[0]] == 0)))
    print('Number of Random Under Sample Target 1 Value: {}'.format(sum(train_target_rus[y_col_names[0]] == 1)))
    
    return train_data_rus, train_target_rus

def create_smote_data(train_data, train_target, rand_st, sample_strat):

    x_col_names = list(train_data.columns.values)
    y_col_names = list(train_target.columns.values)

    print('Number of Original Target 0 Value: {}'.format(sum(train_target[y_col_names[0]] == 0)))
    print('Number of Original Target 1 Value: {}'.format(sum(train_target[y_col_names[0]] == 1)))
    print('\n')
    
    sm = SMOTE(random_state = rand_st, sampling_strategy = sample_strat)
    train_data_sm_np, train_target_sm_np = sm.fit_resample(train_data, train_target)

    train_data_sm = pd.DataFrame(train_data_sm_np, columns = x_col_names)
    train_sm_indices = train_data_sm.index.values
    train_target_sm = pd.DataFrame(train_target_sm_np, index = train_sm_indices, columns = y_col_names)

    print('Number of SMOTE Target 0 Value: {}'.format(sum(train_target_sm[y_col_names[0]] == 0)))
    print('Number of SMOTE Target 1 Value: {}'.format(sum(train_target_sm[y_col_names[0]] == 1)))
    
    return train_data_sm, train_target_sm

def fit_val_data(train_data, train_target, val_data, val_target, model, model_desc, 
                 model_type):
    
    model.fit(train_data, train_target)

    scores_ACC = model.score(val_data, val_target)
    print(model_desc, model_type, 'Validation Acc:', scores_ACC)
    scores_AUC = metrics.roc_auc_score(val_target, model.predict(val_data))
    print(model_desc, model_type, 'Validation AUC:', scores_AUC)                         
    print('\n')
    
    return model

def model_data(train_data, train_target, val_data, val_target, model, scorers, 
               model_type, model_desc, cv_num):
    
    start_ts = time.time()

    scores = cross_validate(model, train_data, train_target, scoring = scorers, 
                            cv = cv_num)
    scores_Acc = scores['test_Accuracy']                                                                                                                                    
    print(model_desc, model_type, 'CV Acc: {} +/- {}'.format(scores_Acc.mean(), 
          scores_Acc.std() * 2))                                                                                                    
    scores_AUC= scores['test_roc_auc']                                                                                   
    print(model_desc, model_type, 'CV AUC: {} +/- {}'.format(scores_AUC.mean(), 
          scores_AUC.std() * 2))                                                                                                    
    print("CV Runtime:", time.time() - start_ts)
    print('\n')

    fit_model = fit_val_data(train_data, train_target, val_data, val_target, 
                             model, model_desc, model_type)

    return fit_model

def perform_base_models(train_data, train_target, val_data, val_target, 
                        train_data_sm, train_target_sm, 
                        model, scorers, model_type):
    
    model_desc = 'Base'
    base_model = model_data(train_data, train_target, val_data, val_target, model, 
                            scorers, model_type, model_desc, 5)   
    
    model_desc = 'SMOTE Base'
    base_model_sm = model_data(train_data_sm, train_target_sm, val_data, val_target, 
                               model, scorers, model_type, model_desc, 5) 
    
    return base_model, base_model_sm
    
def perform_feat_sel_models(train_data, train_target, val_data, val_target, 
                            model, model_type, thresh_vals, k_vals):
    
    results_dict = {}
    
    lv_filter_data, del_cols_lvf = low_var_filter(train_data, train_target, 
                                                  model, thresh_vals) 
    print('\n')
    val_data_lvf = val_data.drop(del_cols_lvf, axis = 1)
    model_desc = 'Low Variance Filter'
    lvf_model = fit_val_data(lv_filter_data, train_target, val_data_lvf, val_target, 
                             model, model_desc, model_type)
    
    results_dict['lvf'] = {'data':lv_filter_data, 'del_cols':del_cols_lvf, 
                'model':lvf_model}

    if model_type.find('Neural Network') < 0 and model_type.find('SVM') < 0:
        mod_wrap_data, del_cols_mod_wrap = model_wrapper_select(train_data, 
                                                            train_target, model, 
                                                            thresh_type = 'mean', 
                                                            max_feats = None) 
        print('\n')
        val_data_mod_wrap = val_data.drop(del_cols_mod_wrap, axis = 1)
        model_desc = 'Model Wrapper'
        mod_wrap_model = fit_val_data(mod_wrap_data, train_target, val_data_mod_wrap, 
                                      val_target, model, model_desc, model_type)

        results_dict['modw'] = {'data':mod_wrap_data, 'del_cols':del_cols_mod_wrap,
                    'model':mod_wrap_model}
        
    if model_type.find('Neural Network') < 0 and model_type.find('SVM') < 0:
        step_rec_data, del_cols_step = stepwise_recur_select(train_data, 
                                                             train_target, model,  
                                                             0.1, k_vals)
        print('\n')
        val_data_step = val_data.drop(del_cols_step, axis = 1)
        model_desc = 'Stepwise Recursive'
        step_model = fit_val_data(step_rec_data, train_target, val_data_step, 
                                  val_target, model, model_desc, model_type)

        results_dict['step'] = {'data':step_rec_data, 'del_cols':del_cols_step,
                    'model':step_model}
   
    uni_chisq_data, del_cols_chisq = univariate_chisq_select(train_data, train_target, 
                                                             model, k_vals)
    print('\n')
    val_data_chisq = val_data.drop(del_cols_chisq, axis = 1)
    model_desc = 'Univariate Chi-Squared'
    chisq_model = fit_val_data(uni_chisq_data, train_target, val_data_chisq, 
                               val_target, model, model_desc, model_type)
    
    results_dict['chisq'] = {'data':uni_chisq_data, 'del_cols':del_cols_chisq,
                'model':chisq_model}

    mut_info_data, del_cols_mut_inf = mutual_info_select(train_data, train_target, 
                                                         model, k_vals)
    print('\n')
    val_data_mut_inf = val_data.drop(del_cols_mut_inf, axis = 1)
    model_desc = 'Mutual Information'
    mut_inf_model = fit_val_data(mut_info_data, train_target, val_data_mut_inf, 
                                 val_target, model, model_desc, model_type)
        
    results_dict['mutinf'] = {'data':mut_info_data, 'del_cols':del_cols_mut_inf,
                'model':mut_inf_model}
    
    return results_dict
        
def perform_grid_search(model, data_train, target_train, param_dict, score, cv_num, 
                        model_type, model_desc):

    print(model_desc, model_type, 'Grid Search')
    
    start_ts = time.time()

    gs = GridSearchCV(model, param_dict, verbose = 1, cv = cv_num, scoring = score)
    gs.fit(data_train, target_train)
    opt_params = gs.best_params_
    opt_score = gs.best_score_
    print("Grid Search Runtime:", time.time() - start_ts)
    print('\n')
    print('Grid Search Optimal Parameters:', opt_params)
    print('Grid Search Optimal Parameter Score:', opt_score)
    print('\n')
    
    for key, value in opt_params.items():
        model.set_params(**{key: value})
    print('Final Model Parameter Settings:')
    print(model)
    print('\n')
    
    return model


