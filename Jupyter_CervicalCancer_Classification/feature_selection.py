import numpy as np
import pandas as pd

from sklearn.feature_selection import RFE, VarianceThreshold, SelectFromModel
from sklearn.feature_selection import SelectKBest, mutual_info_classif, chi2
from sklearn.model_selection import cross_validate

import warnings, sklearn.exceptions
warnings.filterwarnings("ignore", category=sklearn.exceptions.ConvergenceWarning)
warnings.filterwarnings("ignore", category=sklearn.exceptions.DataConversionWarning)

#############################################################################

def get_keep_del_cols(col_names, sel_idx):
    keep_cols = []
    del_cols = []
    for i in range(len(col_names)):
        if sel_idx[i] == 1:                                                           #Selected Features get added to temp header
            keep_cols.append(col_names[i])
        else:                                                                       #Indexes of non-selected features get added to delete array
            del_cols.append(col_names[i])

    return keep_cols, del_cols

def low_var_filter(data_df, target_df, model, thresh_vals = []):

    col_names = list(data_df.columns.values)
    data_np = data_df.values
    target_np = target_df.values
    
    scorers = {'Accuracy': 'accuracy', 'roc_auc': 'roc_auc'}   

    auc_results = []
    acc_results = []
    auc_std_results = []
    acc_std_results = []
    feature_select = []
    for thresh_val in thresh_vals:                                                                                                             
        sel = VarianceThreshold(threshold = thresh_val)
        data_np_fs = sel.fit_transform(data_np, target_np)
        
        scores = cross_validate(model, data_np_fs, target_np, scoring = scorers, 
                                cv = 5)
        auc_score = scores['test_roc_auc'].mean()
        acc_score = scores['test_Accuracy'].mean()
        auc_results = np.append(auc_results, auc_score)        
        acc_results = np.append(acc_results, acc_score)        
        auc_std = scores['test_roc_auc'].std() * 2
        acc_std = scores['test_Accuracy'].std() * 2
        auc_std_results = np.append(auc_std_results, auc_std)        
        acc_std_results = np.append(acc_std_results, acc_std)        
        feature_select.append(sel.get_support())

    optimal_ndx = np.where(auc_results == auc_results.max())[0]
    if len(optimal_ndx) > 1:
        optimal_ndx = optimal_ndx[0]
    else:
        optimal_ndx = int(optimal_ndx) 
    sel_idx = feature_select[int(optimal_ndx)]

    keep_cols, del_cols = get_keep_del_cols(col_names, sel_idx)

    print('-- Low Variance Filtering --')
    print('Threshold Selected: {}'.format(thresh_vals[optimal_ndx]))
    print('Selected Model Mean Accuracy Score: {}'.format(acc_results[optimal_ndx]))
    print('Selected Model Accuracy Deviation: {}'.format(acc_std_results[optimal_ndx]))
    print('Selected Model Mean AUC Score: {}'.format(auc_results[optimal_ndx]))
    print('Selected Model AUC Deviation: {}'.format(auc_std_results[optimal_ndx]))
    print('Number of Original Features: {}'.format(len(col_names)))
    print('Number of Selected Features: {}'.format(len(keep_cols)))
    print('Features Selected:')
    print(keep_cols)
    print('Features Removed:')
    print(del_cols)

    new_data_df = data_df.drop(del_cols, axis = 1)
    
    return new_data_df, del_cols

def model_wrapper_select(data_df, target_df, model, thresh_type = 'mean', 
                         max_feats = None):

    col_names = list(data_df.columns.values)
    data_np = data_df.values
    target_np = target_df.values

    sel = SelectFromModel(model, prefit = False, threshold = thresh_type, 
                          max_features = max_feats)                                                           #to select only based on max_features, set to integer value and set threshold=-np.inf

    data_np_fs = sel.fit_transform(data_np, target_np)
    
    scorers = {'Accuracy': 'accuracy', 'roc_auc': 'roc_auc'}   
    scores = cross_validate(model, data_np_fs, target_np, scoring = scorers, 
                            cv = 5)
    auc_result = scores['test_roc_auc'].mean()
    acc_result = scores['test_Accuracy'].mean()
    auc_std = scores['test_roc_auc'].std() * 2
    acc_std = scores['test_Accuracy'].std() * 2
    sel_idx = sel.get_support()

    keep_cols, del_cols = get_keep_del_cols(col_names, sel_idx)
            
    print('-- Model Wrapper Feature Selection --')
    print('Model Mean Accuracy Score: {}'.format(acc_result))
    print('Selected Model Accuracy Deviation: {}'.format(acc_std))
    print('Model Mean AUC Score: {}'.format(auc_result))
    print('Selected Model AUC Deviation: {}'.format(auc_std))
    print('Number of Original Features: {}'.format(len(col_names)))
    print('Number of Selected Features: {}'.format(len(keep_cols)))
    print('Features Selected:')
    print(keep_cols)
    print('Features Removed:')
    print(del_cols)

    new_data_df = data_df.drop(del_cols, axis = 1)
    
    return new_data_df, del_cols
                
def stepwise_recur_select(data_df, target_df, model, step_val = 0.1, k_vals = []):
    
    col_names = list(data_df.columns.values)
    data_np = data_df.values
    target_np = target_df.values

    scorers = {'Accuracy': 'accuracy', 'roc_auc': 'roc_auc'}   

    auc_results = []
    acc_results = []
    feature_select = []
    auc_std_results = []
    acc_std_results = []
    for k_val in k_vals: 
        sel = RFE(model, n_features_to_select = k_val, step = step_val)                                                                                                            
        data_np_fs = sel.fit_transform(data_np, target_np)
        
        scores = cross_validate(model, data_np_fs, target_np, scoring = scorers, 
                                cv = 5)
        auc_score = scores['test_roc_auc'].mean()
        acc_score = scores['test_Accuracy'].mean()
        auc_results = np.append(auc_results, auc_score)        
        acc_results = np.append(acc_results, acc_score)        
        auc_std = scores['test_roc_auc'].std() * 2
        acc_std = scores['test_Accuracy'].std() * 2
        auc_std_results = np.append(auc_std_results, auc_std)        
        acc_std_results = np.append(acc_std_results, acc_std)        
        feature_select.append(sel.get_support())

    optimal_ndx = np.where(auc_results == auc_results.max())[0]
    if len(optimal_ndx) > 1:
        optimal_ndx = optimal_ndx[0]
    else:
        optimal_ndx = int(optimal_ndx) 
    sel_idx = feature_select[int(optimal_ndx)]

    keep_cols, del_cols = get_keep_del_cols(col_names, sel_idx)
            
    print('-- Stepwise Recursive Feature Selection --')
    print('K Selected: {}'.format(k_vals[optimal_ndx]))
    print('Selected Model Mean Accuracy Score: {}'.format(acc_results[optimal_ndx]))
    print('Selected Model Accuracy Deviation: {}'.format(acc_std_results[optimal_ndx]))
    print('Selected Model Mean AUC Score: {}'.format(auc_results[optimal_ndx]))
    print('Selected Model AUC Deviation: {}'.format(auc_std_results[optimal_ndx]))
    print('Number of Original Features: {}'.format(len(col_names)))
    print('Number of Selected Features: {}'.format(len(keep_cols)))
    print('Features Selected:')
    print(keep_cols)
    print('Features Removed:')
    print(del_cols)

    new_data_df = data_df.drop(del_cols, axis = 1)
    
    return new_data_df, del_cols

    
def univariate_chisq_select(data_df, target_df, model, k_vals = []):
    
    col_names = list(data_df.columns.values)
    data_np = data_df.values
    target_np = target_df.values

    scorers = {'Accuracy': 'accuracy', 'roc_auc': 'roc_auc'}   

    auc_results = []
    acc_results = []
    feature_select = []
    auc_std_results = []
    acc_std_results = []
    for k_val in k_vals:                                                                                                             
        sel = SelectKBest(chi2, k = k_val)
        data_np_fs = sel.fit_transform(data_np, target_np)
        
        scores = cross_validate(model, data_np_fs, target_np, scoring = scorers, 
                                cv = 5)
        auc_score = scores['test_roc_auc'].mean()
        acc_score = scores['test_Accuracy'].mean()
        auc_results = np.append(auc_results, auc_score)        
        acc_results = np.append(acc_results, acc_score)        
        auc_std = scores['test_roc_auc'].std() * 2
        acc_std = scores['test_Accuracy'].std() * 2
        auc_std_results = np.append(auc_std_results, auc_std)        
        acc_std_results = np.append(acc_std_results, acc_std)        
        feature_select.append(sel.get_support())

    optimal_ndx = np.where(auc_results == auc_results.max())[0]
    if len(optimal_ndx) > 1:
        optimal_ndx = optimal_ndx[0]
    else:
        optimal_ndx = int(optimal_ndx) 
    sel_idx = feature_select[int(optimal_ndx)]

    keep_cols, del_cols = get_keep_del_cols(col_names, sel_idx)
            
    print('-- Univariate Chi-Sq Feature Selection --')
    print('K Selected: {}'.format(k_vals[optimal_ndx]))
    print('Selected Model Mean Accuracy Score: {}'.format(acc_results[optimal_ndx]))
    print('Selected Model Accuracy Deviation: {}'.format(acc_std_results[optimal_ndx]))
    print('Selected Model Mean AUC Score: {}'.format(auc_results[optimal_ndx]))
    print('Selected Model AUC Deviation: {}'.format(auc_std_results[optimal_ndx]))
    print('Number of Original Features: {}'.format(len(col_names)))
    print('Number of Selected Features: {}'.format(len(keep_cols)))
    print('Features Selected:')
    print(keep_cols)
    print('Features Removed:')
    print(del_cols)

    new_data_df = data_df.drop(del_cols, axis = 1)
    
    return new_data_df, del_cols

def mutual_info_select(data_df, target_df, model, k_vals = []):
    
    col_names = list(data_df.columns.values)
    data_np = data_df.values
    target_np = target_df.values
    
    scorers = {'Accuracy': 'accuracy', 'roc_auc': 'roc_auc'}   

    auc_results = []
    acc_results = []
    feature_select = []
    auc_std_results = []
    acc_std_results = []
    for k_val in k_vals:                                                                                                             
        sel = SelectKBest(mutual_info_classif, k = k_val)
        data_np_fs = sel.fit_transform(data_np, target_np)
        
        scores = cross_validate(model, data_np_fs, target_np, scoring = scorers, 
                                cv = 5)
        auc_score = scores['test_roc_auc'].mean()
        acc_score = scores['test_Accuracy'].mean()
        auc_results = np.append(auc_results, auc_score)        
        acc_results = np.append(acc_results, acc_score)        
        auc_std = scores['test_roc_auc'].std() * 2
        acc_std = scores['test_Accuracy'].std() * 2
        auc_std_results = np.append(auc_std_results, auc_std)        
        acc_std_results = np.append(acc_std_results, acc_std)        
        feature_select.append(sel.get_support())

    optimal_ndx = np.where(auc_results == auc_results.max())[0]
    if len(optimal_ndx) > 1:
        optimal_ndx = optimal_ndx[0]
    else:
        optimal_ndx = int(optimal_ndx) 
    sel_idx = feature_select[int(optimal_ndx)]
    
    keep_cols, del_cols = get_keep_del_cols(col_names, sel_idx)
            
    print('-- Mutual Information Feature Selection --')
    print('K Selected: {}'.format(k_vals[optimal_ndx]))
    print('Selected Model Mean Accuracy Score: {}'.format(acc_results[optimal_ndx]))
    print('Selected Model Accuracy Deviation: {}'.format(acc_std_results[optimal_ndx]))
    print('Selected Model Mean AUC Score: {}'.format(auc_results[optimal_ndx]))
    print('Selected Model AUC Deviation: {}'.format(auc_std_results[optimal_ndx]))
    print('Number of Original Features: {}'.format(len(col_names)))
    print('Number of Selected Features: {}'.format(len(keep_cols)))
    print('Features Selected:')
    print(keep_cols)
    print('Features Removed:')
    print(del_cols)

    new_data_df = data_df.drop(del_cols, axis = 1)
    
    return new_data_df, del_cols
