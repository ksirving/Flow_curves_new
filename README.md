# SOC_Irving_et_al_data

please cite: 

Irving, K., Taniguchi-Quan, K. T. R., Aprahamian, A., Rivers, C., Sharp, G., Mazor, R.D., Theroux, S., Holt, A., Peek, R., Stein, E. D. (2022). Application of flow ecology analysis to inform prioritization for stream restoration and management actions. Frontiers in Environmental Science - Freshwater Science.

## Code index

1 - 00_formatting_bug_algae_hydo.R
2a - 01_csci_median_logistic regression thresholds.R
2b - 01a_asci_median_logistic regression_thresholds.R
3 - 03_GLM_figures.R
4 - 04_correlation_matrix.R
5a - 05_csci_brts.R
5b - 05_asci_brts.R
6 - 06_relative_importance_figure.R
7 - 07_delta_h_limits.R
8 - 08_SOC_alteration.R
9 - 09_comparison_of_thresholds.R
10 - 10_Alteration_maps.R
11 - 11_final_alteration.R

## code and data descriptions

# 1 - 00_formatting_bug_algae_hydo.R

Formatting raw CSCI and ASCI data extracted from SMC (By Liesl)
Combining delta H FFM woth CSCI ASCI data
Taking the median delta H over all years, sites/FFM with just one year removed
Output is dataframe with bio (CSCI/ASCI) and hydro (delta H FFM) for GLMs:

00_csci_delta_formatted_median_updated_Nov2021.csv
00_asci_delta_formatted_median_Nov2021.csv

# 2a - 01_csci_median_logistic regression thresholds.R

Logistic regression (GLMs) for CSCI and FFM (delta H)
output is predicted probability for all combinations, with input binary response and hydro

## just CSCI
01_csci_neg_pos_logR_metrics_figures_April2021.csv

## OoverE, MMI & CSCI

01_bugs_all_data_neg_pos_logR_metrics_figures_April2021.csv

# 2b - 01a_asci_median_logistic regression_thresholds.R

Same as above but with ASCI

## Just Hybrid ASCI

01_h_asci_neg_pos_logR_metrics_figures_April2021.csv

## Hybrid and Diatoms

01_algae_all_data_neg_pos_logR_metrics_figures_April2021.csv

# 3 - 03_GLM_figures.R

Figures for all FFM and bio indices with all index thresholds
These figures are to be used in the FFM filtering process and are avilable in the manuscript supplementary material (Figures xxx)

# 4 - 04_correlation_matrix.R

Correlation of FFM delta H values at sites matched with CSCI and ASCI. Produces two correltion matrices:

04_ffm_cor_csci.csv
04_ffm_cor_asci.csv

# 5a - 05_csci_brts.R

brts for CSCI. Takes sveral parameter criteria to find the best model. results in relative importance for each FFM

05_rel_imp_csci_labels.csv

# 5b - 05_asci_brts.R

brts for ASCI. Takes sveral parameter criteria to find the best model. results in relative importance for each FFM

05_rel_imp_asci_labels.csv


# 6 - 06_relative_importance_figure.R

boxplot of both CSCI and ASCI relative importance

05_rel_imp_csci_asci_bar_plot_all_data.jpg


# 07_delta_h_limits.R

Extract delta H limits from curves for each threshold combination, FFM/index threshold/probability
Produces 2 dataframes:

07_CSCI_delta_thresholds_scaled.csv
07_ASCI_delta_thresholds_scaled.csv

with delta h limits and number of observations per combinations

and combined data 

07_ALL_delta_thresholds_scaled.csv

# 08_SOC_alteration.R

formatting delta h limits from regional curves: 

08_all_delta_h_limits_all_combinations_thresh_combs.csv - all delta h limits
08_delta_H_all_years_and_subbasins.csv - all delta in SOC area, for supplementary material table xx
08_alteration_by_year_site_all_sites.csv - biologically relevant alteration

## current conditions

08_altered_metrics_direction_current_by_year.csv - direction of alteration by year
08_altered_metrics_direction_current_overall.csv - direction of alteration overall
08_metric_suitability_tally_by_site_current.csv - number of years altered by site, threshold combo
08_sites_metrics_number_of_years_with_data.csv - number of years with data per site and FFM
08_metric_suitability_tally_condensed_all_sites_current.cs - % alteration per site, per combo

## water conservation
08_altered_metrics_direction_watercon_by_year.csv - direction of alteration by year
08_altered_metrics_direction_watercon_overall.csv - direction of alteration overall
08_metric_suitability_tally_by_site_watercon.csv - number of years altered by site, threshold combo
08_metric_suitability_tally_condensed_all_sites_watercon.csv - % alteration per site, per combo

# 09_comparison_of_thresholds.R

A working script to find the best threshold combination. Produces summary tables and alteration over time plots:

09_subbasin_alteration_median_mean.csv - overall alteration per metric and combo

# CSCI
CSCI_SP_Tim_Alt_over_time.jpg
CSCI_DS_Dur_WS_Alt_over_time.jpg
CSCI_Q99_Alt_over_time.jpg

# ASCI
ASCI_SP_Dur_Alt_over_time.jpg
ASCI_DS_Dur_WS_Alt_over_time.jpg
ASCI_Q99_Alt_over_time.jpg

# 10_Alteration_maps.R

Alteration maps comparing threshold combos spatially. Produces 6 figures, one for each index/fmm

# 11_final_alteration.R

plots biological alteration with the final combinations for ASCI, ASCI and synthesis

11_SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Current.csv

