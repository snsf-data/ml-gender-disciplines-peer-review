# ---------------------------------------------------------------------------- #
# Robustness Analysis for Gender and Discipline Differences
# Double ML for Semi-Parametric Estimation of the Partially Linear Model
# Reference: Chernozhukov et al. (2018), DoubleML package
# ---------------------------------------------------------------------------- #
# Project: Gender and Discipline Shape Length, Content and Tone of Grant
#          Peer Review Reports
#
# Authors:
# Stefan Müller (ORCID: 0000-0002-6315-4125)
# Gabriel Okasa (ORCID: 0000-0002-3573-7227)
# Michaela Strinzel (ORCID: 0000-0003-3181-0623)
# Anne Jorstad (ORCID: 0000-0002-6438-1979)
# Katrin Milzow (ORCID: 0009-0002-8959-2534)
# Matthias Egger (ORCID: 0000-0001-7462-5132)
#
# Contact: stefan.mueller@ucd.ie
# ---------------------------------------------------------------------------- #

# Load packages
library(here)
library(tidyverse)
library(magrittr)
library(tinytable)
library(fastDummies)
library(DoubleML)
library(mlr3)
library(mlr3learners)
library(caret)
library(xtable)
library(writexl)
library(ranger)

# ---------------------------------------------------------------------------- #
# Data
# ---------------------------------------------------------------------------- #

# define relevant variables and their roles
# outcome
outcome_vars <- c("criterion_track_record_mean",
                  "criterion_relevance_originality_topicality_mean",
                  "criterion_suitability_mean", "criterion_feasibility_mean",
                  "positive_mean", "negative_mean")
# policy
policy_vars <- c('gender_reviewer', 'gender_applicant', 'ResearchArea')
# cluster on the proposal level
cluster_vars <- 'Number_anon'
# potential confounders
control_vars <- c('ntoken_review', 'CallEndDate', 'reviewer_region',
                  'MatchToApplicationTopic', 'ReviewerDegree', 'IsUseInspired',
                  'DurationRequestedMonth', 'ResponsibleApplicantAgeAtSubmission',
                  'IsResubmission', 'IsMultiApplicant', 'IsHasProjectPartners', 
                  'ResponsibleApplicantProfessorshipType', 'IsLeadAgencySNF',
                  'IsHasPreviousProjectRequested', 'IsHasPreviousProjectApproved',
                  'ResearchInstitutionAtProjectStartDateType', 'ReminderCount')

# ---------------------------------------------------------------------------- #
# load the estimation sample and pre-process it for estimation
dat_reg <- readRDS(here("data", "data_gender_disc.rds")) |>
    # select variables for analysis
    select(c(outcome_vars, policy_vars, cluster_vars, control_vars)) |>
    # rename the outcomes
    rename(`Track_Record` = criterion_track_record_mean,
           `Relevance_Originality_Topicality` = criterion_relevance_originality_topicality_mean,
           `Methods` = criterion_suitability_mean,
           `Feasibility` = criterion_feasibility_mean,
           `Positive` = positive_mean,
           `Negative` = negative_mean) |>
    # format call end date as character and remove the special characters
    mutate(CallEndDate = str_replace_all(as.character(CallEndDate), "-", "_")) |>
    # recode gender variables to numeric dummies
    mutate(gender_applicant = if_else(gender_applicant == "Female", 1, 0)) |>
    mutate(gender_reviewer = if_else(gender_reviewer == "Female", 1, 0)) |>
    # recode all binary variables to numeric values (0,1)
    mutate(across(c(IsUseInspired, IsResubmission, IsMultiApplicant, 
                    IsHasProjectPartners, IsLeadAgencySNF, 
                    IsHasPreviousProjectRequested, IsHasPreviousProjectApproved), 
                  as.numeric)) |>
    # get rid of blank spaces and other not allowed characters in the var values
    # region
    mutate(across(c(reviewer_region, ResponsibleApplicantProfessorshipType,
                    MatchToApplicationTopic, ResearchInstitutionAtProjectStartDateType), 
                  ~ str_replace_all(., "[ ,:/()\\-]", "_"))) |>
    # create dummy variables for categorical predictors - one-hot encoding
    dummy_cols(select_columns = c("CallEndDate", "reviewer_region",
                                  "MatchToApplicationTopic", "ReviewerDegree",
                                  "ResearchInstitutionAtProjectStartDateType", 
                                  "ResponsibleApplicantProfessorshipType",
                                  "ResearchArea"),
               remove_first_dummy = FALSE, remove_selected_columns = TRUE) |>
    # and remove LS as serving as reference category - include SSH and MINT
    select(-ResearchArea_LS) |>
    # and all the other reference categories
    select(-c(CallEndDate_2016_10_01,
              reviewer_region_Western_Europe,
              MatchToApplicationTopic_is_within_my_area_of_specialisation,
              ReviewerDegree_UG_MA_Dipl,
              ResearchInstitutionAtProjectStartDateType_Cantonal_University,
              ResponsibleApplicantProfessorshipType_Full_Professor,
              )) |>
    # relocate research area vars to cleanup
    relocate(gender_reviewer, .after = "Number_anon") |>
    relocate(gender_applicant, .after = "gender_reviewer") |>
    relocate(ResearchArea_SSH, .after = "gender_applicant") |>
    relocate(ResearchArea_MINT, .after = "ResearchArea_SSH") |>
    # format as dataframe
    as.data.frame()

# ---------------------------------------------------------------------------- #
# redefine variable groups after transformations
outcome_vars <- c("Track_Record", "Relevance_Originality_Topicality",
                  "Methods", "Feasibility", "Positive", "Negative")
# policy vars with dummies
policy_vars <- c('gender_reviewer', 'gender_applicant',
                 'ResearchArea_SSH', 'ResearchArea_MINT')
# control vars with dummies
control_vars <- dat_reg |>
    select(-c(all_of(outcome_vars), all_of(policy_vars), all_of(cluster_vars))) |>
    colnames()

# ---------------------------------------------------------------------------- #
# Regression Models
# ---------------------------------------------------------------------------- #

# Estimate partially linear models via Double ML:
# for workflow, see: https://docs.doubleml.org/stable/workflow/workflow.html

# specify number of cross-fitting folds
n_folds <- 5

# specify the dimensionality of the nuisance regressions
model_dim <- length(c(control_vars, policy_vars)) - 1

# specify ML methods for the nuisance functions
# use the ranger implementation of RF, i.e. standard regression RF
ml_l_rf <- lrn("regr.ranger",
               num.trees = 1000,
               mtry = round(model_dim/3), # recommendation in Hastie et al
               min.node.size = 5) # recommendation in Hastie et al

# classification RF for prediction of policy vars, i.e. gender, discipline
ml_m_rf <- lrn("classif.ranger",
               num.trees = 1000,
               mtry = round(sqrt(model_dim)), # recommendation in Hastie et al
               min.node.size = 1) # recommendation in Hastie et al

# ---------------------------------------------------------------------------- #
# prepare results storage as an empty list
results <- list()
# loop through all outcomes: start the main loop
for (outcome_idx in outcome_vars) {
    
    # ---------------------------------------------------------------------------- #
    # print the current outcome being analyzed
    cat(paste0("Estimation of the partially linear model for: ", outcome_idx, "\n\n"))
    
    # ---------------------------------------------------------------------------- #
    # Construct DoubleMLData object from the dataframe
    dml_data_df <- double_ml_data_from_data_frame(df = dat_reg,
                                                  y_col = outcome_idx, # loop idx
                                                  d_cols = policy_vars,
                                                  x_cols = control_vars,
                                                  cluster_cols = cluster_vars)
    
    # ---------------------------------------------------------------------------- #
    # initialize the structural model - partially linear model
    set.seed(123) # fix the seed for reproducibility due to RF randomness
    # parametrize the model
    dml_model <- DoubleMLPLR$new(dml_data_df,
                                 ml_l = ml_l_rf,
                                 ml_m = ml_m_rf,
                                 n_rep = 1, # no repeated cross-fitting
                                 n_folds = n_folds, # number of cross-fitting folds
                                 score = 'partialling out', # Frish-Waugh Theorem
                                 dml_procedure = 'dml2') # k-fold cross-fitting
    
    # ---------------------------------------------------------------------------- #
    # estimate the model
    dml_model$fit()
    # Summary of the model
    dml_model$summary()
    # adjust p values for multiple treatment variables via Bonferroni correction
    pval_bonferroni <- dml_model$p_adjust(method = "bonferroni")[, 2]
    # adjust also confidence intervals accordingly at the 5% significance level
    bonferroni_correction_factor <- 1 - (0.05/length(policy_vars))
    confint_bonferroni_lower <- dml_model$confint(level = bonferroni_correction_factor)[, 1]
    confint_bonferroni_upper <- dml_model$confint(level = bonferroni_correction_factor)[, 2]
    
    # format and save as html for inspection
    model_summary <- dml_model$summary() |>
        # replace the p-values and confidence intervals with the bonferroni ones
        as.data.frame() |>
        mutate(`Pr(>|t|)` = pval_bonferroni,
               `CI lower` = confint_bonferroni_lower,
               `CI upper` = confint_bonferroni_upper)
    
    # save the model summary for this outcome
    results[[outcome_idx]] <- model_summary
    
    # ---------------------------------------------------------------------------- #
    # export the table with results for policy vars as html table
    model_summary |>
        xtable(caption = paste0("Predicting ", outcome_idx, ": Partially Linear Model"),
               digits = 4) |>
        print(type = "html",
              file = here("output", "tables",
                          paste0("reg_partially_linear_model_summary_",
                                 outcome_idx, ".html")))
    
    # and as a latex table as well
    model_summary |>
        xtable(caption = paste0("Predicting ", outcome_idx, ": Partially Linear Model"),
               digits = 4) |>
        print(type = "latex",
              file = here("output", "tables",
                          paste0("reg_partially_linear_model_summary_",
                                 outcome_idx, ".tex")))
    
    # export the table with results for policy vars as excel table
    write_xlsx(model_summary |>
                   as.data.frame() |>
                   mutate("Variable" = rownames(model_summary)) |>
                   relocate("Variable", .before = "Estimate."),
               path = here("output", "tables",
                           paste0("reg_partially_linear_model_summary_",
                                  outcome_idx, ".xlsx")))

    # ---------------------------------------------------------------------------- #
}

# ---------------------------------------------------------------------------- #
# prepare final results table
results_report <- data.frame(matrix(NA,
                                    ncol = length(outcome_vars),
                                    nrow = length(policy_vars)*2)) |>
    # add colnames
    set_colnames(outcome_vars) |>
    # and column with policy var names
    mutate("Variable" = c(rbind(policy_vars, rep("", length(policy_vars))))) |>
    # and set as a first column
    relocate("Variable", .before = 1)

# populate the values from the model summaries
for (outcome_idx in outcome_vars) {
    
    # get the results for the given outcome
    results_idx <- results[[outcome_idx]]
    
    # assign the estimates to main table
    for (policy_idx in policy_vars) {
        
        # get the row idx to write
        row_idx <- which(results_report$Variable == policy_idx)
        # assign the coefficient estimate
        results_report[row_idx, outcome_idx] <- results_idx |>
            filter(row.names(results_idx) == policy_idx) |>
            pull(Estimate.) |>
            round(2)
        
        # extract assign the confidence intervals as well
        conf_intervals <- results_idx |>
            filter(row.names(results_idx) == policy_idx) |>
            select(`CI lower`, `CI upper`)
        # format
        conf_intervals_formatted <- paste0("[",
                                           round(conf_intervals$`CI lower`, 2),
                                           "; ",
                                           round(conf_intervals$`CI upper`,2),
                                           "]")
        # assign them into main table below the coefficient estimate
        results_report[row_idx + 1, outcome_idx] <- conf_intervals_formatted
        
    }
}

# ---------------------------------------------------------------------------- #
# final formatting of the reporting table
results_report <- results_report |>
    # add the number of reviews
    rbind(c("N Reviews", rep(nrow(dat_reg), ncol(results_report) - 1))) |>
    # and number of proposals
    rbind(c("N Proposals", rep(length(unique(dat_reg$Number_anon)),
                               ncol(results_report) - 1))) |>
    # and rename the columns and rows for reporting
    mutate("Variable" = c("Reviewer: Female (ref.: Male)", "",
                          "Applicant: Female (ref: Male)", "",
                          "Domain: SSH (ref.: LS)", "",
                          "Domain: MINT (ref.: LS)", "",
                          "N Reviews", "N Proposals")) |>
    # and columns
    set_colnames(c("", "Track Record", "Relevance, Originality, Topicality",
                   "Methods", "Feasibility", "Positive", "Negative"))

# and export as .docx using tinytable
results_report |>
    tt() |>
    save_tt(here("output", "tables", "tab_main_results_plm.docx"),
            overwrite = TRUE)

# and as a latex table as well
results_report |>
    xtable(caption = paste0("Partially Linear Model"),
           digits = 4) |>
    print(type = "latex",
          file = here("output", "tables",
                      paste0("reg_partially_linear_model_summary_all.tex")))

# export the table with results for policy vars as excel table
write_xlsx(results_report |>
               as.data.frame(),
           path = here("output", "tables",
                       paste0("reg_partially_linear_model_summary_all.xlsx")))

# ---------------------------------------------------------------------------- #
