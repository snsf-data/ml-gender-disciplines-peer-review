# ---------------------------------------------------------------------------- #
# Mixed-effects Regression Results
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

# Load required packages
library(tidyverse)
library(texreg)
library(modelsummary)
library(broom.mixed)
library(xtable)
library(lme4)

# Load the main dataset
dat_reg <- readRDS(here("data", "data_gender_disc.rds"))


# Set ggplot2 theme to match internal SNSF scheme
theme_snsf <- function(base_size = 12, base_family = "") {
    theme_light(base_size = base_size, base_family = base_family) %+replace%
        theme(
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(
                face = "bold", size = 17,
                margin = margin(0, 0, 5, 0),
                hjust = 0.5
            ),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 15),
            axis.title = element_text(colour = "black", size = 15),
            axis.text = element_text(colour = "black", size = 14),
            strip.text = element_text(
                hjust = 0.5,
                colour = "black", face = "bold",
                size = 15,
                margin = margin(0, 0, 5, 0)
            ),
            strip.background = element_blank()
        )
}


# Set theme globally and increase base font size
ggplot2::theme_set(theme_snsf(base_size = 12))


# Rename variables for regression dependent variables (DVs)
dat_reg <- dat_reg |>
    mutate(`Track Record` = criterion_track_record_mean,
           `Relevance, Originality, Topicality` = criterion_relevance_originality_topicality_mean,
           `Methods` = criterion_suitability_mean,
           `Feasibility` = criterion_feasibility_mean,
           `Positive` = positive_mean,
           `Negative` = negative_mean)

# Define variable sets for regression formulas
policy_vars <- "gender_reviewer + gender_applicant + ResearchArea +"
policy_vars_int_gender <- "gender_reviewer * gender_applicant + ResearchArea +"
policy_vars_gender <- "gender_reviewer * gender_applicant +"

# Define control variables for regression models
controls <- paste("ntoken_review",
                  "CallEndDate", "reviewer_region", "MatchToApplicationTopic",
                  "ReviewerDegree", "IsUseInspired", "DurationRequestedMonth",
                  "ResponsibleApplicantAgeAtSubmission", "IsResubmission",
                  "IsMultiApplicant", "IsHasProjectPartners",
                  "ResponsibleApplicantProfessorshipType", "IsLeadAgencySNF",
                  "IsHasPreviousProjectRequested", "IsHasPreviousProjectApproved",
                  "ResearchInstitutionAtProjectStartDateType", "ReminderCount",
                  sep = " + "
)

# Combine all variable strings for data subsetting
all_vars_str <- paste(
    policy_vars,
    policy_vars_int_gender,
    controls,
    sep = " + "
)

# Extract unique variable names from combined string
all_vars <- unique(trimws(unlist(strsplit(all_vars_str, "\\+"))))

# Remove interaction terms and clean variable names
all_vars <- unique(unlist(strsplit(all_vars, "\\*")))
all_vars <- trimws(all_vars)

# Subset the data frame to include only relevant variables
df_subset <- dat_reg[, all_vars, drop = FALSE]

# Create a long-format data frame of variable-value pairs for inspection
df_long <- df_subset %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "values") %>%
    distinct(variable, values)

df_long

# Define dependent variables for regression models
dvs <- names(select(dat_reg, `Track Record`:Negative))

# Specify regression formulas for different model specifications
variable_formula_main <- paste0(
    policy_vars,
    controls)

variable_formula_int_gender <- paste0(
    policy_vars_int_gender,
    controls)

variable_formula_split_disc <- paste0(
    policy_vars_gender, controls)

# Specify formula for models with rescaled grade variable
variable_formula_grade_rescaled <- paste0(
    policy_vars, " grade_rescaled +", controls)

# Initialise list to store main regression models
model_list <- list()

# Fit main regression models for each dependent variable
for (i in dvs) {
    cat("Run model for", i, "\n")
    reg_formula <- as.formula(paste0("`", i, "` ~ ", variable_formula_main,
                                     " + (1 | `Number_anon`)"))

    model <- lmer(formula = reg_formula, data = dat_reg)
    model_list[[i]] <- model

}

# Define mapping for coefficient labels in output tables
coefs_mappped <- c("gender_reviewerFemale" = "Reviewer: Female (ref.: Male)",
                   "gender_applicantFemale" = "Applicant: Female (ref.: Male)",
                   "ResearchAreaMINT" = "Domain: MINT (ref.: LS)",
                   "ResearchAreaSSH" = "Domain: SSH (ref.: LS)",
                   "gender_reviewerFemale:gender_applicantFemale" = "Reviewer: Female x Applicant: Female",
                   "grade_rescaled" = "Grade (rescaled from 1-6)")

# Print full model output to screen for verification
screenreg(model_list)


# -----------------------------------------------
# Table 2: Main Regression Results
# -----------------------------------------------

# Save main regression results as a formatted table
modelsummary(model_list, coef_map = coefs_mappped,
             fmt = 2,
             statistic = "conf.int",
             gof_map = NA) |>
    group_tt(j = list(`Evaluation Criteria` = 2:5, Sentiment = 6:7)) |>
    save_tt(here("output", "plots_gender_disc", "tab_03.docx"),
            overwrite = TRUE)

names(model_list)[names(model_list) == "Relevance, Originality, Topicality"] <- "Rel., Orig., Topic."

modelsummary(model_list, coef_map = coefs_mappped,
             fmt = 2,
             title = "\\textit{Coefficients from mixed-effects models show percentage point differences in prevalence relative to reference category}. Analysis based on 39,280 peer review reports on 11,385 proposals. Rel., Orig., Topic: Relevance, Originality, Topicality; Methods: Suitability of Methods; Feasibility: Feasibility of Project.\\label{tab:03}",
             statistic = "conf.int",
             gof_map = NA) |>
    group_tt(j = list(`Evaluation Criteria` = 2:5, Sentiment = 6:7)) |>
    style_tt(fontsize = 1) |>
    save_tt(here("output", "plots_gender_disc", "tab_03.tex"),
            overwrite = TRUE)

# -----------------------------------------------
# Table S3: Regression with Gender Interaction
# -----------------------------------------------

# Initialise list to store models with gender interaction
model_list_int_gender <- list()

# Fit regression models with gender interaction for each dependent variable
for (i in dvs) {
    cat("Run model for", i, "\n")
    reg_formula <- as.formula(paste0("`", i, "` ~ ", variable_formula_int_gender,
                                     " + (1 | `Number_anon`)"))
    model_list_int_gender[[i]] <- lmer(formula = reg_formula, data = dat_reg)
}

# Print output for models with gender interaction
screenreg(model_list_int_gender)

# Save results of gender interaction models as a formatted table
modelsummary(model_list_int_gender, coef_map = coefs_mappped,
             fmt = 2,
             statistic = "conf.int",
             gof_map = NA) |>
    group_tt(j = list(`Evaluation Criteria` = 2:5, Sentiment = 6:7)) |>
    save_tt(here("output", "plots_gender_disc", "tab_s03.docx"),
            overwrite = TRUE)

names(model_list_int_gender)[names(model_list_int_gender) == "Relevance, Originality, Topicality"] <- "Rel., Orig., Topic."



modelsummary(model_list_int_gender, coef_map = coefs_mappped,
             fmt = 2,
             title = "\\textit{Predicting the prevalence of review characteristics by considering interaction effects between the gender of the reviewer and applicant}. Content categories are listed in the first row. Table reports results based on mixed-effects regression models. Coefficients show percentage point differences in prevalence relative to reference category. Models include control variables listed in Table \\ref{tab_s01} and random intercepts for proposal IDs. 95\\% confidence intervals in parentheses. Track Record: Applicant's Track Record; Rel., Orig., Topic: Relevance, Originality, Topicality; Methods: Suitability of Methods; Feasibility: Feasibility of Project.\\label{tab:s03}",
             statistic = "conf.int",
             gof_map = NA) |>
    group_tt(j = list(`Evaluation Criteria` = 2:5, Sentiment = 6:7)) |>
    style_tt(fontsize = 1) |>
    save_tt(here("output", "plots_gender_disc", "tab_s03.tex"),
            overwrite = TRUE)


modelsummary(model_list_int_gender)


# -----------------------------------------------
# Table S4: Regression considering Grades
# -----------------------------------------------

## Standardise grade scale (range 1 to 6) for full sample analysis

dat_reg_harmonised <- dat_reg |>
    filter(OverallGrade != "not considered") |> # Exclude ungraded cases
    mutate(grade_rescaled = ifelse(grade_dummy == "new",
                                   ((grade_numeric - 1) * (5 / 8) + 1), grade_numeric))

# Initialise list to store models with rescaled grade
model_list_grade_rescaled <- list()

# Fit models with rescaled grade for each dependent variable
for (i in dvs) {
    cat("Run model for", i, "\n")

    reg_formula_rescaled <- as.formula(paste0("`", i, "` ~ ",
                                              variable_formula_grade_rescaled,
                                              " + (1 | `Number_anon`)"))

    model_list_grade_rescaled[[i]] <- lmer(formula = reg_formula_rescaled,
                                           data = dat_reg_harmonised)
}

# Save results of models with rescaled grade as a formatted table
modelsummary(model_list_grade_rescaled, coef_map = coefs_mappped,
             fmt = 2,
             statistic = "conf.int",
             gof_map = NA) |>
    group_tt(j = list(`Evaluation Criteria` = 2:5, Sentiment = 6:7)) |>
    save_tt(here("output", "plots_gender_disc", "tab_s04.docx"),
            overwrite = TRUE)


names(model_list_grade_rescaled)[names(model_list_grade_rescaled) == "Relevance, Originality, Topicality"] <- "Rel., Orig., Topic."

modelsummary(model_list_grade_rescaled, coef_map = coefs_mappped,
             fmt = 2,
             title = "\\textit{Predicting the prevalence of review characteristics by the grade assigned by the reviewer}. Content categories are listed in the first row. Table reports results based on mixed-effects regression models. The variable Grade (rescaled) rescales the new grade scale from a nine- to a six-point scale to facilitate comparability across the full sample. Coefficients show percentage point differences in prevalence relative to reference category. Models include control variables listed in Table \\ref{tab_s01} and random intercepts for proposal IDs. 95\\% confidence intervals in parentheses. Track Record: Applicant's Track Record; Rel., Orig., Topic: Relevance, Originality, Topicality; Methods: Suitability of Methods; Feasibility: Feasibility of Project.\\label{tab:s04}",
             statistic = "conf.int",
             gof_map = NA)) |> 
    group_tt(j = list(`Evaluation Criteria` = 2:5, Sentiment = 6:7)) |>
    style_tt(fontsize = 1) |>
    save_tt(here("output", "plots_gender_disc", "tab_s04.tex"),
            overwrite = TRUE)

# -----------------------------------------------
# Figure S07: Interaction Effect by Discipline
# -----------------------------------------------

# Run models separately for each discipline and store output in a data frame
all_models_area_gender <- data.frame()

# Fit models by discipline for each dependent variable
for (i in dvs) {
    cat("Run model for", i, "\n")

    reg_formula <- as.formula(paste0("`", i, "` ~ ", variable_formula_split_disc,
                                     " + (1 | `Number_anon`)"))

    for (j in unique(dat_reg$ResearchArea)) {

        # Filter data for current discipline
        dat_sub <- dat_reg %>% filter(ResearchArea == j)

        # Fit model for current discipline
        lm_split <- try(lmer(formula = reg_formula,
                             data = dat_sub), silent = TRUE)

        # Tidy model output
        lm_split_tidy <- broom.mixed::tidy(lm_split)

        # Add metadata for dependent variable and discipline
        lm_split_tidy$dv <- i
        lm_split_tidy$subsample <- j

        # Append results to combined data frame
        all_models_area_gender <- bind_rows(all_models_area_gender, lm_split_tidy)
    }
}

# Filter to keep only the interaction coefficient for plotting
dat_coef_disc_gender <- filter(all_models_area_gender, term == "gender_reviewerFemale:gender_applicantFemale") |>
    mutate(dv = ifelse(str_detect(dv, "Rel."), "Relevance, Originality, Topicality",
                       ifelse(str_detect(dv, "Feasibility"), "Feasibility of Project",
                              ifelse(str_detect(dv, "Methods"), "Suitability of Methods", dv)))) |>
    mutate(dv = factor(dv, levels = levels_category)) |>
    mutate(subsample = factor(subsample, levels = c("SSH", "LS", "MINT")))


# Plot interaction effect by discipline and dependent variable
ggplot(dat_coef_disc_gender,
       aes(x = term,
           y = estimate,  colour = subsample,
           ymin = estimate - 1.96 * std.error,
           ymax = estimate + 1.96 * std.error)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               colour = "red") +
    geom_linerange(linewidth = 1.08) +
    geom_point(size = 2) +

    coord_flip() +
    scale_colour_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    geom_label(size = 3.5, alpha = 0.8, nudge_x = -0.35, hjust = 0.5, colour = "grey50", fill = "white",
               label.size = 0,
               aes(        label = paste0(
                   sprintf("%.1f", estimate),
                   "\n[", sprintf("%.1f", estimate - 1.96 * std.error), ", ",
                   sprintf("%.1f", estimate + 1.96 * std.error), "]"
               )
               )
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.2, 0.1)), breaks = c(seq(-3, 3, 3))) +
    facet_grid(subsample~dv, labeller = label_wrap_gen(15) ) +
    labs(y = "Interaction Term: Female Reviewer x Female Applicant\n(and 95% Confidence Intervals)",
         x = NULL) +
    theme(axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none")
ggsave(here("output", "plots_gender_disc", "fig_s07.pdf"),
       width = 9, height = 4.5)

