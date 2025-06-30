# ---------------------------------------------------------------------------- #
# Descriptive Plots and Tables
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

# Load required libraries
library(here)
library(scales)
library(arsenal)
library(sjlabelled)
library(patchwork)
library(gtable)
library(grid)
library(tinytable)
library(quanteda)
library(quanteda.textstats)
library(tidyverse)
library(xtable)

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


# -----------------------------------------------
# Recode categories for more readable names
# -----------------------------------------------
recode_categories <- c(
    "candidate_other" = "Applicant",
    "candidate_quantity" = "Applicant: Quantity",
    "criterion_feasibility" = "Feasibility of Project",
    "criterion_relevance_originality_topicality" = "Relevance, Originality, Topicality",
    "criterion_suitability" = "Suitability of Methods",
    "criterion_track_record" = "Track Record",
    "impact_beyond" = "Impact Beyond",
    "negative" = "Negative",
    "positive" = "Positive",
    "proposal_general" = "Proposal",
    "proposal_method" = "Methods",
    "rationale_positive_or_negative" = "Rationale",
    "suggestion" = "Suggestion"
)

# Set order for category levels
levels_category <- c(
    "Track Record", "Relevance, Originality, Topicality", "Suitability of Methods", "Feasibility of Project",
    "Proposal", "Methods", "Applicant", "Applicant: Quantity", "Positive", "Negative", "Rationale", "Suggestion"
)

# Function to recode themes for categories
recode_themes <- function(category) {
    case_when(
        category %in% c("Feasibility of Project", "Track Record", "Relevance, Originality, Topicality", "Suitability of Methods") ~ "Evaluation Criterion",
        category %in% c("Positive", "Negative", "Rationale") ~ "Sentiment",
        category %in% c("Applicant", "Applicant: Quantity", "Proposal", "Methods") ~ "Focus"
    )
}

# -----------------------------------------------
# Load main review-level data
# -----------------------------------------------
dat_reviewlevel <- readRDS(here("data", "data_gender_disc.rds"))

# Calculate total number of words and sentences in all reviews
sum(dat_reviewlevel$ntoken_review)
sum(dat_reviewlevel$n_sentences)

# -----------------------------------------------
# Table 1: Reviewer-level and Applicant-level Summary Tables
# -----------------------------------------------

# Prepare reviewer-level summary data
dat_summary <- dat_reviewlevel |>
    mutate(gender_applicant = dplyr::recode(ResponsibleApplicantGender, "f" = "Female", "m" = "Male")) |>
    filter(gender_applicant != "x") |>
    mutate(gender_reviewer = dplyr::recode(ReviewerGender, "female" = "Female", "male" = "Male"))

dat_summary <- dat_summary |>
    mutate(gender_reviewer = fct_rev(gender_reviewer)) |>
    mutate(gender_applicant = fct_rev(gender_applicant)) |>
    mutate(gender_reviewer_label = set_label(gender_reviewer, "Gender of Reviewer")) |>
    mutate(`Research Area` = ResearchArea)

# Recode reviewer country: group countries with <1000 reviews as "Other"
dat_summary <- dat_summary |>
    group_by(ReviewerCountry) |>
    mutate(count_country = n()) |>
    ungroup() |>
    mutate(country_recoded = ifelse(count_country < 1000, "Other", ReviewerCountry)) |>
    mutate(country_recoded = dplyr::recode(country_recoded, "Great Britain and Northern Ireland" = "Great Britain and N. Ireland")) |>
    mutate(country_recoded = ifelse(country_recoded == "", "Unknown", country_recoded)) |>
    mutate(country_recoded = fct_infreq(country_recoded))  |> # Order by frequency
    mutate(country_recoded = fct_relevel(country_recoded, "Other", "Unknown", after = Inf)) |> # Move 'Other' and 'Unknown' to end
    mutate(country_recoded = set_label(country_recoded, "Country of Affiliation"))

# Create reviewer-level cross-table (Table 1a)
tab_rev <- arsenal::tableby(`Research Area` ~ gender_reviewer_label +
                                country_recoded,
                            numeric.stats = c("meansd"),
                            data = dat_summary,test = FALSE)

summary(tab_rev)
# Save Table 1a as Word document
write2word(tab_rev, here("output", "plots_gender_disc", "tab_01a.doc"))

# Prepare applicant-level summary data (unique applications only)
dat_summary_app <- dat_summary |>
    select(gender_applicant,ResponsibleApplicantAgeAtSubmission, Number_anon,
           `Research Area`,
           ResearchInstitutionAtProjectStartDateType, ResponsibleApplicantProfessorshipType) |>
    unique() |> # Keep each application only once
    mutate(ResearchInstitutionAtProjectStartDateType = set_label(ResearchInstitutionAtProjectStartDateType, "Affiliation")) |>
    mutate(gender_applicant_label = set_label(gender_applicant, "Gender")) |>
    mutate(age_label = set_label(ResponsibleApplicantAgeAtSubmission, "Age")) |>
    mutate(ResponsibleApplicantProfessorshipType = set_label(ResponsibleApplicantProfessorshipType, "Academic Rank"))

# Ensure each application is included only once
stopifnot(nrow(dat_summary_app) == length(unique(dat_summary_app$Number_anon)))

# Create applicant-level cross-table (Table 1b)
tab_app <- arsenal::tableby(`Research Area` ~ gender_applicant_label +
                                age_label +
                                ResearchInstitutionAtProjectStartDateType +
                                ResponsibleApplicantProfessorshipType,
                            data = dat_summary_app,
                            test = FALSE,
                            digits = 1,
                            numeric.stats = c("meansd"))

summary(tab_app)
# Save Table 1b as Word document
write2word(tab_app, here("output", "plots_gender_disc", "tab_01b.doc"))

# Note: Tables 1a and 1b are manually combined in the manuscript.

# -----------------------------------------------
# Figure 1: Mean Review Length by Discipline
# -----------------------------------------------

# Calculate mean review length and confidence intervals by discipline
dat_avg_disc <- dat_reviewlevel |>
    group_by(ResearchArea, MainDisciplineLevel2) |>
    summarise(
        mean_ntoken_review = mean(ntoken_review, na.rm = TRUE),
        sd = sd(ntoken_review, na.rm = TRUE),
        n = sum(!is.na(ntoken_review)), # Count of non-NA values
        se = sd / sqrt(n), # Standard error
        ci_lower = mean_ntoken_review - 1.96 * se,
        ci_upper = mean_ntoken_review + 1.96 * se
    ) |>
    mutate(ResearchArea = factor(ResearchArea, levels = c("SSH", "LS", "MINT")))

# Calculate mean review length by research area for reference lines
dat_avg_full <- dat_reviewlevel |>
    group_by(ResearchArea) |>
    summarise(
        mean_ntoken_review = mean(ntoken_review, na.rm = TRUE),
        sd = sd(ntoken_review, na.rm = TRUE),
        n = sum(!is.na(ntoken_review)), # Count of non-NA values
        se = sd / sqrt(n), # Standard error
        ci_lower = mean_ntoken_review - 1.96 * se,
        ci_upper = mean_ntoken_review + 1.96 * se
    ) |>
    mutate(ResearchArea = factor(ResearchArea, levels = c("SSH", "LS", "MINT")))

# Plot Figure 1 and save as PDF
ggplot(dat_avg_disc, aes(
    x = mean_ntoken_review,
    xmin = ci_lower, xmax = ci_upper,
    colour = ResearchArea,
    shape = ResearchArea,
    y = reorder(MainDisciplineLevel2, mean_ntoken_review)
)) +
    geom_vline(
        data = dat_avg_full,
        linetype = "dashed", linewidth = 1.05,
        aes(xintercept = mean_ntoken_review)
    ) +
    geom_rect(
        data = dat_avg_full, inherit.aes = FALSE,
        aes(
            xmin = ci_lower, xmax = ci_upper,
            fill = ResearchArea,
            ymin = -Inf, ymax = Inf
        ),
        alpha = 0.1
    ) +
    geom_point(size = 3) +
    geom_linerange(linewidth = 1.5) +
    scale_colour_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    scale_fill_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    scale_shape_manual(values = c(1, 15, 16)) +
    scale_x_continuous(breaks = c(seq(400, 1500, 100)), limits = c(500, 1200)) +
    facet_grid(ResearchArea ~ ., scales = "free_y", space = "free_y") +
    geom_text(
        size = 4.3, alpha = 0.8, nudge_y = 0.3,
        aes(
            x = mean_ntoken_review, y = MainDisciplineLevel2,
            colour = ResearchArea,
            label = paste0(
                round(mean_ntoken_review, 0),
                " [", round(ci_lower, 0), ", ",
                round(ci_upper, 0), "]"
            )
        )
    ) +
    geom_label(
        data = dat_avg_full, inherit.aes = FALSE, hjust = 0,
        size = 5, alpha = 0.9,
        aes(
            x = ci_upper + 25, y = 1.5,
            colour = ResearchArea,
            label = paste0(
                round(mean_ntoken_review, 0),
                " [", round(ci_lower, 0), ", ",
                round(ci_upper, 0), "]"
            )
        )
    ) +
    labs(
        x = "Mean Review Length (and 95% Confidence Intervals)",
        y = NULL,
        fill = NULL,
        colour = NULL,
        shape = NULL
    ) +
    theme(
        legend.position = "none",
        strip.text.y = element_text(margin = margin(l = 10), vjust = 0),
        legend.background = element_rect(fill = "white", colour = "grey50"), # White background with black border
        legend.box.background = element_rect(colour = "grey50")
    )
ggsave(here("output", "plots_gender_disc", "fig_01.pdf"),
       width = 9, height = 12
)

# -----------------------------------------------
# Prepare Data for Gender-based Plots
# -----------------------------------------------

# Create data object for plots, recoding gender and professorship type
dat_reviewlevel_plot <- dat_reviewlevel |>
    mutate(gender_applicant = dplyr::recode(ResponsibleApplicantGender, "f" = "Female", "m" = "Male")) |>
    filter(gender_applicant != "x") |>
    mutate(gender_reviewer = dplyr::recode(ReviewerGender, "female" = "Female", "male" = "Male")) |>
    mutate(gender_applicant = relevel(factor(gender_applicant), ref = "Male")) |>
    mutate(gender_reviewer = relevel(factor(gender_reviewer), ref = "Male")) |>
    mutate(ResearchArea = factor(ResearchArea, levels = c("SSH", "MINT", "LS"))) |>
    mutate(professor_type = dplyr::recode(
        ResponsibleApplicantProfessorshipType,
        "None" = "None",
        "Full professor" = "Full Professor",
        "Associate professor" = "Associate Professor",
        "Honorary professor or Titular professor" = "Honorary/Titular/Visiting Professor",
        "Assistant professor with tenure track" = "Assistant Professor (Tenure Track)",
        "Assistant professor without tenure track" = "Assistant Professor (no Tenure Track)",
        "Professor at UAS / UTE" = "UAS/UTE Professor",
        "Visiting professor" = "Honorary/Titular/Visiting Professor"
    ))

# -----------------------------------------------
# Figure 2: Mean Review Length by Reviewer and Applicant Gender
# -----------------------------------------------

# Calculate means and CIs by reviewer and applicant gender
dat_reviewlevel_plot_reviewer_app <- dat_reviewlevel_plot |>
    group_by(ResearchArea, gender_reviewer, gender_applicant) |>
    summarise(
        mean_ntoken = mean(ntoken_review, na.rm = TRUE),
        sd = sd(ntoken_review, na.rm = TRUE),
        n = sum(!is.na(ntoken_review)), # Count of non-NA values
        se = sd / sqrt(n), # Standard error
        ci_lower = mean_ntoken - 1.96 * se,
        ci_upper = mean_ntoken + 1.96 * se
    ) |>
    mutate(ResearchArea = dplyr::recode(ResearchArea,
                                        "SSH" = "(A) SSH",
                                        "LS" = "(B) LS",
                                        "MINT" = "(C) MINT")) |>
    mutate(ResearchArea = factor(ResearchArea, levels = c("(A) SSH", "(B) LS", "(C) MINT"))) |>
    mutate(gender_applicant = paste0(gender_applicant, " Applicant")) |>
    mutate(gender_reviewer = paste0(gender_reviewer, " Reviewer")) |>
    mutate(gender_reviewer = factor(gender_reviewer, levels = c("Male Reviewer", "Female Reviewer")))

# Plot Figure 2 and save as PDF
ggplot(dat_reviewlevel_plot_reviewer_app, aes(
    y = gender_reviewer,
    colour = fct_rev(gender_applicant),
    x = mean_ntoken,
    xmin = ci_lower,
    xmax = ci_upper,
    shape = fct_rev(gender_applicant)
)) +
    scale_colour_manual(values = c("Female Applicant" = "#9D90B9",
                                   "Male Applicant" = "#FBBE5E"),
                        guide = guide_legend(reverse = TRUE)) +
    geom_point(size = 4, position = position_dodge(width = 0.6)) +
    geom_linerange(linewidth = 1.5, position = position_dodge(width = 0.6)) +
    facet_wrap(ResearchArea ~ ., ncol = 1, labeller = label_wrap_gen(width = 16)) +
    scale_shape_manual(values = c("Male Applicant" = 16,  "Female Applicant" = 17),
                       guide = guide_legend(reverse = TRUE)) +
    geom_text(
        hjust = 0, #nudge_y = 0,
        size = 4.5, alpha = 0.8,
        show.legend = FALSE,
        position = position_dodge(width = 0.6),
        aes(
            x = ci_upper + 5,
            colour = fct_rev(gender_applicant),
            label = paste0(
                round(mean_ntoken, 0),
                " [", round(ci_lower, 0), ", ", round(ci_upper, 0), "]"
            )
        )
    ) +
    scale_x_continuous(breaks = c(seq(600, 1200, 100)),
                       limits = c(680, 1200)) +
    labs(
        y = NULL, x = "Mean Review Length (and 95% Confidence Intervals)",
    ) +
    theme(
        legend.position = "bottom", plot.title = element_text(size = 17),
        strip.text.y = element_text(angle = 0, hjust = 0),
        legend.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85")
    )
ggsave(here("output", "plots_gender_disc", "fig_02.pdf"),
       width = 9, height = 6)

# -----------------------------------------------
# Figure 3: Prevalence of Categories (Histograms)
# -----------------------------------------------

# Transform review-level means to long format for plotting
dat_means_long <- dat_reviewlevel |>
    select(ends_with("_mean"), Review_anon, ResearchArea, ReviewerGender, CallEndDate) |>
    pivot_longer(cols = -c(Review_anon, ResearchArea, ReviewerGender, CallEndDate),
                 names_to = "category", values_to = "percentage") |>
    filter(!is.na(percentage)) |>
    mutate(category = str_remove_all(category, "_mean")) |>
    mutate(category = dplyr::recode(category, !!!recode_categories)) |>
    mutate(measure = "Prevalence (in %)") |>
    mutate(theme = recode_themes(category)) |>
    mutate(category = factor(category, levels_category))

# Calculate mean and median prevalence by category
dat_prev_mean_median <- dat_means_long |>
    group_by(category) |>
    summarise(mean = mean(percentage),
              median = median(percentage),
              sd = sd(percentage),
              n = n()) |>
    arrange(-mean) |>
    mutate(theme = recode_themes(category)) |>
    mutate(category = factor(category, levels_category))

# Prepare data for histograms (convert percentage to proportion)
dat_means_long_hist <- dat_means_long |>
    mutate(percentage = percentage / 100)

# Calculate mean and CI for histogram data
dat_prev_mean_median_hist <- dat_means_long_hist |>
    group_by(category) |>
    summarise(
        mean = mean(percentage, na.rm = TRUE),
        sd = sd(percentage, na.rm = TRUE),
        n = sum(!is.na(percentage)),
        se = sd / sqrt(n),
        ci_lower = mean - 1.96 * se,
        ci_upper = mean + 1.96 * se) |>
    arrange(-mean) |>
    mutate(theme = recode_themes(category)) |>
    mutate(category = factor(category, levels_category))

# Calculate mean and median prevalence by research area and category
dat_prev_mean_median_disc <- dat_means_long |>
    group_by(ResearchArea, category) |>
    summarise(mean = mean(percentage),
              median = median(percentage),
              sd = sd(percentage),
              n = n()) |>
    arrange(-mean) |>
    mutate(theme = recode_themes(category)) |>
    mutate(category = factor(category, levels_category))

# Plot histograms of prevalence by research area and category
p_disc <- ggplot(dat_means_long, aes(x = percentage / 100,
                                     fill = fct_rev(ResearchArea))) +
    geom_histogram(binwidth = 0.01) +
    facet_grid(fct_rev(ResearchArea)~category, scales = "free_y",
               #nrow = 1
               labeller = label_wrap_gen(width = 15))  +
    geom_vline(dat = dat_prev_mean_median_disc,
               aes(xintercept = mean / 100), colour = "black",
               linetype = "dashed") +
    scale_colour_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    scale_fill_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    scale_y_continuous(expand = c(0, 30)) +
    scale_x_continuous(labels = scales::percent_format(accuray = 1),
                       breaks = c(seq(0, 0.9, 0.3))) +
    geom_label(data = dat_prev_mean_median_disc,
               aes(x = mean / 100, y = 1, colour = ResearchArea,
                   label = scales::percent(mean / 100, 0.1)),
               nudge_x = 0.05, hjust = 0,
               vjust = -0.5, inherit.aes = FALSE)  +
    labs(x = "Percentage of Sentences", y = NULL) +
    theme(axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(size = 12),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_blank())

p_disc

# Add "All Reviews" as a group for comparison
dat_means_long_allrev <- dat_means_long
dat_means_long_allrev$ResearchArea <- "All Reviews"

# Calculate mean and median for "All Reviews"
dat_prev_mean_median_all <- dat_means_long_allrev |>
    group_by(category) |>
    summarise(mean = mean(percentage),
              median = median(percentage),
              sd = sd(percentage),
              n = n()) |>
    arrange(-mean) |>
    mutate(ResearchArea = "All Reviews")

# Combine research area and "All Reviews" data for plotting
dat_prev_mean_median_disc_all <- bind_rows(dat_prev_mean_median_disc,
                                           dat_prev_mean_median_all) |>
    mutate(ResearchArea = fct_rev(factor(ResearchArea, levels = c("All Reviews", "SSH", "MINT", "LS"))))

dat_hist_all <- bind_rows(dat_means_long, dat_means_long_allrev) |>
    mutate(ResearchArea = fct_rev(factor(ResearchArea, levels = c("All Reviews", "SSH", "MINT", "LS"))))

# Plot combined histogram for Figure 3
p <- ggplot(dat_hist_all, aes(x = percentage / 100,
                              fill = fct_rev(ResearchArea))) +
    geom_histogram(binwidth = 0.01) +
    facet_grid(fct_rev(ResearchArea)~category, scales = "free_y",
               #nrow = 1
               labeller = label_wrap_gen(width = 15))  +
    geom_vline(dat = dat_prev_mean_median_disc_all,
               aes(xintercept = mean / 100), colour = "black",
               linetype = "dashed") +
    scale_colour_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    scale_fill_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    scale_y_continuous(expand = c(0, 30)) +
    scale_x_continuous(labels = scales::percent_format(accuray = 1),
                       breaks = c(seq(0, 0.9, 0.3))) +
    geom_label(data = dat_prev_mean_median_disc_all,
               aes(x = mean / 100, y = 1, colour = ResearchArea,
                   label = scales::percent(mean / 100, 0.1)),
               nudge_x = 0.05, hjust = 0,
               vjust = -0.5, inherit.aes = FALSE)  +
    labs(x = "Percentage of Sentences", y = NULL) +
    theme(axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(size = 12),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(size = 11),
          strip.text.y = element_text(margin = margin(l = 5), vjust = 0),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_blank())

p

# Create custom layout for Figure 3 with group labels above facets
gt <- ggplotGrob(p)

# Identify facet strip rows
strip_t_indices <- grep("strip-t", gt$layout$name)

# Calculate column ranges for each group of facets
col_ranges <- list(
    EvaluationCriteria = range(gt$layout[strip_t_indices[1:4], ]$l),
    Sentiment = range(gt$layout[strip_t_indices[5:6], ]$l)
)

# Create group label grobs
group_labels <- list(
    EvaluationCriteria = textGrob("Evaluation Criteria", gp = gpar(fontsize = 14, fontface = "bold")),
    Sentiment = textGrob("Sentiment", gp = gpar(fontsize = 14, fontface = "bold"))
)

# Create line grobs under group labels
group_lines <- list(
    EvaluationCriteria = linesGrob(
        x = unit(c(0, 1), "npc"),
        y = unit(1, "npc"), gp = gpar(lwd = 2)
    ),
    Sentiment = linesGrob(
        x = unit(c(0, 1), "npc"), y = unit(1, "npc"),
        gp = gpar(lwd = 2)
    )
)

# Insert rows for group labels above facet strips
strip_t_top <- min(gt$layout[strip_t_indices, ]$t)
gt <- gtable_add_rows(gt,
                      heights = unit(c(1.2, 0.7), "lines"),
                      pos = strip_t_top - 1
)

# Add group labels and lines to the gtable
for (group in names(group_labels)) {
    col_start <- col_ranges[[group]][1]
    col_end <- col_ranges[[group]][2]

    gt <- gtable_add_grob(gt, group_labels[[group]], t = strip_t_top, l = col_start, r = col_end, name = paste0(group, "_label"))
    gt <- gtable_add_grob(gt, group_lines[[group]], t = strip_t_top + 1, l = col_start, r = col_end, name = paste0(group, "_line"))
}

# Draw and save Figure 3 as PDF
grid.newpage()
pdf(here("output", "plots_gender_disc", "fig_03.pdf"), width = 9, height = 6)
grid.draw(gt)
dev.off()

# -----------------------------------------------
# Figure 4: Prevalence of Positive/Negative by Discipline
# -----------------------------------------------

# Calculate mean prevalence and CI for each category by discipline
dat_avg_category <- dat_reviewlevel_plot |>
    select(ResearchArea, professor_type, MainDisciplineLevel2, ends_with("_mean")) |>
    gather(category, prevalence, -c(ResearchArea, professor_type, MainDisciplineLevel2)) |>
    mutate(category = str_remove_all(category, "_mean")) |>
    filter(category != "rationale_positive_or_negative") |>
    group_by(ResearchArea, MainDisciplineLevel2, category) |>
    summarise(
        mean = mean(prevalence, na.rm = TRUE),
        sd = sd(prevalence, na.rm = TRUE),
        n = sum(!is.na(prevalence)),
        se = sd / sqrt(n),
        ci_lower = mean - 1.96 * se,
        ci_upper = mean + 1.96 * se
    ) |>
    mutate(category = dplyr::recode(category, !!!recode_categories)) |>
    mutate(category = factor(category, levels = levels_category)) |>
    mutate(theme = recode_themes(category)) |>
    mutate(ResearchArea = factor(ResearchArea, levels = c("SSH", "LS", "MINT")))

# Filter for positive and negative categories for main figure
dat_avg_category_disc_posneg <- dat_avg_category |>
    filter(category %in% c("Positive", "Negative"))

# Reorder disciplines for "Positive" category by mean
dat_avg_category_disc_posneg_order <- dat_avg_category_disc_posneg |>
    group_by(category) |>
    mutate(MainDisciplineLevel2 = if_else(
        category == "Positive",
        factor(MainDisciplineLevel2, levels = MainDisciplineLevel2[order(mean)]),
        factor(MainDisciplineLevel2)
    )) |>
    ungroup() |>
    mutate(category = ifelse(category == "Positive", "(A) Positive", "(B) Negative")) |>
    mutate(ResearchArea = factor(ResearchArea, levels = c("MINT", "LS", "SSH")))

# Plot Figure 4 and save as PDF
dat_avg_category_disc_posneg_order |>
    ggplot(aes(
        x = mean,
        xmin = ci_lower, xmax = ci_upper,
        colour = ResearchArea,
        shape = ResearchArea,
        y = MainDisciplineLevel2
    )) +
    geom_point(size = 3) +
    geom_linerange(linewidth = 1.5) +
    scale_colour_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    scale_shape_manual(values = c(16, 15, 1)) +
    scale_y_discrete(expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(
        expand = expansion(mult = c(0.3, 0.3)),
        breaks = c(seq(0, 80, 5)),
        labels = scales::percent_format(accuarcy = 1, scale = 1)
    ) +
    facet_wrap(~category, nrow = 1, scales = "free_x") +
    geom_text(
        inherit.aes = FALSE, hjust = 0.5,
        size = 3.5, alpha = 0.8, nudge_y = 0.4,
        aes(
            x = mean, y = MainDisciplineLevel2,
            colour = ResearchArea,
            label = paste0(
                sprintf("%.1f", mean), "%",
                " [", sprintf("%.1f", ci_lower), ", ",
                sprintf("%.1f", ci_upper), "]"
            )
        )
    ) +
    labs(
        x = "Mean Prevalence (and 95% Confidence Intervals)",
        y = NULL,
        fill = "Research Area:",
        colour = "Research Area:",
        shape = "Research Area:"
    ) +
    theme(
        axis.text.y = element_text(size = 12),
        legend.position = "bottom"
    )
ggsave(here("output", "plots_gender_disc", "fig_04.pdf"),
       width = 9, height = 9)

# -----------------------------------------------
# Figure S1: Mean Review Length by Reviewer and Applicant Gender
# -----------------------------------------------

# Calculate means and CIs by reviewer gender
dat_reviewlevel_plot_reviewer <- dat_reviewlevel_plot |>
    group_by(ResearchArea, gender_reviewer) |>
    summarise(
        mean_ntoken = mean(ntoken_review, na.rm = TRUE),
        sd = sd(ntoken_review, na.rm = TRUE),
        n = sum(!is.na(ntoken_review)), # Count of non-NA values
        se = sd / sqrt(n), # Standard error
        ci_lower = mean_ntoken - 1.96 * se,
        ci_upper = mean_ntoken + 1.96 * se
    ) |>
    mutate(ResearchArea = factor(ResearchArea, levels = c("SSH", "LS", "MINT")))

# Plot reviewer gender panel
p_rev <- ggplot(dat_reviewlevel_plot_reviewer, aes(
    y = gender_reviewer,
    x = mean_ntoken,
    xmin = ci_lower,
    xmax = ci_upper,
    colour = ResearchArea
)) +
    scale_colour_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    geom_point(size = 3) +
    geom_linerange(linewidth = 1.5) +
    facet_wrap(ResearchArea ~ ., ncol = 1, labeller = label_wrap_gen(width = 16)) +
    geom_text(
        hjust = 0, nudge_y = 0,
        size = 4.5, alpha = 0.8,
        aes(
            x = ci_upper + 5,
            colour = ResearchArea,
            label = paste0(
                round(mean_ntoken, 0),
                " [", round(ci_lower, 0), ", ", round(ci_upper, 0), "]"
            )
        )
    ) +
    # coord_cartesian(xlim = c(650, 1050)) +
    scale_x_continuous(breaks = c(seq(600, 1100, 100)),
                       limits = c(680, 1100)) +
    labs(
        y = "Reviewer Gender", x = "Mean Review Length (and 95% Confidence Intervals)",
        title = "(A) Gender of Reviewer\n"
    ) +
    theme(
        legend.position = "none", plot.title = element_text(size = 17),
        strip.text.y = element_text(angle = 0, hjust = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85")
    )
p_rev

# Calculate means and CIs by applicant gender
dat_reviewlevel_plot_applicant <- dat_reviewlevel_plot |>
    group_by(ResearchArea, gender_applicant) |>
    summarise(
        mean_ntoken = mean(ntoken_review, na.rm = TRUE),
        sd = sd(ntoken_review, na.rm = TRUE),
        n = sum(!is.na(ntoken_review)), # Count of non-NA values
        se = sd / sqrt(n), # Standard error
        ci_lower = mean_ntoken - 1.96 * se,
        ci_upper = mean_ntoken + 1.96 * se
    ) |>
    mutate(ResearchArea = factor(ResearchArea, levels = c("SSH", "LS", "MINT")))

# Plot applicant gender panel
p_app <- ggplot(dat_reviewlevel_plot_applicant, aes(
    y = gender_applicant,
    x = mean_ntoken,
    xmin = ci_lower,
    xmax = ci_upper,
    colour = ResearchArea
)) +
    scale_colour_manual(values = c("SSH" = "#3D7D9F",  "MINT" = "#D9534F", "LS" = "#71B294")) +
    geom_point(size = 3) +
    geom_linerange(linewidth = 1.5) +
    facet_wrap(ResearchArea ~ ., ncol = 1, labeller = label_wrap_gen(width = 16)) +
    geom_text(
        hjust = 0, nudge_y = 0,
        size = 4.5, alpha = 0.8,
        aes(x = ci_upper + 5,
            colour = ResearchArea,
            label = paste0(
                round(mean_ntoken, 0),
                " [", round(ci_lower, 0), ", ", round(ci_upper, 0), "]"
            )
        )
    ) +
    scale_x_continuous(breaks = c(seq(600, 1100, 100)),
                       limits = c(680, 1100)) +
    labs(
        y = "Applicant Gender", x = "Mean Review Length (and 95% Confidence Intervals)",
        title = "(B) Gender of Applicant\n"
    ) +
    theme(
        legend.position = "none", plot.title = element_text(size = 17),
        strip.text.y = element_text(angle = 0, hjust = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey85")

    )
p_app

# Combine reviewer and applicant gender panels and save as PDF
(p_rev / p_app)
ggsave(here("output", "plots_gender_disc", "fig_s01.pdf"),
       width = 9, height = 8.5
)

# -----------------------------------------------
# Figure S2: Review Length by Grade Scale
# -----------------------------------------------

# Prepare data for review length by grade (old/new scale)
dat_long_grades_ntoken <- dat_reviewlevel_plot |>
    filter(OverallGrade != "not considered") |>  # Remove ungraded reviews
    mutate(grade_dummy = ifelse(CallEndDate %in% c("2022-04-01", "2022-10-03", "2023-04-04"), "new", "old")) |>
    mutate(grade_numeric_orig = as.integer(OverallGradeSortNumber)) |>
    mutate(grade_numeric = ifelse(grade_dummy == "old", 7 - grade_numeric_orig, grade_numeric_orig)) |>  # Reverse scale for old grades
    mutate(grade_factor = as.factor(grade_numeric)) |>
    select(ResearchArea, grade_factor, grade_dummy, ntoken_review) |>
    gather(category, prevalence, -c(ResearchArea, grade_factor, grade_dummy)) |>
    mutate(category = str_remove_all(category, "_mean")) |>
    group_by(category, grade_factor, grade_dummy) |>
    summarise(
        mean = mean(prevalence, na.rm = TRUE),
        sd = sd(prevalence, na.rm = TRUE),
        n = sum(!is.na(prevalence)),
        se = sd / sqrt(n),
        ci_lower = mean - 1.96 * se,
        ci_upper = mean + 1.96 * se
    ) |>
    mutate(grade_dummy = dplyr::recode(grade_dummy,"old" = "Old Grade Scale (2016-2021)",
                                       "new" = "New Grade Scale (since 2022)"))

# Plot review length by grade and save as PDF
dat_long_grades_ntoken |>
    ggplot(aes(x = factor(grade_factor),
               colour = fct_rev(grade_dummy), shape = fct_rev(grade_dummy),
               y = mean, ymin = ci_lower, ymax = ci_upper)) +
    geom_linerange(position = position_dodge(width = 0.3), linewidth = 1.05) +
    geom_point(size = 2, position = position_dodge(width = 0.3)) +
    scale_shape_manual(values = c(16, 15)) +
    scale_colour_manual(values = c("#3D7D9F", "#71B294")) +
    guides(colour = guide_legend(reverse=TRUE),
           shape = guide_legend(reverse=TRUE)) +
    labs(x = "Grade", y = "Mean Review Length\n(and 95% Confidence Intervals)") +
    theme(
        legend.position = "inside",
        legend.position.inside = c(0.75, 0.9),
        legend.title = element_blank(),
        strip.text.y = element_text(margin = margin(l = 10), vjust = 0),
        legend.background = element_rect(fill = "white", colour = "grey50"), # White background with black border
        legend.box.background = element_rect(colour = "grey50"))
ggsave(here("output", "plots_gender_disc", "fig_s02.pdf"),
       width = 9, height = 5)

# -----------------------------------------------
# Figures S3-S6: Prevalence of Evaluation Criteria by Discipline
# -----------------------------------------------

# List of evaluation criteria categories for plotting
categories_plot <- c("Feasibility of Project",
                     "Relevance, Originality, Topicality",
                     "Suitability of Methods", "Track Record")

# Loop through each category and create/save plots
for (i in categories_plot) {
    cat("Analysing", i, "\n")

    dat_cateory <- filter(dat_avg_category, category == i)

    ggplot(dat_cateory, aes(
        x = mean,
        xmin = ci_lower, xmax = ci_upper,
        colour = ResearchArea,
        shape = ResearchArea,
        y = reorder(MainDisciplineLevel2, mean)
    )) +
        geom_point(size = 3) +
        geom_linerange(linewidth = 1.5) +
        scale_colour_manual(values = c("#3D7D9F", "#71B294", "#D9534F")) +
        scale_fill_manual(values = c("#3D7D9F", "#71B294", "#D9534F")) +
        scale_shape_manual(values = c(1, 15, 16)) +
        scale_x_continuous(
            expand = expansion(mult = c(0.1, 0.1)),
            labels = scales::percent_format(accuarcy = 1, scale = 1)
        ) +
        geom_text(
            data = dat_cateory, inherit.aes = FALSE, hjust = 0.5,
            size = 4, alpha = 0.8, nudge_y = 0.3,
            aes(
                x = mean, y = MainDisciplineLevel2,
                colour = ResearchArea,
                label = paste0(
                    sprintf("%.1f", mean),
                    " [", sprintf("%.1f", ci_lower), ", ",
                    sprintf("%.1f", ci_upper), "]"
                )
            )
        ) +
        labs(
            x = "Mean Prevalence (and 95% Confidence Intervals)",
            y = NULL, title = i,
            fill = NULL,
            colour = NULL,
            shape = NULL
        ) +
        theme(
            legend.position = "inside",
            legend.position.inside = c(0.75, 0.1),
            strip.text.y = element_text(margin = margin(l = 10), vjust = 0),
            legend.background = element_rect(fill = "white", colour = "grey50"), # White background with black border
            legend.box.background = element_rect(colour = "grey50")
        )
    ggsave(here("output", "plots_gender_disc", paste0("fig_mean_prev_bydomain_", str_to_lower(i), ".pdf")),
           width = 9, height = 12
    )
}

# -----------------------------------------------
# Table S6: Overview of Classifier Peformance
# -----------------------------------------------

# Function to read each CSV and add a filename column
read_and_label_csv <- function(file) {
    data <- read.csv(file)
    data$filename <- basename(file)
    return(data)
}

# List all test result CSV files for fine-tuning binary classification
all_files_test <- list.files(
    path = paste0(here("output", "fine_tuning", "binary", "test")),
    pattern = "\\.csv$",
    full.names = TRUE
)

# Set order for category levels in table
levels_category <- c(
    "Track Record",
    "Relevance, Originality, Topicality",
    "Suitability of Methods",
    "Feasibility of Project",
    "Positive",
    "Negative"
)

# Load and combine all filtered CSV files, recode category names
combined_data_test <- bind_rows(lapply(all_files_test, read_and_label_csv)) |>
    mutate(category = str_remove_all(filename, "\\.csv")) |>
    mutate(category = str_remove_all(category, "test_results_binary_")) |>
    mutate(category = dplyr::recode(category, !!!recode_categories)) |>
    filter(category %in% c("Feasibility of Project",
                           "Suitability of Methods",
                           "Track Record",
                           "Relevance, Originality, Topicality",
                           "Positive", "Negative")) |>
    mutate(category = dplyr::recode(category, "Relevance, Originality, Topicality" = "Rel., Orig., Topic.")) |> 
    select(Category = category,
           Acc. = accuracy,
           `Bal. Acc.` = balanced.accuracy,
           `F1` = f1.macro,
           `F1 Lab=1` = f1.label1,
           `F1 Lab=0` = f1.label0,
           `Prec.` = precision.macro,
           `Prec. Lab=1` = precision.label1,
           `Prec. Lab=0` = precision.label0,
           `Recall` = recall.macro,
           `Recall Lab=1` = recall.label1,
           `Recall Lab=0` = recall.label0) |>
    arrange(factor(Category, levels = levels_category))

# Round numeric columns to two digits for table
combined_data_test <- combined_data_test |>
    mutate(across(where(is.numeric), ~ format(round(., 2), nsmall = 2)))

# Save Table S6 as Word document
tt(combined_data_test) |>
    save_tt(here("output", "plots_gender_disc", "tab_s06.docx"),
            overwrite = TRUE)

# Save Table S6 as .tex file

tt(combined_data_test,
   caption = "\\textit{Overview of the classification test results based on binary classification}. Table shows Accuracy (Acc.), Balanced Accuracy (Bal. Acc.), the F1 score, Precision (Prec.), Recall (Rec.), and class-specific performance metrics for F1 score, Precision, and Recall for the presence (Lab=1) and absence (Lab=0) of a characteristic. Results are based on six separate fine-tuned transformer models. The metrics can range from 0 to 1 with higher values indicating better model classification. Detailed results and systematic comparisons of various classification approaches are presented in Okasa et al. \\cite{okasa2024}.\\label{tab:s06}") |>
    style_tt(fontsize = 1) |>
    save_tt(here("output", "plots_gender_disc", "tab_s06.tex"),
            overwrite = TRUE)



# -----------------------------------------------
# Table 2: Keyness Analysis
# -----------------------------------------------

# Load sentence-level data for keyness analysis
dat_keyness <- readRDS(here("data", "data_keyness.rds"))

# Remove reviews written in multiple languages (keep only English)
# These reviews would distort length and prevalence calculations
dat_multiple_1 <- read.csv(here("data", "data_multiple_languages.csv"), fileEncoding = "utf-8")
dat_multiple_2 <- read.csv(here("data", "data_multiple_languages_2.csv"), fileEncoding = "utf-8")

# Check for differences between the two datasets of multi-language reviews
length(setdiff(dat_multiple_2$Review_anon, dat_multiple_1$Review_anon))
length(setdiff(dat_multiple_1$Review_anon, dat_multiple_2$Review_anon))

# Combine and deduplicate multi-language review IDs
dat_multiple <- bind_rows(dat_multiple_1, dat_multiple_2) |>
    select(Review_anon) |>
    distinct()

nrow(dat_multiple)
length(unique(dat_multiple$Review_anon))

# Retrieve the substantive sample used in the analysis
dat_substantive <- dat_keyness |>
    filter(!Review_anon %in% dat_multiple$Review_anon) |>
    filter(ResponsibleApplicantGender != "x") |>
    filter(between(ResponsibleApplicantAgeAtSubmission, 18, 100)) |>
    filter(!CallEndDate %in% c("2024-04-03", "2023-10-02")) # remove calls without gender information

# create quanteda text corpus, create sample for collocation ananlysis
set.seed(213)
corp_sentences_substantive <- dat_substantive |>
    corpus(text_field = "sentence") |>
    corpus_sample(size = 50000)


# identify collocations
set.seed(245)
toks_coll <- corp_sentences_substantive |>
    tokens(
        remove_punct = TRUE,
        remove_numbers = TRUE,
        remove_symbols = TRUE,
        remove_url = TRUE, padding = TRUE,
        what = "word4" # use quanteda v 4 tokenizer
    ) |>
    tokens_remove(
        pattern = stopwords("en"),
        padding = TRUE
    )


# run collocation analysis for compounding
tstat_col <- textstat_collocations(toks_coll,
                                   min_count = 3, size = 2:4
) |>
    subset(z > 3)

# Transform to long data frame
dat_all_long_substantive <- dat_substantive |>
    select(ends_with("_label"), sentence, Review_anon, CallEndDate) |>
    pivot_longer(
        cols = -c(sentence, Review_anon, CallEndDate),
        names_to = "category", values_to = "prediction"
    )

dat_all_long_substantive |>
    group_by(category) |>
    summarise(mean(prediction))

# get two types of documents for loop
unique(dat_all_long_substantive$category)

cats_substantive <- c("criterion_track_record_label",
                      "criterion_relevance_originality_topicality_label",
                      "criterion_suitability_label",
                      "criterion_feasibility_label",
                      "positive_label", "negative_label")

# empty data frame to store keyness results
dat_keyness_words_substantive <- data.frame()

table(dat_all_long_substantive$category)

for (i in cats_substantive) {
    cat("Keyness analysis for", i, "\n")

    dat_cat <- filter(dat_all_long_substantive, category == i)

    dfmat_keyness <- dat_cat |>
        corpus(text_field = "sentence") |>
        tokens(remove_numbers = TRUE, remove_symbols = TRUE) |>
        tokens_compound(pattern = phrase("google scholar")) |>
        tokens_compound(pattern = phrase(tstat_col)) |>
        tokens_remove(pattern = stopwords("en")) |>
        dfm() |>
        dfm_keep(min_nchar = 2) |> # only terms with at least two characters
        dfm_group(groups = dat_cat$prediction) # group by prediction

    # run keyness analysis
    keyness <- textstat_keyness(dfmat_keyness, target = "1") |>
        arrange(-chi2) |> # arrange by chi2
        mutate(rank = 1:n()) |>
        filter(rank <= 100) # top 100 terms

    # create variable indicating the category
    keyness$category <- i

    # bind data frame
    dat_keyness_words_substantive <- bind_rows(keyness, dat_keyness_words_substantive)
}


# bind terms for each measure (unit of observation is not the measure, rather than the term)
dat_keyness_words_substantive_tab <- dat_keyness_words_substantive |>
    filter(rank <= 25) |>
    group_by(category) |>
    arrange(-chi2) |>
    summarise(Terms = paste(feature, collapse = ", ")) |>
    ungroup() |>
    mutate(category = str_remove_all(category, "_label")) |>
    mutate(category = dplyr::recode(category, !!!recode_categories))

table(dat_keyness_words_substantive_tab$category)


levels_category <- c(
    "Track Record",
    "Relevance, Originality, Topicality",
    "Suitability of Methods",
    "Feasibility of Project",
    "Positive",
    "Negative"
)

# arrange by categories
dat_keyness_words_substantive_tab_clean <- dat_keyness_words_substantive_tab |>
    arrange(factor(category, levels = levels_category)) |>
    rename(Category = category) |>
    rename(`Predictive Terms` = Terms)


dat_keyness_words_substantive_tab_clean

# Save Table 2 as Word document
tt(dat_keyness_words_substantive_tab_clean) |>
    group_tt(i = list(
        "Evaluation Criteria" = 1,
        "Sentiment" = 5)) |>
    style_tt(
        i = c(1, 6),
        bold = TRUE
    ) |>
    save_tt(here("output", "plots_gender_disc", "tab_02.docx"),
            overwrite = TRUE)

# Save Table 2 as tex file
# save table for papers
print(
    xtable(dat_keyness_words_substantive_tab_clean,
           label = "tab:02",
           align = c(
               "p{0.05\\textwidth}",
               "p{0.35\\textwidth}",
               "p{0.6\\textwidth}"
           ),
           caption = "\\textit{Frequent predictive terms for each content category.} The results are based on keyness analyses using $\\chi^2$ tests for each word or multi-word expression, comparing frequencies in sentences where a content characteristic was present (target group) with those where it was absent (reference group). The term ``unk'' is a placeholder for anonymized names of persons involved in a proposal."
    ),
    type = "latex",
    table.placement = "!h",
    caption.placement = "bottom",
    add.to.row = list(pos = list(1:5), command = "\\hdashline \n"),
    file = here("output", "plots_gender_disc", "tab_02.tex"),
    size = "small",
    include.rownames = FALSE
)
