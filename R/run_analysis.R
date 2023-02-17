# load libraries
library(tidyverse)   # v1.3.1
library(fastDummies) # v1.6.3
library(broomExtra)  # v4.3.2 
library(ggrepel)     # v0.9.1    
library(gghalves)    # v0.1.3
library(estimatr)    # v1.0.0   
library(lme4)        # v1.1-29
library(matrixStats) # v0.62.0
library(gtsummary)   # v1.6.1 
library(patchwork)   # v1.1.1 
library(sf)          # v1.0-7
library(scales)      # v1.2.0
library(pROC)        # v1.18.0
library(extrafont)   # v0.18 
loadfonts(device = "win")

# load functions
source("R/custom_functions.R")

# load survey data
df <- read_csv("data/surveydata.csv", guess_max = 1500)

# load census data (downloaded from https://www.e-stat.go.jp on 2022-07-08, "Update Date: 2021-11-30")
df_census <- read_csv2("data/jp_census_2020_age_sex_pref.csv") 

# load wvs longitudinal values data (based on "WVS_TimeSeries_1981_2022_Rds_v3_0.rds")
df_longitud <- read_csv("data/values_wvs_longitudinal.csv")

# load japan shape data frame (downloaded from https://data.humdata.org/ on 2022-07-08)
japan_lev1 <- readRDS("data/japan_lev1.rds")

# define outcome variables
outcome_EVI         <- c("EVI", "equal_educ", "homosexuality", "abortion", "divorce")
outcome_EVI_long    <- c("EVI", "equal_educ", "equal_polit", "equal_job", "homosexuality", "abortion", "divorce")
outcome_SVI         <- c("SVI", "rel_imp", "rel_person", "nation_pride", "parents_proud", "respect_author")
outcome             <- as.list(c(outcome_EVI, outcome_SVI))
outcome_long        <- as.list(c(outcome_EVI_long, outcome_SVI))
names(outcome)      <- unlist(outcome)
names(outcome_long) <- unlist(outcome_long)

# define covariates for adjustment
vars_cont     <- c("age", "children", "hhsize")
vars_nom_pref <- c("gender","education", "maritalst",  "townsize", "prefecture", "rel_service")
vars_nom_reg  <- c("gender","education", "maritalst",  "townsize", "region",     "rel_service")
vars_bfive    <- c(names(df)[grepl("bfive", names(df))], "distress")
vars_psych    <- c(vars_cont, vars_nom_reg, "hhincome", vars_bfive)

###############################
###############################
#### National-level Change ####
###############################
###############################

# filter for sufficiently complete cases
df_filt <- df %>% 
  drop_na(!!vars_cont, !!vars_nom_pref) %>% 
  filter(!(is.na(EVI) & is.na(SVI)))

# create new variables
df_filt <- df_filt %>% 
  mutate(WVS = ifelse(wave == "WVS7", 1, 0),
         VIC = ifelse(wave %in% c("VIC1", "VIC2"), 1, 0),
         EVI = EVI_reduced) # for this analysis, reduced version of EVI is needed from VIC respondents

# split in two datasets
df1 <- df_filt %>% filter(wave %in% c("WVS7", "VIC1"))
df2 <- df_filt %>% filter(wave %in% c("WVS7", "VIC2"))

# prepare census data to create population weights
df_census <- df_census %>% 
  pivot_longer(cols = !contains("p") & !contains("s"), names_to = "age", values_to = "pop") %>%
  mutate(age = as.numeric(age)) %>% 
  mutate(prefecture = str_remove(prefecture, "-ken")) %>%
  mutate(prefecture = case_when(
    prefecture == "Kyoto-fu" ~ "Kyoto",
    prefecture == "Oita"     ~ "Ooita",
    prefecture == "Osaka-fu" ~ "Osaka",
    prefecture == "Tokyo-to" ~ "Tokyo",
    prefecture == "Gumma"    ~ "Gunma",
    prefecture == "Ibaraki"  ~ "Ibaragi",
    TRUE ~ prefecture
  )) %>%
  filter(age >= 18) # adult population only
  
census <- df_census %>% mutate(age_group = case_when(
      age <= 35 ~ "18-35 years",
      age <= 50 ~ "36-50 years",
      TRUE      ~ "50+ years")) %>% 
  mutate(pop_total_above18 = sum(pop)) %>% 
  group_by(prefecture, sex, age_group) %>% 
  summarise(pop = sum(pop),
            pop_total_above18 = unique(pop_total_above18), .groups = "drop") %>% 
  mutate(stratum_prop_pop = pop / pop_total_above18) %>% 
  rename(gender = sex)

# estimate propensity and post-stratification weights
df1_wgts_added <- add_poststrat_wgts(census, df1, add_iptw = TRUE)
df1 <- df1_wgts_added[[1]]

# plotting covariate distributions in samples
df1$wave <- factor(df1$wave, ordered = TRUE, levels = c("WVS7", "VIC1"))

p_balance <- df1 %>%
  select(!!vars_nom_reg | !!vars_cont | wave) %>%
  mutate(age = as.character(cut_interval(age, length = 10)),
         hhsize = as.factor(as.character(hhsize)),
         children = as.factor(as.character(children))) %>%
  pivot_longer(cols = c(!!vars_nom_reg, !!vars_cont), names_to = "covariate", values_to = "value") %>%
  group_by(wave, covariate, value) %>%
  summarise(freq = n(), .groups = "drop") %>%
  group_by(wave, covariate) %>%
  mutate(total = sum(freq)) %>%
  ungroup %>%
  mutate(prop = freq/total) %>%
  mutate(covariate = factor(covariate, ordered = TRUE,
                            levels = c("age",
                                       "children",
                                       "hhsize",
                                       "gender",
                                       "education",
                                       "maritalst",
                                       "townsize",
                                       "region",
                                       "rel_service"
                                     ))) %>%
  mutate(covariate = fct_recode(covariate,
                                "Age"                        = "age",
                                "Gender"                     = "gender",
                                "No. of children"            = "children",
                                "Household size"             = "hhsize",
                                "Education"                  = "education",
                                "Marital status"             = "maritalst",
                                "Town size"                  = "townsize",
                                "Region"                     = "region",
                                "Prior religious attendance" = "rel_service"
                                )) %>%
  mutate(value = fct_recode(value,
                            "Primary or Junior high school"    = "Primary or Junior high school",
                            "High school"                      = "High school",
                            "Vocational school"                = "Vocational school/University-preparator",
                            "University-level"                 = "University-level education",
                            "Master or Doctoral degree"        = "Master or Doctoral degree")) %>%
  mutate(value = factor(value, ordered = TRUE, levels = c(
    
    "Primary or Junior high school",
    "High school",
    "Vocational school",
    "University-level",
    "Master or Doctoral degree",
    
    "Married",
    "Living together as married",
    "Separated",
    "Divorced",
    "Widowed",
    "Single",
    
    "Once a week or more",
    "Once a month",
    "Only on special holidays",
    "Once a year",
    "Less often or never",
    
    "Less than 5,000",
    "5,000-20,000",
    "20,000-100,000",
    "100,000-500,000",
    "500,000 and more",
    
    "None",
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "10",
    "11",
    "12",
    
    "Female",
    "Male",
    
    "[10,20]",
    "(20,30]",
    "(30,40]",
    "(40,50]",
    "(50,60]",
    "(60,70]",
    "(70,80]",
    "(80,90]",
    "(90,100]",
    
    "Hokkaido and Tohoku",
    "Kanto",
    "Chubu",
    "Kansai",
    "Chugoku",
    "Shikoku",
    "Kyushu and Okinawa"))) %>%
  ggplot() +
  geom_bar(aes(x = value, fill = wave, y = prop), stat = "identity", position = "dodge", width = 0.4) +
  facet_wrap(~ covariate, scales = "free", nrow = 3) +
  scale_y_continuous(labels = label_percent(accuracy = 1), breaks = seq(0,0.8,0.1)) +
  theme_minimal(base_size = 13) +
  theme(legend.key = element_blank()) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = unit(c(0,0,0,1.2), "cm"),
        axis.text.x = element_text(angle = 55, hjust = 1),
        axis.title.y = element_text(vjust = 4),
        text = element_text(family = "Segoe UI Semilight"),
        legend.position = "bottom",
        plot.title.position = "plot") +
  labs(x = "",
       y = "Relative frequency",
       fill = "Survey",
       title = "") +
  scale_fill_manual(values = c("black", "grey70"))

png("results/p_balance.png", width = 2700, height = 3300, res = 365)
print(p_balance)
dev.off()

# plotting propensity score overlap
propensit_stripe <- ggplot() +
  geom_line(data = df1, aes(x = wave, y = propensit_total), alpha = 0.03) +
  geom_jitter(data = df1, aes(x = wave, y = propensit_total), width = 0.08, height = 0, size = 0.5) +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        text = element_text(family = "Segoe UI Semilight")) +
  labs(y = "Estimated probability of being surveyed in WVS7",
       x = "",
       title = "a")

wvs_margin <- ggplot(df1 %>% filter(WVS == 1)) +
  geom_density(aes(x = propensit_total), show.legend = FALSE) +
  theme_void() +
  coord_flip() +
  scale_y_reverse() +
  theme(plot.title.position = "plot",
        text = element_text(family = "Segoe UI Semilight"))

vic1_margin <- ggplot(df1 %>% filter(WVS == 0)) +
  geom_density(aes(x = propensit_total), show.legend = FALSE) +
  theme_void() +
  coord_flip()

p_scores <- wvs_margin + propensit_stripe + vic1_margin + plot_layout(ncol = 3, nrow = 1)

# plotting standardized mean differences unweighted
df1_dummied_unweighted <- dummy_cols(
  df1 %>% select(wave, all_of(c(vars_cont, vars_nom_reg))),
  remove_selected_columns = TRUE)

stats_cont_unweighted <- df1_dummied_unweighted %>%
  group_by(wave) %>% 
  summarise(across(!contains("_"), 
                   list(mean=~mean(.x), var=~var(.x)), .names = "{.col}.{.fn}"))

stats_bin_unweighted <- df1_dummied_unweighted %>%
  group_by(wave) %>% 
  summarise(across(contains("_"), 
                   list(mean=~mean(.x), var=~mean(.x) * (1-mean(.x))), .names = "{.col}.{.fn}" ))

d_unweighted <- bind_cols(stats_cont_unweighted, stats_bin_unweighted %>% select(-wave)) %>% 
  pivot_longer(cols = contains("."),
               names_to = "covariate",
               values_to = "val") %>% 
  separate(covariate, into = c("covariate", "stat"), sep = "\\.") %>% 
  pivot_wider(names_from = "stat", values_from = "val") %>% 
  group_by(covariate) %>% 
  summarise(d = abs((mean[wave=="WVS7"] - mean[wave=="VIC1"]) /
                      sqrt((var[wave=="WVS7"] + var[wave=="VIC1"])/2)
  ))

# plotting standardized mean differences weighted
df1_dummied_weighted <- dummy_cols(
  df1 %>% mutate(wgt_combined_total = wgt_combined_total/mean(wgt_combined_total)) %>% 
    select(wave, wgt_combined_total, all_of(c(vars_cont, vars_nom_reg))),
  remove_selected_columns = TRUE)

stats_cont_weighted <- df1_dummied_weighted %>%
  group_by(wave) %>% 
  summarise(across(!contains("_"), 
                   list(mean = ~weighted.mean(.x, wgt_combined_total), 
                        var  = ~sum(wgt_combined_total)/(sum(wgt_combined_total)^2 - 
                                                           sum(wgt_combined_total^2)) * 
                          sum(wgt_combined_total*(.x - weighted.mean(.x, wgt_combined_total))^2) ), .names = "{.col}.{.fn}"))

stats_bin_weighted <- df1_dummied_weighted %>%
  group_by(wave) %>% 
  summarise(across(contains("_") & !contains("wgt_combined_total"), 
                   list(mean = ~weighted.mean(.x, wgt_combined_total),
                        var  = ~weighted.mean(.x, wgt_combined_total) * (1-weighted.mean(.x, wgt_combined_total))),
                   .names = "{.col}.{.fn}"
                   ))

d_weighted <- bind_cols(stats_cont_weighted, stats_bin_weighted %>% select(-wave)) %>% 
  pivot_longer(cols = contains("."),
               names_to = "covariate",
               values_to = "val") %>% 
  separate(covariate, into = c("covariate", "stat"), sep = "\\.") %>% 
  pivot_wider(names_from = "stat", values_from = "val") %>% 
  group_by(covariate) %>% 
  summarise(d = abs((mean[wave=="WVS7"] - mean[wave=="VIC1"])
                    / sqrt((var[wave=="WVS7"] + var[wave=="VIC1"])/2)))

p_smd <- rbind(d_unweighted %>% mutate(source = "Unweighted"),
               d_weighted %>% mutate(source = "Weighted")) %>% 
  mutate(covariate = fct_reorg(covariate,
                               "Age"                                                  = "age",
                               "No. of children"                                      = "children",
                               "Houseshold size"                                      = "hhsize",
                               "Gender: Male"                                         = "gender_Male",
                               "Gender: Female"                                       = "gender_Female",
                               "Education: Primary or Junior high school"             = "education_Primary or Junior high school",
                               "Education: High school"                               = "education_High school",
                               "Education: Vocational school"                         = "education_Vocational school/University-preparator",
                               "Education: University-level"                          = "education_University-level education",
                               "Education: Master or Doctoral degree"                 = "education_Master or Doctoral degree",
                               "Marital status: Married"                              = "maritalst_Married",
                               "Marital status: Living together as married"           = "maritalst_Living together as married",
                               "Marital status: Separated"                            = "maritalst_Separated",
                               "Marital status: Divorced"                             = "maritalst_Divorced",
                               "Marital status: Widowed"                              = "maritalst_Widowed",
                               "Marital status: Single"                               = "maritalst_Single",
                               "Town size: Less than 5,000"                           = "townsize_Less than 5,000",
                               "Town size: 5,000-20,000"                              = "townsize_5,000-20,000",
                               "Town size: 20,000-100,000"                            = "townsize_20,000-100,000",
                               "Town size: 100,000-500,000"                           = "townsize_100,000-500,000",
                               "Town size: 500,000 and more"                          = "townsize_500,000 and more",
                               "Region: Hokkaido and Tohoku"                          = "region_Hokkaido and Tohoku",
                               "Region: Kanto"                                        = "region_Kanto",
                               "Region: Chubu"                                        = "region_Chubu",
                               "Region: Kansai"                                       = "region_Kansai",
                               "Region: Chugoku"                                      = "region_Chugoku",
                               "Region: Shikoku"                                      = "region_Shikoku",
                               "Region: Kyushu and Okinawa"                           = "region_Kyushu and Okinawa",
                               "Prior religious attendance: Once a week or more"      = "rel_service_Once a week or more",
                               "Prior religious attendance: Once a month"             = "rel_service_Once a month",
                               "Prior religious attendance: Only on special holidays" = "rel_service_Only on special holidays",
                               "Prior religious attendance: Once a year"              = "rel_service_Once a year",
                               "Prior religious attendance: Less often or never"      = "rel_service_Less often or never")) %>%
  mutate(source = factor(source, ordered = TRUE, levels = c("Weighted", "Unweighted")),
         covariate = fct_rev(covariate)) %>% 
  ggplot(aes(x = covariate, y = d, group = source, color = source)) +
  geom_line(lwd = 0.2) +
  geom_point(size = 2, pch = 18) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(legend.key = element_blank()) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y        = element_text(hjust = 0, size = 9),
        text = element_text(family = "Segoe UI Semilight"),
        legend.position = "bottom",
        plot.title.position = "plot") +
  labs(x = "",
       y = "Abs. std. mean difference",
       col = "",
       title = "b") +
  scale_color_manual(values = c("grey70", "black"))

p_overlap <- p_scores + p_smd + plot_layout(ncol = 4, widths = c(1/3, 6/5, 1/3, 7/4))

png("results/p_overlap.png", width = 3300, height = 2500, res = 410)
print(p_overlap)
dev.off()

# wvs7 vs. vic1: model estimation
mods <- map_dfr(outcome, .id = "outcome", function(x){
  
  if(x %in% outcome_EVI){
    data <- df1 %>% filter(!is.na(EVI))
    data$wgt <- data$wgt_combined_EVI
    
    
  } else{
    data <- df1 %>% filter(!is.na(SVI))
    data$wgt <- data$wgt_combined_SVI
  }
  
  m <- lm_robust(as.formula(paste(x, "~ VIC +",
                                  paste(c(vars_cont, vars_nom_reg, "age_sq"),
                                        collapse = "+"), sep ="")),
                 data = data,
                 weights = wgt/mean(wgt),
                 se_type = "stata")
  
  bind_cols(tidy(m), glance(m))})

# forest plots
forestA <- forestplots(mods      = mods %>% filter(outcome %in% outcome_EVI),
                       version   = "EVI",
                       color     = "slateblue",
                       plotlabel = "b",
                       scale_lim = c(-24,12),
                       suppr_x   = FALSE,
                       coef_of   = "VIC")

forestB <- forestplots(mods      = mods %>% filter(outcome %in% outcome_SVI),
                       version   = "SVI",
                       color     = "darkorange",
                       plotlabel = "d",
                       scale_lim = c(-24,12),
                       suppr_x   = FALSE,
                       coef_of   = "VIC")

# violin plots
violinA <- violinplots(data      = df1 %>% filter(!is.na(EVI)), 
                       weights   = "wgt_combined_EVI",
                       color     = "slateblue",
                       y         = "EVI",
                       y_label   = "EMANCIPATIVE VALUES",
                       plotlabel = "a")

violinB <- violinplots(data      = df1 %>% filter(!is.na(SVI)),
                       weights   = "wgt_combined_SVI",
                       color     = "darkorange",
                       y         = "SVI",
                       y_label   = "SECULAR VALUES",
                       plotlabel = "c")

p_coeff_models1 <- ((violinA/violinB) | (forestA/forestB)) + plot_layout(widths = c(4/5, 2))
png("results/p_coeff_models1.png", width = 3550, height = 1600, res = 380)
print(p_coeff_models1)
dev.off()

# wvs7 vs. vic2: model estimation (remaining sample)
df2_remained <- df_filt %>% filter(participation %in% c("W0 only", "Both W1 and W2") & wave %in% c("WVS7", "VIC2"))
df2_remained <- add_poststrat_wgts(census, df2_remained, add_iptw = TRUE)[[1]]

mods_vic2_remained <- map_dfr(outcome, .id = "outcome", function(x){
  
  if(x %in% outcome_EVI){
    data <- df2_remained %>% filter(!is.na(EVI))
    data$wgt <- data$wgt_combined_EVI
    
    
  } else{
    data <- df2_remained %>% filter(!is.na(SVI))
    data$wgt <- data$wgt_combined_SVI
  }
  
  m <- lm_robust(as.formula(paste(x, "~ VIC +",
                                  paste(c(vars_cont, vars_nom_reg, "age_sq"),
                                        collapse = "+"), sep ="")),
                 data = data,
                 weights = wgt/mean(wgt),
                 se_type = "stata")
  
  bind_cols(tidy(m), glance(m))})

# wvs7 vs. vic2: model estimation (remaining sample + refreshment sample)
df2 <- add_poststrat_wgts(census, df2, add_iptw = TRUE)[[1]]

mods_vic2_full <- map_dfr(outcome, .id = "outcome", function(x){
  
  if(x %in% outcome_EVI){
    data <- df2 %>% filter(!is.na(EVI))
    data$wgt <- data$wgt_combined_EVI
    
    
  } else{
    data <- df2 %>% filter(!is.na(SVI))
    data$wgt <- data$wgt_combined_SVI
  }
  
  m <- lm_robust(as.formula(paste(x, "~ VIC +",
                                  paste(c(vars_cont, vars_nom_reg, "age_sq"),
                                        collapse = "+"), sep ="")),
                 data = data,
                 weights = wgt / mean(wgt),
                 se_type = "stata")
  
  bind_cols(tidy(m), glance(m))})

# plotting comparison over repeated surveys
modres <- rbind(mods %>% 
                  mutate(data = "VIC1 - weighted", time = "VIC1"),
                mods_vic2_remained %>% 
                  mutate(data = "VIC1 - weighted", time = "VIC2"),
                mods_vic2_full %>% 
                  mutate(data = "VIC2 - weighted", time = "VIC2")) %>% 
  filter(term == "VIC") %>% 
  select(outcome, estimate, conf.low, conf.high, data, time) %>% 
  rbind(
    tibble(outcome = unique(.$outcome),
           estimate = 0,
           conf.low = NA,
           conf.high = NA,
           data = "VIC1 - weighted",
           time = "WVS7"))

timeA <- timeplot(data = modres,
                  version = "EVI",
                  plotlabel = "a",
                  color = "slateblue",
                  scale_lim = c(-20,20),
                  suppr_y = FALSE)

timeB <- timeplot(data = modres,
                  version = "SVI",
                  plotlabel = "b",
                  color = "darkorange",
                  scale_lim = c(-25,20),
                  suppr_y = FALSE)

p_repeated <- timeA / timeB
png("results/p_repeated.png", width = 4300, height = 2500, res = 425)
print(p_repeated)
dev.off()

##############################################
##############################################
#### Prefecture-level Variation in Change ####
##############################################
##############################################

# prefecture interaction effect: model estimation
mods_pref <- map_dfr(outcome, .id = "outcome", function(x){

  if(x %in% outcome_EVI){
    data <- df1 %>% filter(!is.na(EVI))
    data$wgt <- data$wgt_combined_EVI
    
    
  } else{
    data <- df1 %>% filter(!is.na(SVI))
    data$wgt <- data$wgt_combined_SVI
  }
  
  m <- lm_robust(as.formula(paste(x, "~ VIC * cum_infected_in_pref_VIC1 +", 
                                  paste(c(vars_cont, vars_nom_reg, "age_sq"), 
                                        collapse = "+"), sep ="")),
                 data = data,
                 weights = wgt/mean(wgt),
                 clusters = prefecture, 
                 se_type = "stata"
  ) 
  bind_cols(tidy(m), glance(m))})

# forest plots
forest_intc_A <- forestplots(mods      = mods_pref %>% filter(outcome %in% outcome_EVI),
                             version   = "EVI", 
                             color     = "slateblue", 
                             plotlabel = "a",
                             scale_lim = c(-0.7,0.7),
                             digits    = 2,
                             suppr_x   = FALSE,
                             coef_of   = "VIC:cum_infected_in_pref_VIC1",
                             xlabel    = expression(paste("Interaction: ", Delta, " WVS7-VIC1 x infections in prefecture")))

forest_intc_B <- forestplots(mods      = mods_pref %>% filter(outcome %in% outcome_SVI), 
                             version   = "SVI", 
                             color     = "darkorange", 
                             plotlabel = "b",
                             scale_lim = c(-0.7,0.7),
                             digits    = 2,
                             suppr_x   = FALSE,
                             coef_of   = "VIC:cum_infected_in_pref_VIC1",
                             xlabel    = expression(paste("Interaction: ", Delta, " WVS7-VIC1 x infections in prefecture")))

p_coeff_models2 <- (forest_intc_A + plot_layout(widths = c(5/4, 1))) /
  (forest_intc_B + plot_layout(widths = c(5/4, 1)))

png("results/p_coeff_models2.png", width = 2800, height = 1500, res = 365)
print(p_coeff_models2)
dev.off()

# prediction plot for interaction term
pred_ticks <- 300

pred_values <- map_dfr(outcome, .id = "outcome", function(x){
  
  if(x %in% outcome_EVI){
    data <- df1 %>% filter(!is.na(EVI))
    data$wgt <- data$wgt_combined_EVI
    
    
  } else{
    data <- df1 %>% filter(!is.na(SVI))
    data$wgt <- data$wgt_combined_SVI
  }
  
  m <- lm_robust(as.formula(paste(x, "~ VIC * cum_infected_in_pref_VIC1 +",
                                  paste(c(vars_cont, vars_nom_reg, "age_sq"),
                                        collapse = "+"), sep ="")),
                 data = data,
                 weights = wgt/mean(wgt),
                 clusters = prefecture,
                 se_type = "stata")
  
  data_wvs <- data %>% filter(wave == "WVS7")
  
  newdata <- tibble(
    VIC                       = c(rep(0,pred_ticks),rep(1,pred_ticks)),
    cum_infected_in_pref_VIC1 = rep(seq(0, 50, length.out=pred_ticks),2),
    age                       = rep(weightedMedian(data_wvs$age,         data_wvs$wgt),    pred_ticks*2),
    age_sq                    = rep(weightedMedian(data_wvs$age,         data_wvs$wgt)^2,  pred_ticks*2),
    hhsize                    = rep(weightedMedian(data_wvs$hhsize,      data_wvs$wgt),    pred_ticks*2),
    children                  = rep(weighted_mode(data_wvs$children),    pred_ticks*2),
    gender                    = rep(weighted_mode(data_wvs$gender),      pred_ticks*2),
    region                    = rep(weighted_mode(data_wvs$region),      pred_ticks*2),
    townsize                  = rep(weighted_mode(data_wvs$townsize),    pred_ticks*2),
    maritalst                 = rep(weighted_mode(data_wvs$maritalst),   pred_ticks*2),
    education                 = rep(weighted_mode(data_wvs$education),   pred_ticks*2),
    rel_service               = rep(weighted_mode(data_wvs$rel_service), pred_ticks*2))
  
  
  as_tibble(predict(m, newdata, interval = "confidence")$fit) %>%
    mutate(VIC = newdata$VIC,
           cum_infected_in_pref_VIC1 = newdata$cum_infected_in_pref_VIC1)
})

pred_intA <- pred_intct_cases(preds     = pred_values,
                              data      = df1,
                              version   = "EVI",
                              color     = "slateblue",
                              y_label   = "EMANCIPATIVE VALUES",
                              plotlabel = "a")

pred_intB <- pred_intct_cases(pred_values,
                              df1,
                              "SVI",
                              "darkorange",
                              "SECULAR VALUES",
                              "b")

p_predvalues <- (pred_intA | pred_intB) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

png("results/p_predvalues.png", width = 3000, height = 1900, res = 420)
print(p_predvalues)
dev.off()

#################################################
#################################################
#### Individual-level Psychological Distress ####
#################################################
#################################################

# between-individual analysis
df3 <-  df %>% 
  drop_na(all_of(vars_psych)) %>% 
  filter(!(is.na(EVI) & is.na(SVI))) %>% 
  group_by(subject_id) %>% 
  mutate(times = n()) %>% 
  filter(wave != "WVS7") %>% 
  ungroup %>% 
  filter(times == 1 | (times == 2 & wave == "VIC1")) # only allow one data point per respondent

df3 <- add_poststrat_wgts(census, df3, add_iptw = FALSE)

# psychological distress effect: model estimation (between)
mods_psych <- map_dfr(outcome_long, .id = "outcome", function(x){
  
  if(x %in% outcome_EVI_long){
    data <- df3 %>% filter(!is.na(EVI))
    data$wgt <- data$popwgt_EVI

  } else{
    data <- df3 %>% filter(!is.na(SVI))  
    data$wgt <- data$popwgt_SVI
  }
  
  m <- lm_robust(as.formula(paste(x, "~ distress + wave +", paste(c(vars_psych),
                                                                  collapse = "+"), sep ="")),
                 data = data,
                 weights = wgt/mean(wgt),
                 se_type = "stata"
  )
  
  bind_cols(tidy(m), glance(m))
  }) %>% mutate(analysis = NA)

# forest plots (between)
forestA <- forestplots_psych(mods      = mods_psych %>% filter(outcome %in% outcome_EVI_long),
                             version   = "EVI", 
                             color     = "slateblue", 
                             plotlabel = "a",
                             scale_lim = c(-6,7),
                             suppr_x   = FALSE,
                             coef_of   = "distress",
                             xlabel    = "Effect of psycholgical distress")

forestB <- forestplots_psych(mods      = mods_psych %>% filter(outcome %in% outcome_SVI), 
                             version   = "SVI", 
                             color     = "darkorange", 
                             plotlabel = "b",
                             scale_lim = c(-6,7),
                             suppr_x   = FALSE,
                             coef_of   = "distress",
                             xlabel    = "Effect of psycholgical distress")

p_coeff_models3 <- (forestA + plot_layout(widths = c(5/4, 1))) /
  (forestB + plot_layout(widths = c(5/4, 1)))

png("results/p_coeff_models3.png", width = 2800, height = 1500, res = 365)
print(p_coeff_models3)
dev.off()

# within-individual analysis
# psychological distress effect: model estimation (within)
mods_psych_within <- map_dfr(outcome_long, .id = "outcome", function(x){

  if(x %in% outcome_EVI_long){
    data <- df %>% filter(!is.na(EVI)) %>% 
      group_by(subject_id) %>%
      mutate(times = n()) %>%
      filter(wave != "WVS7") %>%
      ungroup %>%
      filter(times == 2) # only allow respondents with two data points

    data <- add_poststrat_wgts(census, data, add_iptw = FALSE)
    
    data$wgt <- data$popwgt_EVI


  } else{
    data <- df %>% filter(!is.na(SVI)) %>% 
      group_by(subject_id) %>%
      mutate(times = n()) %>%
      filter(wave != "WVS7") %>%
      ungroup %>%
      filter(times == 2) # only allow respondents with two data points
    
    data <- add_poststrat_wgts(census, data, add_iptw = FALSE)
    
    data$wgt <- data$popwgt_SVI
  }

  m <- lm_robust(as.formula(paste(x, "~ distress + wave")),
                 data = data,
                 fixed_effects = subject_id,
                 weights = wgt/mean(wgt),
                 se_type = "stata"
  )

  bind_cols(tidy(m), glance(m))
}) %>% mutate(analysis = NA,
              nobs = nobs/2) # sample size as subjects

# forest plots
forestA <- forestplots_psych(mods      = mods_psych_within %>% filter(outcome %in% outcome_EVI_long),
                             version   = "EVI",
                             color     = "slateblue",
                             plotlabel = "a",
                             scale_lim = c(-6,7),
                             suppr_x   = FALSE,
                             coef_of   = "distress",
                             xlabel    = "Effect of psycholgical distress")

forestB <- forestplots_psych(mods      = mods_psych_within %>% filter(outcome %in% outcome_SVI),
                             version   = "SVI",
                             color     = "darkorange",
                             plotlabel = "b",
                             scale_lim = c(-6,7),
                             suppr_x   = FALSE,
                             coef_of   = "distress",
                             xlabel    = "Effect of psycholgical distress")

p_coeff_models4 <- (forestA + plot_layout(widths = c(5/4, 1))) /
  (forestB + plot_layout(widths = c(5/4, 1)))

png("results/p_coeff_models4.png", width = 2800, height = 1500, res = 365)
print(p_coeff_models4)
dev.off()

#################################
#################################
#### More Figures and Tables ####
#################################
#################################

# add year of japan's survey implementations for each wave
df_longitud  <- df_longitud %>%  mutate(surveyyear_jpn = case_when(
  wave == "WVS3" ~ 1995,
  wave == "WVS4" ~ 2000,
  wave == "WVS5" ~ 2005,
  wave == "WVS6" ~ 2010,
  wave == "WVS7" ~ 2019))

# line plot of japan's trajectory
p_jpn <- df_longitud %>% 
  filter(countryname == "Japan") %>% 
  pivot_longer(cols = c(EVI, SVI),
               names_to = "value", values_to = "score") %>% 
  mutate(surveyyear_jpn = paste0(wave, "\n(", surveyyear_jpn, ")"),
         value = fct_reorder(ifelse(value == "EVI", "EMANCIPATIVE VALUES", "SECULAR VALUES"), score, .desc = TRUE)) %>% 
  ggplot(aes(x = surveyyear_jpn, y = score, col = value, group = value)) + 
  geom_point(size = 2.5) +
  geom_text(aes(label = format(round(score,1),nsmall=1)),
            vjust = -1,
            size = 4,
            family = "Segoe UI Semilight",
            show.legend = FALSE) +
  geom_line( show.legend = FALSE) + 
  scale_y_continuous(limits = c(30,80), breaks = seq(0,100,10)) + 
  scale_color_manual(values = c("darkorange", "slateblue")) + 
  theme_minimal(base_size = 13) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = c(0.5, 0.15),
        text = element_text(family = "Segoe UI Semilight")) +
  labs(col = "",
       y = "Japan national average",
       x = "Survey wave")

# scatter plot of nations
set.seed(123)
p_nations <- df_longitud %>% 
  mutate(is_japan = ifelse(countryname == "Japan", TRUE, FALSE)) %>% 
  filter(wave=="WVS7") %>% 
  ggplot() + 
  geom_point(aes(x = SVI, y = EVI, col = is_japan), size = 2.5, show.legend = FALSE) + 
  geom_label_repel(data = df_longitud %>% 
                     filter(countryname == "Japan", wave=="WVS7"),
                   aes(x = SVI, y = EVI, label = countryname), col = "brown", size = 2.5,
                   family = "Segoe UI Semilight") + 
  
  geom_text_repel(data = df_longitud %>% 
                    filter(countryname != "Japan", wave=="WVS7"),
                  aes(x = SVI, y = EVI, label = countryname), col = "black", size = 2.5, max.overlaps = 200,
                  show.legend = FALSE,
                  box.padding = 0.25,
                  family = "Segoe UI Semilight") + 
  scale_y_continuous(limits = c(0,80), breaks = seq(0,100,20)) + 
  scale_x_continuous(limits = c(0,80), breaks = seq(0,100,20)) + 
  theme_minimal(base_size = 13) + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        text = element_text(family = "Segoe UI Semilight")) + 
  labs(x =  "SECULAR VALUES",
       y = "EMANCIPATIVE VALUES") + 
  scale_color_manual(values = c("black", "brown"))

p_nations_trend <- (p_nations  + labs(title = "a")+ theme(plot.title.position = "plot")) +
  (p_jpn + labs(title = "b")+ theme(plot.title.position = "plot"))

png("results/p_nations_trend.png", width = 4400, height = 2300, res = 430)
print(p_nations_trend)
dev.off()

# table with summary statistics of all covariates
sumtable_unwgt <- create_summary_tab(df_filt)
sumtable_wgt   <- create_summary_tab(df1, use_wgt = TRUE, wgt_var = "wgt_combined_total")

sumtable_unwgt <- sumtable_unwgt$table_body %>% 
  select(Characteristic = label, 
         WVS7 = stat_1,
         VIC1 = stat_2,
         VIC2 = stat_3)

sumtable_wgt <- sumtable_wgt$table_body %>% 
  select("WVS7 (weighted)" = stat_1,
         "VIC1 (weighted)" = stat_2)

sum_tab <- bind_cols(sumtable_unwgt, sumtable_wgt)

# add true census population data for few variables to table
truepop_reg <- df_census %>% 
  left_join(df %>% select(prefecture, region) %>% distinct) %>% 
  mutate(pop_total = sum(pop)) %>% 
  group_by(var=region) %>% 
  summarise(truepop = paste0(round(sum(pop)/unique(pop_total)*100,0), "%"))

truepop_gender <- df_census %>% 
  mutate(pop_total = sum(pop)) %>% 
  group_by(var=sex) %>% 
  summarise(truepop = paste0(round(sum(pop)/unique(pop_total)*100,0), "%"))

truepop_age <- df_census %>% 
  summarise(var = "Age",
            truepop = round(weighted.mean(age, pop),1))

truepop <- rbind(truepop_reg, truepop_gender, truepop_age) %>% 
  rename(Characteristic = var,
         `Census 2020` = truepop)

sum_tab <- left_join(sum_tab, truepop) %>% 
  mutate(across(.cols = everything(), as.character)) %>% 
  mutate(across(.cols = everything(), ~ifelse(is.na(.x) | .x %in% c("NA (NA)", "0.0 (0.0)"), "", .x))) %>% 
  filter(Characteristic != "(Missing)")%>% 
  add_row(tibble(
    Characteristic = "N",
    WVS7 = as.character(table(df_filt$wave)["WVS7"]),
    VIC1 = as.character(table(df_filt$wave)["VIC1"]),
    VIC2 = as.character(table(df_filt$wave)["VIC2"]),
    `WVS7 (weighted)` = "",
    `VIC1 (weighted)` = "",
    `Census 2020` = ""),
    .before = 1)

saveRDS(sum_tab, file = "results/sum_tab.rds")

# table with propensity model results
propmod_tab <- tbl_regression(df1_wgts_added[[2]],
                              label = list(
                                `(Intercept)` ~ "Intercept",
                                age           ~ "Age",
                                gender        ~ "Gender",
                                children      ~ "No. of children",
                                hhsize        ~ "Household size",
                                education     ~ "Education",
                                maritalst     ~ "Marital status",
                                rel_service   ~ "Prior religious attendance",
                                prefecture    ~ "Prefecture",
                                townsize      ~ "Town size"),
                              intercept = TRUE) %>% 
  add_glance_table(include = c(nobs, logLik, deviance, df.residual, null.deviance, df.null)) %>% 
  modify_column_hide(columns = c(p.value)) %>%
  modify_column_unhide(columns = std.error) %>% 
  modify_header(label = "Predictor",
                estimate = "Coefficient",
                std.error = "Standard error") %>%
  modify_footnote(everything() ~ NA, abbreviation = TRUE) %>%
  modify_caption("Estimated coefficients in the logistic regression model for predicting propensity scores.")

propmod_tab <- propmod_tab$table_body %>% 
  select(var_label, label, estimate, std.error) %>%
  mutate(across(.cols = where(is.numeric), ~
                  ifelse(is.na(.x), NA, format(round(.x, 3), nsmall = 3)))) %>% 
  mutate(estimate = ifelse(is.na(estimate) & var_label != label, "-", estimate)) %>% 
  filter(!is.na(estimate) | (var_label == label)) %>% 
  select(-var_label) %>% 
  mutate(across(.cols = everything(), ~ifelse(is.na(.x), "", .x))) %>% 
  mutate(estimate = ifelse(label %in% c("No. Obs.", 
                                        "Residual df",
                                        "Null df",
                                        "logLik"),
                           str_replace(estimate,"\\...[0-9]",""), estimate)) %>% 
  add_row(tibble(
    label = "AUC",
    estimate = as.character(
      round(auc(roc(df1_wgts_added[[2]]$model$WVS, predict(df1_wgts_added[[2]]))),3)),
    std.error = ""
  ))

saveRDS(propmod_tab, file = "results/propmod_tab.rds")

# plotting infections by prefecture on map
df_japan_lev1 <- fortify(japan_lev1 , region = "ADM1_EN") %>%
  mutate(ADM1_EN = str_sub(ADM1_EN, 2, 100)) %>%
  mutate(prefecture = case_when(
    ADM1_EN == "Hyōgo"   ~ "Hyogo",
    ADM1_EN == "Ibaraki" ~ "Ibaragi",
    ADM1_EN == "Kōchi"   ~ "Kochi",
    ADM1_EN == "Ōita"    ~ "Ooita",
    TRUE ~ ADM1_EN
  ))%>% 
  left_join(df %>% group_by(prefecture) %>% summarise(cum_infected_in_pref_VIC1 = unique(cum_infected_in_pref_VIC1)), by = "prefecture")

set.seed(1234)
p_map <- df_japan_lev1 %>% 
  mutate(CENTROID = map(geometry, st_centroid),
         COORDS = map(CENTROID, st_coordinates),
         COORDS_X = map_dbl(COORDS, 1),
         COORDS_Y = map_dbl(COORDS, 2)) %>% 
  ggplot() +
  geom_sf(aes(fill = cum_infected_in_pref_VIC1)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    text = element_text(family = "Segoe UI Semilight"),
    legend.position = "bottom"
  ) + 
  labs(fill = "Infections in prefecture\n(per 100,000)", x = "", y = "") + 
  scale_fill_gradient(low = "lightblue",
                      high = "red") +
  geom_text_repel(aes(x = COORDS_X, y = COORDS_Y, label = prefecture),
                  max.overlaps = 100, size = 4,
                  col = "grey50",
                  box.padding = 1.25,
                  point.padding = 0.1,
                  segment.size = 0.25,
                  family = "Segoe UI Semilight")

png("results/p_map.png", width = 3000, height = 3000, res = 300)
p_map
dev.off()

#####################################
#####################################
#### Some more simple statistics ####
#####################################
#####################################

# descriptive statistics psychological distress
summary(df$distress)
# Min.    1st Qu.   Median    Mean    3rd Qu.    Max.    NA's 
# 1.000   1.000     1.400     1.673   2.000     4.000    1353 
sd(df$distress, na.rm = TRUE) # 0.8319614

# correlations cumulative infections at VIC 1 with other prefecture data
df %>% 
  filter(!duplicated(prefecture)) %>% 
  summarise(cor(cum_infected_in_pref_VIC1, cum_deaths_in_pref_VIC1), # 0.904
            cor(cum_infected_in_pref_VIC1, emergency_in_pref_VIC1))  # 0.620

# correlations of age and EVI/SVI among WVS7 respondents
df %>% 
  filter(wave == "WVS7") %>% 
  summarise(cor(age, EVI, use = "pair"), # -0.327 
            cor(age, SVI, use = "pair")) # -0.282

# distress comparisons between retained and drop-outs, as well as retained and replacements
t.test(df$distress[df$participation=="Both W1 and W2" & df$wave == "VIC1"], 
       df$distress[df$participation=="W1 only"]) # 1.653348  1.747406, p-value = 0.002621
t.test(df$distress[df$participation=="Both W1 and W2" & df$wave == "VIC2"], 
       df$distress[df$participation=="W2 only"]) # 1.611052  1.735957, p-value = 0.0001223

# EVI and SVI comparisons between retained and drop-outs
t.test(df$SVI[df$participation=="Both W1 and W2" & df$wave == "VIC1"], 
       df$SVI[df$participation=="W1 only"]) # 66.46298  65.16100, p-value = 0.02645
t.test(df$EVI[df$participation=="Both W1 and W2" & df$wave == "VIC1"], 
       df$EVI[df$participation=="W1 only"]) # 61.60901  61.47221, p-value = 0.8416

# explore distress deltas for retained
delta_distress <- df %>% 
  filter(participation=="Both W1 and W2") %>% 
  select(subject_id, wave, distress) %>% 
  pivot_wider(names_from = wave, values_from = distress) %>% 
  mutate(delta_distress = VIC2-VIC1) %>% 
  pull(delta_distress)
t.test(delta_distress, mu = 0) # mean: -0.04229543, p-value = 0.02011

# estimate ICC of distress between waves for repeated respondents
lmm <- lmer(
  distress ~ (1|subject_id),
  data = df %>% 
    group_by(subject_id) %>%
    filter(n()>1) %>%
    ungroup)

vars <- as_tibble(summary(lmm)$varcor)
vars$vcov[vars$grp=="subject_id"] / 
  (vars$vcov[vars$grp=="subject_id"] + vars$vcov[vars$grp=="Residual"]) # 0.5259601

# the single respondent for illustration (Appendix A)
df1 %>% 
  filter(subject_id == "VIC706") %>% 
  select(all_of(c(vars_cont, vars_nom_pref)), 
         propensit_total, wgt_att_total, stratum_prop_samp_total, stratum_prop_pop, popwgt_total)

# between-individual analysis of distress when removing nation_pride from SVI
data <- df3 %>% filter(!is.na(SVI))  
data$wgt <- data$popwgt_SVI

data$defiance <- (data$parents_proud + data$respect_author) / 2
data$agnostic <- (data$rel_imp + data$rel_person) / 2
data$SVI_no_nation <- (data$defiance + data$agnostic) / 2

x <- "SVI_no_nation"
m <- lm_robust(as.formula(paste(x, "~ distress + wave +", paste(c(vars_psych),
                                                                collapse = "+"), sep ="")),
               data = data,
               weights = wgt/mean(wgt),
               se_type = "stata"
               )
tidy(m) %>% filter(term == "distress") # -1.296828 (-2.057364 -0.5362924)
