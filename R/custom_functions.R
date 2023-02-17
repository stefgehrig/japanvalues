# labels for SVI items
rel_imp_label        <- "Religion not imp."
rel_person_label     <- "Not a religious person"
nation_pride_label   <- "Not proud of nation"
parents_proud_label  <- "Parents proud not goal"
respect_author_label <- "Disrespect authority"

# factor coding and ordering in one function from https://rdrr.io/github/svmiller/stevemisc/src/R/misc.R
fct_reorg <- function(fac, ...) { 
  fct_recode(fct_relevel(fac, ...), ...)
}

# strict rounding
round1 <- function(x, digits){format(round(x,digits), nsmall = digits, trim = TRUE)}

# forest plots
forestplots <- function(mods, version, color, plotlabel, scale_lim, suppr_x, coef_of, xlabel = NA, digits = 1){
  if(version == "EVI"){
    
    # patch 1
    mods %>% 
      filter(term == coef_of) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c("EVI", 
                                                                      "equal_educ", 
                                                                      "homosexuality",
                                                                      "abortion", 
                                                                      "divorce")))) %>% 
      mutate(outcome = fct_recode(outcome,
                                  "EMANCIPATIVE VALUES"            = "EVI",
                                  "Gender equality education"      = "equal_educ", 
                                  "Acceptance homosexuality"       = "homosexuality",
                                  "Acceptance abortion"            =  "abortion", 
                                  "Acceptance divorce"             = "divorce")) %>% 
      
      ggplot() + 
      geom_errorbar(aes(x = outcome, 
                        ymin = conf.low, 
                        ymax = conf.high,
                        alpha = index),
                    width = 0.25,
                    lwd = 1, 
                    col = color,
                    show.legend = FALSE) +
      geom_point(aes(y = estimate, x = outcome, alpha = index), 
                 pch = 18, size = 3, show.legend = FALSE, col = color) +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, col = "grey50") +
      theme_minimal(base_size = 13) + 
      coord_flip() +
      scale_alpha_discrete(range = c(0.4,1)) +
      scale_y_continuous(limits = scale_lim) -> p1_temp
    
    if(is.na(xlabel)){
      p1_temp + labs(x = "",
                     y = expression(paste(Delta, " WVS7-VIC1")),
                     title = plotlabel) -> p1
    } else {
      p1_temp + labs(x = "",
                     y = xlabel,
                     title = plotlabel) -> p1
    }
    
    if(suppr_x == FALSE){
      p1 + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y        = element_text(hjust = 1),
              text = element_text(family = "Segoe UI Semilight"),
              plot.title.position = "plot") -> p1
    }else{
      p1 + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y        = element_text(hjust = 1),
              axis.title.x       = element_blank(), 
              text = element_text(family = "Segoe UI Semilight"),
              plot.title.position = "plot") -> p1
    }
    
    # patch 2
    mods %>% 
      filter(term == coef_of) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c("EVI", 
                                                                      "equal_educ", 
                                                                      "homosexuality",
                                                                      "abortion", 
                                                                      "divorce")))) %>% 
      mutate(label = paste0(round1(estimate, digits), " (", round1(conf.low, digits), "; ", round1(conf.high, digits), ")")) %>% 
      ggplot() + 
      geom_text(aes(y = 0, x = outcome, label = label), 
                hjust = "left", 
                family = "Segoe UI Semilight",
                size = 3.5) + 
      scale_y_continuous(limits = c(0,1)) +
      theme_void() +
      coord_flip() -> p2
    
    # patch 3
    mods %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c("EVI", 
                                                                      "equal_educ", 
                                                                      "homosexuality",
                                                                      "abortion", 
                                                                      "divorce")))) %>% 
      mutate(label = paste0("N = ", nobs)) %>% 
      select(index, outcome, label) %>% 
      distinct %>% 
      ggplot() + 
      geom_text(aes(y = 0, x = outcome, label = label), 
                hjust = "left", 
                family = "Segoe UI Semilight",
                size = 3.5) + 
      scale_y_continuous(limits = c(0,1)) +
      theme_void() +
      coord_flip() -> p3
    
    # gluing together
    labels <- p2+p3
    p1 + labels
  } else{
    
    # patch 1
    mods %>% 
      filter(term == coef_of) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c(
               "SVI",
               "rel_imp", 
               "rel_person", 
               "nation_pride", 
               "parents_proud", 
               "respect_author")))) %>% 
      mutate(outcome = fct_recode(outcome,
                                  "SECULAR VALUES"                        = "SVI",
                                  !!as.symbol(parents_proud_label) := "parents_proud",
                                  !!as.symbol(nation_pride_label) := "nation_pride",
                                  !!as.symbol(respect_author_label) := "respect_author",
                                  !!as.symbol(rel_imp_label) := "rel_imp",
                                  !!as.symbol(rel_person_label) := "rel_person")) %>% 
      
      ggplot() + 
      geom_errorbar(aes(x = outcome, 
                        ymin = conf.low, 
                        ymax = conf.high,
                        alpha = index),
                    width = 0.25,
                    lwd = 1, 
                    col = color,
                    show.legend = FALSE) +
      geom_point(aes(y = estimate, x = outcome, alpha = index), 
                 pch = 18, size = 3, show.legend = FALSE, col = color) +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, col = "grey50") +
      theme_minimal(base_size = 13) + 
      coord_flip() +
      scale_alpha_discrete(range = c(0.4,1)) +
      scale_y_continuous(limits = scale_lim) -> p1_temp
    
    if(is.na(xlabel)){
      p1_temp + labs(x = "",
                     y = expression(paste(Delta, " WVS7-VIC1")),
                     title = plotlabel) -> p1
    } else {
      p1_temp + labs(x = "",
                     y = xlabel,
                     title = plotlabel) -> p1
    }
    
    if(suppr_x == FALSE){
      p1 + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y        = element_text(hjust = 1),
              text = element_text(family = "Segoe UI Semilight"),
              plot.title.position = "plot") -> p1
    }else{
      p1 + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y        = element_text(hjust = 1),
              axis.title.x       = element_blank(), 
              text = element_text(family = "Segoe UI Semilight"),
              plot.title.position = "plot") -> p1
    }
    # patch 2
    mods %>% 
      filter(term == coef_of) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c(
               "SVI",
               "rel_imp", 
               "rel_person", 
               "nation_pride", 
               "parents_proud", 
               "respect_author")))) %>% 
      mutate(label = paste0(round1(estimate, digits), " (", round1(conf.low, digits), "; ", round1(conf.high, digits), ")")) %>% 
      ggplot() + 
      geom_text(aes(y = 0, x = outcome, label = label), 
                hjust = "left", 
                family = "Segoe UI Semilight",
                size = 3.5) + 
      scale_y_continuous(limits = c(0,1)) +
      theme_void() +
      coord_flip() -> p2
    
    # patch 3
    mods %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c(
               "SVI",
               "rel_imp", 
               "rel_person", 
               "nation_pride", 
               "parents_proud", 
               "respect_author")))) %>% 
      mutate(label = paste0("N = ", nobs)) %>% 
      select(index, outcome, label) %>% 
      distinct %>% 
      ggplot() + 
      geom_text(aes(y = 0, x = outcome, label = label), 
                hjust = "left", 
                family = "Segoe UI Semilight",
                size = 3.5) + 
      scale_y_continuous(limits = c(0,1)) +
      theme_void() +
      coord_flip() -> p3
    
    # gluing together
    labels <- p2+p3
    p1 + labels
  }
}

# forest plots for psychological distress analysis
forestplots_psych <- function(mods, version, color, plotlabel, scale_lim, suppr_x, coef_of, xlabel = NA, digits = 1){
  if(version == "EVI"){
    
    # patch 1
    mods %>% 
      filter(term == coef_of) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c("EVI", 
                                                                      "equal_educ", 
                                                                      "equal_polit",
                                                                      "equal_job",
                                                                      "homosexuality",
                                                                      "abortion", 
                                                                      "divorce")))) %>% 
      mutate(outcome = fct_recode(outcome,
                                  "EMANCIPATIVE VALUES"            = "EVI",
                                  "Gender equality education"      = "equal_educ",
                                  "Gender equality politics"       = "equal_polit",
                                  "Gender equality jobs"           = "equal_job",
                                  "Acceptance homosexuality"       = "homosexuality",
                                  "Acceptance abortion"            =  "abortion", 
                                  "Acceptance divorce"             = "divorce")) %>% 
      
      ggplot() + 
      geom_errorbar(aes(x = outcome, 
                        ymin = conf.low, 
                        ymax = conf.high,
                        alpha = index, 
                        group = analysis),
                    width = 0.25,
                    lwd = 1, 
                    col = color, 
                    position = position_dodge(0.7)) +
      geom_point(aes(y = estimate, 
                     x = outcome, alpha = index, group = analysis#, pch = analysis
      ), 
      pch = 18, 
      size = 3, col = color, position = position_dodge(0.7)) +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, col = "grey50") +
      theme_minimal(base_size = 13) + 
      coord_flip() +
      scale_alpha_discrete(range = c(0.4,1)) +
      scale_y_continuous(limits = scale_lim) + 
      guides(alpha = "none",
             pch = guide_legend(reverse = TRUE, ncol = 1)) -> p1_temp
    
    if(is.na(xlabel)){
      p1_temp + labs(x = "",
                     y = expression(paste(Delta, " WVS7-VIC1")),
                     title = plotlabel,
                     pch = "") -> p1
    } else {
      p1_temp + labs(x = "",
                     y = xlabel,
                     title = plotlabel,
                     pch = "") -> p1
    }
    
    if(suppr_x == FALSE){
      p1 + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y        = element_text(hjust = 1),
              text = element_text(family = "Segoe UI Semilight"),
              plot.title.position = "plot",
              legend.position = "none") -> p1
    }else{
      p1 + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y        = element_text(hjust = 1),
              axis.title.x       = element_blank(), 
              text = element_text(family = "Segoe UI Semilight"),
              plot.title.position = "plot",
              legend.position = "none") -> p1
    }
    
    # patch 2
    mods %>% 
      filter(term == coef_of) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c("EVI", 
                                                                      "equal_educ", 
                                                                      "equal_polit",
                                                                      "equal_job",
                                                                      "homosexuality",
                                                                      "abortion", 
                                                                      "divorce")))) %>% 
      mutate(label = paste0(round1(estimate, digits), " (", round1(conf.low, digits), "; ", round1(conf.high, digits), ")")) %>% 
      ggplot() + 
      geom_text(aes(y = 0, x = outcome, label = label, group = analysis), 
                hjust = "left", 
                family = "Segoe UI Semilight",
                size = 3.5, position = position_dodge(0.7)) + 
      scale_y_continuous(limits = c(0,1)) +
      theme_void() +
      coord_flip() -> p2
    
    # patch 3
    mods %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c("EVI", 
                                                                      "equal_educ", 
                                                                      "homosexuality",
                                                                      "equal_polit",
                                                                      "equal_job",
                                                                      "abortion", 
                                                                      "divorce")))) %>% 
      mutate(label = paste0("N = ", nobs)) %>% 
      select(index, outcome, label, analysis) %>% 
      distinct %>% 
      ggplot() + 
      geom_text(aes(y = 0, x = outcome, label = label, group = analysis), 
                hjust = "left", 
                family = "Segoe UI Semilight",
                size = 3.5, position = position_dodge(0.7)) + 
      scale_y_continuous(limits = c(0,1)) +
      theme_void() +
      coord_flip() -> p3
    
    # gluing together
    labels <- p2+p3
    p1 + labels + plot_layout(width = c(1.5,1))
    
  } else{
    
    # patch 1
    mods %>% 
      filter(term == coef_of) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c(
               "SVI",
               "rel_imp", 
               "rel_person", 
               "nation_pride", 
               "parents_proud", 
               "respect_author")))) %>% 
      mutate(outcome = fct_recode(outcome,
                                  "SECULAR VALUES"                        = "SVI",
                                  !!as.symbol(parents_proud_label) := "parents_proud",
                                  !!as.symbol(nation_pride_label) := "nation_pride",
                                  !!as.symbol(respect_author_label) := "respect_author",
                                  !!as.symbol(rel_imp_label) := "rel_imp",
                                  !!as.symbol(rel_person_label) := "rel_person")) %>% 
      
      ggplot() + 
      geom_errorbar(aes(x = outcome, 
                        ymin = conf.low, 
                        ymax = conf.high,
                        alpha = index,
                        group = analysis),
                    width = 0.25,
                    lwd = 1, 
                    col = color,
                    position = position_dodge(0.7)) +
      geom_point(aes(y = estimate, x = outcome, alpha = index, #pch = analysis,
                     group = analysis
      ), 
      size = 3, 
      pch = 18,
      col = color, position = position_dodge(0.7)) +
      geom_hline(yintercept = 0, lty = 2, lwd = 1, col = "grey50") +
      theme_minimal(base_size = 13) + 
      coord_flip() +
      scale_alpha_discrete(range = c(0.4,1)) +
      scale_y_continuous(limits = scale_lim) + 
      guides(alpha = "none",
             pch = guide_legend(reverse = TRUE, ncol = 1)) -> p1_temp
    
    if(is.na(xlabel)){
      p1_temp + labs(x = "",
                     y = expression(paste(Delta, " WVS7-VIC1")),
                     title = plotlabel,
                     pch = "") -> p1
    } else {
      p1_temp + labs(x = "",
                     y = xlabel,
                     title = plotlabel,
                     pch = "") -> p1
    }
    
    if(suppr_x == FALSE){
      p1 <- p1 + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y        = element_text(hjust = 1),
              text = element_text(family = "Segoe UI Semilight"),
              plot.title.position = "plot",
              legend.position = "bottom") +
        guides(shape = guide_legend(override.aes = list(color = "black"), ncol = 1, reverse = TRUE)) 
      
    }else{
      p1 <- p1 + 
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.y        = element_text(hjust = 1),
              axis.title.x       = element_blank(), 
              text = element_text(family = "Segoe UI Semilight"),
              plot.title.position = "plot",
              legend.position = "bottom")  +
        guides(shape = guide_legend(override.aes = list(col = "black"), ncol = 1, reverse = TRUE)) 
    }
    # patch 2
    mods %>% 
      filter(term == coef_of) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c(
               "SVI",
               "rel_imp", 
               "rel_person", 
               "nation_pride", 
               "parents_proud", 
               "respect_author")))) %>% 
      mutate(label = paste0(round1(estimate, digits), " (", round1(conf.low, digits), "; ", round1(conf.high, digits), ")")) %>% 
      ggplot() + 
      geom_text(aes(y = 0, x = outcome, label = label, group = analysis), 
                hjust = "left", 
                family = "Segoe UI Semilight",
                size = 3.5,
                position = position_dodge(0.7)) + 
      scale_y_continuous(limits = c(0,1)) +
      theme_void() +
      coord_flip() -> p2
    
    # patch 3
    mods %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = rev(c(
               "SVI",
               "rel_imp", 
               "rel_person", 
               "nation_pride", 
               "parents_proud", 
               "respect_author")))) %>% 
      mutate(label = paste0("N = ", nobs)) %>% 
      select(index, outcome, label, analysis) %>% 
      distinct %>% 
      ggplot() + 
      geom_text(aes(y = 0, x = outcome, label = label, group = analysis), 
                hjust = "left", 
                family = "Segoe UI Semilight",
                size = 3.5,
                position = position_dodge(0.7)) + 
      scale_y_continuous(limits = c(0,1)) +
      theme_void() +
      coord_flip() -> p3
    
    # gluing together
    labels <- p2+p3
    p1 + labels + plot_layout(width = c(1.5,1))
  }
}

# violin plots
violinplots <- function(data, weights, color, y, y_label, plotlabel){
  
  data <- data %>% 
    mutate(prepost = ifelse(WVS == 1, "WVS7", "VIC1")) %>% 
    group_by(prepost) %>% 
    mutate(w = !!as.symbol(weights) / sum(!!as.symbol(weights))) %>% 
    ungroup
  
  data %>% 
    ggplot() + 
    geom_half_boxplot(aes(x = prepost, 
                          y = !!as.symbol(y),
                          weight = w), 
                      nudge = 0.1, 
                      outlier.shape = NA,
                      errorbar.draw = FALSE,
                      center = TRUE,
                      fill = color, alpha = 0.25) + 
    geom_half_violin(aes(x = prepost, y = !!as.symbol(y),
                         weight = w), side = "r",
                     fill = color, alpha = 0.25, col = NA) +
    theme_minimal(base_size = 13) + 
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y        = element_text(hjust = 0),
          text = element_text(family = "Segoe UI Semilight"),
          plot.title.position = "plot") + 
    labs(x = "",
         y = y_label,
         title = plotlabel)
}

# prediction plot for value change depending on infections
pred_intct_cases <- function(preds,data,version,color,y_label,plotlabel){
  
  #patch 1
  preds %>% 
    filter(outcome %in% version) %>% 
    mutate(outcome = ifelse(outcome == "EVI", "EVI", "SVI"),
           VIC = factor(ifelse(VIC == 0, "WVS7", "VIC1"), ordered = TRUE, levels = c("WVS7","VIC1"))) %>% 
    ggplot() +
    geom_ribbon(aes(x = cum_infected_in_pref_VIC1,
                    ymin = lwr,
                    ymax = upr,
                    group = VIC), 
                alpha = 0.1) +
    geom_line(aes(x = cum_infected_in_pref_VIC1, 
                  y = fit, 
                  col = outcome,
                  lty = VIC),
              lwd = 0.9) + 
    scale_y_continuous(limits = c(0,100)) + 
    scale_color_manual(values = color) + 
    theme_bw(base_size = 13) + 
    theme(panel.grid = element_blank(),
          text = element_text(family = "Segoe UI Semilight"),
          strip.background = element_blank(),
          legend.position = "bottom",
          plot.title.position = "plot") +
    guides(col = "none", lty = guide_legend(title = "")) + 
    labs(x = "Infections in prefecture\n(per 100,000)",
         y = y_label,
         title = plotlabel) -> p1
  p1
}

# comparison plot over repeated surveys
timeplot <- function(data, version, plotlabel, color, scale_lim, suppr_y){
  
  if(version == "EVI"){
    data <- data %>% filter(outcome %in% outcome_EVI) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = c("EVI", 
                                                                  "equal_educ", 
                                                                  "homosexuality",
                                                                  "abortion", 
                                                                  "divorce"))) %>% 
      mutate(outcome = fct_recode(outcome,
                                  "EMANCIPATIVE VALUES"            = "EVI",
                                  "Gender equality education"      = "equal_educ", 
                                  "Acceptance homosexuality"       = "homosexuality",
                                  "Acceptance abortion"            =  "abortion", 
                                  "Acceptance divorce"             = "divorce")) %>% 
      mutate(time = factor(time, ordered = TRUE, levels = c("WVS7", "VIC1", "VIC2")))
  } else{
    data <- data %>% filter(outcome %in% outcome_SVI) %>% 
      mutate(index = ifelse(outcome %in% c("EVI", "SVI"), TRUE, FALSE),
             outcome = factor(outcome, ordered = TRUE, levels = c(
               "SVI",
               "rel_imp", 
               "rel_person", 
               "nation_pride", 
               "parents_proud", 
               "respect_author"))) %>% 
      mutate(outcome = fct_recode(outcome,
                                  "SECULAR VALUES"                  = "SVI",
                                  !!as.symbol(parents_proud_label)  := "parents_proud",
                                  !!as.symbol(nation_pride_label)   := "nation_pride",
                                  !!as.symbol(respect_author_label) := "respect_author",
                                  !!as.symbol(rel_imp_label)        := "rel_imp",
                                  !!as.symbol(rel_person_label)     := "rel_person")) %>% 
      mutate(time = factor(time, ordered = TRUE, levels = c("WVS7", "VIC1", "VIC2")))
  }
  
  p <- ggplot(data) + 
    geom_point(aes(x = time, y = estimate, alpha = index), col = color, size = 2.5) + 
    geom_line(data = data %>% filter(data == "VIC1 - weighted"),
              aes(x = time, y = estimate, group = outcome, alpha = index), col = color, lwd = 0.75) + 
    geom_line(data = data %>% filter((time == "VIC1" & data == "VIC1 - weighted") | 
                                       (time == "VIC2" & data == "VIC2 - weighted")),
              aes(x = time, y = estimate, group = outcome, alpha = index), lty = 2, col = color, lwd = 0.75) + 
    geom_hline(yintercept = 0, lty = 2, lwd = 0.8, col = "grey50") +
    theme_minimal(base_size = 13) + 
    geom_errorbar(data = data %>% filter(data == "VIC1 - weighted"),
                  aes(x = time, ymin = conf.low, ymax = conf.high, alpha = index), width = 0.2, col = color, lwd = 0.75) +
    geom_errorbar(data = data %>% filter((time == "VIC1" & data == "VIC1 - weighted") | 
                                           (time == "VIC2" & data == "VIC2 - weighted")),
                  aes(x = time, ymin = conf.low, ymax = conf.high, alpha = index),
                  width= 0.2, col = color, lwd = 0.75) +
    facet_wrap(~ outcome, nrow = 1) + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          text = element_text(family = "Segoe UI Semilight"),
          plot.title.position = "plot") +
    scale_alpha_discrete(range = c(0.4,1)) + 
    guides(alpha = "none") + 
    scale_y_continuous(limits = scale_lim)
  
  if(suppr_y){
    p <- p + 
      labs(y = "",
           x = "Survey",
           title = plotlabel)
  }else{
    p <- p + 
      labs(y = expression(paste(Delta, " from WVS7")),
           x = "Survey",
           title = plotlabel)
  }
  
  p
}

# summary statistics table
create_summary_tab <- function(data, use_wgt = FALSE, wgt_var = NULL){
  
  tmp <- data %>%
    mutate(wave = factor(wave, ordered = TRUE, levels = c(
      "WVS7","VIC1","VIC2"
    ))) %>% 
    mutate(education = fct_reorg(education,
                                 "Primary or Junior high school"    = "Primary or Junior high school",
                                 "High school"                      = "High school",
                                 "Vocational school"                = "Vocational school/University-preparator",
                                 "University-level"                 = "University-level education",
                                 "Master or Doctoral degree"        = "Master or Doctoral degree")) %>%
    mutate(
      maritalst = factor(maritalst, ordered = TRUE, levels = c(
        "Married",
        "Living together as married",
        "Separated",
        "Divorced",
        "Widowed",
        "Single"
      )),
      rel_service = factor(rel_service, ordered = TRUE, levels = c(
        "Once a week or more",
        "Once a month",
        "Only on special holidays",
        "Once a year",
        "Less often or never"
      )),
      townsize = factor(townsize, ordered = TRUE, levels = c(
        "Less than 5,000",
        "5,000-20,000",
        "20,000-100,000",
        "100,000-500,000",
        "500,000 and more"
      )),
      region = factor(region, ordered = TRUE, levels = c(
        "Hokkaido and Tohoku",
        "Kanto",
        "Chubu",
        "Kansai",
        "Chugoku",
        "Shikoku",
        "Kyushu and Okinawa"
      )))  %>% 
    select(all_of(c("wave", vars_cont, vars_nom_reg, vars_bfive)), any_of(wgt_var))
  
  if(!use_wgt){
    tb <- tmp %>% 
      select(-any_of(wgt_var)) %>% 
      tbl_summary(
        by = wave,
        type = c(vars_cont, vars_bfive) ~ "continuous",
        statistic = list(all_continuous() ~ "{mean} ({sd})",
                         all_categorical() ~ "{n} ({p}%)"),
        digits = list(all_continuous() ~ 1,
                      all_categorical() ~ 0),
        missing_text = "(Missing)",
        label = list(
          age ~ "Age",
          gender ~ "Gender",
          children ~ "No. of children",
          hhsize ~ "Household size",
          education ~ "Education",
          maritalst ~ "Marital status",
          rel_service ~ "Prior religious attendance",
          region ~ "Region",
          townsize ~ "Town size",
          bfive_extra ~ "Big Five: Extraversion",
          bfive_agree ~ "Big Five: Agreeableness",
          bfive_consc ~ "Big Five: Conscientiousness",
          bfive_neuro ~ "Big Five: Neuroticism",
          bfive_openn ~ "Big Five: Openness",
          distress ~ "Psychological distress")
      )
  } else{
    
    tmp$wgt <- pull(tmp[,wgt_var])
    tmp$wgt <- tmp$wgt /mean(tmp$wgt )
    
    svy_obj <- 
      survey::svydesign(
        id = ~1, 
        weights = ~wgt, 
        data = tmp, 
        fpc = NULL
      ) 
    
    tb <- tbl_svysummary(
      svy_obj,
      by = wave,
      include =     names(svy_obj$variables)[!names(svy_obj$variables) %in% c("wgt", wgt_var)],
      type = c(vars_cont, vars_bfive) ~ "continuous",
      statistic = list(all_continuous() ~ "{mean} ({sd})",
                       all_categorical() ~ "{p}%"),
      digits = list(all_continuous() ~ 1,
                    all_categorical() ~ 0),
      missing_text = "(Missing)",
      label = list(
        age ~ "Age",
        gender ~ "Gender",
        children ~ "No. of children",
        hhsize ~ "Household size",
        education ~ "Education",
        maritalst ~ "Marital status",
        rel_service ~ "Prior religious attendance",
        region ~ "Region",
        townsize ~ "Town size",
        bfive_extra ~ "Big Five: Extraversion",
        bfive_agree ~ "Big Five: Agreeableness",
        bfive_consc ~ "Big Five: Conscientiousness",
        bfive_neuro ~ "Big Five: Neuroticism",
        bfive_openn ~ "Big Five: Openness",
        distress ~ "Psychological distress")
    )
    
    
  }
  
  return(tb)
}

# estimation of survey-wise post-stratification weights and (optionally) propensity weights
add_poststrat_wgts <- function(censusdata, 
                               sampledata,
                               add_iptw = FALSE){

    samp <- sampledata %>%
      group_by(wave) %>% 
      mutate(samp_total_EVI = sum(!is.na(EVI)),
             samp_total_SVI = sum(!is.na(SVI)),
             samp_total = n()) %>% 
      group_by(wave, prefecture, age_group, gender) %>% 
      summarise(n = n(),
                stratum_prop_samp_total = n/first(samp_total),
                
                n_EVI=sum(!is.na(EVI)),
                stratum_prop_samp_EVI = n_EVI/first(samp_total_EVI),
                
                n_SVI=sum(!is.na(SVI)),
                stratum_prop_samp_SVI = n_SVI/first(samp_total_SVI), 
                
                .groups = "drop")
    
    wgts <- left_join(samp, censusdata, by = c("prefecture", "gender", "age_group")) %>% 
      mutate(popwgt_total = stratum_prop_pop / stratum_prop_samp_total,
             popwgt_EVI   = stratum_prop_pop / stratum_prop_samp_EVI,
             popwgt_SVI   = stratum_prop_pop / stratum_prop_samp_SVI) %>% 
      select(wave, prefecture, gender, age_group, contains("popwgt"), contains("stratum"))
    
    sampledata <- sampledata %>% 
      left_join(wgts, by = c("age_group", "gender", "prefecture", "wave")) %>% 
      mutate(id=row_number())

  if(add_iptw){
    
    propmod_total <- glm(as.formula(paste(c("WVS ~ ", vars_cont, vars_nom_pref),
                                    collapse = " + ", sep = "")),
                         data = sampledata,
                         weights = popwgt_total/mean(popwgt_total),
                         family = binomial)
    
    propmod_EVI   <- glm(as.formula(paste(c("WVS ~ ", vars_cont, vars_nom_pref),
                                          collapse = " + ", sep = "")),
                         data =  sampledata %>% filter(!is.na(EVI)),
                         weights = popwgt_EVI/mean(popwgt_EVI),
                         family = binomial)
    
    propmod_SVI   <- glm(as.formula(paste(c("WVS ~ ", vars_cont, vars_nom_pref),
                                          collapse = " + ", sep = "")),
                         data =  sampledata %>% filter(!is.na(SVI)),
                         weights = popwgt_SVI/mean(popwgt_SVI),
                         family = binomial)
    
    tmp_EVI <- sampledata %>% filter(!is.na(EVI)) %>% 
      mutate(propensit_EVI = as.numeric(unname(predict(propmod_EVI, type = "response")))) %>% 
      select(id, propensit_EVI)
    
    tmp_SVI <- sampledata %>% filter(!is.na(SVI)) %>% 
      mutate(propensit_SVI = as.numeric(unname(predict(propmod_SVI, type = "response")))) %>% 
      select(id, propensit_SVI)

    sampledata <- sampledata %>% 
      left_join(tmp_EVI, by = "id") %>% 
      left_join(tmp_SVI, by = "id") %>% 
      mutate(
        
      propensit_total     = as.numeric(unname(predict(propmod_total, type = "response"))),
      
      wgt_att_total       = WVS + (propensit_total * (1-WVS)) / (1-propensit_total),
      wgt_att_EVI         = WVS + (propensit_EVI * (1-WVS)) / (1-propensit_EVI),
      wgt_att_SVI         = WVS + (propensit_SVI * (1-WVS)) / (1-propensit_SVI),
      
      wgt_combined_EVI   = wgt_att_EVI * popwgt_EVI,
      wgt_combined_SVI   = wgt_att_SVI * popwgt_SVI,
      wgt_combined_total = wgt_att_total * popwgt_total
    )
    
    data_and_propmodel <- list(sampledata, propmod_total)
    
    return(data_and_propmodel)
    
  } else{
    return(sampledata)
  }
}

# weighted mode function from: https://rdrr.io/github/marberts/smart/src/R/computations.R
weighted_mode <- function(x, w = rep(1L, length(x)), na.rm = FALSE) { 
  if (length(x) != length(w)) {
    stop("'x' and 'w' must be the same length")
  }
  if (na.rm) {
    if (anyNA(x) || anyNA(w)) { # nested if to prevent anyNA(w) getting called twice
      keep <- !(is.na(x) | is.na(w))
      x <- x[keep]
      w <- w[keep]
    }
  } else if (anyNA(w)) {
    return(x[0][NA]) # impossible to know mode if any weights are missing
  }
  ux <- unique(x)
  if (!length(ux)) return(ux) # prevents max for returning -Inf
  f <- as.factor(match(x, ux))
  tab <- vapply(split(w, f), sum, numeric(1), USE.NAMES = FALSE)
  is_mode <- tab == max(tab) # lines up with ux
  if (anyNA(x)) {
    na <- which(is.na(ux)) # single integer
    modes <- which(is_mode)
    cond <- na %in% modes || # mode is NA if any mode is NA
      # or if the weight for any mode does not exceed the weight for the NA
      # and the weight for the next largest weight
      (length(ux) > 2 && tab[na] + max(tab[-c(modes[1], na)]) >= tab[modes[1]])
    if (cond) return(x[0][NA])
  }
  if (sum(is_mode) > 1) warning("mode is not unique")
  ux[is_mode]
}