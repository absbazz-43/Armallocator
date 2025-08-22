# SQL Runner function
data_loader <- function(project_id, query) {
  bq_auth()
  job <- bq_project_query(project_id, query)
  results <- bq_table_download(job)
  return(results)
}

# Randomizer function
data_maker <- function(experiment_data, arm_proportion, arm_no, arm_name, experiment_name,
                       use_smd = FALSE, confounders, smd_threshold = 0.05, max_runtime = 5) {
  
  # Validate inputs
  if (length(arm_proportion) != arm_no || length(arm_name) != arm_no) {
    stop("Length of arm_proportion and arm_name must equal arm_no.")
  }
  
  if (sum(arm_proportion) != 1) {
    stop("The sum of arm_proportion must be 1.")
  }
  
  # Set seed for reproducibility
  set.seed(as.numeric(digest(experiment_name, algo = "md5", raw = TRUE), 16))
  
  # Initialize time tracking
  start_time <- Sys.time()
  
  # Shuffle the data
  experiment_data <- experiment_data %>% sample_n(nrow(experiment_data), replace = FALSE)
  n <- nrow(experiment_data)
  
  # Compute sample sizes for each arm
  sample_size <- floor(n * arm_proportion)
  # Adjust sample sizes if the total doesn't match n
  extra <- n - sum(sample_size)
  if (extra > 0) {
    sample_size[1:extra] <- sample_size[1:extra] + 1
  }
  
  if (use_smd == FALSE) {
    # Assign treatments without SMD balancing
    treatment <- unlist(mapply(rep, arm_name, sample_size, SIMPLIFY = FALSE))
    experiment_data <- experiment_data %>%
      mutate(treatment_group = sample(treatment, n, replace = FALSE))
    
    return(experiment_data)
  } else {
    # SMD-based treatment assignment
    max_diff_value <- Inf
    iteration_count <- 0
    
    while (any(max_diff_value >= smd_threshold)) {
      iteration_count <- iteration_count + 1
      
      # Check runtime
      elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      if (elapsed_time > max_runtime) {
        stop("Execution time exceeded the time limit. Increase SMD threshold or max_runtime.")
      }
      
      # Assign treatments randomly
      treatment <- unlist(mapply(rep, arm_name, sample_size, SIMPLIFY = FALSE))
      experiment_data <- experiment_data %>%
        mutate(treatment_group = sample(treatment, n, replace = FALSE))
      
      # Check balance using cobalt::bal.tab
      formula <- as.formula(paste("treatment_group", "~", paste(confounders, collapse = " + ")))
      bal_tab <- cobalt::bal.tab(formula, data = experiment_data, s.d.denom = "pooled", un = TRUE, abs = TRUE)
      
      # Extract max imbalance (e.g., Max.Diff.Un) for each covariate
      max_diff_value <- bal_tab$Balance.Across.Pairs$Max.Diff.Un
      
      if (all(max_diff_value < smd_threshold)) {
        return(experiment_data)
      }
    }
  }
}

# SMD Checker function
smd_checker <- function(experiment_data, treatment, confounders) {
  # Create the formula for the balance check
  formula <- as.formula(paste(treatment, "~", paste(confounders, collapse = " + ")))
  
  # Perform balance check using cobalt's bal.tab function
  bal_tab <- cobalt::bal.tab(
    formula,
    data = experiment_data,
    s.d.denom = "pooled",
    un = TRUE,
    abs = TRUE
  )
  
  # Create the love plot for visualizing balance
  love_plot <- cobalt::love.plot(
    bal_tab,
    colors = "firebrick3",
    limits = list(m = c(0, 1))
  ) +
    geom_vline(
      xintercept = 0.1,
      linetype = 2,
      color = "steelblue1",
      linewidth = 0.8
    ) +
    theme_bw(base_size = 16, base_family = "Times") +
    theme(
      legend.position = "none"
    )
  
  # Return the love plot and balance table as a list
  return(list(love_plot = love_plot, balance_tab = bal_tab$Observations))
}

# SRM Checker function
check_srm <- function(experiment_df, allocated_arm_col, expected_proportions) {
  arm_names <- names(expected_proportions)
  if (is.null(arm_names)) {
    stop("Expected proportions must be a named vector (arm_name -> proportion).")
  }
  
  observed_counts <- experiment_df %>%
    count(!!rlang::parse_expr(allocated_arm_col)) %>%
    rename(arm_name = !!rlang::parse_expr(allocated_arm_col), observed_count = n)
  
  if (!all(arm_names %in% observed_counts$arm_name)) {
    stop("Some expected arms are missing in the data.")
  }
  
  total_users <- nrow(experiment_df)
  expected_counts <- expected_proportions * total_users
  
  combined_data <- observed_counts %>%
    mutate(
      expected_count = expected_counts[match(arm_name, arm_names)],
      expected_count = round(expected_count)
    )
  
  chisq_test <- chisq.test(
    x = combined_data$observed_count,
    p = combined_data$expected_count / total_users,
    rescale.p = TRUE
  )
  
  if (chisq_test$p.value < 0.01) {
    message <- paste("Sample Ratio Mismatch Detected\np-value =", round(chisq_test$p.value, 4))
  } else {
    message <- paste("Sample Ratio Matched\np-value =", round(chisq_test$p.value, 4))
  }
  
  return(message)
}

### Density checker

density_checker <- function(experiment_data, treatment, target_variable) {
  # Create the plot using aes_string for dynamic column references
  grapher <- experiment_data %>%
    ggplot() +
    geom_density(aes(x = !!rlang::parse_expr(target_variable),
                     color = !!rlang::parse_expr(treatment),
                     group = !!rlang::parse_expr(treatment)),
                 alpha = 0.7, linewidth = 1.2) +
    geom_density(aes(x = !!rlang::parse_expr(target_variable)),
                 color = "black", linewidth = 1.5, alpha = 0.3) +
    labs(title = paste("Density Plot of", target_variable, "by Treatment Group"),
         x = target_variable,
         y = "Density",
         color = "Treatment") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
  return(grapher)
}

# Bar Chart function for categorical variables
bar_chart_checker <- function(experiment_data, treatment, target_variable) {
  # Create the plot for categorical variables
  grapher <- experiment_data %>%
    ggplot(aes(x = !!rlang::parse_expr(target_variable),
               fill = !!rlang::parse_expr(treatment))) +
    geom_bar(position = "dodge", alpha = 0.8) +
    labs(title = paste("Distribution of", target_variable, "by Treatment Group"),
         x = target_variable,
         y = "Count",
         fill = "Treatment") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set2")
  return(grapher)
}


### Calculate trend plot

calculate_ab_test_results <- function(data, treatment_col, metric_col, control_group = "control",
                                      test_type = "t_test", confidence_level = 0.95) {
  
  # Filter data for treatment and control
  treatment_data <- data[data[[treatment_col]] != control_group, ]
  control_data <- data[data[[treatment_col]] == control_group, ]
  
  # Calculate basic statistics
  n_treatment <- nrow(treatment_data)
  n_control <- nrow(control_data)
  
  metric_treatment <- treatment_data[[metric_col]]
  metric_control <- control_data[[metric_col]]
  
  sum_treatment <- sum(metric_treatment, na.rm = TRUE)
  sum_control <- sum(metric_control, na.rm = TRUE)
  
  mean_treatment <- mean(metric_treatment, na.rm = TRUE)
  mean_control <- mean(metric_control, na.rm = TRUE)
  
  sd_treatment <- sd(metric_treatment, na.rm = TRUE)
  sd_control <- sd(metric_control, na.rm = TRUE)
  
  # Calculate effect size
  pooled_sd <- sqrt(((n_treatment - 1) * sd_treatment^2 + (n_control - 1) * sd_control^2) /
                      (n_treatment + n_control - 2))
  cohens_d <- (mean_treatment - mean_control) / pooled_sd
  
  # Common Language Effect Size
  cle <- mean(outer(metric_treatment, metric_control, ">"), na.rm = TRUE)
  
  # Hypothesis testing
  test_result <- NULL
  ci_lower <- NA
  ci_upper <- NA
  
  if (test_type == "t_test") {
    test <- t.test(metric_treatment, metric_control, conf.level = confidence_level)
    test_result <- tidy(test)
    ci_lower <- test_result$conf.low[1]
    ci_upper <- test_result$conf.high[1]
  }
  else if (test_type == "z_test") {
    # Z-test approximation
    se <- sqrt(sd_treatment^2/n_treatment + sd_control^2/n_control)
    z_score <- (mean_treatment - mean_control) / se
    p_value <- 2 * pnorm(-abs(z_score))
    test_result <- data.frame(statistic = z_score, p.value = p_value)
    
    # Z-test CI
    z_critical <- qnorm(1 - (1 - confidence_level)/2)
    ci_lower <- (mean_treatment - mean_control) - z_critical * se
    ci_upper <- (mean_treatment - mean_control) + z_critical * se
  }
  else if (test_type == "wilcoxon") {
    test <- wilcox.test(metric_treatment, metric_control, conf.int = TRUE,
                        conf.level = confidence_level)
    test_result <- tidy(test)
    ci_lower <- test_result$conf.low[1]
    ci_upper <- test_result$conf.high[1]
  }
  else if (test_type == "bootstrap") {
    # Bootstrap CI
    boot_func <- function(data, indices) {
      sample_data <- data[indices, ]
      treat <- sample_data[sample_data[[treatment_col]] != control_group, metric_col]
      control <- sample_data[sample_data[[treatment_col]] == control_group, metric_col]
      return(mean(treat, na.rm = TRUE) - mean(control, na.rm = TRUE))
    }
    
    boot_results <- boot(data, boot_func, R = 1000)
    boot_ci <- boot.ci(boot_results, conf = confidence_level, type = "bca")
    ci_lower <- boot_ci$bca[4]
    ci_upper <- boot_ci$bca[5]
    
    # For bootstrap, use t-test for p-value (or could use permutation test)
    test <- t.test(metric_treatment, metric_control)
    test_result <- tidy(test)
  }
  
  # Calculate power
  power_result <- power.t.test(
    n = n_treatment,
    delta = mean_treatment - mean_control,
    sd = pooled_sd,
    sig.level = 0.05,
    type = "two.sample"
  )
  
  # Return results
  list(
    summary = data.frame(
      Group = c("Treatment", "Control"),
      N = c(n_treatment, n_control),
      Sum = c(sum_treatment, sum_control),
      Mean = c(mean_treatment, mean_control),
      SD = c(sd_treatment, sd_control)
    ),
    effect_size = cohens_d,
    common_language_es = cle,
    test_result = test_result,
    confidence_interval = c(ci_lower, ci_upper),
    power = power_result$power,
    difference = mean_treatment - mean_control,
    relative_effect = (mean_treatment - mean_control) / mean_control
  )
}

# Trend plot function
create_trend_plot <- function(data, treatment_col, metric_col, date_col, control_group = "control") {
  trend_data <- data %>%
    group_by(!!rlang::parse_expr(date_col), !!rlang::parse_expr(treatment_col)) %>%
    summarise(
      mean_metric = mean(!!rlang::parse_expr(metric_col), na.rm = TRUE),
      se = sd(!!rlang::parse_expr(metric_col), na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      lower_ci = mean_metric - 1.96 * se,
      upper_ci = mean_metric + 1.96 * se
    )
  
  ggplot(trend_data, aes(x = !!rlang::parse_expr(date_col), y = mean_metric,
                         color = !!rlang::parse_expr(treatment_col),
                         group = !!rlang::parse_expr(treatment_col))) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = !!rlang::parse_expr(treatment_col)),
                alpha = 0.2, color = NA) +
    labs(
      title = paste("Trend of", metric_col, "over Time"),
      x = "Date",
      y = metric_col,
      color = "Group",
      fill = "Group"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom") +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1")
}


