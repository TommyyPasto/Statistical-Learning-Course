# MODULE 5 VISUALIZATION

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# THEME COMUNE
theme_paper <- function() {
  theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      strip.background = element_rect(fill = "grey90", color = NA),
      strip.text = element_text(face = "bold", size = 11),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, color = "grey30")
    )
}

METHOD_COLORS <- c("LDA" = "#E69F00", "QDA" = "#56B4E9", "RDA-CV" = "#009E73")

# PLOT 1 ERROR VS N
plot_error_vs_n_4scenarios <- function(long_df, p_filter = 20, alpha_filter = 5) {
  
  df <- long_df %>%
    filter(p == p_filter, alpha == alpha_filter) %>%
    mutate(method = factor(method, levels = c("LDA", "QDA", "RDA-CV"))) %>%
    mutate(error = case_when(
      method == "LDA" & n <= p_filter        ~ NA_real_,
      method == "QDA" & n <= 2 * p_filter    ~ NA_real_,
      TRUE                                   ~ error
    ),
    sd = case_when(
      method == "LDA" & n <= p_filter        ~ NA_real_,
      method == "QDA" & n <= 2 * p_filter    ~ NA_real_,
      TRUE                                   ~ sd
    ))
  
  plots <- lapply(c("A", "B", "C", "D"), function(sc) {
    df_sc <- df %>% filter(scenario == sc)
    
    n_values <- sort(unique(df_sc$n))
    n_lda_start <- min(n_values[n_values > p_filter])
    n_qda_start <- min(n_values[n_values > 2 * p_filter])
    
    ggplot(df_sc, aes(x = n, y = error, color = method, group = method)) +
      geom_vline(xintercept = n_lda_start, linetype = "dashed", 
                 color = "red", alpha = 0.6) +
      geom_vline(xintercept = n_qda_start, linetype = "dotted", 
                 color = "darkred", alpha = 0.6) +
      annotate("text", x = n_lda_start, y = 0.48, 
               label = "n>p", hjust = -0.1, size = 3, color = "red") +
      annotate("text", x = n_qda_start, y = 0.44, 
               label = "n>2p", hjust = -0.1, size = 3, color = "darkred") +
      geom_line(linewidth = 1, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      geom_ribbon(aes(ymin = error - sd, ymax = error + sd, fill = method),
                  alpha = 0.12, color = NA, na.rm = TRUE) +
      scale_x_log10(breaks = c(30, 50, 100, 200, 500)) +
      scale_color_manual(values = METHOD_COLORS, name = "Method") +
      scale_fill_manual(values = METHOD_COLORS, name = "Method") +
      labs(
        title = paste("Scenario", sc),
        subtitle = sprintf("p=%d, α=%d", p_filter, alpha_filter),
        x = "n (log scale)",
        y = "Mean test error"
      ) +
      theme_paper() +
      theme(legend.position = "none")
  })
  
  plots[[4]] <- plots[[4]] + theme(legend.position = "bottom")
  
  combined <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] +
    plot_layout(ncol = 4, guides = "collect") &
    theme(legend.position = "bottom")
  
  combined + plot_annotation(
    title = "Test Error vs Sample Size",
    subtitle = "Comparison across all scenarios — shaded area = ±1 SD"
  ) & theme(plot.title = element_text(face = "bold", size = 14))
}

plot_error_vs_p_4scenarios <- function(long_df, n_filter = 100, alpha_filter = 5) {
  
  df <- long_df %>%
    filter(n == n_filter, alpha == alpha_filter) %>%
    mutate(method = factor(method, levels = c("LDA", "QDA", "RDA-CV")))
  
  p_values <- sort(unique(df$p))
  
  plots <- lapply(c("A", "B", "C", "D"), function(sc) {
    df_sc <- df %>% filter(scenario == sc)
    
    ggplot(df_sc, aes(x = p, y = error, color = method, group = method)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_ribbon(aes(ymin = error - sd, ymax = error + sd, fill = method),
                  alpha = 0.12, color = NA) +
      scale_x_log10(breaks = p_values, limits = range(p_values) * c(0.9, 1.1)) +
      scale_color_manual(values = METHOD_COLORS, name = "Method") +
      scale_fill_manual(values = METHOD_COLORS, name = "Method") +
      labs(
        title = paste("Scenario", sc),
        subtitle = sprintf("n=%d, α=%d", n_filter, alpha_filter),
        x = "p (log scale)",
        y = "Mean test error"
      ) +
      theme_paper() +
      theme(legend.position = "none")
  })
  
  combined <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] +
    plot_layout(ncol = 4, guides = "collect") &
    theme(legend.position = "bottom")
  
  combined + plot_annotation(
    title = "Test Error vs Dimensionality",
    subtitle = "Comparison across all scenarios — shaded area = ±1 SD"
  ) & theme(plot.title = element_text(face = "bold", size = 14))
}

# PLOT 2 ERROR VS P/N RATIO
plot_error_vs_pn <- function(summary_df) {
  
  df_plot <- summary_df %>%
    mutate(pn_ratio = p / n) %>%
    pivot_longer(cols = c(mean_lda, mean_qda, mean_rda),
                 names_to = "method", values_to = "error") %>%
    mutate(method = factor(case_when(
      method == "mean_lda" ~ "LDA",
      method == "mean_qda" ~ "QDA",
      method == "mean_rda" ~ "RDA-CV"
    ), levels = c("LDA", "QDA", "RDA-CV")))
  
  ggplot(df_plot, aes(x = pn_ratio, y = error, color = method, group = method)) +
    geom_point(size = 2, alpha = 0.6) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1) + 
    facet_wrap(~scenario, ncol = 4, labeller = label_both) +
    scale_x_log10() + 
    scale_color_manual(values = c("LDA" = "#E69F00", "QDA" = "#56B4E9", "RDA-CV" = "#009E73")) +
    labs(
      title = "Test Error vs p/n Ratio",
      subtitle = "Trend lines computed across all grid combinations (alpha averaged)",
      x = "p/n ratio (log scale)",
      y = "Mean test error"
    ) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) + 
    theme_bw() +
    theme(legend.position = "bottom")
}

# PLOT 3 RDA GAIN MAP
plot_rda_gain_map <- function(summary_df) {
  
  df <- summary_df %>%
    filter(p/n <= 1) %>% 
    mutate(
      best_baseline = ifelse(is.na(mean_qda), mean_lda, pmin(mean_lda, mean_qda, na.rm = TRUE)),
      rda_gain = best_baseline - mean_rda,
      pn_bin = cut(p/n, breaks = c(0, 0.1, 0.2, 0.5, 1.0),
                   labels = c("<0.1","0.1-0.2","0.2-0.5","0.5-1.0"))
    ) %>%
    group_by(scenario, pn_bin, alpha) %>%
    summarise(mean_gain = mean(rda_gain, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(pn_bin))
  
  plots <- lapply(c("A", "B", "C", "D"), function(sc) {
    df_sc <- df[df$scenario == sc, ]
    
    ggplot(df_sc, aes(x = factor(pn_bin), y = factor(alpha), fill = mean_gain)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low = "grey95", mid = "gold", high = "darkgreen",
        midpoint = 0.05, limits = c(-0.02, 0.25),
        name = "RDA gain",
        labels = scales::percent_format(accuracy = 1)
      ) +
      labs(title = paste("Scenario", sc), x = "p/n ratio", y = "α") +
      theme_paper() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  combined <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] +
    plot_layout(ncol = 4, guides = "collect")
  
  combined + plot_annotation(
    title = "Where Does RDA-CV Help? (Low to Moderate Dimensionality)",
    subtitle = "Green = RDA-CV beats both LDA and QDA; grey = no gain — Restricted to p/n ≤ 1"
  ) & theme(plot.title = element_text(face = "bold", size = 14))
}

# PLOT 4 SCENARIO FINGERPRINT
plot_scenario_fingerprint <- function(summary_df) {
  
  fingerprint <- summary_df %>%
    group_by(scenario) %>%
    summarise(
      mean_lda = mean(mean_lda),
      mean_qda = mean(mean_qda),
      mean_rda = mean(mean_rda),
      mean_lambda = mean(mean_lambda_star),
      mean_gamma = mean(mean_gamma_star),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(mean_lda, mean_qda, mean_rda),
                 names_to = "method", values_to = "error") %>%
    mutate(method = factor(case_when(
      method == "mean_lda" ~ "LDA",
      method == "mean_qda" ~ "QDA",
      method == "mean_rda" ~ "RDA-CV"
    ), levels = c("LDA", "QDA", "RDA-CV")))
  
  p_err <- ggplot(fingerprint, aes(x = scenario, y = error, fill = method)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    scale_fill_manual(values = METHOD_COLORS, name = "Method") +
    labs(title = "Mean Test Error", y = "Error rate", x = NULL) +
    theme_paper() +
    theme(legend.position = "bottom")
  
  fp_param <- summary_df %>%
    group_by(scenario) %>%
    summarise(
      mean_lambda = mean(mean_lambda_star),
      mean_gamma = mean(mean_gamma_star),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = c(mean_lambda, mean_gamma),
                 names_to = "parameter", values_to = "value") %>%
    mutate(parameter = factor(case_when(
      parameter == "mean_lambda" ~ "λ*",
      parameter == "mean_gamma" ~ "γ*"
    ), levels = c("λ*", "γ*")))
  
  p_param <- ggplot(fp_param, aes(x = scenario, y = value, fill = parameter)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
    scale_fill_manual(values = c("λ*" = "#0072B2", "γ*" = "#D55E00"), name = "Parameter") +
    geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "grey60") +
    labs(title = "Mean Regularization", y = "Parameter value", x = NULL) +
    theme_paper() +
    theme(legend.position = "bottom")
  
  (p_err + p_param + plot_layout(widths = c(1, 1))) +
    plot_annotation(
      title = "Scenario Fingerprint",
      subtitle = "A=spheres equal | B=spheres unequal | C=ellipsoids equal | D=ellipsoids unequal"
    ) & theme(plot.title = element_text(face = "bold", size = 14))
}

# PLOT 5 DECISION BOUNDARY
plot_decision_boundary <- function(data, fit, lambda = 0.5, n_grid = 200) {
  stopifnot(ncol(data$X_test) == 2)
  
  X <- data$X_test
  Y <- data$Y_test
  
  x1_range <- range(X[, 1])
  x2_range <- range(X[, 2])
  grid <- expand.grid(
    x1 = seq(x1_range[1], x1_range[2], length.out = n_grid),
    x2 = seq(x2_range[1], x2_range[2], length.out = n_grid)
  )
  Xg <- as.matrix(grid)
  
  Sp_inv <- safe_inv(fit$covs$Sp)
  Sp_ld <- safe_log_det(fit$covs$Sp)
  lda_g <- disc_predict(Xg, fit$covs$mu0, fit$covs$mu1,
                        Sp_inv, Sp_inv, Sp_ld, Sp_ld)
  
  S0i <- safe_inv(fit$covs$S0); S1i <- safe_inv(fit$covs$S1)
  S0ld <- safe_log_det(fit$covs$S0); S1ld <- safe_log_det(fit$covs$S1)
  qda_g <- disc_predict(Xg, fit$covs$mu0, fit$covs$mu1,
                        S0i, S1i, S0ld, S1ld)
  
  lam_name <- paste0("lambda_", round(lambda, 3))
  if (!lam_name %in% names(fit$rda_path)) {
    available <- as.numeric(gsub("lambda_", "", names(fit$rda_path)))
    closest <- available[which.min(abs(available - lambda))]
    lam_name <- paste0("lambda_", round(closest, 3))
  }
  
  S0r <- fit$rda_path[[lam_name]]$Sigma0_reg
  S1r <- fit$rda_path[[lam_name]]$Sigma1_reg
  rda_g <- disc_predict(Xg, fit$covs$mu0, fit$covs$mu1,
                        safe_inv(S0r), safe_inv(S1r),
                        safe_log_det(S0r), safe_log_det(S1r))
  
  grid$LDA <- factor(lda_g)
  grid$QDA <- factor(qda_g)
  grid$RDA <- factor(rda_g)
  
  df_pts <- data.frame(x1 = X[, 1], x2 = X[, 2], class = factor(Y))
  
  df_long <- grid %>%
    pivot_longer(cols = c(LDA, QDA, RDA), names_to = "method", values_to = "pred") %>%
    mutate(method = factor(method, levels = c("LDA", "QDA", "RDA")))
  
  ggplot(df_long, aes(x = x1, y = x2, fill = pred)) +
    geom_tile(alpha = 0.25) +
    geom_point(data = df_pts, aes(x = x1, y = x2, color = class, shape = class),
               inherit.aes = FALSE, size = 1.5, alpha = 0.7) +
    facet_wrap(~method, ncol = 3) +
    scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato"), name = "Predicted") +
    scale_color_manual(values = c("0" = "navy", "1" = "darkred"), name = "True class") +
    scale_shape_manual(values = c("0" = 16, "1" = 17), name = "True class") +
    labs(
      title = "Decision Boundaries (p=2)",
      subtitle = sprintf("Scenario B — RDA shown at λ=%.2f (γ=0)", lambda),
      x = expression(x[1]), y = expression(x[2])
    ) +
    theme_paper() +
    theme(legend.position = "bottom", legend.box = "vertical")
}

plot_params_vs_p <- function(summary_df, n_filter = 100, alpha_filter = 5) {
  df <- summary_df %>%
    filter(n == n_filter, alpha == alpha_filter) %>%
    mutate(scenario = factor(scenario, levels = c("A", "B", "C", "D")))
  
  p1 <- ggplot(df, aes(x = factor(p), y = mean_lambda_star, color = scenario, group = scenario)) + 
    geom_line(linewidth = 1) + 
    geom_point(size = 3) + 
    geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "grey60") + 
    scale_color_manual(values = c("A" = "#CC79A7", "B" = "#0072B2", "C" = "#D55E00", "D" = "#009E73")) + 
    labs(title = "λ* vs p", y = "Mean λ*", x = "p") + 
    theme_paper() + 
    theme(legend.position = "none")
  
  p2 <- ggplot(df, aes(x = factor(p), y = mean_gamma_star, color = scenario, group = scenario)) + 
    geom_line(linewidth = 1) + 
    geom_point(size = 3) + 
    geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "grey60") + 
    scale_color_manual(values = c("A" = "#CC79A7", "B" = "#0072B2", "C" = "#D55E00", "D" = "#009E73")) + 
    labs(title = "γ* vs p", y = "Mean γ*", x = "p") + 
    theme_paper() + 
    theme(legend.position = "none")
  
  (p1 | p2) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = sprintf("Regularization Parameters vs p (n=%d, α=%d)", n_filter, alpha_filter)
    ) &
    theme(legend.position = "bottom")
}

plot_rda_gain_vs_p <- function(summary_df, n_filter = 50, alpha_filter = 5) {
  df <- summary_df %>%
    filter(n == n_filter, alpha == alpha_filter) %>%
    mutate(
      best_baseline = pmin(mean_lda, mean_qda),
      rda_gain = best_baseline - mean_rda
    )
  
  ggplot(df, aes(x = p, y = rda_gain, color = scenario, group = scenario)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    scale_x_log10(breaks = c(5, 20, 50)) +
    scale_color_manual(values = c("A" = "#CC79A7", "B" = "#0072B2", 
                                  "C" = "#D55E00", "D" = "#009E73")) +
    labs(
      title = "RDA-CV Gain vs Dimensionality",
      subtitle = sprintf("n=%d, α=%d — positive = RDA-CV beats both LDA and QDA", 
                         n_filter, alpha_filter),
      x = "p (log scale)", 
      y = "Gain = min(LDA,QDA) − RDA-CV"
    ) +
    theme_paper()
}

plot_lambda_gamma_heatmap <- function(summary_df) {
  
  df <- summary_df %>%
    mutate(pn_bin = cut(p / n, 
                        breaks = c(0, 0.1, 0.2, 0.5, 1, 2, 5, Inf),
                        labels = c("<0.1","0.1-0.2","0.2-0.5","0.5-1","1-2","2-5",">5"))) %>%
    group_by(scenario, pn_bin, alpha) %>%
    summarise(
      mean_lambda = mean(mean_lambda_star),
      mean_gamma  = mean(mean_gamma_star),
      .groups = "drop"
    )
  
  plots_lambda <- lapply(c("A", "B", "C", "D"), function(sc) {
    df_sc <- df[df$scenario == sc, ]
    ggplot(df_sc, aes(x = factor(pn_bin), y = factor(alpha), fill = mean_lambda)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low = "steelblue", mid = "lightyellow", high = "firebrick",
        midpoint = 0.5, limits = c(0, 1),
        name = "λ*"
      ) +
      labs(title = paste("Scenario", sc), x = "p/n ratio", y = "α") +
      theme_paper() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  plots_gamma <- lapply(c("A", "B", "C", "D"), function(sc) {
    df_sc <- df[df$scenario == sc, ]
    ggplot(df_sc, aes(x = factor(pn_bin), y = factor(alpha), fill = mean_gamma)) +
      geom_tile(color = "white", linewidth = 0.3) +
      scale_fill_gradient2(
        low = "steelblue", mid = "lightyellow", high = "firebrick",
        midpoint = 0.5, limits = c(0, 1),
        name = "γ*"
      ) +
      labs(title = paste("Scenario", sc), x = "p/n ratio", y = "α") +
      theme_paper() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 30, hjust = 1))
  })
  
  row_lambda <- plots_lambda[[1]] + plots_lambda[[2]] + 
    plots_lambda[[3]] + plots_lambda[[4]] +
    plot_layout(ncol = 4, guides = "collect")
  
  row_gamma  <- plots_gamma[[1]] + plots_gamma[[2]] + 
    plots_gamma[[3]] + plots_gamma[[4]] +
    plot_layout(ncol = 4, guides = "collect")
  
  (row_lambda / row_gamma) +
    plot_annotation(
      title    = "Optimal Regularization Parameters λ* and γ*",
      subtitle = "Blue = QDA/identity regime | Red = LDA/pooled regime"
    ) & theme(plot.title = element_text(face = "bold", size = 14))
}

# SAVE ALL
save_all_plots <- function(tabs, data_p2 = NULL, fit_p2 = NULL,
                           output_dir = "plots") {
  dir.create(output_dir, showWarnings = FALSE)
  
  plots <- list(
    error_vs_n = plot_error_vs_n_4scenarios(tabs$long),
    error_vs_p = plot_error_vs_p_4scenarios(tabs$long, n_filter = 100, alpha_filter = 5),
    error_vs_pn = plot_error_vs_pn(tabs$summary),
    gain_map = plot_rda_gain_map(tabs$summary),
    fingerprint = plot_scenario_fingerprint(tabs$summary),
    params_vs_p = plot_params_vs_p(tabs$summary),
    rda_gain_vs_p = plot_rda_gain_vs_p(tabs$summary)
  )
  
  if (!is.null(data_p2) && !is.null(fit_p2))
    plots$decision_boundary <- plot_decision_boundary(data_p2, fit_p2)
  
  for (nm in names(plots)) {
    fname <- file.path(output_dir, paste0(nm, ".png"))
    ggsave(fname, plots[[nm]], width = 12, height = 5, dpi = 200, bg = "white")
    cat(sprintf("  Saved: %s\n", fname))
  }
  
  invisible(plots)
}

# MONTECARLO SIMULATION
if (sys.nframe() == 0) {
  source("Parte4.R")
  source("Additional")
  
  cat("Running small MC for visualization test...\n")
  results <- run_monte_carlo_par(
    n_grid     = c(30,50,100,200,500), 
    p_grid     = c(5,20,50),
    alpha_grid = c(1, 5),
    scenarios  = c("A", "B", "C", "D"),
    R          = 100,
    verbose    = TRUE
  )
  
  TABS_2 <- flatten_results(results)
  
  TABS_2
  results
  length(results)
  
  dat_2d <- generate_data(n = 200, p = 2, delta = 2, alpha = 5,
                          scenario = "B", seed = 42)
  fit_2d <- train_models(dat_2d, use_cv = TRUE)
  
  cat("Saving plots to plots/...\n")
  plots <- save_all_plots(TABS_2, data_p2 = dat_2d, fit_p2 = fit_2d, output_dir = "plots")
  cat("✓ Visualization module OK\n")
}