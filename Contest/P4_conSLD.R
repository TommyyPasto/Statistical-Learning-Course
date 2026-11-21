# ============================================================
#  SOLO GRAFICI — FASE FINALE: Solo Discrete SL
# ============================================================
library(ggplot2)
library(dplyr)
library(tidyr)

target_dir <- "./risultati_scelta1" 
file_path <- file.path(target_dir, "scelta1_results.rds")

if (!file.exists(file_path)) stop("Errore: Manca il file 'scelta1_results.rds'. Hai fatto girare la Fase 4?")
results <- readRDS(file_path)

scenario_labels <- c(A = "Scenario A — Lineare", B = "Scenario B — Smooth", C = "Scenario C — Interazioni")
colors_learner <- c(GLM = "#2E86AB", GAM = "#E84855", RF = "#3BB273", SVM = "#F4A261", kNN = "#9B5DE5", Mean = "#aaaaaa", DSL = "#D81B60", Oracle = "#FFD700")
learner_levels <- c("GLM","GAM","RF","SVM","kNN","Mean","DSL")

base_res <- results %>% filter(type == "learner")
dsl_res  <- results %>% filter(type == "discretesuperlearner")

# GRAFICO 1
risk_base <- base_res %>% group_by(scenario, n, learner) %>% summarise(risk_mean = mean(risk, na.rm = TRUE), .groups = "drop")
risk_dsl <- dsl_res %>% group_by(scenario, n) %>% summarise(risk_mean = mean(risk, na.rm = TRUE), .groups = "drop") %>% mutate(learner = "DSL")
risk_oracle <- dsl_res %>% group_by(scenario, n) %>% summarise(risk_mean = mean(oracle_risk, na.rm = TRUE), .groups = "drop") %>% mutate(learner = "Oracle")
risk_all <- bind_rows(risk_base, risk_dsl, risk_oracle)
risk_all$learner <- factor(risk_all$learner, levels = c(learner_levels, "Oracle"))
risk_all$ltype <- case_when(risk_all$learner == "DSL" ~ "DSL", risk_all$learner == "Oracle" ~ "Oracle", TRUE ~ "learner")

p1 <- ggplot(risk_all, aes(x = n, y = risk_mean, colour = learner, linetype = ltype, linewidth = ltype)) +
  geom_line() + geom_point(data = filter(risk_all, learner %in% c("DSL","Oracle")), size = 2.5) +
  geom_hline(yintercept = 1, linetype = "dotted", colour = "grey40", linewidth = 0.5) +
  scale_colour_manual(values = colors_learner) + scale_linetype_manual(values = c(DSL = "solid", Oracle = "dashed", learner = "solid"), guide = "none") + scale_linewidth_manual(values = c(DSL = 1.4, Oracle = 1.4, learner = 0.55), guide = "none") +
  scale_x_log10(breaks = c(100, 250, 500, 1000, 2000)) + facet_wrap(~scenario, scales = "free_y", ncol = 3, labeller = labeller(scenario = scenario_labels)) + theme_bw(base_size = 13) + theme(legend.position = "bottom")

# GRAFICO 2
gap_summary <- dsl_res %>% group_by(scenario, n) %>% summarise(gap_mean = mean(gap, na.rm = TRUE), gap_q25 = quantile(gap, 0.25, na.rm = TRUE), gap_q75 = quantile(gap, 0.75, na.rm = TRUE), .groups = "drop")
p2 <- ggplot(gap_summary, aes(x = n, y = gap_mean, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = gap_q25, ymax = gap_q75), alpha = 0.15, colour = NA) + geom_line(linewidth = 1.2) + geom_point(size = 2.5) + geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30", linewidth = 0.6) +
  scale_colour_manual(values = c(A = "#2E86AB", B = "#E84855", C = "#3BB273"), labels = scenario_labels) + scale_fill_manual(values = c(A = "#2E86AB", B = "#E84855", C = "#3BB273"), labels = scenario_labels) +
  scale_x_log10(breaks = c(100, 250, 500, 1000, 2000)) + theme_bw(base_size = 13) + theme(legend.position = "bottom")

# GRAFICO 3
box_data <- bind_rows(base_res %>% filter(n == 500) %>% select(scenario, learner, risk), dsl_res %>% filter(n == 500) %>% select(scenario, learner, risk)) %>% mutate(learner = factor(learner, levels = learner_levels))
p3 <- ggplot(box_data, aes(x = learner, y = risk, fill = learner)) + geom_boxplot(alpha = 0.8, outlier.size = 0.8, outlier.alpha = 0.4, linewidth = 0.4, na.rm = TRUE) + geom_hline(yintercept = 1, linetype = "dotted", colour = "grey30", linewidth = 0.6) + scale_fill_manual(values = colors_learner) + facet_wrap(~scenario, scales = "free_y", ncol = 3, labeller = labeller(scenario = scenario_labels)) + theme_bw(base_size = 13) + theme(legend.position = "none")

# GRAFICO 4
selection_summary <- dsl_res %>% group_by(scenario, n) %>% left_join(base_res, by = c("scenario", "n", "rep"), suffix = c("_dsl", "_base")) %>% filter(risk_dsl == risk_base) %>% group_by(scenario, n, learner_base) %>% summarise(count = n(), .groups = "drop") %>% mutate(pct = count / 100)
p4 <- ggplot(selection_summary, aes(x = factor(n), y = pct, fill = learner_base)) + geom_col(position = "stack", width = 0.7, alpha = 0.85) + facet_wrap(~scenario, ncol = 3, labeller = labeller(scenario = scenario_labels)) + scale_fill_manual(values = colors_learner) + theme_bw(base_size = 13) + theme(legend.position = "bottom")

png(file.path(target_dir, "scelta1_1_risk_DSL.png"), width = 14*150, height = 5.5*150, res = 150); print(p1); dev.off()
png(file.path(target_dir, "scelta1_2_gap_DSL.png"), width = 12*150, height = 5.5*150, res = 150); print(p2); dev.off()
png(file.path(target_dir, "scelta1_3_boxplot_rischi.png"), width = 14*150, height = 5.5*150, res = 150); print(p3); dev.off()
png(file.path(target_dir, "scelta1_4_frequenze_selezione.png"), width = 14*150, height = 5.5*150, res = 150); print(p4); dev.off()
cat(sprintf("=== GRAFICI SCELTA 1 SALVATI IN '%s' ===\n", target_dir))