# ============================================================
#  SOLO GRAFICI — FASE FINALE: Confronto Convex vs Discrete
# ============================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis) 

target_dir <- "./risultati_scelta2" 
file_path <- file.path(target_dir, "scelta2_results.rds")

if (!file.exists(file_path)) stop("Errore: Manca il file 'scelta2_results.rds'. Esegui prima il Passo 1!")
results <- readRDS(file_path)

scenario_labels <- c(A = "Scenario A — Lineare", B = "Scenario B — Smooth non lineare", C = "Scenario C — Interazioni complesse")
colors_learner <- c(GLM = "#2E86AB", GAM = "#E84855", RF = "#3BB273", SVM = "#F4A261", kNN = "#9B5DE5", Mean = "#aaaaaa", SL = "#1a1a2e", DSL = "#D81B60", Oracle = "#FFD700")
learner_levels <- c("GLM","GAM","RF","SVM","kNN","Mean","SL","DSL")

base_res <- results %>% filter(type == "learner")
sl_res   <- results %>% filter(type == "superlearner")
dsl_res  <- results %>% filter(type == "discretesuperlearner")

# ------------------------------------------------------------
# GRAFICO 1 — Rischio Atteso vs n
# ------------------------------------------------------------
cat("Generazione Grafico 1: Rischio vs n...\n")
risk_base <- base_res %>% group_by(scenario, n, learner) %>% summarise(risk_mean = mean(risk, na.rm = TRUE), .groups = "drop")
risk_sl <- sl_res %>% group_by(scenario, n) %>% summarise(risk_mean = mean(risk, na.rm = TRUE), .groups = "drop") %>% mutate(learner = "SL")
risk_dsl <- dsl_res %>% group_by(scenario, n) %>% summarise(risk_mean = mean(risk, na.rm = TRUE), .groups = "drop") %>% mutate(learner = "DSL")
risk_oracle <- sl_res %>% group_by(scenario, n) %>% summarise(risk_mean = mean(oracle_risk, na.rm = TRUE), .groups = "drop") %>% mutate(learner = "Oracle")
risk_all <- bind_rows(risk_base, risk_sl, risk_dsl, risk_oracle)
risk_all$learner <- factor(risk_all$learner, levels = c(learner_levels, "Oracle"))
risk_all$ltype <- case_when(risk_all$learner %in% c("SL", "DSL") ~ "SL_family", risk_all$learner == "Oracle" ~ "Oracle", TRUE ~ "learner")

p1 <- ggplot(risk_all, aes(x = n, y = risk_mean, colour = learner, linetype = ltype, linewidth = ltype)) +
  geom_line() + geom_point(data = filter(risk_all, learner %in% c("SL","DSL","Oracle")), size = 2.5) +
  geom_hline(yintercept = 1, linetype = "dotted", colour = "grey40", linewidth = 0.5) +
  scale_colour_manual(values = colors_learner, name = "Learner") + 
  scale_linetype_manual(values = c(SL_family = "solid", Oracle = "dashed", learner = "solid"), guide = "none") + 
  scale_linewidth_manual(values = c(SL_family = 1.4, Oracle = 1.4, learner = 0.55), guide = "none") +
  scale_x_log10(breaks = c(100, 250, 500, 1000, 2000)) + 
  facet_wrap(~scenario, scales = "free_y", ncol = 3, labeller = labeller(scenario = scenario_labels)) + 
  
  labs(title = "Rischio atteso vs numerosità campionaria", 
       subtitle = "Media su 100 repliche Monte Carlo | scala x logaritmica | linea tratteggiata = Oracle",
       x = "n", y = "risk_mean") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom", strip.background = element_rect(fill = "#f0f0f0"), 
        strip.text = element_text(face = "bold"), panel.grid.minor = element_blank(),
        plot.title = element_text(size = 13), plot.subtitle = element_text(size = 10, face = "italic", colour = "grey30"))

# ------------------------------------------------------------
# GRAFICO 2 — Gap Oracle con Bande IQR 
# ------------------------------------------------------------
cat("Generazione Grafico 2: Gap Oracle Unificato con IQR...\n")


gap_summary <- sl_res %>% 
  group_by(scenario, n) %>% 
  summarise(
    gap_mean = mean(gap, na.rm = TRUE),
    gap_q25  = quantile(gap, 0.25, na.rm = TRUE),
    gap_q75  = quantile(gap, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(gap_summary, aes(x = n, y = gap_mean, colour = scenario, fill = scenario)) + 
  
  geom_ribbon(aes(ymin = gap_q25, ymax = gap_q75), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 2.5) + 
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey30", linewidth = 0.6) + 
 
  scale_colour_manual(values = c(A = "#2E86AB", B = "#E84855", C = "#3BB273"), labels = scenario_labels, name = "scenario") +
  scale_fill_manual(values = c(A = "#2E86AB", B = "#E84855", C = "#3BB273"), labels = scenario_labels, name = "scenario") +
  scale_x_log10(breaks = c(100, 250, 500, 1000, 2000)) + 
  
  labs(title = "Gap oracle R(SL) - R(Oracle) vs numerosità campionaria",
       subtitle = "Linea = media | banda = IQR [Q25, Q75] su 100 repliche | scala x logaritmica",
       x = "n", y = "gap_mean") +
  theme_bw(base_size = 12) + 
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        plot.title = element_text(size = 13), plot.subtitle = element_text(size = 10, face = "italic", colour = "grey30"))

# ------------------------------------------------------------
# GRAFICO 3 — Boxplot di stabilità a n=500
# ------------------------------------------------------------
cat("Generazione Grafico 3: Boxplot variabilità...\n")
box_data <- bind_rows(base_res %>% filter(n == 500) %>% select(scenario, learner, risk), sl_res %>% filter(n == 500) %>% select(scenario, learner, risk), dsl_res %>% filter(n == 500) %>% select(scenario, learner, risk)) %>% mutate(learner = factor(learner, levels = learner_levels))
p3 <- ggplot(box_data, aes(x = learner, y = risk, fill = learner)) + 
  geom_boxplot(alpha = 0.8, outlier.size = 0.8, outlier.alpha = 0.4, linewidth = 0.4, na.rm = TRUE) + 
  geom_hline(yintercept = 1, linetype = "dotted", colour = "grey30", linewidth = 0.6) + 
  scale_fill_manual(values = colors_learner) + facet_wrap(~scenario, scales = "free_y", ncol = 3, labeller = labeller(scenario = scenario_labels)) + 
  labs(title = "Distribuzione del rischio empirico a n = 500", x = NULL, y = "Rischio (MSE)") +
  theme_bw(base_size = 13) + theme(legend.position = "none", strip.background = element_rect(fill = "#f0f0f0"), strip.text = element_text(face = "bold"), axis.text.x = element_text(face = "bold"))

# ------------------------------------------------------------
# GRAFICO 4 — Heatmap dei pesi α 
# ------------------------------------------------------------
cat("Generazione Grafico 4: Heatmap pesi (Sfumatura Blu)...\n")
weights_summary <- base_res %>% group_by(scenario, n, learner) %>% summarise(alpha_mean = mean(alpha, na.rm = TRUE), .groups = "drop") %>% mutate(learner = factor(learner, levels = rev(c("GLM","GAM","RF","SVM","kNN","Mean"))))
p4 <- ggplot(weights_summary, aes(x = factor(n), y = learner, fill = alpha_mean)) + 
  geom_tile(colour = "white", linewidth = 0.7) + 
  geom_text(aes(label = sprintf("%.2f", alpha_mean)), size = 3.5, fontface = "bold", colour = ifelse(weights_summary$alpha_mean > 0.35, "white", "grey20")) + 
  scale_fill_gradient(low = "#f0f9ff", high = "#075985", name = "Peso \u03b1") + 
  facet_wrap(~scenario, ncol = 3, labeller = labeller(scenario = scenario_labels)) + 
  labs(title = "Distribuzione asintotica dei Pesi del Convex SuperLearner", x = "Dimensione Campionaria (n)", y = NULL) +
  theme_bw(base_size = 12) + theme(strip.background = element_rect(fill = "#f0f0f0"), strip.text = element_text(face = "bold"), axis.text.y = element_text(face = "bold"), axis.text.x = element_text(face = "bold"))

# ------------------------------------------------------------
# GRAFICO 5 — Heatmap MSE Faceted (Stile Matrice ordinata)
# ------------------------------------------------------------
cat("Generazione Grafico 5: Matrice MSE Globale ...\n")

heatmap_labels_clean <- c( 
  A = "Scenario A\nLineare", 
  B = "Scenario B\nSmooth non lineare", 
  C = "Scenario C\nInterazioni complesse" 
) 

mse_global <- results %>%  
  group_by(scenario, learner) %>%  
  summarise(mse_mean = mean(risk, na.rm = TRUE), .groups = "drop") %>%  
  mutate( 
    learner  = factor(learner, levels = rev(c("GLM","GAM","RF","SVM","kNN","Mean","SL","DSL"))), 
    scenario = factor(scenario, levels = c("A", "B", "C"), labels = heatmap_labels_clean) 
  ) 

p5 <- ggplot(mse_global, aes(x = scenario, y = learner, fill = mse_mean)) +
  geom_tile(colour = "white", linewidth = 1.0) +
  geom_text(aes(label = sprintf("%.2f", mse_mean)), size = 4.2, fontface = "bold", 
            colour = ifelse(mse_global$mse_mean > 2.0, "white", "#111827")) +
  
  scale_fill_gradient(low = "#a7f3d0", high = "#dc2626", name = "MSE Medio\nGlobale") + 
  
  labs(title = "Profilo delle performance predittive complessive (Sanity Check)", 
       subtitle = "Rischio atteso globale per scenario: Verde = performance ottimali, Rosso = errori elevati",
       x = NULL, y = NULL) +
  
  theme_bw(base_size = 13) + 
  theme(
    axis.text.x = element_text(face = "bold", size = 11, colour = "grey15"), 
    axis.text.y = element_text(face = "bold", size = 11),
    panel.grid = element_blank(), 
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, colour = "grey10"),

    aspect.ratio = 0.78 
  )
# ------------------------------------------------------------
# Salvataggio protetto sul disco
# ------------------------------------------------------------
png(file.path(target_dir, "scelta2_1_risk_vs_n.png"), width = 14*150, height = 5.5*150, res = 150); print(p1); dev.off()
png(file.path(target_dir, "scelta2_2_gap_oracle.png"), width = 10*150, height = 5.5*150, res = 150); print(p2); dev.off() # Dimensioni ottimizzate per pannello singolo
png(file.path(target_dir, "scelta2_3_boxplot_rischi.png"), width = 14*150, height = 5.5*150, res = 150); print(p3); dev.off()
png(file.path(target_dir, "scelta2_4_heatmap_pesi.png"), width = 14*150, height = 5.5*150, res = 150); print(p4); dev.off()
png(file.path(target_dir, "scelta2_5_sanity_heatmap_mse.png"), width = 14*150, height = 5.5*150, res = 150); print(p5); dev.off()

cat(sprintf("=== GRAFICI SCELTA 2 SALVATI IN '%s' ===\n", target_dir))