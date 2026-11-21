# ============================================================
#  SuperLearner vs Oracle — FASE 2: Library dei Learner
#  Descrizione: definizione, fit e sanity-check di ogni learner
#               sui tre scenari della Fase 2
# ============================================================

library(mgcv)           # GAM
library(randomForest)   # Random Forest
library(e1071)          # SVM
library(knn.covertree)  # kNN
library(ggplot2)
library(dplyr)
library(tidyr)

set.seed(42)

# ============================================================
#  0. CARICA DATI FASE 2
# ============================================================
output_dir <- "./output"
datasets   <- readRDS(file.path(output_dir, "fase2_datasets.rds"))

scenarios <- c("A", "B", "C")

# ============================================================
#  1. INTERFACCIA COMUNE DEI LEARNER
# ============================================================

learner_glm <- list(
  name = "GLM",
  fit = function(df) {
    lm(Y ~ X1 + X2 + X3, data = df)
  },
  predict = function(model, df) {
    as.numeric(predict(model, newdata = df))
  }
)


learner_gam <- list(
  name = "GAM",
  fit = function(df) {
    mgcv::gam(Y ~ s(X1, k = 6) + s(X2, k = 6) + s(X3, k = 6),
              data = df, method = "REML")
  },
  predict = function(model, df) {
    as.numeric(predict(model, newdata = df))
  }
)


learner_rf <- list(
  name = "RF",
  fit = function(df) {
    randomForest::randomForest(
      Y ~ X1 + X2 + X3,
      data     = df,
      ntree    = 500,
      mtry     = 2,      
      nodesize = 5       
    )
  },
  predict = function(model, df) {
    as.numeric(predict(model, newdata = df))
  }
)


learner_svm <- list(
  name = "SVM",
  fit = function(df) {
    e1071::svm(
      Y ~ X1 + X2 + X3,
      data    = df,
      kernel  = "radial",
      cost    = 1,
      gamma   = 1/3,     
      epsilon = 0.1
    )
  },
  predict = function(model, df) {
    as.numeric(predict(model, newdata = df))
  }
)


learner_knn <- list(
  name = "kNN",
  fit = function(df) {
    
    list(
      X_train = as.matrix(df[, c("X1","X2","X3")]),
      Y_train = df$Y,
      k       = 10
    )
  },
  predict = function(model, df) {
    X_test <- as.matrix(df[, c("X1","X2","X3")])
    idx <- knn.covertree::find_knn(
      data  = model$X_train,
      query = X_test,
      k     = model$k
    )$index                         
    # media dei k vicini più prossimi
    apply(idx, 1, function(ii) mean(model$Y_train[ii]))
  }
)


learner_mean <- list(
  name = "Mean",
  fit = function(df) {
    list(mu = mean(df$Y))
  },
  predict = function(model, df) {
    rep(model$mu, nrow(df))
  }
)

# ============================================================
#  2. LIBRARY COMPLETA
# ============================================================
library_learners <- list(
  learner_glm,
  learner_gam,
  learner_rf,
  learner_svm,
  learner_knn,
  learner_mean
)

learner_names <- sapply(library_learners, `[[`, "name")
cat("Library learner:", paste(learner_names, collapse = ", "), "\n")

# ============================================================
#  3. FUNZIONE DI RISCHIO EMPIRICO
# ============================================================
mse <- function(y_true, y_pred) mean((y_true - y_pred)^2)

# ============================================================
#  4. SANITY CHECK — fit su ogni scenario, rischio su test set
# ============================================================


SIGMA <- 1.0

cat("\n========== SANITY CHECK — MSE su test set (n=10000) ==========\n")

results_list <- list()

for (s in scenarios) {
  train <- datasets[[s]]$train
  test  <- datasets[[s]]$test
  cat(sprintf("\n--- Scenario %s ---\n", s))
  cat(sprintf("  Bayes risk (irriducibile σ²): %.4f\n", SIGMA^2))

  for (lrn in library_learners) {
    model   <- lrn$fit(train)
    y_hat   <- lrn$predict(model, test)
    risk    <- mse(test$Y, y_hat)
    # rischio sul segnale puro (esclude rumore irriducibile)
    excess  <- risk - SIGMA^2

    cat(sprintf("  %-6s | MSE = %.4f | excess = %+.4f\n",
                lrn$name, risk, excess))

    results_list[[length(results_list) + 1]] <- data.frame(
      scenario = s,
      learner  = lrn$name,
      mse      = risk,
      excess   = excess
    )
  }
}

results_df <- bind_rows(results_list)

# ============================================================
#  5. ORACOLO PER SCENARIO (minimo MSE tra i learner)
# ============================================================
cat("\n========== ORACOLO (min MSE per scenario) ==========\n")

oracle_df <- results_df %>%
  group_by(scenario) %>%
  slice_min(mse, n = 1) %>%
  ungroup()

print(oracle_df[, c("scenario","learner","mse")])

# ============================================================
#  6. VISUALIZZAZIONE — MSE per learner e scenario
# ============================================================
results_df$learner <- factor(results_df$learner,
                              levels = c("GLM","GAM","RF","SVM","kNN","Mean"))

colors_learner <- c(
  GLM  = "#2E86AB",
  GAM  = "#E84855",
  RF   = "#3BB273",
  SVM  = "#F4A261",
  kNN  = "#9B5DE5",
  Mean = "#aaaaaa"
)

# Bayes risk come linea orizzontale
bayes_line <- data.frame(scenario = scenarios, bayes = SIGMA^2)

p_mse <- ggplot(results_df, aes(x = learner, y = mse, fill = learner)) +
  geom_col(width = 0.65, alpha = 0.85) +
  geom_hline(data = bayes_line,
             aes(yintercept = bayes),
             linetype = "dashed", colour = "grey30", linewidth = 0.7) +
  annotate("text", x = 0.6, y = SIGMA^2 + 0.03,
           label = "Bayes risk", hjust = 0, size = 3.2, colour = "grey30") +
  facet_wrap(~scenario, labeller = labeller(scenario = c(
    A = "Scenario A — Lineare",
    B = "Scenario B — Smooth",
    C = "Scenario C — Interazioni"
  ))) +
  scale_fill_manual(values = colors_learner) +
  labs(
    title    = "MSE su test set per learner e scenario  (n_train = 500)",
    subtitle = "La linea tratteggiata è il rischio di Bayes σ² = 1  (irriducibile)",
    x = NULL, y = "MSE"
  ) +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#f0f0f0"),
        strip.text       = element_text(face = "bold"),
        axis.text.x      = element_text(face = "bold"))

# Heatmap MSE (learner × scenario) — anteprima della Fase 7
heatmap_df <- results_df %>%
  mutate(scenario_label = paste0("Sc. ", scenario))

p_heat <- ggplot(heatmap_df,
                 aes(x = scenario_label, y = learner, fill = mse)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.3f", mse)),
            size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#d4f1c0", high = "#b5182b",
                      name = "MSE") +
  scale_y_discrete(limits = rev(levels(results_df$learner))) +
  labs(title   = "Heatmap MSE — Fase 3 sanity check",
       subtitle = "Verde = basso errore, rosso = alto errore",
       x = NULL, y = NULL) +
  theme_bw(base_size = 13) +
  theme(strip.background = element_rect(fill = "#f0f0f0"))

# ============================================================
#  7. SALVA
# ============================================================
ggsave(file.path(output_dir, "fase3_mse_barplot.png"),
       p_mse, width = 12, height = 4.5, dpi = 150)

ggsave(file.path(output_dir, "fase3_mse_heatmap.png"),
       p_heat, width = 6, height = 4.5, dpi = 150)

saveRDS(library_learners, file.path(output_dir, "fase3_library.rds"))
saveRDS(results_df,       file.path(output_dir, "fase3_results_sanity.rds"))

cat("\n== File salvati ==\n")
cat("  fase2_library.rds          (oggetto library, importabile nelle fasi successive)\n")
cat("  fase2_results_sanity.rds   (MSE sanity check)\n")
cat("  fase2_mse_barplot.png\n")
cat("  fase2_mse_heatmap.png\n")
cat("\n=== FASE 2 completata ===\n")
