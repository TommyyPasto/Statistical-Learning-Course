# ============================================================
#  SuperLearner vs Oracle — FASE 1: Design Simulativo
#  Autore: progetto tesi
#  Descrizione: definizione dei DGP
#               per i tre scenari e analisi esplorativa
# ============================================================

# ---- 0. Pacchetti ----
required_packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra", "GGally")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

set.seed(42)

# ============================================================
#  1. PARAMETRI GLOBALI
# ============================================================
N_TRAIN   <- 500       # numerosità train (varierà nella Fase 4)
N_TEST    <- 10000     # test set grande per stimare R(f) ≈ E[loss]
P         <- 3         # numero di covariate (X1, X2, X3)
SIGMA     <- 1.0       # deviazione standard del rumore ε ~ N(0, σ²)

# ============================================================
#  2. FUNZIONE GENERATRICE DATI  (DGP)
# ============================================================

generate_data <- function(n, scenario = c("A", "B", "C"),
                           sigma = SIGMA, seed = NULL) {
  scenario <- match.arg(scenario)
  if (!is.null(seed)) set.seed(seed)

  if (scenario == "A") {
    # ── Scenario A: tutte U(-1,1), semplice e pulito ──────────────────
    X1 <- runif(n, -1, 1)
    X2 <- runif(n, -1, 1)
    X3 <- runif(n, -1, 1)

  } else if (scenario == "B") {
    # ── Scenario B: distribuzioni eterogenee, indipendenti ────────────
    # X1 ~ N(0,1) troncata in [-3, 3]
    X1 <- local({
      out <- numeric(n)
      needed <- n
      while (needed > 0) {
        draws <- rnorm(needed * 2)
        draws <- draws[abs(draws) <= 3]
        take  <- min(length(draws), needed)
        out[(n - needed + 1):(n - needed + take)] <- draws[seq_len(take)]
        needed <- needed - take
      }
      out
    })
    # X2 ~ Exp(2) centrata e scalata in [-1, ~2]: meno varianza esplosiva
    X2 <- rexp(n, rate = 2) - 0.5
    # X3 non entra nel DGP di B: U(-1,1)
    X3 <- runif(n, -1, 1)

  } else {
    # ── Scenario C: X1 e X3 correlate (ρ = 0.7) via Cholesky ─────────

    rho <- 0.7
    L   <- chol(matrix(c(1, rho, rho, 1), 2, 2))   # decomposizione di Cholesky
    Z   <- matrix(rnorm(n * 2), ncol = 2) %*% L     # Z ~ N(0, Sigma)
    X1  <- Z[, 1]
    X3  <- Z[, 2]
    # X2 ~ Beta(2,5) riscalata in [-1,1]: asimmetrica a sinistra
    X2  <- 2 * rbeta(n, shape1 = 2, shape2 = 5) - 1
  }

  mu <- switch(scenario,
               
    "A" = X1 + 2 * X2,
    
    "B" = sin(pi * X1) + X2^2,

    "C" = sin(pi * X1) + X2^2 + X1 * X3
  )

  epsilon <- rnorm(n, mean = 0, sd = sigma)
  Y <- mu + epsilon

  data.frame(X1 = X1, X2 = X2, X3 = X3, mu = mu, Y = Y,
             scenario = scenario)
}

# ============================================================
#  3. GENERA I DATASET (train + test per ogni scenario)
# ============================================================
scenarios <- c("A", "B", "C")

# Lista con train e test per ogni scenario
datasets <- lapply(setNames(scenarios, scenarios), function(s) {
  list(
    train = generate_data(N_TRAIN, scenario = s, seed = 100),
    test  = generate_data(N_TEST,  scenario = s, seed = 200)
  )
})

cat("== Dataset generati ==\n")
for (s in scenarios) {
  d <- datasets[[s]]$train
  cat(sprintf(
    "Scenario %s | n_train=%d | E[Y]=%.3f | Var(Y)=%.3f | SNR=%.2f\n",
    s, nrow(d), mean(d$Y), var(d$Y),
    var(d$mu) / (SIGMA^2)   # Signal-to-Noise Ratio
  ))
}

# ============================================================
#  4. FUNZIONE DI RISCHIO TEORICO (Bayes Risk)
# ============================================================


bayes_risk <- SIGMA^2
cat(sprintf("\nRischio di Bayes (irriducibile): %.4f\n", bayes_risk))

# ============================================================
#  5. SUMMARY STATISTICO PER SCENARIO
# ============================================================
summarise_scenario <- function(df, label) {
  cat(sprintf("\n--- Scenario %s ---\n", label))
  cat(sprintf("  Range Y    : [%.3f, %.3f]\n", min(df$Y), max(df$Y)))
  cat(sprintf("  Media Y    : %.4f\n", mean(df$Y)))
  cat(sprintf("  Var(μ)     : %.4f  (varianza del segnale)\n", var(df$mu)))
  cat(sprintf("  Var(ε)     : %.4f  (rumore teorico σ²)\n", SIGMA^2))
  cat(sprintf("  Var(Y)     : %.4f  = Var(μ) + σ²\n", var(df$Y)))
  cat(sprintf("  SNR        : %.2f\n", var(df$mu) / SIGMA^2))
  cat(sprintf("  R² teorico : %.4f  = Var(μ)/Var(Y)\n",
              var(df$mu) / var(df$Y)))
}

for (s in scenarios) summarise_scenario(datasets[[s]]$train, s)


cat("\n--- Correlazioni empiriche Scenario C ---\n")
cor_C <- cor(datasets$C$train[, c("X1","X2","X3")])
print(round(cor_C, 3))

# ============================================================
#  6. VISUALIZZAZIONI ESPLORATIVE
# ============================================================


all_train <- bind_rows(lapply(scenarios, function(s) datasets[[s]]$train))

p_hist <- ggplot(all_train, aes(x = Y, fill = scenario)) +
  geom_histogram(bins = 40, alpha = 0.7, colour = "white", linewidth = 0.2) +
  facet_wrap(~scenario, labeller = labeller(scenario = c(
    A = "Scenario A — Lineare",
    B = "Scenario B — Smooth non lineare",
    C = "Scenario C — Interazioni complesse"
  ))) +
  scale_fill_manual(values = c(A = "#2E86AB", B = "#E84855", C = "#3BB273")) +
  labs(title = "Distribuzione di Y nei tre scenari",
       subtitle = sprintf("n = %d per scenario, σ = %.1f", N_TRAIN, SIGMA),
       x = "Y", y = "Conteggio") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#f0f0f0"),
        strip.text = element_text(face = "bold"))


x1_grid <- seq(-1, 1, length.out = 300)
signal_df <- bind_rows(lapply(scenarios, function(s) {
  mu_vals <- switch(s,
    "A" = x1_grid + 2 * 0,           # X2=0
    "B" = sin(pi * x1_grid) + 0^2,   # X2=0
    "C" = sin(pi * x1_grid) + 0^2 + x1_grid * 0  # X2=X3=0
  )
  data.frame(X1 = x1_grid, mu = mu_vals, scenario = s)
}))

p_signal_x1 <- ggplot(signal_df, aes(x = X1, y = mu, colour = scenario)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~scenario, labeller = labeller(scenario = c(
    A = "Scenario A — Lineare",
    B = "Scenario B — Smooth",
    C = "Scenario C — Interazioni"
  ))) +
  scale_colour_manual(values = c(A = "#2E86AB", B = "#E84855", C = "#3BB273")) +
  labs(title = "Segnale vero μ(X1) con X2 = X3 = 0",
       x = "X1", y = "μ(x)") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#f0f0f0"),
        strip.text = element_text(face = "bold"))


p_scatter <- ggplot(all_train, aes(x = mu, y = Y, colour = scenario)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey30", linewidth = 0.8) +
  facet_wrap(~scenario, labeller = labeller(scenario = c(
    A = "Scenario A",
    B = "Scenario B",
    C = "Scenario C"
  ))) +
  scale_colour_manual(values = c(A = "#2E86AB", B = "#E84855", C = "#3BB273")) +
  labs(title = "Y vs segnale vero μ(X)  —  la linea tratteggiata è Y = μ",
       subtitle = "La dispersione attorno alla diagonale è il rumore irriducibile ε",
       x = "μ(X)  (segnale vero)", y = "Y  (osservato)") +
  theme_bw(base_size = 13) +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "#f0f0f0"),
        strip.text = element_text(face = "bold"))


cov_long <- bind_rows(
  # Scenario A
  data.frame(var = "X1", value = datasets$A$train$X1, scenario = "A"),
  data.frame(var = "X2", value = datasets$A$train$X2, scenario = "A"),
  data.frame(var = "X3", value = datasets$A$train$X3, scenario = "A"),
  # Scenario B
  data.frame(var = "X1", value = datasets$B$train$X1, scenario = "B"),
  data.frame(var = "X2", value = datasets$B$train$X2, scenario = "B"),
  data.frame(var = "X3", value = datasets$B$train$X3, scenario = "B"),
  # Scenario C
  data.frame(var = "X1", value = datasets$C$train$X1, scenario = "C"),
  data.frame(var = "X2", value = datasets$C$train$X2, scenario = "C"),
  data.frame(var = "X3", value = datasets$C$train$X3, scenario = "C")
)

cov_long$label <- factor(
  paste0("Scenario ", cov_long$scenario),
  levels = c("Scenario A", "Scenario B", "Scenario C")
)

p_margins <- ggplot(cov_long, aes(x = value, fill = var)) +
  geom_density(alpha = 0.55, colour = "white", linewidth = 0.3) +
  facet_grid(var ~ label) +
  scale_fill_manual(values = c(X1 = "#2E86AB", X2 = "#E84855", X3 = "#F4A261"),
                    name = "Covariata") +
  labs(title = "Distribuzioni marginali delle covariate per scenario",
       subtitle = paste0(
         "A: tutte U(-1,1)  |  ",
         "B: X\u2081~N(0,1) troncata, X\u2082~Exp(1) centrata  |  ",
         "C: (X\u2081,X\u2083) correlate \u03c1=0.7, X\u2082~Beta(2,5) riscalata"),
       x = "Valore", y = "Densit\u00e0") +
  theme_bw(base_size = 12) +
  theme(strip.background = element_rect(fill = "#f0f0f0"),
        strip.text = element_text(face = "bold"),
        legend.position = "none")


x2_mean_C <- 2 * (2/7) - 1
grid_2d <- expand.grid(
  X1 = seq(-3, 3, length.out = 80),
  X3 = seq(-3, 3, length.out = 80)
)
grid_2d$mu_C <- sin(pi * grid_2d$X1) + x2_mean_C^2 + grid_2d$X1 * grid_2d$X3

p_surface_C <- ggplot(grid_2d, aes(x = X1, y = X3, fill = mu_C)) +
  geom_tile() +
  scale_fill_gradient2(low = "#1a3a6b", mid = "#f5f5f5", high = "#9b1c1c",
                       midpoint = 0, name = "\u03bc(x)") +
  labs(title = "Scenario C — Superficie \u03bc(X\u2081, X\u2083) con X\u2082 = E[X\u2082]",
       subtitle = sprintf(
         "X\u2082 fissato a %.2f (media di Beta(2,5) riscalata) | dominio ampliato per X\u2081,X\u2083 ~ N(0,1)",
         x2_mean_C),
       x = "X\u2081", y = "X\u2083") +
  coord_fixed() +
  theme_bw(base_size = 13)

# ============================================================
#  7. SALVA GRAFICI
# ============================================================
output_dir <- "./output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(file.path(output_dir, "fase1_distribuzione_Y.png"),
       p_hist, width = 11, height = 4, dpi = 150)

ggsave(file.path(output_dir, "fase1_segnale_X1.png"),
       p_signal_x1, width = 11, height = 4, dpi = 150)

ggsave(file.path(output_dir, "fase1_scatter_Y_vs_mu.png"),
       p_scatter, width = 11, height = 4, dpi = 150)

ggsave(file.path(output_dir, "fase1_distribuzioni_covariate.png"),
       p_margins, width = 11, height = 7, dpi = 150)

ggsave(file.path(output_dir, "fase1_superficie_scenarioC.png"),
       p_surface_C, width = 7, height = 6, dpi = 150)

cat("\n== Grafici salvati in", output_dir, "==\n")

# ============================================================
#  8. SALVA I DATASET 
# ============================================================
saveRDS(datasets,    file.path(output_dir, "fase1_datasets.rds"))
saveRDS(generate_data, file.path(output_dir, "fase1_generate_data_fn.rds"))

cat("== Oggetti R salvati: fase1_datasets.rds, fase1_generate_data_fn.rds ==\n")
cat("\n=== FASE 1 completata ===\n")