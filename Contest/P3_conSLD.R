# ============================================================
#  SuperLearner vs Oracle — FASE 3: Solo Discrete SL
# ============================================================

# ---- 0. Controllo e Installazione Pacchetti ----
required_packages <- c("SuperLearner", "mgcv", "randomForest", "e1071", 
                       "KernelKnn", "dplyr", "tidyr", "future", "future.apply")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(SuperLearner)
library(mgcv)
library(randomForest)
library(e1071)
library(KernelKnn)
library(dplyr)
library(tidyr)
library(future)
library(future.apply)

output_dir <- "."

options(future.rng.onMisuse = "ignore") 

# ============================================================
#  1. FUNZIONE GENERATRICE DATI (DGP Scenari A, B, C)
# ============================================================
generate_data <- function(n, scenario, sigma = 1.0, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  if (scenario == "A") {
    X1 <- runif(n, -1, 1); X2 <- runif(n, -1, 1); X3 <- runif(n, -1, 1)
  } else if (scenario == "B") {
    X1 <- local({
      out <- numeric(n); needed <- n
      while (needed > 0) {
        draws <- rnorm(needed * 2); draws <- draws[abs(draws) <= 3]
        take  <- min(length(draws), needed)
        out[(n - needed + 1):(n - needed + take)] <- draws[seq_len(take)]
        needed <- needed - take
      }; out
    })
    X2 <- rexp(n, rate = 2) - 0.5
    X3 <- runif(n, -1, 1)
  } else {
    # Scenario C: X1 e X3 fortemente correlate (rho = 0.7)
    rho <- 0.7
    L   <- chol(matrix(c(1, rho, rho, 1), 2, 2))
    Z   <- matrix(rnorm(n * 2), ncol = 2) %*% L
    X1  <- Z[, 1]; X3 <- Z[, 2]
    X2  <- 2 * rbeta(n, 2, 5) - 1
  }
  
  mu <- switch(scenario,
               "A" = X1 + 2 * X2,
               "B" = sin(pi * X1) + X2^2,
               "C" = sin(pi * X1) + X2^2 + X1 * X3
  )
  Y <- mu + rnorm(n, 0, sigma)
  data.frame(X1 = X1, X2 = X2, X3 = X3, mu = mu, Y = Y)
}

mse <- function(y, yhat) mean((y - yhat)^2)

# ============================================================
#  2. LIBRERIA ALGORITMI SUPERLEARNER
# ============================================================
sl_library <- c(
  "SL.glm",          # GLM lineare classico
  "SL.gam",          # GAM (Spline non lineari)
  "SL.randomForest", # Random Forest
  "SL.svm",          # Support Vector Machine
  "SL.kernelKnn",    # k-Nearest Neighbors (versione robusta)
  "SL.mean"          # Controllo negativo (Modello stupido)
)

learner_names <- c("GLM", "GAM", "RF", "SVM", "kNN", "Mean")
K             <- length(sl_library)

# ============================================================
#  3. PARAMETRI DEL DISEGNO SPERIMENTALE
# ============================================================
N_SIZES   <- c(100, 250, 500, 1000, 2000)
N_REP     <- 100               # Numero di repliche Monte Carlo per cella
N_TEST    <- 5000              # Test set grande per calcolare il rischio empirico stabile
SIGMA     <- 1.0               # Errore irriducibile (Rischio di Bayes)
SCENARIOS <- c("A", "B", "C")  # Tutti e tre gli scenari sbloccati
V_FOLDS   <- 5                 # Numero di Fold per la Cross-Validation dei pesi

# ============================================================
#  4. FUNZIONE DELLA SINGOLA REPLICA MONTE CARLO
# ============================================================
run_replica <- function(scenario, n, rep_id) {
  
 
  id_scen <- match(scenario, c("A", "B", "C"))
  
  # Generazione Train e Test Set
  train <- generate_data(n, scenario, seed = (rep_id * 10000 + id_scen * 1000 + n))
  test  <- generate_data(N_TEST, scenario, seed = (id_scen * 99991))
  
  X_train <- train[, c("X1","X2","X3")]
  Y_train <- train$Y
  X_test  <- test[,  c("X1","X2","X3")]
  
  # Addestramento del SuperLearner ufficiale
  sl_fit <- tryCatch(
    SuperLearner(
      Y          = Y_train,
      X          = X_train,
      newX       = X_test,
      family     = gaussian(),
      SL.library = sl_library,
      cvControl  = list(V = V_FOLDS),
      verbose    = FALSE
    ),
    error = function(e) NULL
  )
  
  if (is.null(sl_fit)) return(NULL)
  
  # Estrazione dei coefficienti alfa (i pesi dell'ensemble)
  alpha <- sl_fit$coef
  names(alpha) <- learner_names
  
  # Calcolo del rischio (MSE) del SuperLearner sul Test Set
  y_hat_sl <- sl_fit$SL.predict
  risk_sl  <- mse(test$Y, y_hat_sl)
  
  # Estrazione delle predizioni dei singoli modelli fittati sul train intero
  Z_test <- sl_fit$library.predict
  risks  <- apply(Z_test, 2, function(yhat) mse(test$Y, yhat))
  names(risks) <- learner_names
  
  # Individuazione dell'Oracolo della Libreria
  risk_oracle <- min(risks)
  

  rows_learners <- data.frame(
    scenario = scenario, n = n, rep = rep_id, learner = learner_names,
    risk = risks, alpha = alpha, is_oracle = risks == risk_oracle,
    oracle_risk = risk_oracle, gap = risks - risk_oracle, type = "learner"
  )
  

  row_sl <- data.frame(
    scenario = scenario, n = n, rep = rep_id, learner = "SL",
    risk = risk_sl, alpha = NA_real_, is_oracle = FALSE,
    oracle_risk = risk_oracle, gap = risk_sl - risk_oracle, type = "superlearner"
  )
  
  rbind(rows_learners, row_sl)
}

# ============================================================
#  5. CONFIGURAZIONE DEL MULTIPROCESSING BILANCIATO (Anti-Crash)
# ============================================================
disponibili <- availableCores()
utilizzati <- min(4, max(1, disponibili - 2)) 

cat(sprintf("Rilevati %d core totali. Attivazione simulazione su %d core paralleli...\n", disponibili, utilizzati))
plan(multisession, workers = utilizzati, gc = TRUE)

# ============================================================
#  6. LOOP DI SIMULAZIONE PRINCIPALE (15 Combinazioni Totali)
# ============================================================
cat("\nAvvio simulazione nei tre Scenari (A, B, C)...\n")
t_start <- proc.time()

all_results  <- list()
total_cells  <- length(SCENARIOS) * length(N_SIZES)
cell_i       <- 0

for (scenario in SCENARIOS) {
  for (n in N_SIZES) {
    cell_i <- cell_i + 1
    cat(sprintf("[%d/%d] Scenario %s | n=%4d | Elaborazione di %d repliche in corso... ",
                cell_i, total_cells, scenario, n, N_REP))
    t0 <- proc.time()
    
    # Distribuzione parallela e riproducibile delle 100 repliche sui core
    reps <- future_lapply(seq_len(N_REP), function(r) {
      run_replica(scenario, n, r)
    }, future.seed = TRUE)
    
    # Rimozione di eventuali repliche fallite 
    reps <- Filter(Negate(is.null), reps)
    
    elapsed <- (proc.time() - t0)[["elapsed"]]
    cat(sprintf("%.1f secondi (%d repliche OK)\n", elapsed, length(reps)))
    
    if(length(reps) > 0) {
      all_results[[length(all_results) + 1]] <- bind_rows(reps)
    }
  }
}


plan(sequential)

t_total <- (proc.time() - t_start)[["elapsed"]]
cat(sprintf("\nSimulazione globale completata in %.2f minuti.\n", t_total / 60))

# ============================================================
#  7. AGGREGAZIONE, STAMPA E SALVATAGGIO DEI RISULTATI (LE RIGHE MANCANTI)
# ============================================================
results <- bind_rows(all_results)


snr_map <- c(A = 1.72, B = 0.87, C = 2.59)
results$snr <- snr_map[results$scenario]

cat("\n========== ANTEPRIMA: Gap Medio dall'Oracolo ==========\n")
summary_gap <- results %>%
  filter(type == "superlearner") %>%
  group_by(scenario, n) %>%
  summarise(
    gap_mean = mean(gap, na.rm = TRUE),
    gap_sd   = sd(gap, na.rm = TRUE),
    .groups  = "drop"
  )
print(summary_gap, n = Inf)


saveRDS(results,     file.path(output_dir, "fase3_results.rds"))
saveRDS(summary_gap, file.path(output_dir, "fase3_summary_gap.rds"))

cat(sprintf("\n[OK COMPIUTO] Oggetti salvati nella cartella: '%s'\n", output_dir))
cat("=== FASE 3 COMPLETATA AL 100% IN TUTTI I DETTAGLI ===\n")