# ============================================================
#  SuperLearner vs Oracle — FASE 3: : Confronto Convex vs Discrete
# ============================================================

library(SuperLearner)
library(mgcv)
library(randomForest)
library(e1071)
library(KernelKnn) 
library(dplyr)
library(tidyr)
library(future)
library(future.apply)


output_dir <- "./risultati_scelta2"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

options(future.rng.onMisuse = "ignore") 

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
    X2 <- rexp(n, rate = 2) - 0.5; X3 <- runif(n, -1, 1)
  } else {
    rho <- 0.7; L <- chol(matrix(c(1, rho, rho, 1), 2, 2))
    Z <- matrix(rnorm(n * 2), ncol = 2) %*% L
    X1 <- Z[, 1]; X3 <- Z[, 2]; X2 <- 2 * rbeta(n, 2, 5) - 1
  }
  mu <- switch(scenario, "A" = X1 + 2 * X2, "B" = sin(pi * X1) + X2^2, "C" = sin(pi * X1) + X2^2 + X1 * X3)
  Y <- mu + rnorm(n, 0, sigma)
  data.frame(X1 = X1, X2 = X2, X3 = X3, mu = mu, Y = Y)
}

mse <- function(y, yhat) mean((y - yhat)^2)

sl_library <- c("SL.glm", "SL.gam", "SL.randomForest", "SL.svm", "SL.kernelKnn", "SL.mean")
learner_names <- c("GLM", "GAM", "RF", "SVM", "kNN", "Mean")

N_SIZES   <- c(100, 250, 500, 1000, 2000)
N_REP     <- 100               
N_TEST    <- 5000              
SIGMA     <- 1.0               
SCENARIOS <- c("A", "B", "C")  
V_FOLDS   <- 5                 

run_replica <- function(scenario, n, rep_id) {
  id_scen <- match(scenario, c("A", "B", "C"))
  train <- generate_data(n, scenario, seed = (rep_id * 10000 + id_scen * 1000 + n))
  test  <- generate_data(N_TEST, scenario, seed = (id_scen * 99991))
  
  X_train <- train[, c("X1","X2","X3")]; Y_train <- train$Y; X_test <- test[, c("X1","X2","X3")]
  
  sl_fit <- tryCatch(
    SuperLearner(Y = Y_train, X = X_train, newX = X_test, family = gaussian(), 
                 SL.library = sl_library, cvControl = list(V = V_FOLDS), verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(sl_fit)) return(NULL)
  
  # --- Convex SL ---
  y_hat_sl <- sl_fit$SL.predict
  risk_sl  <- mse(test$Y, y_hat_sl)
  
  Z_test <- sl_fit$library.predict
  risks <- apply(Z_test, 2, function(yhat) mse(test$Y, yhat))
  names(risks) <- learner_names
  risk_oracle <- min(risks)
  
  # --- Discrete SL ---
  discrete_sl_idx <- which.min(sl_fit$cvRisk)
  y_hat_dsl       <- Z_test[, discrete_sl_idx]
  risk_dsl        <- mse(test$Y, y_hat_dsl)
  
  rows_learners <- data.frame(scenario = scenario, n = n, rep = rep_id, learner = learner_names,
                              risk = risks, alpha = sl_fit$coef, is_oracle = risks == risk_oracle,
                              oracle_risk = risk_oracle, gap = risks - risk_oracle, type = "learner")
  
  row_sl <- data.frame(scenario = scenario, n = n, rep = rep_id, learner = "SL",
                       risk = risk_sl, alpha = NA_real_, is_oracle = FALSE,
                       oracle_risk = risk_oracle, gap = risk_sl - risk_oracle, type = "superlearner")
  
  row_dsl <- data.frame(scenario = scenario, n = n, rep = rep_id, learner = "DSL",
                        risk = risk_dsl, alpha = NA_real_, is_oracle = FALSE,
                        oracle_risk = risk_oracle, gap = risk_dsl - risk_oracle, type = "discretesuperlearner")
  rbind(rows_learners, row_sl, row_dsl)
}

disponibili <- availableCores()
utilizzati <- max(1, disponibili - 1) 
cat(sprintf("Rilevati %d core totali. ATTIVAZIONE SPRINT: Uso di %d core paralleli...\n", disponibili, utilizzati))
plan(multisession, workers = utilizzati, gc = TRUE)

cat("\nAvvio simulazione Scelta 2 (Scenari A, B, C)...\n")
t_start <- proc.time()
all_results <- list(); total_cells <- length(SCENARIOS) * length(N_SIZES); cell_i <- 0

for (scenario in SCENARIOS) {
  for (n in N_SIZES) {
    cell_i <- cell_i + 1
    cat(sprintf("[%d/%d] Scenario %s | n=%4d... ", cell_i, total_cells, scenario, n))
    t0 <- proc.time()
    reps <- future_lapply(seq_len(N_REP), function(r) run_replica(scenario, n, r), future.seed = TRUE)
    reps <- Filter(Negate(is.null), reps)
    cat(sprintf("%.1f sec\n", (proc.time() - t0)[["elapsed"]]))
    if(length(reps) > 0) all_results[[length(all_results) + 1]] <- bind_rows(reps)
  }
}
plan(sequential)
cat(sprintf("\nSimulazione globale completata in %.2f minutes.\n", (proc.time() - t_start)[["elapsed"]] / 60))

results <- bind_rows(all_results)
saveRDS(results, file.path(output_dir, "scelta2_results.rds")) 
cat(sprintf("=== FASE 3 SALVATA NELLA CARTELLA '%s' ===\n", output_dir))