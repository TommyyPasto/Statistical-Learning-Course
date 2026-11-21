# MODULE 3 EVALUATION
# Input output di train models piu Y test
# Output errori di test per LDA QDA RDA CV

source("Parte2.R")

evaluate_models <- function(fit, Y_test) {
  
  lda_error <- mean(fit$lda$pred_test != Y_test)
  qda_error <- mean(fit$qda$pred_test != Y_test)
  
  rda_cv_error <- NA
  lambda_cv    <- NA
  gamma_cv     <- NA
  
  if (!is.null(fit$rda_cv)) {
    rda_cv_error <- mean(fit$rda_cv$pred_test != Y_test)
    lambda_cv    <- fit$rda_cv$lambda_star
    gamma_cv     <- fit$rda_cv$gamma_star
  }
  
  list(
    lda_error    = lda_error,
    qda_error    = qda_error,
    rda_cv_error = rda_cv_error,
    lambda_cv    = lambda_cv,
    gamma_cv     = gamma_cv
  )
}

# SANITY CHECK
if (sys.nframe() == 0) {
  cat("=== EVALUATION MODULE Sanity Check ===\n\n")
  
  for (sc in c("A", "B", "C", "D")) {
    dat <- generate_data(n = 100, p = 20, delta = 2, alpha = 8,
                         scenario = sc, seed = 42)
    
    fit <- train_models(dat, 
                        cv_lambda = seq(0, 1, by = 0.2),
                        cv_gamma  = c(0, 0.5, 0.8, 0.9, 0.95, 1),
                        n_folds   = 5,
                        use_cv    = TRUE,
                        cv_seed   = 123)
    
    res <- evaluate_models(fit, dat$Y_test)
    
    cat(sprintf("Scenario %s\n", sc))
    cat(sprintf("  LDA:     %.3f\n", res$lda_error))
    cat(sprintf("  QDA:     %.3f\n", res$qda_error))
    cat(sprintf("  RDA CV:  %.3f (λ*=%.2f, γ*=%.2f)\n",
                res$rda_cv_error, res$lambda_cv, res$gamma_cv))
    cat("\n")
  }
  
  cat("✓ Evaluation module OK\n")
}