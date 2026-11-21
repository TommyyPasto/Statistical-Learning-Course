# MODULE 4 MONTE CARLO CONTROLLER

source("Parte3.R")

# EXPERIMENTAL GRIDS
N_GRID     = c(30, 50, 100, 300, 1000)
P_GRID     = c(2, 5, 10, 20, 50, 100)
ALPHA_GRID = c(1, 2, 5, 10, 20)
SCENARIOS  = c("A", "B", "C", "D")   
R          = 100       
DELTA      = 2        

CV_LAMBDA_GRID = c(0, 0.1, 0.2, 0.35, 0.5, 0.65, 0.8, 1.0)
CV_GAMMA_GRID  = c(0, 0.25, 0.5, 0.75, 1.0)
N_FOLDS        = 5

# SINGLE RUN
single_run = function(n, p, alpha, scenario, seed) {
  dat = generate_data(n = n, p = p, delta = DELTA, alpha = alpha,
                      scenario = scenario, seed = seed)
  
  fit = train_models(dat, 
                     cv_lambda = CV_LAMBDA_GRID,
                     cv_gamma  = CV_GAMMA_GRID,
                     n_folds   = N_FOLDS,
                     use_cv    = TRUE,
                     cv_seed   = seed + 1000)
  
  res = evaluate_models(fit, dat$Y_test)
  
  cond_Sp = tryCatch({
    ev = eigen(fit$covs$Sp, symmetric = TRUE, only.values = TRUE)$values
    max(ev) / max(min(ev), 1e-10)
  }, error = function(e) NA_real_)
  
  c(res, list(cond_Sp = cond_Sp))
}

# MONTE CARLO LOOP
run_monte_carlo = function(n_grid     = N_GRID,
                           p_grid     = P_GRID,
                           alpha_grid = ALPHA_GRID,
                           scenarios  = SCENARIOS,
                           R          = 100,
                           verbose    = TRUE) {
  
  grid = expand.grid(
    scenario = scenarios,
    n        = n_grid,
    p        = p_grid,
    alpha    = alpha_grid,
    stringsAsFactors = FALSE
  )
  
  n_configs = nrow(grid)
  results   = vector("list", n_configs)
  
  if (verbose) {
    cat(sprintf("Starting Monte Carlo: %d configurations x %d reps = %d runs\n\n",
                n_configs, R, n_configs * R))
  }
  
  for (i in seq_len(n_configs)) {
    cfg = grid[i, ]
    
    if (verbose) {
      cat(sprintf("[%d/%d] scenario=%s n=%d p=%d alpha=%d ... ",
                  i, n_configs, cfg$scenario, cfg$n, cfg$p, cfg$alpha))
    }
    
    high_dim = cfg$p >= cfg$n / 2
    
    reps = lapply(seq_len(R), function(r) {
      seed = (i - 1) * R + r  
      tryCatch(
        single_run(cfg$n, cfg$p, cfg$alpha, cfg$scenario, seed),
        error = function(e) NULL  
      )
    })
    
    reps = Filter(Negate(is.null), reps)
    n_ok = length(reps)
    
    lda_errors   = sapply(reps, `[[`, "lda_error")
    qda_errors   = sapply(reps, `[[`, "qda_error")
    rda_errors   = sapply(reps, `[[`, "rda_cv_error")
    lambda_stars = sapply(reps, `[[`, "lambda_cv")
    gamma_stars  = sapply(reps, `[[`, "gamma_cv")
    cond_Sps     = sapply(reps, `[[`, "cond_Sp")
    
    results[[i]] = list(
      scenario   = cfg$scenario,
      n          = cfg$n,
      p          = cfg$p,
      alpha      = cfg$alpha,
      high_dim   = high_dim,
      n_ok       = n_ok,
      mean_lda   = mean(lda_errors),
      sd_lda     = sd(lda_errors),
      mean_qda   = mean(qda_errors),
      sd_qda     = sd(qda_errors),
      mean_rda   = mean(rda_errors),
      sd_rda     = sd(rda_errors),
      mean_lambda_star = mean(lambda_stars, na.rm = TRUE),
      sd_lambda_star   = sd(lambda_stars, na.rm = TRUE),
      mean_gamma_star  = mean(gamma_stars, na.rm = TRUE),
      sd_gamma_star    = sd(gamma_stars, na.rm = TRUE),
      mean_cond_Sp     = mean(cond_Sps, na.rm = TRUE)
    )
    
    if (verbose) {
      cat(sprintf("LDA=%.3f QDA=%.3f RDA CV=%.3f λ*=%.2f γ*=%.2f\n",
                  results[[i]]$mean_lda, results[[i]]$mean_qda,
                  results[[i]]$mean_rda, results[[i]]$mean_lambda_star,
                  results[[i]]$mean_gamma_star))
    }
  }
  
  results
}

library(parallel)

single_run_par = function(n, p, alpha, scenario, seed) {
  dat = generate_data(n = n, p = p, delta = DELTA, alpha = alpha,
                      scenario = scenario, seed = seed)
  
  fit = train_models(dat,
                     cv_lambda = CV_LAMBDA_GRID,
                     cv_gamma  = CV_GAMMA_GRID,
                     n_folds   = N_FOLDS,
                     use_cv    = TRUE,
                     cv_seed   = seed + 1000)
  
  res = evaluate_models(fit, dat$Y_test)
  
  cond_Sp = tryCatch({
    ev = eigen(fit$covs$Sp, symmetric = TRUE, only.values = TRUE)$values
    max(ev) / max(min(ev), 1e-10)
  }, error = function(e) NA_real_)
  
  c(res, list(cond_Sp = cond_Sp))
}

run_monte_carlo_par = function(n_grid     = N_GRID,
                               p_grid     = P_GRID,
                               alpha_grid = ALPHA_GRID,
                               scenarios  = SCENARIOS,
                               R          = 100,
                               verbose    = TRUE,
                               n_cores    = NULL) {
  
  if (is.null(n_cores)) n_cores = max(1, detectCores() - 1)
  
  grid = expand.grid(
    scenario = scenarios,
    n        = n_grid,
    p        = p_grid,
    alpha    = alpha_grid,
    stringsAsFactors = FALSE
  )
  
  n_configs = nrow(grid)
  
  if (verbose) {
    cat(sprintf("Starting Monte Carlo: %d configs x %d reps = %d runs\n",
                n_configs, R, n_configs * R))
    cat(sprintf("Using %d cores\n\n", n_cores))
  }
  
  run_config = function(i) {
    cfg = grid[i, ]
    high_dim = cfg$p >= cfg$n / 2
    
    reps = lapply(seq_len(R), function(r) {
      seed = (i - 1) * R + r
      tryCatch(
        single_run_par(cfg$n, cfg$p, cfg$alpha, cfg$scenario, seed),
        error = function(e) NULL
      )
    })
    
    reps = Filter(Negate(is.null), reps)
    n_ok = length(reps)
    if (n_ok == 0) return(NULL)
    
    lda_errors   = sapply(reps, `[[`, "lda_error")
    qda_errors   = sapply(reps, `[[`, "qda_error")
    rda_errors   = sapply(reps, `[[`, "rda_cv_error")
    lambda_stars = sapply(reps, `[[`, "lambda_cv")
    gamma_stars  = sapply(reps, `[[`, "gamma_cv")
    cond_Sps     = sapply(reps, `[[`, "cond_Sp")
    
    list(
      scenario         = cfg$scenario,
      n                = cfg$n,
      p                = cfg$p,
      alpha            = cfg$alpha,
      high_dim         = high_dim,
      n_ok             = n_ok,
      mean_lda         = mean(lda_errors),
      sd_lda           = sd(lda_errors),
      mean_qda         = mean(qda_errors),
      sd_qda           = sd(qda_errors),
      mean_rda         = mean(rda_errors),
      sd_rda           = sd(rda_errors),
      mean_lambda_star = mean(lambda_stars, na.rm = TRUE),
      sd_lambda_star   = sd(lambda_stars,   na.rm = TRUE),
      mean_gamma_star  = mean(gamma_stars,  na.rm = TRUE),
      sd_gamma_star    = sd(gamma_stars,    na.rm = TRUE),
      mean_cond_Sp     = mean(cond_Sps,     na.rm = TRUE)
    )
  }
  
  cl = makeCluster(n_cores)
  
  clusterEvalQ(cl, {
    setwd("C:/Users/tomma/Desktop/SL_FinalProject")
    source("Parte1.R")
    source("Parte2.R")
    source("Parte3.R")
    library(MASS)
  })
  
  clusterExport(cl, varlist = c(
    "grid", "R", "N_FOLDS", "DELTA",
    "CV_LAMBDA_GRID", "CV_GAMMA_GRID",
    "single_run_par", "run_config"
  ), envir = environment())
  
  results = parLapply(cl, seq_len(n_configs), run_config)
  stopCluster(cl)
  
  if (verbose) {
    for (i in seq_len(n_configs)) {
      r = results[[i]]
      if (!is.null(r)) {
        cat(sprintf("[%d/%d] scenario=%s n=%d p=%d alpha=%d -> LDA=%.3f QDA=%.3f RDA=%.3f\n",
                    i, n_configs, r$scenario, r$n, r$p, r$alpha,
                    r$mean_lda, r$mean_qda, r$mean_rda))
      }
    }
  }
  
  Filter(Negate(is.null), results)
}

# FLATTEN RESULTS
flatten_results = function(results) {
  
  summary_df = do.call(rbind, lapply(results, function(r) {
    data.frame(
      scenario         = r$scenario,
      n                = r$n,
      p                = r$p,
      alpha            = r$alpha,
      pn_ratio         = r$p / r$n,
      high_dim         = r$high_dim,
      n_ok             = r$n_ok,
      mean_lda         = r$mean_lda,
      sd_lda           = r$sd_lda,
      mean_qda         = r$mean_qda,
      sd_qda           = r$sd_qda,
      mean_rda         = r$mean_rda,
      sd_rda           = r$sd_rda,
      mean_lambda_star = r$mean_lambda_star,
      sd_lambda_star   = r$sd_lambda_star,
      mean_gamma_star  = r$mean_gamma_star,
      sd_gamma_star    = r$sd_gamma_star,
      mean_cond_Sp     = r$mean_cond_Sp,
      stringsAsFactors = FALSE
    )
  }))
  
  long_df = do.call(rbind, lapply(results, function(r) {
    rbind(
      data.frame(scenario=r$scenario, n=r$n, p=r$p, alpha=r$alpha,
                 method="LDA", error=r$mean_lda, sd=r$sd_lda,
                 lambda=NA_real_, gamma=NA_real_, stringsAsFactors=FALSE),
      data.frame(scenario=r$scenario, n=r$n, p=r$p, alpha=r$alpha,
                 method="QDA", error=r$mean_qda, sd=r$sd_qda,
                 lambda=NA_real_, gamma=NA_real_, stringsAsFactors=FALSE),
      data.frame(scenario=r$scenario, n=r$n, p=r$p, alpha=r$alpha,
                 method="RDA CV", error=r$mean_rda, sd=r$sd_rda,
                 lambda=r$mean_lambda_star, gamma=r$mean_gamma_star,
                 stringsAsFactors=FALSE)
    )
  }))
  
  list(summary = summary_df, long = long_df)
}

# SANITY CHECK
if (sys.nframe() == 0) {
  cat("=== MONTE CARLO CONTROLLER Sanity Check ===\n\n")
  
  set.seed(1)
  results = run_monte_carlo(
    n_grid     = c(30, 50),
    p_grid     = c(5, 40),
    alpha_grid = c(1, 5),
    scenarios  = c("A", "B", "C", "D"),
    R          = 10,
    verbose    = TRUE
  )
  
  tabs = flatten_results(results)
  
  cat("\n Summary table \n")
  print(head(tabs$summary[, c("scenario","n","p","alpha",
                              "mean_lda","mean_qda","mean_rda",
                              "mean_lambda_star","mean_gamma_star")], 8))
  
  cat("\n Long table dims:", nrow(tabs$long), "x", ncol(tabs$long), "\n")
  cat("\n✓ Monte Carlo module OK\n")
}