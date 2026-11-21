# MODULE 1 DATA GENERATION
# Regularized Discriminant Analysis misspecification e high dimensional regimes
# Genera dataset controllati con struttura di covarianza nota

library(MASS)


# HELPERS NUMERICI
safe_log_det <- function(S) {
  ch <- tryCatch(chol(S), error = function(e) NULL)
  if (is.null(ch)) {
    ev <- pmax(eigen(S, symmetric = TRUE, only.values = TRUE)$values, 1e-10)
    return(sum(log(ev)))
  }
  2 * sum(log(diag(ch)))
}

safe_inv <- function(S, eps = 1e-6) {
  p <- nrow(S)
  tryCatch(solve(S + eps * diag(p)), error = function(e) MASS::ginv(S))
}




# HELPER build block diagonal correlation matrix 
build_block_cov <- function(p, block_size, rho, scale_vec = NULL) {
  Sigma <- diag(p)
  n_blocks <- floor(p / block_size)
  
  for (b in seq_len(n_blocks)) {
    idx <- ((b - 1) * block_size + 1):(b * block_size)
    block <- matrix(rho, nrow = block_size, ncol = block_size)
    diag(block) <- 1
    Sigma[idx, idx] <- block
  }
  
  if (!is.null(scale_vec)) {
    D <- diag(sqrt(scale_vec))
    Sigma <- D %*% Sigma %*% D
  }
  
  return(Sigma)
}



# HELPER matrice simmetrica definita positiva
make_spd <- function(M, eps = 1e-6) {
  M <- (M + t(M)) / 2
  eig <- eigen(M, symmetric = TRUE)
  eig$values <- pmax(eig$values, eps)
  M_spd <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  return(M_spd)
}

# CORE FUNCTION generate data
# Parametri n training, p features, delta separazione, alpha livello, scenario A B C D, n test, seed

generate_data <- function(n,
                          p,
                          delta  = 1,
                          alpha  = 1,
                          scenario = c("A", "B", "C", "D"),
                          n_test = 2000,
                          seed   = NULL) {
  
  scenario <- match.arg(scenario)
  if (!is.null(seed)) set.seed(seed)
  
  mu0 <- rep(0, p)
  if (scenario %in% c("A", "B")) {
    mu1 <- c(delta, rep(0, p - 1))
  }
  
  if (scenario == "A") {
    # Ben specificato classi con covarianza identita
    Sigma0 <- diag(p)
    Sigma1 <- diag(p)
    
  } else if (scenario == "B") {
    # Sfere disuguali LDA biased perche assume covarianze uguali
    Sigma0 <- diag(p)
    Sigma1 <- 2 * diag(p)
  } else if (scenario == "C") {
    # Ellissoidi uguali anisotropi con autovalori crescenti
    eigvals <- (alpha * (0:(p-1)) / max(p-1, 1) + 1)^2
    
    rng_state <- .Random.seed
    set.seed(as.integer(p * 1000 + alpha * 7 + 42))
    Q <- qr.Q(qr(matrix(rnorm(p * p), p, p)))
    .Random.seed <- rng_state
    
    Sigma0 <- make_spd(Q %*% diag(eigvals) %*% t(Q))
    Sigma1 <- Sigma0
    
    eigval_max <- max(eigvals)
    mu1 <- delta * sqrt(eigval_max) * Q[, p]
    
  } else if (scenario == "D") {
    # Ellissoidi disuguali autospazi complementari crescenti e decrescenti
    eigvals0 <- (alpha * (0:(p-1)) / max(p-1, 1) + 1)^2
    eigvals1 <- (alpha * ((p-1):0) / max(p-1, 1) + 1)^2
    
    rng_state <- .Random.seed
    set.seed(as.integer(p * 2000 + alpha * 13 + 99))
    Q <- qr.Q(qr(matrix(rnorm(p * p), p, p)))
    .Random.seed <- rng_state
    
    Sigma0 <- make_spd(Q %*% diag(eigvals0) %*% t(Q))
    Sigma1 <- make_spd(Q %*% diag(eigvals1) %*% t(Q))
    
    mid_idx <- ceiling(p / 2)
    Sigma_avg <- (Sigma0 + Sigma1) / 2
    eig_avg <- eigen(Sigma_avg, symmetric=TRUE, only.values=TRUE)$values
    eigval_mid <- eig_avg[ceiling(p/2)]
    mu1 <- delta * sqrt(eigval_mid) * Q[, ceiling(p/2)]
  }
  
  n0_train <- floor(n / 2)
  n1_train <- n - n0_train
  n0_test  <- floor(n_test / 2)
  n1_test  <- n_test - n0_test
  
  X0_train <- mvrnorm(n0_train, mu = mu0, Sigma = Sigma0)
  X1_train <- mvrnorm(n1_train, mu = mu1, Sigma = Sigma1)
  
  X_train <- rbind(X0_train, X1_train)
  Y_train <- c(rep(0L, n0_train), rep(1L, n1_train))
  
  perm_train <- sample(n)
  X_train <- X_train[perm_train, , drop = FALSE]
  Y_train <- Y_train[perm_train]
  
  X0_test <- mvrnorm(n0_test, mu = mu0, Sigma = Sigma0)
  X1_test <- mvrnorm(n1_test, mu = mu1, Sigma = Sigma1)
  
  X_test <- rbind(X0_test, X1_test)
  Y_test <- c(rep(0L, n0_test), rep(1L, n1_test))
  
  perm_test <- sample(n_test)
  X_test <- X_test[perm_test, , drop = FALSE]
  Y_test <- Y_test[perm_test]
  
  list(
    X_train  = X_train,
    Y_train  = Y_train,
    X_test   = X_test,
    Y_test   = Y_test,
    Sigma0   = Sigma0,
    Sigma1   = Sigma1,
    params   = list(
      n        = n,
      p        = p,
      delta    = delta,
      alpha    = alpha,
      scenario = scenario,
      n_test   = n_test,
      seed     = seed
    )
  )
}

# BAYES RISK
# Rischio di Bayes per classificazione binaria gaussiana tramite Monte Carlo
compute_bayes_risk <- function(mu0, mu1, Sigma0, Sigma1,
                               pi0 = 0.5, pi1 = 0.5,
                               n_mc = 50000) {
  p  <- length(mu0)
  n0 <- round(pi0 * n_mc)
  n1 <- n_mc - n0
  X  <- rbind(mvrnorm(n0, mu = mu0, Sigma = Sigma0),
              mvrnorm(n1, mu = mu1, Sigma = Sigma1))
  Y  <- c(rep(0L, n0), rep(1L, n1))
  
  log_gauss <- function(X, mu, Sigma) {
    Sinv <- safe_inv(Sigma)
    ld   <- safe_log_det(Sigma)
    apply(X, 1, function(x) {
      d <- x - mu
      -0.5 * (as.numeric(d %*% Sinv %*% d) + ld + p * log(2 * pi))
    })
  }
  
  lp0 <- log_gauss(X, mu0, Sigma0) + log(pi0)
  lp1 <- log_gauss(X, mu1, Sigma1) + log(pi1)
  
  pred <- as.integer(lp1 > lp0)
  mean(pred != Y)
}

# SANITY CHECK eseguito solo se il file viene lanciato direttamente
if (sys.nframe() == 0) {
  
  cat("=== DATA GENERATION MODULE Sanity Check ===\n\n")
  
  for (sc in c("A", "B", "C", "D")) {
    dat <- generate_data(n = 100, p = 10, delta = 1, alpha = 3,
                         scenario = sc, seed = 42)
    
    cat(sprintf("Scenario %s:\n", sc))
    cat(sprintf("  X_train: %d x %d\n", nrow(dat$X_train), ncol(dat$X_train)))
    cat(sprintf("  Y_train class balance: %d / %d\n",
                sum(dat$Y_train == 0), sum(dat$Y_train == 1)))
    cat(sprintf("  X_test:  %d x %d\n", nrow(dat$X_test), ncol(dat$X_test)))
    cat(sprintf("  Sigma0[1:3,1:3]:\n"))
    print(round(dat$Sigma0[1:3, 1:3], 3))
    cat(sprintf("  Sigma1[1:3,1:3]:\n"))
    print(round(dat$Sigma1[1:3, 1:3], 3))
    cat("\n")
  }
  
  dat_check <- generate_data(n = 2000, p = 5, delta = 2, alpha = 5,
                             scenario = "B", seed = 1)
  emp_mu1 <- colMeans(dat_check$X_train[dat_check$Y_train == 1, ])
  cat("Empirical mu1 (should be ~[2,0,0,0,0]):", round(emp_mu1, 3), "\n\n")
  
  cat("Bayes risk check (delta=1, p=5):\n")
  for (sc in c("A", "B", "C", "D")) {
    dat <- generate_data(n = 100, p = 5, delta = 1, alpha = 3,
                         scenario = sc, seed = 1)
    br <- compute_bayes_risk(mu0    = rep(0, 5),
                             mu1    = dat$params$delta * c(1, rep(0, 4)),
                             Sigma0 = dat$Sigma0,
                             Sigma1 = dat$Sigma1)
    cat(sprintf("  Scenario %s: Bayes risk = %.4f\n", sc, br))
  }
  
  cat("\n✓ Data generation module OK\n")
}