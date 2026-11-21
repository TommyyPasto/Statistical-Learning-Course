set.seed(123)
# Parametri di immagini #
yscale <- 1.20
xscale <- 1.10
####################
# Salvataggio dei parametri
image_width <- 4096
image_length <- 4096
image_res <- 400
####################
# Altri parfametri #
# Kernel manuali:
h3 <- 1.5
h3_1 <- 5
h3_2 <- 0.05
####################
# Distribuzione
x1 <- c(
  rnorm(300, mean = 0, sd = 1),        # Cluster centrale
  rnorm(250, mean = -6, sd = 0.8),     # Primo cluster
  rgamma(150, shape = 4, scale = 2),   # Componente asimmetrica
  rnorm(200, mean = 4, sd = 1.2),      # Secondo cluster
  rgamma(150, shape = 2, scale = 1),   # Componente asimmetrica
  rnorm(50, mean = 13, sd = 0.5)       # Outlier
)

x2 <- c(
  rgamma(150, shape = 2, scale = 1), # Componente asimmetrica 1
  rgamma(350, shape = 4, scale = 2), # Componente asimmetrica 2
  rnorm(150, mean = 25, sd = 3)      # Primo cluster
)

x3 <- c(
  rnorm(300, mean = 0, sd = 1),        # Cluster centrale
  rnorm(250, mean = -6, sd = 0.8),     # Primo cluster
  rnorm(200, mean = 4, sd = 1.2),      # Secondo cluster
  rgamma(150, shape = 2, scale = 1),   # Componente asimmetrica
  rnorm(150, mean = 8, sd = 0.6),      # Terzo cluster
  rnorm(50, mean = 13, sd = 0.5),      # Outlier
  rnorm(150, mean = 20, sd = 0.3)      # Quarto cluster
)

X <- list(x1,x2,x3)
####################

# Funzioni #
save_plot <- function(filename, expr, width = image_width, height = image_length, res = image_res, cex_global = 1.6, cex_main = 2, cex_lab = 1.8, cex_axis = 1.5) {
  png(filename, width = width, height = height, res = res)
  par(cex = cex_global)
  eval(expr)
  dev.off()
}

# Valore di silverman
h_silverman <- function(x) {
  1.06 * sd(x) * length(x)^(-1/5)
}

# Cross validation minimi quadrati
lscv_obj <- function(h, x) {
  n <- length(x)
  
  fhat_loo <- sapply(1:n, function(i) {
    mean(dnorm(x[i] - x[-i], sd = h))
  })
  term1 <- mean(fhat_loo)
  
  # Integrale di f_hat^2 con la stessa griglia di density()
  d <- density(x, bw = h, kernel = "gaussian", n = 1024)
  term2 <- sum(d$y^2) * diff(d$x[1:2])
  
  return(term2 - 2 * term1)
}

# IQR
h_iqr <- function(x) {
  0.9 * min(sd(x), IQR(x) / 1.34) * length(x)^(-1/5)
}

#############
i <- 1
for(x in X) {
  
  h1 <- h_silverman(x)
  
  h2 <- optimize(lscv_obj, interval = c(0.01, 2), x = x)$minimum
  
  h4 <- h_iqr(x)
  
  # Per poter adattare l'istogramma alla variazione dei dati
  range_x <- range(x)
  min_x <- floor(range_x[1])
  max_x <- ceiling(range_x[2])
  
  breaks_x <- seq(min_x, max_x, by = 1)
  
  margin <- diff(range_x) * abs(xscale - 1)
  xlim_max <- c(min_x, max_x) + c(-margin, margin)
  
  
  dens_list <- list(
    density(x, bw = h1),
    density(x, bw = h2),
    density(x, bw = h3),
    density(x, bw = h4)
  )
  
  max_kde <- max(sapply(dens_list, function(d) max(d$y)))
  
  ylim_max <- max(max(h$density), max_kde) * yscale
  ####################
  
  filename <- paste0("grafico_", i, ".png")
  save_plot(filename, {
    hist(x,
         breaks = breaks_x,
         freq = FALSE,
         col = "lightgray",
         border = "white",
         main = "Istogramma vs Manual",
         xlab = "x",
         xlim = xlim_max,
         ylim = c(0, ylim_max))
    
    lines(density(x, bw = h3), col = "red", lwd = 2)
    lines(density(x, bw = h3_1), col = "darkgreen", lwd = 2)
    lines(density(x, bw = h3_2), col = "cornflowerblue", lwd = 2)
    
    legend("topright",
           legend = c(
             paste("Manual 1 =", round(h3, 3)),
             paste("Manual 2 =", round(h3_1, 3)),
             paste("Manual 3 =", round(h3_2, 3))
           ),
           col = c("red", "darkgreen", "cornflowerblue"), lwd = 2)
  })
  
  i <- i + 1
  filename <- paste0("grafico_", i, ".png")
  save_plot(filename, {
    hist(x,
         breaks = breaks_x,
         freq = FALSE,
         col = "lightgray",
         border = "white",
         main = "Istogramma vs KDE",
         xlab = "x",
         xlim = xlim_max,
         ylim = c(0, ylim_max))
    
    lines(density(x, bw = h1), col = "red", lwd = 2)
    lines(density(x, bw = h2), col = "blue", lwd = 2)
    lines(density(x, bw = h3), col = "darkgreen", lwd = 2)
    lines(density(x, bw = h4), col = "orange", lwd = 2)
    
    legend("topright",
           legend = c(
             paste("Silverman =", round(h1, 3)),
             paste("LSCV =", round(h2, 3)),
             paste("Manual =", round(h3, 3)),
             paste("IQR =", round(h4, 3))
           ),
           col = c("red", "blue", "darkgreen", "orange"),
           lwd = 2,
           bty = "n")
  })
  i <- i + 1
} 