# Final Project: Regularized Discriminant Analysis

Simulation study comparing LDA, QDA and Regularized Discriminant Analysis (Friedman, 1989), which moves between the two by shrinking class covariances toward the pooled covariance (λ) and toward the identity (γ).

- **Scenarios:** 4 covariance structures (equal or unequal, spherical or not)
- **Grid:** n in {30, 50, 100, 200, 500}, p in {5, 20, 50}, two levels of misspecification
- **Tuning:** (λ, γ) selected by stratified 5-fold cross-validation over the paper's 25-point grid
- **Scale:** 120 configurations × 100 replications = 12,000 runs, test set of 2,000 observations
- **Output:** test error vs n and p, selected (λ, γ), gain of RDA over the best of LDA and QDA

| File | Content |
|---|---|
| `Parte1.R` | Module 1: data generation |
| `Parte2.R` | Module 2: models |
| `Parte3.R` | Module 3: evaluation |
| `Parte4.R` | Module 4: Monte Carlo controller |
| `Parte5.R` | Module 5: plots |
| `Presentazione_SL_TommasoPastorelli.pdf` | Slides presented at the exam |

Author: Tommaso Pastorelli
