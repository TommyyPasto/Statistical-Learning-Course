# Statistical Learning Course

R code and slides from the **Foundations of Statistical Learning** course, M.Sc. in Artificial Intelligence, University of Florence (A.Y. 2025/26).

The work is built mostly on **Monte Carlo simulation studies**: each project defines a data-generating process, runs many replications with fixed seeds and compares methods on their test error.

## Contents

| Folder | Topic | Methods |
|---|---|---|
| [`Hw1`](Hw1) | Omitted variable bias in linear regression | OLS, Monte Carlo, parallel computing |
| [`Hw2`](Hw2) | Kernel density estimation and bandwidth selection | KDE, Silverman's rule, cross-validation |
| [`Contest`](Contest) | Oracle properties of the Super Learner: **1st place** 🏆 | Stacking, GLM, GAM, Random Forest, SVM, kNN, NNLS |
| [`Final Project`](Final%20Project) | Regularized Discriminant Analysis | LDA, QDA, RDA, cross-validation |

Each folder contains the R code, the slides presented in class and a short README.

## Highlights

- **Contest win:** a Monte Carlo study of about 18,000 model fits showing how the Super Learner approaches the oracle learner as n grows
- **Final project:** 12,000 simulation runs comparing LDA, QDA and RDA across sample sizes, dimensions and covariance structures

## Requirements

R (4.x) with the packages loaded at the top of each script, mainly `ggplot2`, `dplyr`, `tidyr`, `MASS`, `mgcv`, `randomForest`, `e1071`, `SuperLearner`, `future` and `future.apply`.

## Authors

Tommaso Pastorelli, with Maria Li and Francesco Stocchi for the group assignments (see each folder).
