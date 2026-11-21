# Contest: Oracle Properties of the Super Learner 🏆

**1st place in the course contest.** Simulation study of how close the Super Learner gets to the oracle, the best learner in the library chosen knowing the true distribution, as the sample size grows.

- **Library:** GLM, GAM, Random Forest, SVM (RBF), kNN and a mean model as negative control
- **Weights:** estimated by 5-fold cross-validation and non-negative least squares (NNLS)
- **Design:** 3 data-generating scenarios (linear, nonlinear, nonlinear with interaction), n from 100 to 2,000, 100 replications per cell
- **Scale:** 1,500 replications and about 18,000 model fits, run in parallel with `future`
- **Comparison:** convex Super Learner vs discrete Super Learner (pure model selection)

| File | Content |
|---|---|
| `Parte1.R` | Phase 1: data-generating processes and exploratory analysis |
| `Parte2.R` | Phase 2: learner library, fitting and sanity checks |
| `P3_conSLC.R` | Phase 3: Monte Carlo simulation, convex vs discrete Super Learner |
| `P3_conSLD.R` | Phase 3: Monte Carlo simulation, discrete Super Learner only |
| `P4_conSLC.R` | Final phase: plots for the convex vs discrete comparison |
| `P4_conSLD.R` | Final phase: plots for the discrete Super Learner |
| `PRESENTAZIONE_CONTEST.pdf` | Slides presented in class |

Reference: van der Laan, Polley & Hubbard (2007), *Super Learner*.

Authors: Tommaso Pastorelli, Maria Li, Francesco Stocchi
