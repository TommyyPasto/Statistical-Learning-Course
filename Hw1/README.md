# Homework 1: Omitted Variable Bias

Monte Carlo study of the bias that appears in linear regression when a relevant variable correlated with the regressor is left out of the model.

- **Setup:** true model `y = β0 + β1·x1 + β2·x2 + ε` with β1 = 1, β2 = −2; 300 observations, 500 simulations
- **Scenarios:** correlation between x1 and x2 equal to 0.5 (biased reduced model) and 0 (no bias)
- **Extra:** sequential vs parallel execution of the simulation (×2.2 speedup on 7 cores)

| File | Content |
|---|---|
| `HW1.Rmd` | R Markdown source (ioslides presentation) |
| `Homework1_ La distorsione dalla variabile omessa.pdf` | Slides presented in class |

Authors: Maria Li, Tommaso Pastorelli
