# CoxianPhaseTypeR

**CoxianPhaseTypeR** is an R package designed for fitting Coxian Phase-Type (Coxian-PH) distributions to time-to-event data using the Expectation-Maximization (EM) algorithm. The package facilitates parameter estimation, probability density function (PDF) evaluation, survival probability computation, and model selection criteria, making it a valuable tool for survival analysis and other applications requiring phase-type distributions.

## Features

- **Flexible Model Fitting**: Fit Coxian-PH models with an arbitrary number of phases.
- **Parameter Estimation**: Utilize the EM algorithm for efficient parameter estimation.
- **Model Evaluation**: Compute log-likelihood, Akaike Information Criterion (AIC), and Bayesian Information Criterion (BIC) for model comparison.
- **Simulation Tools**: Generate synthetic data from specified Coxian-PH distributions for testing and validation.
- **Visualization**: Plot fitted distributions and survival functions for intuitive analysis.

## Installation

You can install the stable version of **CoxianPhaseTypeR** from CRAN:

```r
install.packages("CoxianPhaseTypeR")
```

For the latest development version, install directly from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install from GitHub
devtools::install_github("DulanDias/CoxianPhaseTypeR")
```

## Usage

Here's a basic example demonstrating how to simulate data, fit a Coxian-PH model, and evaluate its performance:

```r
# Load the package
library(CoxianPhaseTypeR)

# Simulate survival data from a 2-phase Coxian-PH model
set.seed(123)  # For reproducibility
data <- simulate_coxian(n = 100, lambda = c(2.0, 1.5), mu = c(1.0, 0.8))

# Fit a Coxian-PH model with 2 phases
fit <- fit_coxian(data, num_phases = 2)

# Print estimated parameters
print(fit)

# Evaluate model performance using Log-Likelihood, AIC, and BIC
model_fit <- compute_model_fit(fit)
print(model_fit)

# Determine the optimal number of phases by iterating from m = 1 to 5
best_model <- determine_optimal_phases(data, max_phases = 5)

# Print the best-fitting model
print(best_model)
```

## Functions

- `simulate_coxian()`: Simulates survival data from a specified Coxian-PH distribution.
- `fit_coxian()`: Fits a Coxian-PH model to observed data using the EM algorithm.
- `compute_model_fit()`: Calculates log-likelihood, AIC, and BIC for a fitted model.
- `determine_optimal_phases()`: Identifies the optimal number of phases by fitting models with varying phase counts and selecting the best based on model selection criteria.
- `plot_coxian()`: Visualizes the fitted Coxian-PH distribution.
- `plot_survival_fit()`: Plots the survival function of the fitted model against empirical survival data.

## Comparison with Other R Packages

While several R packages support general phase-type modeling, **CoxianPhaseTypeR** offers a specialized and structured implementation for Coxian-PH distributions, particularly for models with more than two phases. It provides:

- A dedicated EM algorithm optimized for Coxian-PH estimation.
- Automated model selection tools to determine the optimal number of phases.
- Integrated simulation and visualization functions tailored for Coxian models.
- Scalability for multi-phase models beyond \( m = 2 \), which existing packages may not efficiently support.

## References

- Ross, S. M. (2014). *Introduction to Probability Models* (11th ed.). Academic Press.
- Bladt, M., & Nielsen, B. F. (2005). *Matrix-Exponential Distributions in Applied Probability*. Springer.
- Asmussen, S., & Albrecher, H. (2010). *Ruin Probabilities* (2nd ed.). World Scientific.

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request on GitHub.





