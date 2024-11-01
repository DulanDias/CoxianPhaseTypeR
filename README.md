# CoxianPhaseTypeR

**CoxianPhaseTypeR** is an R package for fitting Coxian Phase-Type distributions to continuous variables using the Expectation-Maximization (EM) algorithm. The package leverages a generalized formula for Coxian distributions, as derived by Sheldon Ross, to estimate parameters, compute the PDF, and evaluate survival probabilities. This tool is designed for survival analysis and other applications where phase-type distributions are useful.

## Features
- Fit Coxian Phase-Type distributions with an arbitrary number of phases.
- Estimate parameters via the Expectation-Maximization (EM) algorithm.
- Calculate the PDF and survival probability for Coxian distributions.

## Installation

Install from GitHub (requires `devtools` package):

```r
# install.packages("devtools")
devtools::install_github("yourusername/CoxianPhaseTypeR")
