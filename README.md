# README: Cost of Living and Local Purchasing Power Study

## Introduction 

The data for this study is sourced from Numbeo and includes indices of cost of living, rent, cost of living and rent, grocery, restaurant, and local purchasing power for 578 cities from 5 regions in 2022.

### Objectives

The main interest is to compare the cost of living and rent (colnrent) versus local purchasing power (localpp) and answer the following questions:
1. Which model (equal mean, separate mean, hierarchical normal model) performs the best in finding the posterior mean and variance?
2. Is colnrent higher or lower than localpp?
3. Is the difference significant?

### Hypotheses

- If we believe the mean and variance across the 5 regions are the same, the prediction error of the equal mean model should be lower.
- If the 5 regions share some information, such as policy, the prediction error in the hierarchical normal model should be the lowest.
- Otherwise, the separate mean model should be used, implying that the 5 regions have different means and variances and are independent of each other.

The motivation of this study is to assess if there is a significant difference between the cost of living and rent and local purchasing power. If yes, what would be the mean difference? Which region has the largest difference, and do they share some common information in the posterior mean and variance?

Once the best-performing model is concluded, a diagnosis check will be performed on the model assumptions, including the prior distribution, convergence, and autocorrelation.

## Methodology

Three models (equal mean, separate mean, hierarchical normal model) will be fitted for both colnrent and localpp. The model assumptions are the same for colnrent and localpp as the priors are set to be weakly informative, and both indices use New York as the reference level at 100.
