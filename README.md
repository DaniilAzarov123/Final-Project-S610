# K-Means Clustering Visualization
**STAT-S 610 — Introduction to Statistical Computing**

## Overview

This project implements k-means clustering from scratch in R, with two main functions: one that runs the algorithm and visualizes each iteration step-by-step, and one that helps identify the optimal number of clusters using the elbow method.

The implementation is demonstrated on the built-in `iris` dataset, clustering observations by sepal length and petal width.

## Files

| File | Description |
|---|---|
| `K_Means_Visual.r` | Core implementation: `k_means_visual()` and `optimal_k()` |
| `Test_functions.R` | Unit tests for both functions using `testthat` |
| `Example_Visualization_iris.pdf` | Slide deck showing the algorithm's convergence on the iris dataset |

## Functions

### `k_means_visual(k, data, print_plot)`

Runs k-means clustering on a 2D data frame and optionally animates the convergence.

- **`k`** — number of clusters (randomly chosen between 1–10 if omitted)
- **`data`** — a 2-column data frame
- **`print_plot`** — if `TRUE`, prints a plot at each iteration showing centroid movement and cluster reassignments (default: `TRUE`)

Returns a list with:
- `clusters` — integer vector of cluster labels for each observation
- `cluster_locations` — k × 2 matrix of final centroid coordinates

Empty clusters are automatically reinitialized with random coordinates rather than causing the algorithm to fail.

### `optimal_k(max_k, data)`

Finds the optimal number of clusters using the elbow method. Runs `k_means_visual` for each k from 1 to `max_k`, computes the total within-cluster sum of squares (WCSS), and plots the results.

- **`max_k`** — maximum number of clusters to test (positive integer)
- **`data`** — a 2-column data frame

Returns a matrix with columns `k` and `tot_sum_sq`.

## Dependencies

```r
install.packages(c("ggplot2", "scales", "testthat"))
```

## Usage

```r
source("K_Means_Visual.r")

# Cluster iris by sepal length and petal width
data("iris")
df <- iris[, c("Sepal.Length", "Petal.Width")]

# Run k-means with k = 3, showing each iteration
set.seed(1)
result <- k_means_visual(k = 3, data = df, print_plot = TRUE)

# Find optimal k up to 10
set.seed(1)
optimal_k(max_k = 10, data = df)
```

## Running Tests

```r
library(testthat)
source("K_Means_Visual.r")
source("Test_functions.R")
```

Tests are written with `testthat` and cover both functions:

**`k_means_visual()`**
- Rejects non-data-frame input (e.g. a matrix)
- Rejects data frames with more than 2 columns
- Runs without error and returns correct output structure when `k` is omitted
- Runs without error and returns correct output structure when all arguments are provided

**`optimal_k()`**
- Rejects non-data-frame input
- Rejects data frames with more than 2 columns
- Rejects non-integer values of `max_k` (e.g. 10.5)
- Rejects `max_k` values less than 1
- Returns a matrix with the correct dimensions and column names (`k`, `tot_sum_sq`)
