#' This file tests the functions from K_Means_Visual.r:
#'  k_means_visual
#'  optimal_k

library(testthat)


# ------------------------ Test 1 ------------------------


test_that("k_means_visual takes only data frames", {
  data("iris")
  df <- iris[,c("Sepal.Length","Petal.Width")]
  df_mat <- as.matrix(df)
  expect_error(
    k_means_visual(k = 5, data = df_mat, print_plot = FALSE), 
    "Please provide a data set\\."
  )
})


# ------------------------ Test 2 ------------------------


test_that("k_means_visual takes only 2D data frames", {
  data("iris")
  expect_error(
    k_means_visual(k = 5, data = iris, print_plot = FALSE), 
    "data frame is in a 2-dimensional space"
  )
})


# ------------------------ Test 3 ------------------------


test_that("k_means_visual works even if k is not provided", {
  data("iris")
  df <- iris[, c("Sepal.Length", "Petal.Width")]
  
  # No errors
  expect_no_error(
    result <- k_means_visual(data = df, print_plot = FALSE)
  )
  
  # It should return a list
  expect_type(result, "list")
  # with two elements
  expect_true(all(c("clusters", "cluster_locations") %in% names(result)))
  # and cluster labels for each data point
  expect_length(result$clusters, nrow(df))
})
