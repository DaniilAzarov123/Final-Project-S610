#' This file tests the functions from K_Means_Visual.r:
#'  k_means_visual
#'  optimal_k

library(testthat) # For tests
library(ggplot2) # For visualization (inside functions)
library(scales) # To extract palettes (inside functions)


# ------------------------ Test k_means_visual() ------------------------


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


# ------------------------ Test 4 ------------------------


test_that("k_means_visual works when given all the data", {
  data("iris")
  df <- iris[, c("Sepal.Length", "Petal.Width")]
  k <- 3
  result <- k_means_visual(k = k, data = df, print_plot = FALSE)
  
  # No errors
  expect_no_error(result)
  
  # It should return a list
  expect_type(result, "list")
  # with two elements
  expect_true(all(c("clusters", "cluster_locations") %in% names(result)))
  # and cluster labels for each data point
  expect_length(result$clusters, nrow(df))
})




# ------------------------ Test optimal_k() ------------------------


# ------------------------ Test 1 ------------------------


test_that("optimal_k takes only data frames", {
  data("iris")
  df <- iris[,c("Sepal.Length","Petal.Width")]
  df_mat <- as.matrix(df)
  
  expect_error(
    optimal_k(max_k = 10,data = df_mat), 
    "Please provide a data set\\."
  )
  
})


# ------------------------ Test 2 ------------------------


test_that("optimal_k takes only 2D data frames", {
  data("iris")
  
  expect_error(
    optimal_k(max_k = 10,data = iris), 
    "data frame is in a 2-dimensional space"
  )
  
})


# ------------------------ Test 3 ------------------------


test_that("optimal_k takes only whole numbers for max_k", {
  data("iris")
  df <- iris[,c("Sepal.Length","Petal.Width")]
  
  expect_error(
    optimal_k(max_k = 10.5,data = df), 
    "max_k should be a whole number"
  )
  
})


# ------------------------ Test 4 ------------------------


test_that("optimal_k takes max_k that is >= 1", {
  data("iris")
  df <- iris[,c("Sepal.Length","Petal.Width")]
  
  expect_error(
    optimal_k(max_k = -1,data = df), 
    "1 or larger"
  )
  
})


# ------------------------ Test 5 ------------------------


test_that("optimal_k provides correct output", {
  data("iris")
  df <- iris[, c("Sepal.Length", "Petal.Width")]
  maximum_k <- 10
  result <- optimal_k(max_k = maximum_k, data = df)
  
  # No errors
  expect_no_error(result)
  
  # It should return a matrix
  expect_true(is.matrix(result))
  # with two columns
  expect_equal(ncol(result), 2)
  # with names
  expect_true(all(c("k", "tot_sum_sq") %in% colnames(result)))
  # and with maximum_k rows
  expect_equal(nrow(result), maximum_k)
})
