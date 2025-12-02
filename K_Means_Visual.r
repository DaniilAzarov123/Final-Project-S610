# Load packages
library(ggplot2) # For visualization
library(scales) # To extract palettes


# Load data to test functions later
data("iris")
df <- iris[,c("Sepal.Length","Petal.Width")]



# ------------------------ Visualization function ------------------------




# Predefined function to compute distances in 2D space.
# Takes two matrices:
#   data_mat      - matrix of observed data points
#   centroid_mat  - matrix of centroid coordinates
# Returns:
#   A matrix of distances from each data point to each centroid.

get_distances <- function(data_mat, centroid_mat){
  # Ensure both are matrices
  mat1 <- as.matrix(data_mat)
  mat2 <- as.matrix(centroid_mat)
  
  n <- nrow(mat1)
  k <- nrow(mat2)
  
  # Using outer product for vectorization
  dist_mat <- outer(1:n, 1:k, Vectorize(function(i, j) {
    sqrt(sum((mat1[i, ] - mat2[j, ])^2))
  }))
  
  return(dist_mat)
}


#' The function `k_means_visual` performs k-means clustering on a 2D data frame.
#' It requires a dataset with two numeric columns and a specified number of
#' clusters `k`. If `k` is not provided, the function randomly selects an
#' integer between 1 and 10.
#'
#' If `print_plot = TRUE`, the function displays a sequence of plots showing
#' how the algorithm converges over iterations. Edge colors represent the new
#' cluster assignments, while fill colors show the assignments from the
#' previous iteration.
#'
#' If, during the convergence process, a centroid receives no assigned points,
#' it is reinitialized with new random coordinates in the next iteration.
#'
#' The function returns a list containing:
#'   * `clusters`: a vector of length `n` indicating each point's cluster label
#'   * `cluster_locations`: a `k x 2` matrix with the coordinates 
#'      of the final centroids

k_means_visual <- function(k, data, print_plot = TRUE){
  
  # ------------------------ Validate inputs ------------------------
  
  # If k is missing, generate a random value between 1 and 10
  k <- ifelse(missing(k),
              round(runif(1,1,10),0),
              k)
  
  # Check that data is a 2D data frame
  if (missing(data) || !is.data.frame(data)){
    stop(
      paste("Please provide a data set.",
             "Note that it should be a 2-dimensional data frame.",
             sep = " ")
      )
  }
  if (ncol(data) != 2){
    stop(
      "Please make sure that the data frame is in a 2-dimensional space."
      )
  }
  # Standardize column names
  colnames(data) <- c("x","y")
  # Convert to matrix for distance calculations
  df_mat <- as.matrix(data)
  
  # Color palette for plotting
  my_palette <- hue_pal()(k) 
  
  # ------------------------ Initialization ------------------------
  
  # Random initial centroid positions within data range
  x_range <- range(data$x)
  y_range <- range(data$y)
  k_mat <- matrix(
    c(
      runif(k, x_range[1], x_range[2]),
      runif(k, y_range[1], y_range[2])
    ),
    nrow = k
  )
  colnames(k_mat) <- c("x","y")
  
  # ------------------------ Iterative update loop ------------------------
  
  need_update <- TRUE           # stop condition
  eps <- 1e-6                   # convergence threshold
  
  while(need_update){
    # Distances from each point to each centroid (n × k)
    dist_mat <- get_distances(df_mat, k_mat) 
    
    # Assign points to nearest centroid
    clusters <- apply(dist_mat, 1, which.min)
    
    # Visualization of current step
    if (print_plot){
      p1 <- ggplot()+
        # Data points
        geom_point(
          data = as.data.frame(df_mat), 
          aes(x = x, y = y,
              fill = factor(clusters, levels = 1:k),
              color = factor(clusters, levels = 1:k)),
          size = 2, shape = 21, stroke = 2
          )+
        # Centroids
        geom_point(
          data = as.data.frame(k_mat), 
          aes(x = x, y = y,
              fill = factor(1:nrow(k_mat), levels = 1:k),
              color = factor(1:nrow(k_mat), levels = 1:k)),
          size = 8, shape = 21
          )+
        # Make sure colors are the same for
        # fill and edge colors
        scale_color_manual(values = my_palette, 
                           breaks = 1:k,
                           labels = 1:k)+
        scale_fill_manual(values = my_palette, 
                          breaks = 1:k,
                          labels = 1:k)+
        scale_x_continuous(limits = x_range)+
        scale_y_continuous(limits = y_range)+
        labs(x = "X", y = "Y", 
             color = "Cluster", 
             fill = "Cluster")+
        theme_minimal()
      
      print(p1)
      Sys.sleep(1) # wait to see the differences between plots
    }
    
    # ------------------------ Update cenroids ------------------------
    
    k_mat_new <- matrix(NA, nrow = k, ncol = 2)
    colnames(k_mat_new) <- c("x","y")
    
    # Compute centroid means (coordinates)
    x_means <- tapply(df_mat[, "x"], clusters, mean)
    y_means <- tapply(df_mat[, "y"], clusters, mean)
    
    # Update centroids that have assigned points
    existing_clusters <- as.numeric(names(x_means))
    k_mat_new[existing_clusters, "x"] <- x_means
    k_mat_new[existing_clusters, "y"] <- y_means
    
    # Reinitialize empty clusters with random coordinates
    empty_clusters <- which(is.na(k_mat_new[, 1]))
    if(length(empty_clusters) > 0){
      for(i in empty_clusters){
        k_mat_new[i, ] <- c(runif(1, x_range[1], x_range[2]),
                            runif(1, y_range[1], y_range[2]))
      }
    }
    
    # Check for convergence
    converged <- all(abs(k_mat - k_mat_new) < eps)
    
    # Visualization of updated assignments (only if not converged)
    if(!converged){
      
      # Next clusters
      dist_mat_new <- get_distances(df_mat, k_mat_new)
      clusters_new <- apply(dist_mat_new, 1, which.min)
      
      if (print_plot) {
        p2 <- ggplot()+
          geom_point(
            data = as.data.frame(df_mat), 
            aes(x = x, y = y,
                fill = factor(clusters, levels = 1:k),       # previous step
                color = factor(clusters_new, levels = 1:k)), # updated step
            size = 2, shape = 21, stroke = 2
            )+
          geom_point(
            data = as.data.frame(k_mat_new), 
            aes(x = x, y = y,
                color = factor(1:nrow(k_mat_new), levels = 1:k),
                fill = factor(1:nrow(k_mat_new), levels = 1:k)),
            size = 8, shape = 21
            )+
          scale_color_manual(values = my_palette,
                             breaks = 1:k,
                             labels = 1:k)+
          scale_fill_manual(values = my_palette,
                            breaks = 1:k,
                            labels = 1:k)+
          scale_x_continuous(limits = x_range)+
          scale_y_continuous(limits = y_range)+
          labs(x = "X", y = "Y", 
               color = "Cluster", 
               fill = "Cluster")+
          theme_minimal()
        
        print(p2)
        Sys.sleep(1)
      }
    }
    
    # ------------------------ Prepare next iteration ------------------------
    if (converged){
      need_update <- FALSE
    } else {
      k_mat <- k_mat_new
      need_update <- TRUE
    }
  }
  
  # ------------------------ Output ------------------------
  out <- list(
    clusters = clusters,
    cluster_locations = k_mat
  )
  
  return(out)
}

# Example
k_means_visual(k=5,data = df,print_plot = TRUE)





# ------------------------ Optimal k function ------------------------



#' Computes the total within-cluster sum of squares for k = 1 to max_k
#' on a given 2D dataset. For each k, the function runs k-means (via
#' k_means_visual), extracts cluster assignments and centroid locations,
#' and calculates the sum of squared distances within clusters.
#'
#' Returns:
#'   A matrix with two columns:
#'     k           — number of clusters
#'     tot_sum_sq  — total within-cluster sum of squares
#'
#' Also prints a plot visualizing tot_sum_sq as a function of k.

optimal_k <- function(max_k, data){
  
  # Store results for each k
  within_total_sum <- numeric(length = max_k)
  
  # Loop over all possible k values
  for (k in 1:max_k){
    # Run k-means
    centroids_list <- k_means_visual(k = k, data = data, print_plot = FALSE)
    
    # Distances from every point to every centroid
    distances_to_all_centr <- get_distances(data_mat = as.matrix(data),
                                            centroid_mat = 
                                              centroids_list$cluster_locations)
    # Indices linking rows to their assigned cluster
    which_cluster <- cbind(1:nrow(distances_to_all_centr), 
                           centroids_list$clusters)
    # Distance from each point to its assigned centroid
    distances_to_cluster <- distances_to_all_centr[which_cluster]
    
    # Sum of squared distances within each cluster
    within_sum_sq_dist <- tapply(distances_to_cluster, 
                                 centroids_list$clusters,
                                 function(clust_dist){
                                   sum(clust_dist^2)
                                 })
    # Total within-cluster sum of squares for this k
    within_total_sum[k] <- sum(within_sum_sq_dist)
  }
  
  # Output matrix of results
  out <- matrix(
    c(1:max_k,
      within_total_sum), 
    ncol = 2
  )
  colnames(out) <- c("k","tot_sum_sq")
  
  # Visualization
  p1 <- ggplot(out,aes(k,tot_sum_sq))+
    geom_point(size = 5)+
    geom_line(linewidth = 1)+
    scale_x_continuous(breaks = 1:max_k)+
    labs(x = "Number of clusters, k",
         y = "Total within-cluster sum of squares")+
    theme_minimal()
  
  print(p1)
  return(out)
}

# Example
optimal_k(10,df)






















