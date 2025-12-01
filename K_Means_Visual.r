# Load packages
library(ggplot2) # For visualization
library(scales) # To extract palettes


# Load data
data("iris")
df <- iris[,c("Sepal.Length","Petal.Width")]

# Pre-define a function 
# to compute distances in a 2D space
# Returns distances from each point to each centroid
get_distances <- function(mat1, mat2){
  # Ensure both are matrices
  mat1 <- as.matrix(mat1)
  mat2 <- as.matrix(mat2)
  
  n <- nrow(mat1)
  k <- nrow(mat2)
  
  # Using outer product for vectorization
  dist_mat <- outer(1:n, 1:k, Vectorize(function(i, j) {
    sqrt(sum((mat1[i, ] - mat2[j, ])^2))
  }))
  
  return(dist_mat)
}


#' The function k_means_visual takes a 2D data frame
#' and the number of clusters k. If k is not provided
#' it generates a random value between 1 and 10 for k.
#' While executing, it prints multiple plots to show
#' how the algorithm converges. Note, that edge colors
#' show new cluster classifications, while fill colors
#' indicate previous clusters. It returns a list of
#' clusters (length = n) and their coordinates
#' in the 2D space

k_means_visual <- function(k = NULL, data){
  
  # ------------------------ Check hyper-variables first ------------------------
  
  # Check if k is provided, otherwise generate a random number
  # between 1 and 10
  k <- ifelse(is.null(k),
              round(runif(1,1,10),0),
              k)
  
  # Check if it's a 2D data
  if (is.data.frame(data)){
    df <- data
  } else{
    stop("Please provide a data set")
  }
  if (ncol(data) != 2){
    stop("Please make sure that the data frame is in a 2-dimensional space")
  }
  # Make sure column names are X and Y
  colnames(df) <- c("x","y")
  # Create a matrix from df - we'll use it later
  df_mat <- as.matrix(df)
  
  # Create a palette for visualization
  my_palette <- hue_pal()(k) 
  
  # ------------------------ Initialize function ------------------------
  
  # Set initial random values for k centroids
  # within the range of X and Y
  x_range <- range(df$x)
  y_range <- range(df$y)
  k_mat <- matrix(
    c(
      runif(k, x_range[1], x_range[2]),
      runif(k, y_range[1], y_range[2])
    ),
    nrow = k
  )
  colnames(k_mat) <- c("x","y")
  
  # ------------------------ Initialize the cycle ------------------------
  
  # Hyper-variables
  need_update <- TRUE # flag when to stop the cycle
  eps <- 1e-6 # precision
  
  while(need_update){
    # Calculate distances
    # n x k matrix - distances from each data point
    # to each centroid
    dist_mat <- get_distances(df_mat, k_mat) 
    
    # Define clusters based on distances
    clusters <- apply(dist_mat, 1, which.min)
    
    # Visualize
    p1 <- ggplot()+
      # Data points
      geom_point(data = as.data.frame(df_mat), aes(x = x, y = y,
                                                   fill = factor(clusters, levels = 1:k),
                                                   color = factor(clusters, levels = 1:k)),
                 size = 2, shape = 21, stroke = 2)+
      # Centroids
      geom_point(data = as.data.frame(k_mat), aes(x = x, y = y,
                                                  fill = factor(1:nrow(k_mat), levels = 1:k),
                                                  color = factor(1:nrow(k_mat), levels = 1:k)),
                 size = 8, shape = 21)+
      scale_color_manual(values = my_palette, 
                         breaks = 1:k,
                         labels = 1:k)+
      scale_fill_manual(values = my_palette, 
                        breaks = 1:k,
                        labels = 1:k)+
      scale_x_continuous(limits = x_range)+
      scale_y_continuous(limits = y_range)+
      labs(x = "X", y = "Y", color = "Cluster", fill = "Cluster")+
      theme_minimal()
    
    print(p1)
    Sys.sleep(1) # wait to see the differences between plots
    
    # ------------------------ Update locations ------------------------
    
    # Define new centroid positions
    # Address a problem that sometimes centroids can
    # fall out (be too far away from data points)
    # and k can become larger than the
    # number of actual centroids
    k_mat_new <- matrix(NA, nrow = k, ncol = 2)
    colnames(k_mat_new) <- c("x","y")
    
    x_means <- tapply(df_mat[, "x"], clusters, mean)
    y_means <- tapply(df_mat[, "y"], clusters, mean)
    
    # Fill in the centroids that exist
    existing_clusters <- as.numeric(names(x_means))
    k_mat_new[existing_clusters, "x"] <- x_means
    k_mat_new[existing_clusters, "y"] <- y_means
    
    # Handle empty clusters if they exist
    # Simply assign random values within the range
    empty_clusters <- which(is.na(k_mat_new[, 1]))
    if(length(empty_clusters) > 0){
      for(i in empty_clusters){
        k_mat_new[i, ] <- c(runif(1, x_range[1], x_range[2]),
                            runif(1, y_range[1], y_range[2]))
      }
    }
    
    # Check if a new iteration is needed BEFORE creating p2
    converged <- all(abs(k_mat - k_mat_new) < eps)
    
    # Only create and show p2 if not converged yet
    if(!converged){
      # New distances 
      dist_mat_new <- get_distances(df_mat, k_mat_new)
      
      # Define new clusters
      clusters_new <- apply(dist_mat_new, 1, which.min)
      
      # New visualization with updated clusters
      # Fill color - old classifications
      # Edge color - new classifications
      p2 <- ggplot()+
        # Data points
        geom_point(data = as.data.frame(df_mat), aes(x = x, y = y,
                                                     fill = factor(clusters, levels = 1:k),
                                                     color = factor(clusters_new, levels = 1:k)),
                   size = 2, shape = 21, stroke = 2)+
        # Centroids
        geom_point(data = as.data.frame(k_mat_new), aes(x = x, y = y,
                                                        color = factor(1:nrow(k_mat_new), levels = 1:k),
                                                        fill = factor(1:nrow(k_mat_new), levels = 1:k)),
                   size = 8, shape = 21)+
        scale_color_manual(values = my_palette,
                           breaks = 1:k,
                           labels = 1:k)+
        scale_fill_manual(values = my_palette,
                          breaks = 1:k,
                          labels = 1:k)+
        scale_x_continuous(limits = x_range)+
        scale_y_continuous(limits = y_range)+
        labs(x = "X", y = "Y", color = "Cluster", fill = "Cluster")+
        theme_minimal()
      
      print(p2)
      Sys.sleep(1)
    }
    
    # Update for next iteration
    if (converged){
      need_update <- FALSE
    } else {
      k_mat <- k_mat_new # update centroid locations
      need_update <- TRUE
    }
  }
  
  # Return a list with clusters and their positions
  out <- list(
    clusters = clusters,
    cluster_locations = k_mat
  )
  
  return(out)
}

















k_means_visual(k=10,data = df)

