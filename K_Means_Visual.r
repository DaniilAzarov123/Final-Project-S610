# Load packages
library(ggplot2)


# Load data
data("iris")
df <- iris[,c("Sepal.Length","Petal.Width")]
colnames(df) <- c("x","y")
df_mat <- as.matrix(df) # make it a matrix

# Define funtions ----

# Function to calculate distances in a 2D space
# Returns distances from each point to each centroid
get_distances <- function(mat1, mat2){
  dist_mat <- apply(mat1, 1, function(mat1_row){
    apply(mat2, 1, function(mat2_row){
      sqrt( sum( (mat2_row - mat1_row)^2 )
      )
    })
  })
  dist_mat <- t(dist_mat) # n x k dimensions
  return(dist_mat)
}

# Number of clusters (k)
k <- 3

# Set initial random values for k centroids
x_range <- range(df$x)
y_range <- range(df$y)
set.seed(1)
k_mat <- matrix(
  c(
    runif(k, x_range[1], x_range[2]),
    runif(k, y_range[1], y_range[2])
  ),
  nrow = k
)
colnames(k_mat) <- c("x","y")


# ------------------------ Initialize the cycle ------------------------

# Make it a cycle
need_update <- TRUE
eps <- 1e-6

while(need_update){
  # Calculate distances
  dist_mat <- get_distances(df_mat,k_mat) 
  
  # Define clusters
  clusters_1 <- apply(dist_mat, 1, which.min)
  
  # Visualize
  p1 <- ggplot()+
    # Data points
    geom_point(data = df_mat, aes(x = x, y = y,
                                  color = as.factor(clusters_1)
    ),
    size = 3.5)+
    # Centroids
    geom_point(data = k_mat, aes(x = x, y = y,
                                 color = as.factor(c(1:k))
    ),
    size = 7)+
    scale_x_continuous(limits = x_range)+
    scale_y_continuous(limits = y_range)+
    labs(x = "X", y = "Y", color = "Cluster")+
    theme_minimal()
  
  print(p1)
  Sys.sleep(1)
  
  # ------------------------ Update locations ------------------------
  
  # Define new centroid positions
  k_mat_new <- do.call(rbind, by(df_mat, clusters_1, colMeans))
  
  # New distances
  dist_mat_new <- get_distances(df_mat,k_mat_new) 
  
  # Define new clusters
  clusters_2 <- apply(dist_mat_new, 1, which.min)
  
  # New visualization with updated clusters
  p2 <- ggplot()+
    # Data points
    geom_point(data = df_mat, aes(x = x, y = y,
                                  fill = as.factor(clusters_1),
                                  color = as.factor(clusters_2)
    ),
    size = 2, shape = 21, stroke = 1.5)+
    # Centroids
    geom_point(data = k_mat_new, aes(x = x, y = y,
                                     color = as.factor(c(1:k)),
                                     fill = as.factor(c(1:k))
    ),
    size = 7, shape = 21)+
    scale_x_continuous(limits = x_range)+
    scale_y_continuous(limits = y_range)+
    labs(x = "X", y = "Y", color = "Cluster", fill = "Cluster")+
    theme_minimal()
  
  print(p2)
  Sys.sleep(1)
  
  # Check if need new iteration
  if (all( abs(k_mat - k_mat_new) < eps )){
    need_update <- FALSE
  } else {
    k_mat <- k_mat_new # update centroid locations
    need_update <- TRUE
  }
}















































