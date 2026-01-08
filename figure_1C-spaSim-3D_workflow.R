# Alter plot_cells3D function ----
plot_cells3D <- function(spe,
                         plot_cell_types = NULL,
                         plot_colours = NULL,
                         feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  if (!is.null(plot_cell_types) && !is.character(plot_cell_types)) {
    stop("`plot_cell_types` is not a character vector or NULL.")
  } 
  if (!is.null(plot_colours) && !is.character(plot_colours)) {
    stop("`plot_colours` is not a character vector or NULL.")
  } 
  if (is.character(plot_colours)) {
    non_colours <- plot_colours[which(!(sapply(plot_colours, function(X) {
      tryCatch(is.matrix(col2rgb(X)), 
               error = function(e) FALSE)
    })))]
    if (length(non_colours) > 0) {
      stop(paste("The following plot_colours are not colours:\n   ",
                 paste(non_colours, collapse = ", ")))
    } 
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste(feature_colname, "is not a valid column in your spe object."))
  }
  
  ## Convert spe object to data frame
  df <- data.frame(spatialCoords(spe), "Cell.Type" = spe[[feature_colname]])
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) {
    warning("plot_cell_types not specified, all cell types found in the spe object will be used.")
    plot_cell_types <- unique(df[["Cell.Type"]])
  }
  ## If no colours inputted, use rainbow palette
  if (is.null(plot_colours)) {
    warning("plot_colours not specified, rainbow palette will be used.")
    plot_colours <- rainbow(length(plot_cell_types))
  }
  ## User inputs mismatching cell types and colours
  if (length(plot_cell_types) != length(plot_colours)) {
    stop("Length of plot_cell_types is not equal to length of plot_colours")
  }
  
  ## If cell types have been chosen, check they are found in the spe object
  spe_cell_types <- unique(spe[[feature_colname]])
  unknown_cell_types <- setdiff(plot_cell_types, spe_cell_types)
  
  if (length(unknown_cell_types) == length(plot_cell_types)) {
    stop("None of the plot_cell_types are found in the spe object")
  }
  
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following plot_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    plot_colours <- plot_colours[which(plot_cell_types %in% spe_cell_types)]
    plot_cell_types <- intersect(plot_cell_types, spe_cell_types)
  }
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"],
                              levels = plot_cell_types)
  
  ## Plot
  fig <- plot_ly(df,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~Cell.X.Position,
                 y = ~Cell.Y.Position,
                 z = ~Cell.Z.Position,
                 color = ~Cell.Type,
                 colors = plot_colours,
                 marker = list(size = 4))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  showline = TRUE, linecolor = 'black', linewidth = 2),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  showline = TRUE, linecolor = 'black', linewidth = 2),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  showline = TRUE, linecolor = 'black', linewidth = 2)))
  
  return(fig)
}



# 0. Blank background ----
background_metadata <- spe_metadata_background_template("random")
background_metadata$background$n_cells <- 10000
background_metadata$background$length <- 100
background_metadata$background$width <- 100
background_metadata$background$height <- 100
background_metadata$background$minimum_distance_between_cells <- 0
background_metadata$background$cell_types <- c('O', 'A')
background_metadata$background$cell_proportions <- c(0.99, 0.01)

blank_background_spe <- simulate_spe_metadata3D(background_metadata, plot_image = F)
spatialCoords(blank_background_spe) <- spatialCoords(blank_background_spe) + 1000
plot_cells3D(blank_background_spe, plot_cell_types = c('O', 'A'), plot_colours = c('lightgray', 'orange'))


# 1. Mixed background ----
background_metadata <- spe_metadata_background_template("random")
background_metadata$background$n_cells <- 20000
background_metadata$background$length <- 100
background_metadata$background$width <- 100
background_metadata$background$height <- 100
background_metadata$background$minimum_distance_between_cells <- 0
background_metadata$background$cell_types <- c('A', 'B', 'O')
background_metadata$background$cell_proportions <- c(0.3, 0.1, 0.6)

mixed_background_spe <- simulate_spe_metadata3D(background_metadata, plot_image = F)
spatialCoords(mixed_background_spe) <- spatialCoords(mixed_background_spe) + 1000
plot_cells3D(mixed_background_spe, 
             plot_cell_types = c('A', 'B', 'O'), 
             plot_colours = c('#f77e3b', '#48bbff', 'lightgray'))



# 2. Clusters ----
background_metadata <- spe_metadata_background_template("random")
clusters_metadata <- spe_metadata_cluster_template("regular", "ellipsoid", background_metadata)
clusters_metadata <- spe_metadata_cluster_template("regular", "sphere", clusters_metadata)
clusters_metadata <- spe_metadata_cluster_template("regular", "sphere", clusters_metadata)

clusters_metadata$background$n_cells <- 20000
clusters_metadata$background$length <- 100
clusters_metadata$background$width <- 100
clusters_metadata$background$height <- 100
clusters_metadata$background$minimum_distance_between_cells <- 0
clusters_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
clusters_metadata$background$cell_proportions <- c(0, 0, 0.25, 0.75)

clusters_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
clusters_metadata$cluster_1$cluster_cell_proportions <- c(0.6, 0.4)
clusters_metadata$cluster_1$centre_loc <- c(50, 50, 50)
clusters_metadata$cluster_1$radii <- c(25, 25, 50)
clusters_metadata$cluster_1$axes_rotation <- c(0, 45, 0)


clusters_metadata$cluster_2$cluster_cell_types <- c('A', 'B')
clusters_metadata$cluster_2$cluster_cell_proportions <- c(0.6, 0.4)
clusters_metadata$cluster_2$radius <- 25
clusters_metadata$cluster_2$centre_loc <- c(20, 60, 70)


clusters_metadata$cluster_3$cluster_cell_types <- 'B'
clusters_metadata$cluster_3$cluster_cell_proportions <- 1
clusters_metadata$cluster_3$radius <- 50
clusters_metadata$cluster_3$centre_loc <- c(100, 0, 100)

clusters_spe <- simulate_spe_metadata3D(clusters_metadata, plot_image = F)
spatialCoords(clusters_spe) <- spatialCoords(clusters_spe) + 1000
plot_cells3D(clusters_spe, 
             plot_cell_types = c('A', 'B', 'O'), 
             plot_colours = c('#f77e3b', '#48bbff', 'lightgray'))


# 3. Ringed clusters ----
background_metadata <- spe_metadata_background_template("random")
ringed_metadata <- spe_metadata_cluster_template("ring", "ellipsoid", background_metadata)
ringed_metadata <- spe_metadata_cluster_template("ring", "sphere", ringed_metadata)
ringed_metadata <- spe_metadata_cluster_template("regular", "ellipsoid", ringed_metadata)

ringed_metadata$background$n_cells <- 20000
ringed_metadata$background$length <- 100
ringed_metadata$background$width <- 100
ringed_metadata$background$height <- 100
ringed_metadata$background$minimum_distance_between_cells <- 0
ringed_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
ringed_metadata$background$cell_proportions <- c(0, 0, 0.25, 0.75)

ringed_metadata$cluster_1$cluster_cell_types <- 'A'
ringed_metadata$cluster_1$cluster_cell_proportions <- 1
ringed_metadata$cluster_1$radii <- c(25, 25, 50)
ringed_metadata$cluster_1$axes_rotation <- c(0, 30, 0)
ringed_metadata$cluster_1$centre_loc <- c(50, 50, 35)
ringed_metadata$cluster_1$ring_cell_types <- 'B'
ringed_metadata$cluster_1$ring_cell_proportions <- 1
ringed_metadata$cluster_1$ring_width <- 7

ringed_metadata$cluster_2$cluster_cell_types <- 'A'
ringed_metadata$cluster_2$cluster_cell_proportions <- 1
ringed_metadata$cluster_2$radius <- 25
ringed_metadata$cluster_2$centre_loc <- c(40, 60, 70)
ringed_metadata$cluster_2$ring_cell_types <- 'B'
ringed_metadata$cluster_2$ring_cell_proportions <- 1
ringed_metadata$cluster_2$ring_width <- 7

ringed_metadata$cluster_3$cluster_cell_types <- 'A'
ringed_metadata$cluster_3$cluster_cell_proportions <- 1
ringed_metadata$cluster_3$radii <- c(25, 25, 40)
ringed_metadata$cluster_3$axes_rotation <- c(0, 30, 0)
ringed_metadata$cluster_3$centre_loc <- c(50, 50, 35)

ringed_spe <- simulate_spe_metadata3D(ringed_metadata, plot_image = F)
spatialCoords(ringed_spe) <- spatialCoords(ringed_spe) + 1000
plot_cells3D(ringed_spe, 
             plot_cell_types = c('A', 'B', 'O'), 
             plot_colours = c('#f77e3b', '#48bbff', 'lightgray'))

# 4. Vessels ----
background_metadata <- spe_metadata_background_template("random")
vessels_metadata <- spe_metadata_cluster_template("regular", "cylinder", background_metadata)
vessels_metadata <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata)
vessels_metadata <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata)
vessels_metadata <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata)
vessels_metadata <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata)

vessels_metadata$background$n_cells <- 20000
vessels_metadata$background$length <- 100
vessels_metadata$background$width <- 100
vessels_metadata$background$height <- 100
vessels_metadata$background$minimum_distance_between_cells <- 0
vessels_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
vessels_metadata$background$cell_proportions <- c(0, 0, 0.25, 0.75)

vessels_metadata$cluster_1$cluster_cell_types <- 'C'
vessels_metadata$cluster_1$cluster_cell_proportions <- 1
vessels_metadata$cluster_1$radius <- 12
vessels_metadata$cluster_1$start_loc <- c(100, 80, 20)
vessels_metadata$cluster_1$end_loc <- c(30, 30, 70)

vessels_metadata$cluster_2$cluster_cell_types <- 'C'
vessels_metadata$cluster_2$cluster_cell_proportions <- 1
vessels_metadata$cluster_2$radius <- 9
vessels_metadata$cluster_2$start_loc <- c(79, 65, 35)
vessels_metadata$cluster_2$end_loc <- c(30, 80, 50)

vessels_metadata$cluster_3$cluster_cell_types <- 'C'
vessels_metadata$cluster_3$cluster_cell_proportions <- 1
vessels_metadata$cluster_3$radius <- 10
vessels_metadata$cluster_3$start_loc <- c(65, 55, 45)
vessels_metadata$cluster_3$end_loc <- c(60, 0, 100)

vessels_metadata$cluster_4$cluster_cell_types <- 'C'
vessels_metadata$cluster_4$cluster_cell_proportions <- 1
vessels_metadata$cluster_4$radius <- 8
vessels_metadata$cluster_4$start_loc <- c(30, 30, 70)
vessels_metadata$cluster_4$end_loc <- c(10, 60, 100)

vessels_metadata$cluster_5$cluster_cell_types <- 'C'
vessels_metadata$cluster_5$cluster_cell_proportions <- 1
vessels_metadata$cluster_5$radius <- 8
vessels_metadata$cluster_5$start_loc <- c(30, 80, 50)
vessels_metadata$cluster_5$end_loc <- c(0, 50, 60)

vessels_spe <- simulate_spe_metadata3D(vessels_metadata, plot_image = F)
spatialCoords(vessels_spe) <- spatialCoords(vessels_spe) + 1000
plot_cells3D(vessels_spe, 
             plot_cell_types = c('C', 'O'), 
             plot_colours = c('#bb0036', 'lightgray'))

# 5. Networks ----
background_metadata <- spe_metadata_background_template("random")
background_metadata$background$n_cells <- 20000
background_metadata$background$length <- 100
background_metadata$background$width <- 100
background_metadata$background$height <- 100
background_metadata$background$minimum_distance_between_cells <- 0
background_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
background_metadata$background$cell_proportions <- c(0, 0, 0.25, 0.75)

network_metadata <- spe_metadata_cluster_template("regular", "network", background_metadata)
network_metadata$cluster_1$cluster_cell_types <- c('A', 'B', 'D')
network_metadata$cluster_1$cluster_cell_proportions <- c(0.6, 0.2, 0.2)
network_metadata$cluster_1$radius <- 75
network_metadata$cluster_1$centre_loc <- c(50, 50, 50)
network_metadata$cluster_1$n_edges <- 35
network_metadata$cluster_1$width <- 12
  
network_spe <- simulate_spe_metadata3D(network_metadata, plot_image = F)
spatialCoords(network_spe) <- spatialCoords(network_spe) + 1000
plot_cells3D(network_spe, plot_cell_types = c('A', 'B', 'D', 'O'), plot_colours = c('#bb7438', '#7f64b9', '#72ac5c', 'lightgray'))

# 6. Combination ----
combination_metadata <- spe_metadata_background_template("random")
combination_metadata <- spe_metadata_cluster_template("regular", "cylinder", combination_metadata)
combination_metadata <- spe_metadata_cluster_template("regular", "cylinder", combination_metadata)
combination_metadata <- spe_metadata_cluster_template("regular", "cylinder", combination_metadata)
combination_metadata <- spe_metadata_cluster_template("ring", "ellipsoid", combination_metadata)
combination_metadata <- spe_metadata_cluster_template("regular", "sphere", combination_metadata)

combination_metadata$background$n_cells <- 20000
combination_metadata$background$length <- 100
combination_metadata$background$width <- 100
combination_metadata$background$height <- 100
combination_metadata$background$minimum_distance_between_cells <- 0
combination_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
combination_metadata$background$cell_proportions <- c(0.01, 0.01, 0.20, 0.78)

combination_metadata$cluster_1$cluster_cell_types <- 'C'
combination_metadata$cluster_1$cluster_cell_proportions <- 1
combination_metadata$cluster_1$radius <- 9
combination_metadata$cluster_1$start_loc <- c(0, 20, 80)
combination_metadata$cluster_1$end_loc <- c(70, 70, 30)

combination_metadata$cluster_2$cluster_cell_types <- 'C'
combination_metadata$cluster_2$cluster_cell_proportions <- 1
combination_metadata$cluster_2$radius <- 8
combination_metadata$cluster_2$start_loc <- c(40, 40, 60)
combination_metadata$cluster_2$end_loc <- c(35, 0, 75)

combination_metadata$cluster_3$cluster_cell_types <- 'C'
combination_metadata$cluster_3$cluster_cell_proportions <- 1
combination_metadata$cluster_3$radius <- 10
combination_metadata$cluster_3$start_loc <- c(100, 70, 20)
combination_metadata$cluster_3$end_loc <- c(0, 100, 50)

combination_metadata$cluster_4$cluster_cell_types <- 'A'
combination_metadata$cluster_4$cluster_cell_proportions <- 1
combination_metadata$cluster_4$radii <- c(30, 30, 40)
combination_metadata$cluster_4$axes_rotation <- c(0, 30, 0)
combination_metadata$cluster_4$centre_loc <- c(80, 20, 35)
combination_metadata$cluster_4$ring_cell_types <- 'B'
combination_metadata$cluster_4$ring_cell_proportions <- 1
combination_metadata$cluster_4$ring_width <- 7

combination_metadata$cluster_5$cluster_cell_types <- c('A', 'D')
combination_metadata$cluster_5$cluster_cell_proportions <- c(0.6, 0.4)
combination_metadata$cluster_5$radius <- 25
combination_metadata$cluster_5$centre_loc <- c(10, 80, 90)

final_spe <- simulate_spe_metadata3D(combination_metadata, plot_image = F)
spatialCoords(final_spe) <- spatialCoords(final_spe) + 1000
plot_cells3D(final_spe, 
             plot_cell_types = c('A', 'B', 'C', 'D', 'O'), 
             plot_colours = c('#f77e3b', '#48bbff', '#bb0036', '#007128', 'lightgray'))


# Apply clustering algorithms to combination simulation
# ah_spe <- alpha_hull_clustering3D(final_spe, 'B', 5, 100)
# ah_spe <- alpha_hull_clustering3D(final_spe, 'C', 5, 100)
# ah_spe <- alpha_hull_clustering3D(final_spe, c('A', 'D'), 3.3, 200)
# 
# gbc_spe <- grid_based_clustering3D(final_spe, 'B', 10, 100)
# gbc_spe <- grid_based_clustering3D(final_spe, 'C', 10, 100)
# gbc_spe <- grid_based_clustering3D(final_spe, c('A', 'D'), 10, 100)
# 
# plot_alpha_hull_clusters3D(ah_spe)
