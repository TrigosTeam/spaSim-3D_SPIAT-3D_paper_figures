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



# 1. Mixed background ----
background_metadata <- spe_metadata_background_template("random")
background_metadata$background$n_cells <- 2000
background_metadata$background$length <- 100
background_metadata$background$width <- 100
background_metadata$background$height <- 100
background_metadata$background$minimum_distance_between_cells <- 0
background_metadata$background$cell_types <- c('A', 'B', 'O')
background_metadata$background$cell_proportions <- c(0.3, 0.1, 0.6)

mixed_background_spe <- simulate_spe_metadata3D(background_metadata, plot_image = F)
plot_cells3D(mixed_background_spe, plot_cell_types = c('A', 'B', 'O'), plot_colours = c('orange', 'skyblue', 'lightgray'))


# 2. Clusters ----
background_metadata <- spe_metadata_background_template("random")
background_metadata$background$n_cells <- 2000
background_metadata$background$length <- 100
background_metadata$background$width <- 100
background_metadata$background$height <- 100
background_metadata$background$minimum_distance_between_cells <- 0
background_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
background_metadata$background$cell_proportions <- c(0, 0, 0.5, 0.5)

clusters_metadata1 <- spe_metadata_cluster_template("regular", "ellipsoid", background_metadata)
clusters_metadata1$cluster_1$cluster_cell_types <- c('A', 'B')
clusters_metadata1$cluster_1$cluster_cell_proportions <- c(0.6, 0.4)
clusters_metadata1$cluster_1$x_radius <- 25
clusters_metadata1$cluster_1$y_radius <- 25
clusters_metadata1$cluster_1$z_radius <- 50
clusters_metadata1$cluster_1$centre_loc <- c(50, 50, 50)
clusters_metadata1$cluster_1$x_z_rotation <- 45

clusters_metadata2 <- spe_metadata_cluster_template("regular", "sphere", clusters_metadata1)
clusters_metadata2$cluster_2$cluster_cell_types <- c('A', 'B')
clusters_metadata2$cluster_2$cluster_cell_proportions <- c(0.6, 0.4)
clusters_metadata2$cluster_2$radius <- 25
clusters_metadata2$cluster_2$centre_loc <- c(20, 60, 70)

clusters_metadata3 <- spe_metadata_cluster_template("regular", "sphere", clusters_metadata2)
clusters_metadata3$cluster_3$cluster_cell_types <- 'B'
clusters_metadata3$cluster_3$cluster_cell_proportions <- 1
clusters_metadata3$cluster_3$radius <- 50
clusters_metadata3$cluster_3$centre_loc <- c(100, 0, 100)

clusters_spe <- simulate_spe_metadata3D(clusters_metadata3, plot_image = F)
plot_cells3D(clusters_spe, plot_cell_types = c('A', 'B', 'O'), plot_colours = c('orange', 'skyblue', 'lightgray'))


# 3. Ringed clusters ----
background_metadata <- spe_metadata_background_template("random")
background_metadata$background$n_cells <- 2000
background_metadata$background$length <- 100
background_metadata$background$width <- 100
background_metadata$background$height <- 100
background_metadata$background$minimum_distance_between_cells <- 0
background_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
background_metadata$background$cell_proportions <- c(0, 0, 0.5, 0.5)

ringed_metadata1 <- spe_metadata_cluster_template("ring", "ellipsoid", background_metadata)
ringed_metadata1$cluster_1$cluster_cell_types <- 'A'
ringed_metadata1$cluster_1$cluster_cell_proportions <- 1
ringed_metadata1$cluster_1$x_radius <- 25
ringed_metadata1$cluster_1$y_radius <- 25
ringed_metadata1$cluster_1$z_radius <- 40
ringed_metadata1$cluster_1$centre_loc <- c(50, 50, 35)
ringed_metadata1$cluster_1$x_z_rotation <- 30
ringed_metadata1$cluster_1$ring_cell_types <- 'B'
ringed_metadata1$cluster_1$ring_cell_proportions <- 1
ringed_metadata1$cluster_1$ring_width <- 7

ringed_metadata2 <- spe_metadata_cluster_template("ring", "sphere", ringed_metadata1)
ringed_metadata2$cluster_2$cluster_cell_types <- 'A'
ringed_metadata2$cluster_2$cluster_cell_proportions <- 1
ringed_metadata2$cluster_2$radius <- 25
ringed_metadata2$cluster_2$centre_loc <- c(40, 60, 70)
ringed_metadata2$cluster_2$ring_cell_types <- 'B'
ringed_metadata2$cluster_2$ring_cell_proportions <- 1
ringed_metadata2$cluster_2$ring_width <- 7

ringed_metadata3 <- spe_metadata_cluster_template("regular", "ellipsoid", ringed_metadata2)
ringed_metadata3$cluster_3$cluster_cell_types <- 'A'
ringed_metadata3$cluster_3$cluster_cell_proportions <- 1
ringed_metadata3$cluster_3$x_radius <- 25
ringed_metadata3$cluster_3$y_radius <- 25
ringed_metadata3$cluster_3$z_radius <- 40
ringed_metadata3$cluster_3$centre_loc <- c(50, 50, 35)
ringed_metadata3$cluster_3$x_z_rotation <- 30

ringed_spe <- simulate_spe_metadata3D(ringed_metadata3, plot_image = F)
plot_cells3D(ringed_spe, plot_cell_types = c('A', 'B', 'O'), plot_colours = c('orange', 'skyblue', 'lightgray'))

# 4. Vessels ----
background_metadata <- spe_metadata_background_template("random")
background_metadata$background$n_cells <- 2000
background_metadata$background$length <- 100
background_metadata$background$width <- 100
background_metadata$background$height <- 100
background_metadata$background$minimum_distance_between_cells <- 0
background_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
background_metadata$background$cell_proportions <- c(0, 0, 0.5, 0.5)

vessels_metadata1 <- spe_metadata_cluster_template("regular", "cylinder", background_metadata)
vessels_metadata1$cluster_1$cluster_cell_types <- 'C'
vessels_metadata1$cluster_1$cluster_cell_proportions <- 1
vessels_metadata1$cluster_1$radius <- 12
vessels_metadata1$cluster_1$start_loc <- c(100, 80, 20)
vessels_metadata1$cluster_1$end_loc <- c(30, 30, 70)

vessels_metadata2 <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata1)
vessels_metadata2$cluster_2$cluster_cell_types <- 'C'
vessels_metadata2$cluster_2$cluster_cell_proportions <- 1
vessels_metadata2$cluster_2$radius <- 9
vessels_metadata2$cluster_2$start_loc <- c(79, 65, 35)
vessels_metadata2$cluster_2$end_loc <- c(30, 80, 50)

vessels_metadata3 <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata2)
vessels_metadata3$cluster_3$cluster_cell_types <- 'C'
vessels_metadata3$cluster_3$cluster_cell_proportions <- 1
vessels_metadata3$cluster_3$radius <- 10
vessels_metadata3$cluster_3$start_loc <- c(65, 55, 45)
vessels_metadata3$cluster_3$end_loc <- c(60, 0, 100)

vessels_metadata4 <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata3)
vessels_metadata4$cluster_4$cluster_cell_types <- 'C'
vessels_metadata4$cluster_4$cluster_cell_proportions <- 1
vessels_metadata4$cluster_4$radius <- 8
vessels_metadata4$cluster_4$start_loc <- c(30, 30, 70)
vessels_metadata4$cluster_4$end_loc <- c(10, 60, 100)

vessels_spe <- simulate_spe_metadata3D(vessels_metadata4, plot_image = F)
plot_cells3D(vessels_spe, plot_cell_types = c('C', 'O'), plot_colours = c('tomato', 'lightgray'))

# 5. Networks ----
background_metadata <- spe_metadata_background_template("random")
background_metadata$background$n_cells <- 2000
background_metadata$background$length <- 100
background_metadata$background$width <- 100
background_metadata$background$height <- 100
background_metadata$background$minimum_distance_between_cells <- 0
background_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
background_metadata$background$cell_proportions <- c(0, 0, 0.5, 0.5)

network_metadata <- spe_metadata_cluster_template("regular", "network", background_metadata)
network_metadata$cluster_1$cluster_cell_types <- c('A', 'B', 'D')
network_metadata$cluster_1$cluster_cell_proportions <- c(0.6, 0.2, 0.2)
network_metadata$cluster_1$radius <- 75
network_metadata$cluster_1$centre_loc <- c(50, 50, 50)
network_metadata$cluster_1$n_edges <- 35
network_metadata$cluster_1$width <- 12
  
network_spe <- simulate_spe_metadata3D(network_metadata, plot_image = F)
plot_cells3D(network_spe, plot_cell_types = c('A', 'B', 'D', 'O'), plot_colours = c('orange', 'skyblue', 'orchid', 'lightgray'))

# 6. Combination ----
background_metadata <- spe_metadata_background_template("random")
background_metadata$background$n_cells <- 3000
background_metadata$background$length <- 100
background_metadata$background$width <- 100
background_metadata$background$height <- 100
background_metadata$background$minimum_distance_between_cells <- 0
background_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
background_metadata$background$cell_proportions <- c(0.025, 0.025, 0.30, 0.65)

vessels_metadata1 <- spe_metadata_cluster_template("regular", "cylinder", background_metadata)
vessels_metadata1$cluster_1$cluster_cell_types <- 'C'
vessels_metadata1$cluster_1$cluster_cell_proportions <- 1
vessels_metadata1$cluster_1$radius <- 12
vessels_metadata1$cluster_1$start_loc <- c(0, 20, 80)
vessels_metadata1$cluster_1$end_loc <- c(70, 70, 30)

vessels_metadata2 <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata1)
vessels_metadata2$cluster_2$cluster_cell_types <- 'C'
vessels_metadata2$cluster_2$cluster_cell_proportions <- 1
vessels_metadata2$cluster_2$radius <- 9
vessels_metadata2$cluster_2$start_loc <- c(21, 35, 65)
vessels_metadata2$cluster_2$end_loc <- c(70, 20, 50)

vessels_metadata3 <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata2)
vessels_metadata3$cluster_3$cluster_cell_types <- 'C'
vessels_metadata3$cluster_3$cluster_cell_proportions <- 1
vessels_metadata3$cluster_3$radius <- 10
vessels_metadata3$cluster_3$start_loc <- c(45, 55, 65)
vessels_metadata3$cluster_3$end_loc <- c(40, 100, 0)

vessels_metadata4 <- spe_metadata_cluster_template("regular", "cylinder", vessels_metadata3)
vessels_metadata4$cluster_4$cluster_cell_types <- 'C'
vessels_metadata4$cluster_4$cluster_cell_proportions <- 1
vessels_metadata4$cluster_4$radius <- 8
vessels_metadata4$cluster_4$start_loc <- c(70, 70, 30)
vessels_metadata4$cluster_4$end_loc <- c(90, 40, 0)

ringed_metadata1 <- spe_metadata_cluster_template("ring", "ellipsoid", vessels_metadata4)
ringed_metadata1$cluster_5$cluster_cell_types <- 'A'
ringed_metadata1$cluster_5$cluster_cell_proportions <- 1
ringed_metadata1$cluster_5$x_radius <- 30
ringed_metadata1$cluster_5$y_radius <- 30
ringed_metadata1$cluster_5$z_radius <- 40
ringed_metadata1$cluster_5$centre_loc <- c(80, 20, 35)
ringed_metadata1$cluster_5$x_z_rotation <- 30
ringed_metadata1$cluster_5$ring_cell_types <- 'B'
ringed_metadata1$cluster_5$ring_cell_proportions <- 1
ringed_metadata1$cluster_5$ring_width <- 7

sphere_metadata <- spe_metadata_cluster_template("regular", "sphere", ringed_metadata1)
sphere_metadata$cluster_6$cluster_cell_types <- c('A', 'D')
sphere_metadata$cluster_6$cluster_cell_proportions <- c(0.6, 0.4)
sphere_metadata$cluster_6$radius <- 25
sphere_metadata$cluster_6$centre_loc <- c(10, 90, 90)

final_spe <- simulate_spe_metadata3D(sphere_metadata, plot_image = F)
plot_cells3D(final_spe, 
             plot_cell_types = c('A', 'B', 'C', 'D', 'O'), 
             plot_colours = c('orange', 'skyblue', 'tomato', 'orchid', 'lightgray'))

