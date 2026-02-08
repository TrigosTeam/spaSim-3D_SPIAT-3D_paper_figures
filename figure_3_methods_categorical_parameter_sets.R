# Code for figure 3 of the methods to show each categorical parameter set.
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
  
  ## Factor for feature columnzx
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
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, linecolor = 'black', linewidth = 4, range = c(0, 600)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, linecolor = 'black', linewidth = 4, range = c(0, 600)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, linecolor = 'black', linewidth = 4, range = c(0, 300)),
                                     aspectmode = "data"))
  
  return(fig)
}
# Mixed ellipsoid ----
simulation_metadata <- spe_metadata_background_template("random")
simulation_metadata$background$n_cells <- 30000
simulation_metadata$background$length <- 600
simulation_metadata$background$width <- 600
simulation_metadata$background$height <- 300
simulation_metadata$background$minimum_distance_between_cells <- 10
simulation_metadata$background$cell_types <- c('A', 'B', 'O')
simulation_metadata$background$cell_proportions <- c(0.01, 0, 0.99)

simulation_metadata <- spe_metadata_cluster_template("regular", "ellipsoid", simulation_metadata)
simulation_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
simulation_metadata$cluster_1$cluster_cell_proportions <- c(0.5, 0.5)
simulation_metadata$cluster_1$radii <- c(75, 100, 125)
simulation_metadata$cluster_1$centre_loc <- c(300, 300, 150)
simulation_metadata$cluster_1$axes_rotation <- c(0, 60, 0)

simulate_spe_metadata3D(simulation_metadata, TRUE, c('A', 'B', 'O'), c("#f77e3b", "#48bbff", "lightgray"))


# Mixed network -----
simulation_metadata <- spe_metadata_background_template("random")
simulation_metadata$background$n_cells <- 30000
simulation_metadata$background$length <- 600
simulation_metadata$background$width <- 600
simulation_metadata$background$height <- 300
simulation_metadata$background$minimum_distance_between_cells <- 10
simulation_metadata$background$cell_types <- c('A', 'B', 'O')
simulation_metadata$background$cell_proportions <- c(0.01, 0, 0.99)

simulation_metadata <- spe_metadata_cluster_template("regular", "network", simulation_metadata)
simulation_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
simulation_metadata$cluster_1$cluster_cell_proportions <- c(0.5, 0.5)
simulation_metadata$cluster_1$radius <- 125
simulation_metadata$cluster_1$edges <- 20
simulation_metadata$cluster_1$width <- 30
simulation_metadata$cluster_1$centre_loc <- c(300, 300, 150)

simulate_spe_metadata3D(simulation_metadata, TRUE, c('A', 'B', 'O'), c("#f77e3b", "#48bbff", "lightgray"))
# Ringed ellipsoid ----
simulation_metadata <- spe_metadata_background_template("random")
simulation_metadata$background$n_cells <- 30000
simulation_metadata$background$length <- 600
simulation_metadata$background$width <- 600
simulation_metadata$background$height <- 300
simulation_metadata$background$minimum_distance_between_cells <- 10
simulation_metadata$background$cell_types <- c('A', 'B', 'O')
simulation_metadata$background$cell_proportions <- c(0.01, 0, 0.99)

simulation_metadata <- spe_metadata_cluster_template("ring", "ellipsoid", simulation_metadata)
simulation_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
simulation_metadata$cluster_1$cluster_cell_proportions <- c(1, 0)
simulation_metadata$cluster_1$radii <- c(75, 100, 125)
simulation_metadata$cluster_1$centre_loc <- c(300, 300, 150)
simulation_metadata$cluster_1$axes_rotation <- c(0, 60, 0)
simulation_metadata$cluster_1$ring_cell_types <- c('A', 'B')
simulation_metadata$cluster_1$ring_cell_proportions <- c(0, 1)
simulation_metadata$cluster_1$ring_width <- 15

simulate_spe_metadata3D(simulation_metadata, TRUE, c('A', 'B', 'O'), c("#f77e3b", "#48bbff", "lightgray"))

# Ringed network -----
simulation_metadata <- spe_metadata_background_template("random")
simulation_metadata$background$n_cells <- 30000
simulation_metadata$background$length <- 600
simulation_metadata$background$width <- 600
simulation_metadata$background$height <- 300
simulation_metadata$background$minimum_distance_between_cells <- 10
simulation_metadata$background$cell_types <- c('A', 'B', 'O')
simulation_metadata$background$cell_proportions <- c(0.01, 0, 0.99)

simulation_metadata <- spe_metadata_cluster_template("ring", "network", simulation_metadata)
simulation_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
simulation_metadata$cluster_1$cluster_cell_proportions <- c(1, 0)
simulation_metadata$cluster_1$radius <- 125
simulation_metadata$cluster_1$edges <- 20
simulation_metadata$cluster_1$width <- 30
simulation_metadata$cluster_1$centre_loc <- c(300, 300, 150)
simulation_metadata$cluster_1$ring_cell_types <- c('A', 'B')
simulation_metadata$cluster_1$ring_cell_proportions <- c(0, 1)
simulation_metadata$cluster_1$ring_width <- 4.5

simulate_spe_metadata3D(simulation_metadata, TRUE, c('A', 'B', 'O'), c("#f77e3b", "#48bbff", "lightgray"))
# Separated ellipsoid ----
simulation_metadata <- spe_metadata_background_template("random")
simulation_metadata$background$n_cells <- 30000
simulation_metadata$background$length <- 600
simulation_metadata$background$width <- 600
simulation_metadata$background$height <- 300
simulation_metadata$background$minimum_distance_between_cells <- 10
simulation_metadata$background$cell_types <- c('A', 'B', 'O')
simulation_metadata$background$cell_proportions <- c(0.01, 0, 0.99)

simulation_metadata <- spe_metadata_cluster_template("regular", "ellipsoid", simulation_metadata)
simulation_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
simulation_metadata$cluster_1$cluster_cell_proportions <- c(1, 0)
simulation_metadata$cluster_1$radii <- c(75, 100, 125)
simulation_metadata$cluster_1$centre_loc <- c(150, 300, 150)
simulation_metadata$cluster_1$axes_rotation <- c(0, 60, 0)

simulation_metadata <- spe_metadata_cluster_template("regular", "sphere", simulation_metadata)
simulation_metadata$cluster_2$cluster_cell_types <- c('A', 'B')
simulation_metadata$cluster_2$cluster_cell_proportions <- c(0, 1)
simulation_metadata$cluster_2$radii <- 100
simulation_metadata$cluster_2$centre_loc <- c(450, 300, 150)

simulate_spe_metadata3D(simulation_metadata, TRUE, c('A', 'B', 'O'), c("#f77e3b", "#48bbff", "lightgray"))
# Separated network -----
simulation_metadata <- spe_metadata_background_template("random")
simulation_metadata$background$n_cells <- 30000
simulation_metadata$background$length <- 600
simulation_metadata$background$width <- 600
simulation_metadata$background$height <- 300
simulation_metadata$background$minimum_distance_between_cells <- 10
simulation_metadata$background$cell_types <- c('A', 'B', 'O')
simulation_metadata$background$cell_proportions <- c(0.01, 0, 0.99)

simulation_metadata <- spe_metadata_cluster_template("regular", "network", simulation_metadata)
simulation_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
simulation_metadata$cluster_1$cluster_cell_proportions <- c(1, 0)
simulation_metadata$cluster_1$radius <- 125
simulation_metadata$cluster_1$edges <- 20
simulation_metadata$cluster_1$width <- 30
simulation_metadata$cluster_1$centre_loc <- c(150, 300, 150)

simulation_metadata <- spe_metadata_cluster_template("regular", "sphere", simulation_metadata)
simulation_metadata$cluster_2$cluster_cell_types <- c('A', 'B')
simulation_metadata$cluster_2$cluster_cell_proportions <- c(0, 1)
simulation_metadata$cluster_2$radii <- 100
simulation_metadata$cluster_2$centre_loc <- c(450, 300, 150)

simulate_spe_metadata3D(simulation_metadata, TRUE, c('A', 'B', 'O'), c("#f77e3b", "#48bbff", "lightgray"))