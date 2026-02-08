# Code for figure 2A, to show workflow to perform 3D and 2D simulated dataset comparison (analysis of SD3)
# Figure 2A requires to images of 2 example 3D simulations, and images of those same simulations with a 'slice' being taken from them.

### 1. 2 example 3D simulations ----
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
                 marker = list(size = 2))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 250),
                                                  color = 'black', linewidth = 4),
                                     aspectmode = "data"))
  
  return(fig)
}

# Set up simulation metadata for both simulations
bg_metadata <- spe_metadata_background_template("random")
cluster_metadata <- spe_metadata_cluster_template("regular", "ellipsoid", bg_metadata)

cluster_metadata$background$n_cells <- 50000
cluster_metadata$background$length <- 500
cluster_metadata$background$width <- 500
cluster_metadata$background$height <- 250
cluster_metadata$background$minimum_distance_between_cells <- 10
cluster_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
cluster_metadata$background$cell_proportions <- c(0.02, 0, 0.08, 0.9)

cluster_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
cluster_metadata$cluster_1$cluster_cell_proportions <- c(0.75, 0.25)
cluster_metadata$cluster_1$radii <- c(100, 175, 100)
cluster_metadata$cluster_1$axes_rotation <- c(30, 0, 20)
cluster_metadata$cluster_1$centre_loc <- c(250, 250, 125)

# First simulation: mixed ellipsoid with more A > B cell type proportion
spe_cluster1 <- simulate_spe_metadata3D(cluster_metadata, plot_image = F)
plot_cells3D(spe_cluster1, plot_cell_types = c('A', 'B', 'O'), plot_colours = c('#f77e3b', '#48bbff', 'lightgray'))

# Second simulation: mixed ellipsoid with more B > A cell type proportion and different ellipsoid rotation
cluster_metadata$cluster_1$axes_rotation <- c(30, 0, -20)
cluster_metadata$cluster_1$cluster_cell_proportions <- c(0.25, 0.75)
spe_cluster2 <- simulate_spe_metadata3D(cluster_metadata, plot_image = F)
plot_cells3D(spe_cluster2, plot_cell_types = c('A', 'B', 'O'), plot_colours = c('#f77e3b', '#48bbff', 'lightgray'))

### 2. Add slices to simulations ------------------------
plot_cells3D_with_slices <- function(spe,
                                     plot_cell_types = NULL,
                                     plot_colours = NULL,
                                     feature_colname = "Cell.Type",
                                     slice_positions,
                                     slice_colors) {
  
  # Add 1000 to x, y and z coords to avoid plotting issues with plot_ly
  spatialCoords(spe) <- spatialCoords(spe) + 1000
  
  # Convert spe to data frame
  df <- data.frame(spatialCoords(spe), "Cell.Type" = spe[[feature_colname]])
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"], levels = plot_cell_types)
  
  # Plot cells
  fig <- plot_ly() %>%
    add_trace(
      data = df,
      type = "scatter3d",
      mode = 'markers',
      x = ~Cell.X.Position,
      y = ~Cell.Y.Position,
      z = ~Cell.Z.Position,
      marker = list(size = 2),
      color = ~Cell.Type,
      colors = plot_colours
    )
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(1000, 1500),
                                                  color = 'black', linewidth = 4),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(1000, 1500),
                                                  color = 'black', linewidth = 4),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(1000, 1250),
                                                  color = 'black', linewidth = 4),
                                     aspectmode = "data"))
  
  # Add 'slices' to the plot.
  index <- 1
  for (slice_position in slice_positions) {
    
    # Add 1000, as per previous comment when 1000 was added to all coords
    slice_position <- slice_position + 1000
    
    # 8 vertices for a slice, as a slice is a very thin rectangular prism
    vertices <- data.frame(x = c(1000, 1000, 1500, 1500, 1000, 1000, 1500, 1500),
                           y = c(1000, 1500, 1000, 1500, 1000, 1500, 1000, 1500),
                           z = rep(slice_position, each = 4))
    
    
    # Construct the rectangular prism with triangles that make up each face of the rectangular prism.
    # Each triangle is shown in each row of the below data frame, e.g. i = 1, j = 2, k = 3 means plot a triangle using vertices 1, 2, 3, as above.
    faces_temp <- data.frame(i = c(1, 4, 5, 8, 1, 6, 1, 1, 4, 4, 2, 2),
                             j = c(2, 2, 6, 6, 5, 5, 3, 5, 8, 3, 4, 6),
                             k = c(3, 3, 7, 7, 2, 2, 7, 7, 7, 7, 8, 8))
    
    fig <- fig %>%
      add_trace(
        type = 'mesh3d',
        x = vertices[, 1], 
        y = vertices[, 2], 
        z = vertices[, 3],
        i = faces_temp[, 1] - 1, 
        j = faces_temp[, 2] - 1, 
        k = faces_temp[, 3] - 1,
        opacity = 0.5,
        facecolor = rep(slice_colors[index], nrow(faces_temp))
      )
    
    index <- index + 1
  }
  
  methods::show(fig)
}

slice_positions_temp1 <- list(c(120, 130)) # Slice position is through the middle of the simulation, for first simulation.
slice_positions_temp2 <- list(c(190, 200)) # Slice position is through the 'uppermost' part of the simulation, for second simulation.

plot_cells3D_with_slices(spe_cluster1, 
                         plot_cell_types = c('A', 'B', 'O'), 
                         plot_colours = c('#f77e3b', '#48bbff', 'lightgray'),
                         slice_positions = slice_positions_temp1,
                         slice_colors = c("#9437a8"))

plot_cells3D_with_slices(spe_cluster2, 
                         plot_cell_types = c('A', 'B', 'O'), 
                         plot_colours = c('#f77e3b', '#48bbff', 'lightgray'),
                         slice_positions = slice_positions_temp2,
                         slice_colors = c("#b8db50"))

