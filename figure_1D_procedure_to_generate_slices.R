### 1. Example simulation ----
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
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  return(fig)
}


bg_metadata <- spe_metadata_background_template("random")
cluster_metadata <- spe_metadata_cluster_template("regular", "ellipsoid", bg_metadata)

cluster_metadata$background$n_cells <- 10000
cluster_metadata$background$length <- 500
cluster_metadata$background$width <- 500
cluster_metadata$background$height <- 500
cluster_metadata$background$minimum_distance_between_cells <- 10
cluster_metadata$background$cell_types <- c('A', 'B', 'O')
cluster_metadata$background$cell_proportions <- c(0.02, 0, 0.98)

cluster_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
cluster_metadata$cluster_1$cluster_cell_proportions <- c(0.6, 0.4)
cluster_metadata$cluster_1$x_radius <- 150
cluster_metadata$cluster_1$x_radius <- 150
cluster_metadata$cluster_1$x_radius <- 200
cluster_metadata$cluster_1$x_y_rotation <- -30
cluster_metadata$cluster_1$x_z_rotation <- -30
cluster_metadata$cluster_1$y_z_rotation <- 0
cluster_metadata$cluster_1$centre_loc <- c(250, 250, 250)

spe_cluster <- simulate_spe_metadata3D(cluster_metadata)
plot_cells3D(spe_cluster, plot_cell_types = c('A', 'B'), plot_colours = c('orange', 'skyblue'))

### 2. Add slices to simulation ------------------------
plot_cells3D_with_slices <- function(spe,
                                     plot_cell_types = NULL,
                                     plot_colours = NULL,
                                     feature_colname = "Cell.Type",
                                     slice_positions) {
  
  df <- data.frame(spatialCoords(spe_cluster), "Cell.Type" = spe_cluster[[feature_colname]])
  
  ## Factor for feature column
  df[, "Cell.Type"] <- factor(df[, "Cell.Type"], levels = plot_cell_types)
  
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
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  
  fig <- fig %>%
    add_trace(
      type = 'mesh3d',
      x = vertices[, 1], 
      y = vertices[, 2], 
      z = vertices[, 3],
      i = faces_temp[, 1] - 1, 
      j = faces_temp[, 2] - 1, 
      k = faces_temp[, 3] - 1,
      opacity = opacity_level,
      facecolor = rep(alpha_hull_colours[i], nrow(faces_temp))
    )
  
  methods::show(fig)
}

plot_cells3D_with_slices(spe_cluster, plot_cell_types = c('A', 'B'), plot_colours = c('orange', 'skyblue'))

slice_positions_temp <- list(c(50, 100), c(150, 200), c(250, 300))


