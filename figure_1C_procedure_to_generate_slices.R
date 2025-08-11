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

cluster_metadata$background$n_cells <- 50000
cluster_metadata$background$length <- 500
cluster_metadata$background$width <- 500
cluster_metadata$background$height <- 500
cluster_metadata$background$minimum_distance_between_cells <- 10
cluster_metadata$background$cell_types <- c('A', 'B', 'O', 'fakeO')
cluster_metadata$background$cell_proportions <- c(0.02, 0, 0.08, 0.9)

cluster_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
cluster_metadata$cluster_1$cluster_cell_proportions <- c(0.5, 0.5)
cluster_metadata$cluster_1$x_radius <- 150
cluster_metadata$cluster_1$x_radius <- 150
cluster_metadata$cluster_1$x_radius <- 200
cluster_metadata$cluster_1$x_y_rotation <- -30
cluster_metadata$cluster_1$x_z_rotation <- -30
cluster_metadata$cluster_1$y_z_rotation <- 0
cluster_metadata$cluster_1$centre_loc <- c(250, 250, 250)

spe_cluster <- simulate_spe_metadata3D(cluster_metadata)
plot_cells3D(spe_cluster, plot_cell_types = c('A', 'B', 'O'), plot_colours = c('#f77e3b', '#48bbff', 'lightgray'))

### 2. Add slices to simulation ------------------------
plot_cells3D_with_slices <- function(spe,
                                     plot_cell_types = NULL,
                                     plot_colours = NULL,
                                     feature_colname = "Cell.Type",
                                     slice_positions,
                                     slice_colors) {
  
  spatialCoords(spe) <- spatialCoords(spe) + 1000
  
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
  
  index <- 1
  for (slice_position in slice_positions) {
    
    vertices <- data.frame(x = c(0, 0, 500, 500, 0, 0, 500, 500),
                           y = c(0, 500, 0, 500, 0, 500, 0, 500),
                           z = rep(slice_position, each = 4))
    
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

slice_positions_temp <- list(c(335, 345), c(290, 300), c(245, 255))

plot_cells3D_with_slices(spe_cluster, 
                         plot_cell_types = c('A', 'B', 'O'), 
                         plot_colours = c('#f77e3b', '#48bbff', 'lightgray'),
                         slice_positions = slice_positions_temp,
                         slice_colors = c("#b8db50",
                                          "#73ec81",
                                          "#9437a8"))






### 3. Extract slices -----
# Function to get slices from spe
get_spes_for_slices <- function(spe, slice_positions) {
  
  spes_for_slices <- list()
  
  number_of_slices <- length(slice_positions)
  
  for (i in seq(number_of_slices)) {
    bottom_z_coord_of_slice <- slice_positions[[i]][1]
    top_z_coord_of_slice <- slice_positions[[i]][2]
    z_coords_of_cells_in_spe <- spatialCoords(spe)[ , "Cell.Z.Position"]
    spe_for_slice <- spe[, bottom_z_coord_of_slice < z_coords_of_cells_in_spe & z_coords_of_cells_in_spe < top_z_coord_of_slice]
    spatialCoords(spe_for_slice) <- spatialCoords(spe_for_slice)[ , c("Cell.X.Position", "Cell.Y.Position")]
    
    spes_for_slices[[i]] <- spe_for_slice
  }
  return(spes_for_slices)
}

plot_cells2D <- function(spe_slices,
                         plot_cell_types = NULL,
                         plot_colours = NULL,
                         feature_colname = "Cell.Type") {
  
  for (spe_slice in spe_slices) {
    
    df_slice <- data.frame(spatialCoords(spe_slice), "Cell.Type" = spe_slice[[feature_colname]])
    df_slice$Cell.Type[df_slice$Cell.Type == "fakeO"] <- "O"
    
    df_slice$Cell.Type <- factor(df_slice$Cell.Type, c('A', 'B', 'O'))
    
    fig <- ggplot(df_slice, aes(x = Cell.X.Position, y = Cell.Y.Position, color = Cell.Type)) +
      geom_point(size = 2.5) +
      scale_color_manual(values = c("A" = "#f77e3b", "B" = "#48bbff", "O" = 'gray')) +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),     # Remove x-axis title
        axis.title.y = element_blank(),     # Remove y-axis title
        axis.text.x = element_blank(),      # Remove x-axis tick labels
        axis.text.y = element_blank(),      # Remove y-axis tick labels
        axis.ticks.x = element_blank(),     # Remove x-axis ticks
        axis.ticks.y = element_blank(),     # Remove y-axis ticks
        legend.position = "none",           # Remove legend
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()   # Remove minor grid lines
      )
    
    methods::show(fig)
  }
}


slice_positions_temp <- list(c(335, 345), c(290, 300), c(245, 255))

spe_slices_temp <- get_spes_for_slices(spe_cluster, slice_positions_temp)

plot_cells2D(spe_slices_temp,
             plot_cell_types = NULL,
             plot_colours = NULL,
             feature_colname = "Cell.Type")


### 4. Compare 3D and 2D analysis ----

df_slice1 <- data.frame(x = runif(200))
df_slice1$y <- (df_slice1$x + rnorm(200, 0, 0.1))^(1/3) + 0.2
df_slice1$slice <- '1'

df_slice2 <- data.frame(x = runif(200))
df_slice2$y <- (df_slice2$x + rnorm(200, 0, 0.1))^(1/2)
df_slice2$slice <- '2'

df_slice3 <- data.frame(x = runif(200))
df_slice3$y <- df_slice3$x + rnorm(200, 0, 0.1)
df_slice3$slice <- '3'

df_combined <- rbind(df_slice1, df_slice2, df_slice3)

ggplot(df_combined, aes(x = x, y = y, color = slice)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#bb0036", linewidth = 1.5) +
  theme_bw() +
  labs(x = "3D metric value", y = "2D metric value") +
  theme(
    axis.text.x = element_blank(),      # Remove x-axis tick labels
    axis.text.y = element_blank(),      # Remove y-axis tick labels
    axis.ticks.x = element_blank(),     # Remove x-axis ticks
    axis.ticks.y = element_blank(),     # Remove y-axis ticks
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  ) +
  scale_color_manual(values = c("1" = "#b8db50", "2" = "#73ec81", "3" = "#9437a8"))
  

