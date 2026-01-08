### Pairwise distance between cells ----
plot_pairwise_distance_diagram <- function() {
  x          <- c(400, 300, 100, 250)
  y          <- c(250, 350, 250, 100)
  z          <- c(050, 100, 400, 300)
  cell_types <- c('B', 'A', 'B', 'A')
  
  # Define pairs of points to connect
  pairs <- list(c(1, 2), c(1, 4), c(3, 2), c(3, 4))  # Connect point 1 to 2, 1 to 4, ...
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (p in pairs) {
    x_lines <- c(x_lines, x[p[1]], x[p[2]], NA)
    y_lines <- c(y_lines, y[p[1]], y[p[2]], NA)
    z_lines <- c(z_lines, z[p[1]], z[p[2]], NA)
  }
  
  # Add lines
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'lines',
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = "black", width = 5)
  )
  
  # Add markers
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", "#48bbff"))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig)
}

plot_pairwise_distance_diagram()

### Minimum distance between cells ----
plot_minimum_distance_diagram <- function() {
  x          <- c(150, 350, 100, 300, 150)
  y          <- c(400, 350, 200, 150, 350)
  z          <- c(050, 100, 500, 400, 350)
  cell_types <- c('B', 'A', 'B', 'A', 'A')
  
  # Define pairs of points to connect
  pairs <- list(c(1, 2), c(3, 4), c(3, 5))  # Connect point 1 to 2, 3 to 4, and 3 to 5
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (p in pairs) {
    x_lines <- c(x_lines, x[p[1]], x[p[2]], NA)
    y_lines <- c(y_lines, y[p[1]], y[p[2]], NA)
    z_lines <- c(z_lines, z[p[1]], z[p[2]], NA)
  }
  
  # Add lines
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'lines',
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = "black", width = 5)
  )
  
  # Add markers
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", "#48bbff"))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  return(fig)
}

plot_minimum_distance_diagram()

### CIN ----
plot_random_cells <- function() {
  
  x          <- c(200, 250, 100, 250, 100, 50, 400)
  y          <- c(300, 150, 250, 100, 400, 450, 400)
  z          <- c(200, 350, 300, 400, 150, 450, 200)
  cell_types <- c('A', 'A', 'B', 'B', 'C', 'C', 'C')
  
  
  # Add markers
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", ifelse(cell_types == "B", "#48bbff", "#9437a8")))
  )
  
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig)
}

plot_random_cells_with_spheres <- function() {
 
  x          <- c(200, 250, 100, 250, 100, 50, 400)
  y          <- c(300, 150, 250, 100, 400, 450, 400)
  z          <- c(200, 350, 300, 400, 150, 450, 200)
  cell_types <- c('A', 'A', 'B', 'B', 'C', 'C', 'C')
  
  # Add markers
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", ifelse(cell_types == "B", "#48bbff", "#9437a8")))
  )
  
  # Add spheres
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[cell_types == "A"], 
    y = ~y[cell_types == "A"], 
    z = ~z[cell_types == "A"],
    marker = list(size = 120, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig) 
}

plot_random_cells_with_spheres_and_edges <- function() {
  
  x          <- c(200, 250, 100, 250, 100, 50, 400)
  y          <- c(300, 150, 250, 100, 400, 450, 400)
  z          <- c(200, 350, 300, 400, 150, 450, 200)
  cell_types <- c('A', 'A', 'B', 'B', 'C', 'C', 'C')
  
  # Define pairs of points to connect
  pairs <- list(c(1, 3), c(1, 5), c(2, 3), c(2, 4), c(1, 7))  # Connect point 1 to 3, 1 to 4, ...
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (p in pairs) {
    x_lines <- c(x_lines, x[p[1]], x[p[2]], NA)
    y_lines <- c(y_lines, y[p[1]], y[p[2]], NA)
    z_lines <- c(z_lines, z[p[1]], z[p[2]], NA)
  }
  
  # Add lines
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'lines',
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = "black", width = 5)
  )
  
  # Add markers
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", ifelse(cell_types == "B", "#48bbff", "#9437a8")))
  )
  
  # Add spheres
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[cell_types == "A"], 
    y = ~y[cell_types == "A"], 
    z = ~z[cell_types == "A"],
    marker = list(size = 120, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig)
}


plot_random_cells()
plot_random_cells_with_spheres()
plot_random_cells_with_spheres_and_edges()

### Entropy ----
plot_high_entropy <- function() {
 
  x          <- c(250, 250, 100, 250, 100)
  y          <- c(250, 150, 250, 100, 400)
  z          <- c(250, 150, 300, 350, 150)
  cell_types <- c('A', 'A', 'A', 'B', 'B')
  
  # Define pairs of points to connect
  pairs <- list(c(1, 2), c(1, 3), c(1, 4), c(1, 5))  # Connect point 1 to 2, 1 to 3, ...
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (p in pairs) {
    x_lines <- c(x_lines, x[p[1]], x[p[2]], NA)
    y_lines <- c(y_lines, y[p[1]], y[p[2]], NA)
    z_lines <- c(z_lines, z[p[1]], z[p[2]], NA)
  }
  
  # Add lines
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'lines',
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = "black", width = 5)
  )
  
  # Add markers
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", ifelse(cell_types == "B", "#48bbff", "#9437a8")))
  )
  
  # Add spheres
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[1], 
    y = ~y[1], 
    z = ~z[1],
    marker = list(size = 120, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig)
}

plot_low_entropy <- function() {
  
  x          <- c(250, 250, 100, 250, 100)
  y          <- c(250, 150, 250, 100, 400)
  z          <- c(250, 150, 300, 350, 150)
  cell_types <- c('A', 'B', 'B', 'B', 'B')
  
  # Define pairs of points to connect
  pairs <- list(c(1, 2), c(1, 3), c(1, 4), c(1, 5))  # Connect point 1 to 2, 1 to 3, ...
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (p in pairs) {
    x_lines <- c(x_lines, x[p[1]], x[p[2]], NA)
    y_lines <- c(y_lines, y[p[1]], y[p[2]], NA)
    z_lines <- c(z_lines, z[p[1]], z[p[2]], NA)
  }
  
  # Add lines
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'lines',
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = "black", width = 5)
  )
  
  # Add markers
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", ifelse(cell_types == "B", "#48bbff", "#9437a8")))
  )
  
  # Add spheres
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[1], 
    y = ~y[1], 
    z = ~z[1],
    marker = list(size = 120, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig)
}


plot_high_entropy()
plot_low_entropy()



### Mixing score and normalised mixing score ----
plot_mixing_score_diagram <- function() {
  
  x          <- c(200, 250, 100, 250, 100, 300)
  y          <- c(300, 150, 250, 100, 400, 250)
  z          <- c(200, 350, 300, 400, 150, 250)
  cell_types <- c('A', 'A', 'B', 'B', 'B', 'B')
  
  # Define pairs of points to connect
  pairs <- list(c(1, 3), c(1, 5), c(2, 3), c(2, 4), c(1, 6), c(2, 6), c(1, 2))  # Connect point 1 to 3, 1 to 4, ...
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (p in pairs) {
    x_lines <- c(x_lines, x[p[1]], x[p[2]], NA)
    y_lines <- c(y_lines, y[p[1]], y[p[2]], NA)
    z_lines <- c(z_lines, z[p[1]], z[p[2]], NA)
  }
  
  # Add lines
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'lines',
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = "black", width = 5)
  )
  
  # Add markers
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", ifelse(cell_types == "B", "#48bbff", "#9437a8")))
  )
  
  # Add spheres
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[cell_types == "A"], 
    y = ~y[cell_types == "A"], 
    z = ~z[cell_types == "A"],
    marker = list(size = 120, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig)
}

plot_mixing_score_diagram()
### Cross K-function ----
plot_high_cross_K <- function() {
  
  x          <- c(250, 250, 100, 250, 100)
  y          <- c(250, 150, 250, 100, 400)
  z          <- c(250, 150, 300, 350, 150)
  cell_types <- c('A', 'B', 'B', 'B', 'B')
  
  # Define pairs of points to connect
  pairs <- list(c(1, 2), c(1, 3), c(1, 4), c(1, 5))  # Connect point 1 to 2, 1 to 3, ...
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (p in pairs) {
    x_lines <- c(x_lines, x[p[1]], x[p[2]], NA)
    y_lines <- c(y_lines, y[p[1]], y[p[2]], NA)
    z_lines <- c(z_lines, z[p[1]], z[p[2]], NA)
  }
  
  # Add lines
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'lines',
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = "black", width = 5)
  )
  
  # Add markers
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", ifelse(cell_types == "B", "#48bbff", "#9437a8")))
  )
  
  # Add spheres
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[1], 
    y = ~y[1], 
    z = ~z[1],
    marker = list(size = 120, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig)
}

plot_low_cross_K <- function() {
  
  x          <- c(250, 250, 100, 350, 450)
  y          <- c(250, 150, 450, 100, 400)
  z          <- c(250, 150, 400, 450, 50)
  cell_types <- c('A', 'B', 'B', 'B', 'B')
  
  # Define pairs of points to connect
  pairs <- list(c(1, 2))  # Connect point 1 to 2, 1 to 3, ...
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (p in pairs) {
    x_lines <- c(x_lines, x[p[1]], x[p[2]], NA)
    y_lines <- c(y_lines, y[p[1]], y[p[2]], NA)
    z_lines <- c(z_lines, z[p[1]], z[p[2]], NA)
  }
  
  # Add lines
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'lines',
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = "black", width = 5)
  )
  
  # Add markers
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", ifelse(cell_types == "B", "#48bbff", "#9437a8")))
  )
  
  # Add spheres
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[1], 
    y = ~y[1], 
    z = ~z[1],
    marker = list(size = 120, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig)
}

plot_high_cross_K()
plot_low_cross_K()

### Gradient-based metrics -----
plot_cells_with_multiple_spheres <- function() {
  
  x          <- c(400, 250, 100, 250, 100, 050, 400, 300, 200)
  y          <- c(300, 150, 350, 100, 400, 450, 400, 250, 300)
  z          <- c(200, 350, 400, 400, 150, 450, 200, 300, 250)
  cell_types <- c('A', 'A', 'A', 'B', 'B', 'B', 'B', 'B', 'B')
  
  # Add markers
  fig <- add_trace(
    plot_ly(),
    type = "scatter3d",
    mode = 'markers',
    x = ~x, 
    y = ~y, 
    z = ~z,
    marker = list(size = 10, color = ifelse(cell_types == "A", "#bb0036", ifelse(cell_types == "B", "#48bbff", "#9437a8")))
  )
  
  # Add spheres (small)
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[cell_types == "A"], 
    y = ~y[cell_types == "A"], 
    z = ~z[cell_types == "A"],
    marker = list(size = 40, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Add spheres (medium)
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[cell_types == "A"], 
    y = ~y[cell_types == "A"], 
    z = ~z[cell_types == "A"],
    marker = list(size = 80, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Add spheres (large)
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = 'markers',
    x = ~x[cell_types == "A"], 
    y = ~y[cell_types == "A"], 
    z = ~z[cell_types == "A"],
    marker = list(size = 120, 
                  color = 'darkgray', 
                  opacity = 0.2,
                  line = list(
                    color = "black",  # border color
                    width = 10         # border thickness
                  ))
  )
  
  # Adjust
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "cube"))
  
  
  # Plot
  return(fig) 
}

plot_cells_with_multiple_spheres()


# Get example simulation
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
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = T, showticklabels = T, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = 'y', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = 'z', showgrid = T, showaxeslabels = T, showticklabels = T,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = "data"))
  
  return(fig)
}

bg_metadata <- spe_metadata_background_template("random")
bg_metadata$background$n_cells <- 5000
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 10
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1

cluster_metadata <- spe_metadata_cluster_template("regular", "sphere", bg_metadata)
cluster_metadata$cluster_1$cluster_cell_types <- c('Tumour', 'Immune')
cluster_metadata$cluster_1$cluster_cell_proportions <- c(0.75, 0.25)
cluster_metadata$cluster_1$radius <- 125
cluster_metadata$cluster_1$centre_loc <- c(350, 150, 150)

cluster_metadata <- spe_metadata_cluster_template("regular", "sphere", cluster_metadata)
cluster_metadata$cluster_2$cluster_cell_types <- c('Tumour', 'Immune')
cluster_metadata$cluster_2$cluster_cell_proportions <- c(0.25, 0.75)
cluster_metadata$cluster_2$radius <- 125
cluster_metadata$cluster_2$centre_loc <- c(150, 350, 350)

spe <- simulate_spe_metadata3D(cluster_metadata, 
                               plot_image = T, 
                               plot_cell_types = c('O', 'Tumour', 'Immune'), 
                               plot_colours = c('lightgray', '#f77e3b', '#48bbff'))


# Get cross K-function gradient plot
cross_K_data <- calculate_cross_K_gradient3D(spe, 'Tumour', 'Immune', seq(10, 250, 10))

plot_cross_K_gradient3D(cross_K_data)

### Grid metrics ----
# 3D simulation
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
bg_metadata$background$n_cells <- 10000
bg_metadata$background$length <- 500
bg_metadata$background$width <- 500
bg_metadata$background$height <- 500
bg_metadata$background$minimum_distance_between_cells <- 10
bg_metadata$background$cell_types <- 'O'
bg_metadata$background$cell_proportions <- 1

cluster_metadata <- spe_metadata_cluster_template("regular", "ellipsoid", bg_metadata)
cluster_metadata$cluster_1$cluster_cell_types <- c('A', 'B')
cluster_metadata$cluster_1$cluster_cell_proportions <- c(0.2, 0.8)
cluster_metadata$cluster_1$radii <- c(140, 140, 225)
cluster_metadata$cluster_1$axes_rotation <- c(-45, 45, 0)
cluster_metadata$cluster_1$centre_loc <- c(250, 250, 250)

cluster_metadata <- spe_metadata_cluster_template("regular", "sphere", cluster_metadata)
cluster_metadata$cluster_2$cluster_cell_types <- c('A', 'B')
cluster_metadata$cluster_2$cluster_cell_proportions <- c(0.8, 0.2)
cluster_metadata$cluster_2$radius <- 150
cluster_metadata$cluster_2$centre_loc <- c(300, 200, 300)

spe_cluster <- simulate_spe_metadata3D(cluster_metadata)
plot_cells3D(spe_cluster, plot_cell_types = c('A', 'B', 'O'), plot_colours = c('#f77e3b', '#48bbff', 'lightgray'))

# 3D grid
plot_cells_with_grid3D <- function(spe,
                                   plot_cell_types = NULL,
                                   plot_colours = NULL,
                                   grid_color,
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
  fig <- add_trace(plot_ly(),
                   data = df,
                   type = "scatter3d",
                   mode = "markers",
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
  
  
  ### ADD GRID LINES
  
  x <- c(rep(c(rep(c(0, 500), 6), rep(seq(0, 500, 100), each = 2)), 6), rep(c(rep(seq(0, 500, 100), each = 2), rep(c(0, 500), 6)), 6), rep(seq(0, 500, 100), each = 24))
  y <- c(rep(c(rep(seq(0, 500, 100), each = 2), rep(c(0, 500), 6)), 6), rep(seq(0, 500, 100), each = 24), rep(c(rep(c(0, 500), 6), rep(seq(0, 500, 100), each = 2)), 6))
  z <- c(rep(seq(0, 500, 100), each = 24), rep(c(rep(c(0, 500), 6), rep(seq(0, 500, 100), each = 2)), 6), rep(c(rep(seq(0, 500, 100), each = 2), rep(c(0, 500), 6)), 6))
  
  # Create coordinate vectors with NA between segments
  x_lines <- y_lines <- z_lines <- c()
  for (i in seq(1, length(x), 2)) {
    x_lines <- c(x_lines, x[i], x[i + 1], NA)
    y_lines <- c(y_lines, y[i], y[i + 1], NA)
    z_lines <- c(z_lines, z[i], z[i + 1], NA)
  }
  
  fig <- add_trace(
    fig,
    type = "scatter3d",
    mode = "lines",
    x = ~x_lines,
    y = ~y_lines,
    z = ~z_lines,
    line = list(color = grid_color, width = 3)
  )
  
  return(fig)
}

plot_cells_with_grid3D(spe_cluster, plot_cell_types = c('A', 'B', 'O'), plot_colours = c('#f77e3b', '#48bbff', 'lightgray'), grid_color = "#007128")

# 3D grid metric applied
# Alter plot_grid_metrics_continuous3D function
plot_grid_metrics_continuous3D <- function(grid_metrics, metric_colname) {
  
  ## Check input parameters
  if (!(is.character(metric_colname) && metric_colname %in% c("proportion", "entropy"))) {
    stop("`metric_colname` is not 'proportion' or 'entropy'.")
  }
  if (is.null(grid_metrics[[metric_colname]])) {
    stop("`metric_colname` is not a column in `grid_metrics`.")
  }
  
  ## Color of each dot is related to its entropy
  pal <- colorRampPalette(terrain.colors(10, rev = TRUE)[2:10])
  
  ## Add size column and for NA entropy values, make the size small
  grid_metrics$size <- ifelse(is.na(grid_metrics[[metric_colname]]), 0, 10)
  
  fig <- plot_ly(grid_metrics,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~x_coord,
                 y = ~y_coord,
                 z = ~z_coord,
                 color = as.formula(paste0('~', metric_colname)),
                 colors = pal(nrow(grid_metrics)),
                 marker = list(size = ~size),
                 symbol = 1,
                 symbols = "square")
  
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                     aspectmode = "cube"))
  
  return(fig)
}

calculate_cell_proportion_grid_metrics3D(spe_cluster, 10, 'A', 'B')

# 2D simulation
df <- data.frame(
  x = runif(2000, 0, 100),
  y = runif(2000, 0, 100),
  cell.type = 'O'
)

# ellipse
inside <- ((df$x - 50) / 25)^2 + ((df$y - 50) / 45)^2 <= 1
df$cell.type[inside] <- sample(
  c("A", "B"),
  size = sum(inside),
  replace = TRUE,
  prob = c(0.3, 0.7)
)

# circle
inside <- ((df$x - 60) / 30)^2 + ((df$y - 65) / 30)^2 <= 1
df$cell.type[inside] <- sample(
  c("A", "B"),
  size = sum(inside),
  replace = TRUE,
  prob = c(0.6, 0.4)
)

ggplot(df, aes(x, y, color = cell.type)) +
  geom_point() +
  scale_discrete_manual("color", values = c('A' = '#f77e3b', 'B' = '#48bbff', 'O' = 'lightgray')) +
  theme_void() + 
  theme(legend.position = "none")


# 2D grid
ggplot(df, aes(x, y, color = cell.type)) +
  geom_point() +
  scale_discrete_manual("color", values = c('A' = '#f77e3b', 'B' = '#48bbff', 'O' = 'lightgray')) +
  theme_void() + 
  theme(legend.position = "none") +
  geom_vline(xintercept = seq(floor(min(df$x)), ceiling(max(df$x)), by = 10), color = "#007128", linewidth = 1) + 
  geom_hline(yintercept = seq(floor(min(df$y)), ceiling(max(df$y)), by = 10), color = "#007128", linewidth = 1)

# 2D grid metric applied
get_spe_grid_metrics2D <- function(spe, 
                                   n_splits, 
                                   feature_colname = "Cell.Type") {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  # Check if there are empty strings or string of only spaces in 'cell_types_of_interest'
  if (length(spe[[feature_colname]][trimws(spe[[feature_colname]]) == ""]) > 0) {
    stop("spe cannot contain cell types that are an empty string or a string of only spaces.")
  }
  if (!(is.integer(n_splits) && length(n_splits) == 1 || (is.numeric(n_splits) && length(n_splits) == 1 && n_splits > 0 && n_splits%%1 == 0))) {
    stop("`n_splits` is not a positive integer.")
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste("No column called", feature_colname, "found in spe object."))
  }
  
  spe_coords <- spatialCoords(spe)
  
  ## Get dimensions of the window
  min_x <- min(spe_coords[ , "Cell.X.Position"])
  min_y <- min(spe_coords[ , "Cell.Y.Position"])
  
  max_x <- max(spe_coords[ , "Cell.X.Position"])
  max_y <- max(spe_coords[ , "Cell.Y.Position"])
  
  length <- round(max_x - min_x)
  width  <- round(max_y - min_y)
  
  ## Get distance of row, col and lay
  d_row <- length / n_splits
  d_col <- width / n_splits
  
  # Shift spe_coords so they begin at the origin
  spe_coords[, "Cell.X.Position"] <- spe_coords[, "Cell.X.Position"] - min_x
  spe_coords[, "Cell.Y.Position"] <- spe_coords[, "Cell.Y.Position"] - min_y
  
  ## Figure out which 'grid prism number' each cell is inside
  spe$grid_prism_num <- floor(spe_coords[ , "Cell.X.Position"] / d_row) +
    floor(spe_coords[ , "Cell.Y.Position"] / d_col) * n_splits
  
  ## Determine the cell types found in each grid prism
  n_grid_prisms <- n_splits^2
  grid_prism_cell_matrix <- as.data.frame.matrix(table(spe[[feature_colname]], factor(spe$grid_prism_num, levels = seq(n_grid_prisms))))
  grid_prism_cell_matrix <- data.frame(grid_prism_num = seq(n_grid_prisms),
                                       t(grid_prism_cell_matrix), check.names = FALSE)
  
  ## Determine centre coordinates of each grid prism
  grid_prism_coordinates <- data.frame(grid_prism_num = seq(n_grid_prisms),
                                       x_coord = ((seq(n_grid_prisms) - 1) %% n_splits + 0.5) * d_row + round(min_x),
                                       y_coord = (floor(((seq(n_grid_prisms) - 1) %% (n_splits)^2) / n_splits) + 0.5) * d_col + round(min_y))
  
  spe@metadata[["grid_metrics"]] <- list("grid_prism_cell_matrix" = grid_prism_cell_matrix,
                                         "grid_prism_coordinates" = grid_prism_coordinates)
  
  return(spe)
}


calculate_cell_proportion_grid_metrics2D <- function(spe, 
                                                     n_splits,
                                                     reference_cell_types,
                                                     target_cell_types,
                                                     feature_colname = "Cell.Type",
                                                     plot_image = TRUE) {
  
  # Check input parameters
  if (class(spe) != "SpatialExperiment") {
    stop("`spe` is not a SpatialExperiment object.")
  }
  # Check if there are empty strings or string of only spaces in 'cell_types_of_interest'
  if (length(spe[[feature_colname]][trimws(spe[[feature_colname]]) == ""]) > 0) {
    stop("spe cannot contain cell types that are an empty string or a string of only spaces.")
  }
  if (!(is.integer(n_splits) && length(n_splits) == 1 || (is.numeric(n_splits) && length(n_splits) == 1 && n_splits > 0 && n_splits%%1 == 0))) {
    stop("`n_splits` is not a positive integer.")
  }
  ## Check reference_cell_types are found in the spe object
  unknown_cell_types <- setdiff(reference_cell_types, spe[[feature_colname]])
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following cell types in reference_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    return(NULL)
  }
  ## Check target_cell_types are found in the spe object
  unknown_cell_types <- setdiff(target_cell_types, spe[[feature_colname]])
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following cell types in target_cell_types are not found in the spe object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    return(NULL)
  }
  # Check if there is intersection between reference_cell_types and target_cell_types
  if (length(intersect(reference_cell_types, target_cell_types)) > 0) {
    stop("Cannot have same cells in both reference_cell_types and target_cell_types")
  }
  if (!is.character(feature_colname)) {
    stop("`feature_colname` is not a character.")
  }
  if (is.null(spe[[feature_colname]])) {
    stop(paste("No column called", feature_colname, "found in spe object."))
  }
  if (!is.logical(plot_image)) {
    stop("`plot_image` is not a logical (TRUE or FALSE).")
  }
  
  # Add grid metrics to spe
  spe <- get_spe_grid_metrics2D(spe, n_splits, feature_colname)
  
  # Get grid_prism_cell_matrix from spe
  grid_prism_cell_matrix <- spe@metadata$grid_metrics$grid_prism_cell_matrix
  
  ## Define data frame which contains all results
  n_grid_prisms <- n_splits^2
  result <- data.frame(row.names = seq(n_grid_prisms))
  
  # Fill in the result data frame
  if (length(reference_cell_types) == 1) {
    result$reference <- grid_prism_cell_matrix[[reference_cell_types]]
  }
  else {
    result$reference <- rowSums(grid_prism_cell_matrix[ , reference_cell_types])
  }
  if (length(target_cell_types) == 1) {
    result$target <- grid_prism_cell_matrix[[target_cell_types]]
  }
  else {
    result$target <- rowSums(grid_prism_cell_matrix[ , target_cell_types])
  }
  result$total <- result$reference + result$target
  result$proportion <- result$target / result$total
  
  # Add grid_prism_coordinates info to result
  result <- cbind(result, spe@metadata$grid_metrics$grid_prism_coordinates)
  
  ## Plot
  if (plot_image) {
    fig <- plot_grid_metrics_continuous2D(result, "proportion")
    methods::show(fig)
  }
  
  return(result)
}

plot_grid_metrics_continuous2D <- function(grid_metrics, metric_colname) {
  
  ## Check input parameters
  if (!(is.character(metric_colname) && metric_colname %in% c("proportion", "entropy"))) {
    stop("`metric_colname` is not 'proportion' or 'entropy'.")
  }
  if (is.null(grid_metrics[[metric_colname]])) {
    stop("`metric_colname` is not a column in `grid_metrics`.")
  }
  
  ## Color of each dot is related to its entropy
  pal <- colorRampPalette(terrain.colors(10, rev = TRUE)[2:10])
  
  fig <- ggplot(grid_metrics, aes(x_coord, y_coord, fill = proportion)) + 
    geom_tile() + 
    scale_fill_gradientn(colors = pal(100), na.value = "white") + 
    coord_fixed() + 
    theme_void()
  
  return(fig)
}


colnames(df) <- c("Cell.X.Position", "Cell.Y.Position", "Cell.Type")
spe2D <- SpatialExperiment(
  assay = matrix(data = NA, nrow = nrow(df), ncol = nrow(df)),
  colData = df,
  spatialCoordsNames = c("Cell.X.Position", "Cell.Y.Position"))

grid_metrics2D <- calculate_cell_proportion_grid_metrics2D(spe2D, 10, 'A', 'B', "Cell.Type", T)
