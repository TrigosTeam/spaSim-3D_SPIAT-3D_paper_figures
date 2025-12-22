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
    line = list(color = "black", width = 3)
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
    line = list(color = "black", width = 3)
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
    line = list(color = "black", width = 3)
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

### Mixing score and normalised mixing score ----

### Cross K-function ----

### Gradient-based metrics -----

### Grid metrics ----
