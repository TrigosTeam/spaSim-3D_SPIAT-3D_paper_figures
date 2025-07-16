### 0. Example simulation ----
### 1. Cell colocalisation metrics plots ----

# First plot - cells with black lines connecting the cells
x          <- c(400, 350, 100, 300, 150)
y          <- c(100, 350, 200, 150, 350)
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
trace_lines <- add_trace(
  plot_ly(),
  type = "scatter3d",
  mode = 'lines',
  x = ~x_lines,
  y = ~y_lines,
  z = ~z_lines,
  line = list(color = "black", width = 3)
  )

# Add markers
trace_markers <- add_trace(
  trace_lines,
  type = "scatter3d",
  mode = 'markers',
  x = ~x, 
  y = ~y, 
  z = ~z,
  marker = list(size = 10, color = ifelse(cell_types == "A", "red", "blue"))
)

# Adjust
trace_markers <- trace_markers %>% layout(scene = list(xaxis = list(title = 'x', showgrid = T, showaxeslabels = F, showticklabels = T, gridwidth = 5, 
                                                                    titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                                       yaxis = list(title = 'y', showgrid = T, showaxeslabels = F, showticklabels = T, gridwidth = 5,
                                                                    titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                                       zaxis = list(title = 'z', showgrid = T, showaxeslabels = F, showticklabels = T, gridwidth = 5,
                                                                    titlefont = list(size = 20), tickfont = list(size = 15), range = c(0, 500)),
                                                       aspectmode = 'cube'))

# Plot
trace_markers


### 2. Spatial heterogeneity metrics plots ----
### 3. Cell clustering algorithms plots ----

