# Code for supplementary figure 2 to plot public datasets that correspond to analysis.

### 2. 3D public data plotting ----
library(SpatialExperiment)
library(plotly)

plot_cells3D_df <- function(df,
                            plot_cell_types = NULL,
                            plot_colours = NULL,
                            feature_colname = "Cell.Type",
                            aspectmode = "manual") {
  
  # Check input parameters
  if (class(df) != "data.frame") {
    stop("`df` is not a data.frame object.")
  }
  if (!is.null(plot_cell_types)) {
    if(!is.character(plot_cell_types)) {
      stop("`plot_cell_types` is not a character vector.")
    }
  } 
  if (!is.null(plot_colours)) {
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
  if (is.null(df[[feature_colname]])) {
    stop(paste(feature_colname, "is not a valid column in your df object."))
  }
  
  ## If no cell types chosen, use all cell types found in data frame
  if (is.null(plot_cell_types)) {
    warning("plot_cell_types not specified, all cell types found in the df object will be used.")
    plot_cell_types <- unique(df[[feature_colname]])
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
  
  ## If cell types have been chosen, check they are found in the df object
  df_cell_types <- unique(df[[feature_colname]])
  unknown_cell_types <- setdiff(plot_cell_types, df_cell_types)
  
  if (length(unknown_cell_types) == length(plot_cell_types)) {
    stop("None of the plot_cell_types are found in the df object")
  }
  
  if (length(unknown_cell_types) != 0) {
    warning(paste("The following plot_cell_types are not found in the df object:\n   ",
                  paste(unknown_cell_types, collapse = ", ")))
    plot_colours <- plot_colours[which(plot_cell_types %in% df_cell_types)]
    plot_cell_types <- intersect(plot_cell_types, df_cell_types)
  }
  
  ## Factor for feature column
  df[, feature_colname] <- factor(df[, feature_colname],
                                  levels = plot_cell_types)
  
  ## Plot
  fig <- plot_ly(df,
                 type = "scatter3d",
                 mode = 'markers',
                 x = ~Cell.X.Position,
                 y = ~Cell.Y.Position,
                 z = ~Cell.Z.Position,
                 color = df[[feature_colname]],
                 colors = plot_colours,
                 marker = list(size = 2))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, gridwidth = 0, 
                                                  titlefont = list(size = 20), tickfont = list(size = 15),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, gridwidth = 0,
                                                  titlefont = list(size = 20), tickfont = list(size = 15),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, gridwidth = 0,
                                                  titlefont = list(size = 20), tickfont = list(size = 15),
                                                  color = 'black', linewidth = 4, gridwidth = 4),
                                     aspectmode = aspectmode)) # Alternative is 'data' or "manual' or 'cube'
  
  
  return(fig)
}


# OpenST human metastatic lymph node dataset
setwd("/home/dle/R/data3D/openST_human_metastatic_lymph_node")

human_metastatic_lymph_node_df <- read.csv("human_metastatic_lymph_node_df.csv")

# Convert x, y and z-axis to match real data
human_metastatic_lymph_node_df$Cell.Z.Position <- human_metastatic_lymph_node_df$Cell.Z.Position / max(human_metastatic_lymph_node_df$Cell.Z.Position) * 360 / 1000 + 1000
human_metastatic_lymph_node_df$Cell.X.Position <- (human_metastatic_lymph_node_df$Cell.X.Position - min(human_metastatic_lymph_node_df$Cell.X.Position)) / max(human_metastatic_lymph_node_df$Cell.X.Position) * 4 + 1000
human_metastatic_lymph_node_df$Cell.Y.Position <- (human_metastatic_lymph_node_df$Cell.Y.Position - min(human_metastatic_lymph_node_df$Cell.Y.Position)) / max(human_metastatic_lymph_node_df$Cell.Y.Position) * 3 + 1000

# Remove unknown cell type
human_metastatic_lymph_node_df <- human_metastatic_lymph_node_df[human_metastatic_lymph_node_df$Cell.Type != "unknown", ]

# Move to avoid axis plotting issues
human_metastatic_lymph_node_df$Cell.X.Position <- human_metastatic_lymph_node_df$Cell.X.Position + 10000
human_metastatic_lymph_node_df$Cell.Y.Position <- human_metastatic_lymph_node_df$Cell.Y.Position + 10000
human_metastatic_lymph_node_df$Cell.Z.Position <- human_metastatic_lymph_node_df$Cell.Z.Position + 10000

plot_colours <- c(
  "#33135b",
  "#73ec81",
  "#9437a8",
  "#b8db50",
  "#0062c5",
  "#f77e3b",
  "#48bbff",
  "#bb0036",
  "#4deeac",
  "#770026",
  "#007128",
  "#d99dff",
  "#004a07",
  "#ff8eb3",
  "#01478c",
  "#702c00"
)

plot_cells3D_df(human_metastatic_lymph_node_df, 
                aspectmode = "data", 
                plot_colours = plot_colours)




# CyCIF colorectal cancer dataset
setwd("/home/dle/R/data3D/CyCIF_colorectal_cancer")
colorectal_cancer_df <- read.csv("colorectal_cancer_df.csv")
colorectal_cancer_df$X <- NULL
colnames(colorectal_cancer_df)[1:3] <- c("Cell.X.Position", "Cell.Y.Position", "Cell.Z.Position")


# Subset 1 million random rows
colorectal_cancer_subset_df <- colorectal_cancer_df[sample(nrow(colorectal_cancer_df), 1000000, replace = FALSE), ]

# Move to avoid axis plotting issues
colorectal_cancer_subset_df$Cell.X.Position <- colorectal_cancer_subset_df$Cell.X.Position + 10000
colorectal_cancer_subset_df$Cell.Y.Position <- colorectal_cancer_subset_df$Cell.Y.Position + 10000
colorectal_cancer_subset_df$Cell.Z.Position <- colorectal_cancer_subset_df$Cell.Z.Position + 10000


# Subset 16 cell types from dataset
plot_cell_types <- unique(colorectal_cancer_df$Cell.Type)[1:16]
plot_colours <- c(
  "#33135b",
  "#73ec81",
  "#9437a8",
  "#b8db50",
  "#0062c5",
  "#f77e3b",
  "#48bbff",
  "#bb0036",
  "#4deeac",
  "#770026",
  "#007128",
  "#d99dff",
  "#004a07",
  "#ff8eb3",
  "#01478c",
  "#702c00"
)

plot_cells3D_df(colorectal_cancer_subset_df, 
                aspectmode = "data", 
                plot_colours = plot_colours,
                plot_cell_types = plot_cell_types)






# Merfish mouse brain cortex dataset
mouse_cortex_df <- read.csv("~/R/data3D/merfish_mouse_brain/mouse_cortex_100um_df.csv")
colnames(mouse_cortex_df) <- c("Cell.X.Position", "Cell.Y.Position", "Cell.Z.Position", "Cell.Type")


# Move to avoid axis plotting issues
mouse_cortex_df$Cell.X.Position <- mouse_cortex_df$Cell.X.Position + 10000
mouse_cortex_df$Cell.Y.Position <- mouse_cortex_df$Cell.Y.Position + 10000
mouse_cortex_df$Cell.Z.Position <- mouse_cortex_df$Cell.Z.Position + 10000


# Get cell types and colors for plotting
plot_cell_types <- unique(mouse_cortex_df$Cell.Type)
plot_colours <- c(
  "#33135b",
  "#73ec81",
  "#9437a8",
  "#b8db50",
  "#0062c5",
  "#f77e3b",
  "#48bbff",
  "#bb0036",
  "#4deeac",
  "#770026",
  "#007128"
)

plot_cells3D_df(mouse_cortex_df, 
                aspectmode = "data", 
                plot_colours = plot_colours,
                plot_cell_types = plot_cell_types)




# Merfish mouse hypothalamus dataset
mouse_hypothalamus_df <- read.csv("~/R/data3D/merfish_mouse_brain/mouse_hypothalamus_200um_df.csv")
colnames(mouse_hypothalamus_df) <- c("Cell.X.Position", "Cell.Y.Position", "Cell.Z.Position", "Cell.Type")


# Move to avoid axis plotting issues
mouse_hypothalamus_df$Cell.X.Position <- mouse_hypothalamus_df$Cell.X.Position + 10000
mouse_hypothalamus_df$Cell.Y.Position <- mouse_hypothalamus_df$Cell.Y.Position + 10000
mouse_hypothalamus_df$Cell.Z.Position <- mouse_hypothalamus_df$Cell.Z.Position + 10000


# Get cell types and colors for plotting
plot_cell_types <- unique(mouse_hypothalamus_df$Cell.Type)
plot_colours <- c(
  "#f77e3b",
  "#48bbff",
  "#bb0036",
  "#4deeac",
  "#770026",
  "#007128",
  "#9437a8",
  "#b8db50",
  "#0062c5"
)

plot_cells3D_df(mouse_hypothalamus_df, 
                aspectmode = "data", 
                plot_colours = plot_colours,
                plot_cell_types = plot_cell_types)

