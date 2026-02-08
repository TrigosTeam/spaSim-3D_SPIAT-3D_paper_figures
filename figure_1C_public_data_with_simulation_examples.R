# Code for figure 1C to plot 3D public datasets, along with simulated versions of these datasets using spaSim-3D.

### Functions -----
library(plotly)
library(RColorBrewer)
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
                 color = ~Cell.Type,
                 colors = plot_colours,
                 marker = list(size = 3))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  showline = TRUE, linecolor = 'black', linewidth = 2),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  showline = TRUE, linecolor = 'black', linewidth = 2),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  showline = TRUE, linecolor = 'black', linewidth = 2),
                                     aspectmode = aspectmode))
  
  
  return(fig)
}

convert_df_to_spe <- function(df) {
  spe <- SpatialExperiment(
    assay = matrix(data = NA, nrow = nrow(df), ncol = nrow(df)),
    colData = df,
    spatialCoordsNames = c("Cell.X.Position", "Cell.Y.Position", "Cell.Z.Position"))
  
  return(spe)
}


# Alter plot_cells3D function
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
                 marker = list(size = 3))
  
  fig <- fig %>% layout(scene = list(xaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F, 
                                                  showline = TRUE, linecolor = 'black', linewidth = 2),
                                     yaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  showline = TRUE, linecolor = 'black', linewidth = 2),
                                     zaxis = list(title = '', showgrid = F, showaxeslabels = F, showticklabels = F,
                                                  showline = TRUE, linecolor = 'black', linewidth = 2),
                                     aspectmode = "manual"))
  
  return(fig)
}


### 1. Mouse hypothalamus ----

# Original
setwd("/home/dle/R/data3D/merfish_squidpy")
merfish_squidpy_df <- read.csv("merfish_squidpy_df.csv")
merfish_squidpy_cell_type_color_mapping <- read.csv("merfish_squidpy_cell_type_color_mapping.csv")
merfish_squidpy_cell_type_color_mapping$Color <- c(
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
merfish_squidpy_df$Cell.X.Position <- merfish_squidpy_df$Cell.X.Position * 1.8 + 1000
merfish_squidpy_df$Cell.Y.Position <- merfish_squidpy_df$Cell.Y.Position * 1.8 + 1000
merfish_squidpy_df$Cell.Z.Position <- ((as.integer(merfish_squidpy_df$Cell.Z.Position) + 29) / 100) + 1000
plot_cells3D_df(merfish_squidpy_df,
                merfish_squidpy_cell_type_color_mapping$Cell_Type,
                merfish_squidpy_cell_type_color_mapping$Color,
                aspectmode = "data")


# using spaSim-3D
mouse_hypothalamus_md <- spe_metadata_background_template("random")
mouse_hypothalamus_md <- spe_metadata_cluster_template("regular", "cylinder", mouse_hypothalamus_md)
mouse_hypothalamus_md <- spe_metadata_cluster_template("regular", "cylinder", mouse_hypothalamus_md)
mouse_hypothalamus_md <- spe_metadata_cluster_template("regular", "cylinder", mouse_hypothalamus_md)
mouse_hypothalamus_md <- spe_metadata_cluster_template("regular", "cylinder", mouse_hypothalamus_md)
mouse_hypothalamus_md <- spe_metadata_cluster_template("regular", "cylinder", mouse_hypothalamus_md)
mouse_hypothalamus_md <- spe_metadata_cluster_template("regular", "cylinder", mouse_hypothalamus_md)
mouse_hypothalamus_md <- spe_metadata_cluster_template("regular", "network", mouse_hypothalamus_md)
mouse_hypothalamus_md <- spe_metadata_cluster_template("regular", "ellipsoid", mouse_hypothalamus_md)


mouse_hypothalamus_md$background$n_cells <- 30000
mouse_hypothalamus_md$background$length <- 100
mouse_hypothalamus_md$background$width <- 100
mouse_hypothalamus_md$background$height <- 50
mouse_hypothalamus_md$background$minimum_distance_between_cells <- 0
mouse_hypothalamus_md$background$cell_types <- 
  c("Ambiguous", "Astrocyte", "Endothelial", 
    "Ependymal", "Excitatory", "Inhibitory")
mouse_hypothalamus_md$background$cell_proportions <-
  c(0.125, 0.125, 0,
    0, 0.25, 0.5)

mouse_hypothalamus_md$cluster_1$cluster_cell_types <- "Ependymal"
mouse_hypothalamus_md$cluster_1$cluster_cell_proportions <- 1
mouse_hypothalamus_md$cluster_1$radius <- 3
mouse_hypothalamus_md$cluster_1$start_loc <- c(10, 50, 47)
mouse_hypothalamus_md$cluster_1$end_loc <- c(100, 50, 47)

mouse_hypothalamus_md$cluster_2$cluster_cell_types <- "Ependymal"
mouse_hypothalamus_md$cluster_2$cluster_cell_proportions <- 1
mouse_hypothalamus_md$cluster_2$radius <- 3
mouse_hypothalamus_md$cluster_2$start_loc <- c(10, 50, 44)
mouse_hypothalamus_md$cluster_2$end_loc <- c(90, 50, 44)

mouse_hypothalamus_md$cluster_3$cluster_cell_types <- "Ependymal"
mouse_hypothalamus_md$cluster_3$cluster_cell_proportions <- 1
mouse_hypothalamus_md$cluster_3$radius <- 3
mouse_hypothalamus_md$cluster_3$start_loc <- c(10, 50, 41)
mouse_hypothalamus_md$cluster_3$end_loc <- c(90, 50, 41)

mouse_hypothalamus_md$cluster_4$cluster_cell_types <- "Ependymal"
mouse_hypothalamus_md$cluster_4$cluster_cell_proportions <- 1
mouse_hypothalamus_md$cluster_4$radius <- 3
mouse_hypothalamus_md$cluster_4$start_loc <- c(10, 50, 38)
mouse_hypothalamus_md$cluster_4$end_loc <- c(100, 50, 38)

mouse_hypothalamus_md$cluster_5$cluster_cell_types <- "Ependymal"
mouse_hypothalamus_md$cluster_5$cluster_cell_proportions <- 1
mouse_hypothalamus_md$cluster_5$radius <- 3
mouse_hypothalamus_md$cluster_5$start_loc <- c(10, 50, 35)
mouse_hypothalamus_md$cluster_5$end_loc <- c(90, 50, 35)

mouse_hypothalamus_md$cluster_6$cluster_cell_types <- "Ependymal"
mouse_hypothalamus_md$cluster_6$cluster_cell_proportions <- 1
mouse_hypothalamus_md$cluster_6$radius <- 3
mouse_hypothalamus_md$cluster_6$start_loc <- c(10, 50, 32)
mouse_hypothalamus_md$cluster_6$end_loc <- c(90, 50, 32)


mouse_hypothalamus_md$cluster_7$cluster_cell_types <- "Endothelial"
mouse_hypothalamus_md$cluster_7$cluster_cell_proportions <- 1
mouse_hypothalamus_md$cluster_7$n_edges <- 30
mouse_hypothalamus_md$cluster_7$width <- 6
mouse_hypothalamus_md$cluster_7$centre_loc <- c(80, 50, 0)
mouse_hypothalamus_md$cluster_7$radius <- 40

mouse_hypothalamus_md$cluster_8$cluster_cell_types <- 
  c("Ambiguous", "Astrocyte", "Endothelial", 
    "Ependymal", "Excitatory", "Inhibitory")
mouse_hypothalamus_md$cluster_8$cluster_cell_proportions <-
  c(0.125, 0, 0,
    0, 0.75, 0.125)

mouse_hypothalamus_md$cluster_8$radii <- c(20, 30, 20)
mouse_hypothalamus_md$cluster_8$centre_loc <- c(0, 50, 40)
mouse_hypothalamus_md$cluster_8$axes_rotation <- c(0, 0, 0)

mouse_hypothalamus_spe <- simulate_spe_metadata3D(mouse_hypothalamus_md, plot_image = F)
spatialCoords(mouse_hypothalamus_spe) <- spatialCoords(mouse_hypothalamus_spe) + 1000

mouse_hypothalamus_cell_type_color_mapping <- merfish_squidpy_cell_type_color_mapping <- merfish_squidpy_cell_type_color_mapping
mouse_hypothalamus_cell_type_color_mapping$Cell_Type[mouse_hypothalamus_cell_type_color_mapping$Cell_Type == "Endothelial 2"] <- "Endothelial"

plot_cells3D(mouse_hypothalamus_spe, 
             plot_cell_types = 
               mouse_hypothalamus_cell_type_color_mapping$Cell_Type,
             plot_colours = 
               mouse_hypothalamus_cell_type_color_mapping$Color)




### 2. Drosphilia Egg (E16_18h) ----
# Original
setwd("/home/dle/R/data3D/stomics_fly")
E16_18h_df <- read.csv("E16-18h_df.csv")
E16_18h_df$Cell.Z.Position <- E16_18h_df$Cell.Z.Position * 10 + 1000
E16_18h_df$Cell.X.Position <- E16_18h_df$Cell.X.Position * 10 + 1000
E16_18h_df$Cell.Y.Position <- E16_18h_df$Cell.Y.Position * 10 + 1000
plot_cells3D_df(E16_18h_df, aspectmode = "data", 
                plot_cell_types = 
                  c("salivary gland", "epidermis", "CNS", "carcass", "fat body",
                    "muscle", "trachea", "midgut", "hemolymph", "foregut"),
                plot_colours = c(
                  "#33135b",
                  "#73ec81",
                  "#9437a8",
                  "#b8db50",
                  "#0062c5",
                  "#f77e3b",
                  "#48bbff",
                  "#bb0036",
                  "#4deeac",
                  "#770026"
                ))

# using spasim_3D
egg_md <- spe_metadata_background_template("ordered")
egg_md <- spe_metadata_cluster_template("double ring", "ellipsoid", egg_md)
egg_md <- spe_metadata_cluster_template("regular", "sphere", egg_md)
egg_md <- spe_metadata_cluster_template("regular", "sphere", egg_md)
egg_md <- spe_metadata_cluster_template("regular", "sphere", egg_md)
egg_md <- spe_metadata_cluster_template("regular", "ellipsoid", egg_md)
egg_md <- spe_metadata_cluster_template("regular", "ellipsoid", egg_md)
egg_md <- spe_metadata_cluster_template("regular", "ellipsoid", egg_md)
egg_md <- spe_metadata_cluster_template("regular", "sphere", egg_md)

egg_md$background$n_cells <- 40000
egg_md$background$length <- 100
egg_md$background$width <- 70
egg_md$background$height <- 40
egg_md$background$jitter_proportion <- 0.1
egg_md$background$cell_types <- 
  c("salivary gland", "epidermis", "CNS", "carcass", "fat body",
    "muscle", "trachea", "midgut", "hemolymph", "foregut",
    "other")
egg_md$background$cell_proportions <-
  c(0, 0, 0, 0, 0,
    0, 0, 0, 0, 0,
    1)

egg_md$cluster_1$cluster_cell_types <- "midgut"
egg_md$cluster_1$cluster_cell_proportions <- 1
egg_md$cluster_1$centre_loc <- c(50, 50, 25)
egg_md$cluster_1$radii <- c(34, 14, 8)
egg_md$cluster_1$axes_rotation <- c(0, 0, 0)
egg_md$cluster_1$inner_ring_cell_types <- "fat body"
egg_md$cluster_1$inner_ring_cell_proportions <- 1
egg_md$cluster_1$inner_ring_width <- 4
egg_md$cluster_1$outer_ring_cell_types <- c("epidermis", "carcass", "muscle")
egg_md$cluster_1$outer_ring_cell_proportions <- c(0.8, 0.1, 0.1)
egg_md$cluster_1$outer_ring_width <- 4

egg_md$cluster_2$cluster_cell_types <- "muscle"
egg_md$cluster_2$cluster_cell_proportions <- 1
egg_md$cluster_2$radius <- 14
egg_md$cluster_2$centre_loc <- c(30, 50, 25)

egg_md$cluster_3$cluster_cell_types <- "salivary gland"
egg_md$cluster_3$cluster_cell_proportions <- 1
egg_md$cluster_3$radius <- 10
egg_md$cluster_3$centre_loc <- c(30, 60, 25)

egg_md$cluster_4$cluster_cell_types <- "midgut"
egg_md$cluster_4$cluster_cell_proportions <- 1
egg_md$cluster_4$radius <- 8
egg_md$cluster_4$centre_loc <- c(70, 50, 40)

egg_md$cluster_5$cluster_cell_types <- "carcass"
egg_md$cluster_5$cluster_cell_proportions <- 1
egg_md$cluster_5$centre_loc <- c(50, 65, 25)
egg_md$cluster_5$radii <- c(32, 5, 8)
egg_md$cluster_5$axes_rotation <- c(0, 0, 0)

egg_md$cluster_6$cluster_cell_types <- "carcass"
egg_md$cluster_6$cluster_cell_proportions <- 1
egg_md$cluster_6$centre_loc <- c(50, 35, 25)
egg_md$cluster_6$radii <- c(32, 5, 8)
egg_md$cluster_6$axes_rotation <- c(0, 0, 0)

egg_md$cluster_7$cluster_cell_types <- c("trachea")
egg_md$cluster_7$cluster_cell_proportions <- c(1)
egg_md$cluster_7$centre_loc <- c(75, 60, 32)
egg_md$cluster_7$radii <- c(5, 12, 6)
egg_md$cluster_7$axes_rotation <- c(45, 0, 0)

egg_md$cluster_8$cluster_cell_types <- "foregut"
egg_md$cluster_8$cluster_cell_proportions <- 1
egg_md$cluster_8$radius <- 6
egg_md$cluster_8$centre_loc <- c(80, 50, 32)

egg_spe <- simulate_spe_metadata3D(egg_md, plot_image = F)
spatialCoords(egg_spe) <- spatialCoords(egg_spe) + 1000

plot_cells3D(egg_spe, 
             plot_cell_types = 
               c("salivary gland", "epidermis", "CNS", "carcass", "fat body",
                 "muscle", "trachea", "midgut", "hemolymph", "foregut"),
             plot_colours = c(
               "#33135b",
               "#73ec81",
               "#9437a8",
               "#b8db50",
               "#0062c5",
               "#f77e3b",
               "#48bbff",
               "#bb0036",
               "#4deeac",
               "#770026"
             ))



### 3. Mouse embryo (E11.5h) ----
# Original
setwd("/home/dle/R/data3D/spateo")
mouse_E11.5_embryo <- read.csv("mouse_E11.5_embryo.csv")
cells <- names(sort(table(mouse_E11.5_embryo$Cell.Type), decreasing = TRUE))[1:12]

mouse_E11.5_embryo$Cell.X.Position <- mouse_E11.5_embryo$Cell.X.Position + 1000
mouse_E11.5_embryo$Cell.Y.Position <- mouse_E11.5_embryo$Cell.Y.Position + 1000
mouse_E11.5_embryo$Cell.Z.Position <- mouse_E11.5_embryo$Cell.Z.Position + 1000
plot_cells3D_df(mouse_E11.5_embryo[sample(nrow(mouse_E11.5_embryo), 100000), ],
                aspectmode = "data",
                feature_colname = "Cell.Type",
                plot_cell_types = cells,
                plot_colours = c(
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
                  "#d99dff"
                ))


# using spaSim-3D
mouse_md <- spe_metadata_background_template("random")
mouse_md <- spe_metadata_cluster_template("regular", "sphere", mouse_md)
mouse_md <- spe_metadata_cluster_template("regular", "ellipsoid", mouse_md)
mouse_md <- spe_metadata_cluster_template("regular", "ellipsoid", mouse_md)
mouse_md <- spe_metadata_cluster_template("regular", "sphere", mouse_md)
mouse_md <- spe_metadata_cluster_template("regular", "sphere", mouse_md)
mouse_md <- spe_metadata_cluster_template("regular", "ellipsoid", mouse_md)
mouse_md <- spe_metadata_cluster_template("regular", "ellipsoid", mouse_md)
mouse_md <- spe_metadata_cluster_template("regular", "cylinder", mouse_md)
mouse_md <- spe_metadata_cluster_template("regular", "cylinder", mouse_md)
mouse_md <- spe_metadata_cluster_template("regular", "cylinder", mouse_md)


mouse_md$background$n_cells <- 100000
mouse_md$background$length <- 100
mouse_md$background$width <- 100
mouse_md$background$height <- 50
mouse_md$background$minimum_distance_between_cells <- 0
mouse_md$background$cell_types <- "other"
mouse_md$background$cell_proportions <- 1

mouse_md$cluster_1$cluster_cell_types <- c("Glutamatergic neurons", "Cranial mesenchyme", "Cranial mesoderm")
mouse_md$cluster_1$cluster_cell_proportions <- c(0.3, 0.4, 0.3)
mouse_md$cluster_1$radius <- 25
mouse_md$cluster_1$centre_loc <- c(25, 25, 25)

mouse_md$cluster_2$cluster_cell_types <- c("Sclerotome", "Fibroblasts", "Glutamatergic neurons", "Lateral plate and intermediate mesoderm")
mouse_md$cluster_2$cluster_cell_proportions <- c(0.4, 0.3, 0.2, 0.1)
mouse_md$cluster_2$centre_loc <- c(60, 45, 25)
mouse_md$cluster_2$radii <- c(45, 20, 20)
mouse_md$cluster_2$axes_rotation <- c(-10, 0, 0)

mouse_md$cluster_3$cluster_cell_types <- c("Sclerotome", "Fibroblasts", "Glutamatergic neurons", "Lateral plate and intermediate mesoderm")
mouse_md$cluster_3$cluster_cell_proportions <- c(0.6, 0.20, 0.15, 0.05)
mouse_md$cluster_3$centre_loc <- c(80, 20, 25)
mouse_md$cluster_3$radii <- c(25, 15, 20)
mouse_md$cluster_3$axes_rotation <- c(38, 0, 0)

mouse_md$cluster_4$cluster_cell_types <- c("Limb progenitors and lateral plate mesenchyme")
mouse_md$cluster_4$cluster_cell_proportions <- c(1)
mouse_md$cluster_4$radius <- 9
mouse_md$cluster_4$centre_loc <- c(60, 45, 40)

mouse_md$cluster_5$cluster_cell_types <- c("Limb progenitors and lateral plate mesenchyme")
mouse_md$cluster_5$cluster_cell_proportions <- c(1)
mouse_md$cluster_5$radius <- 10
mouse_md$cluster_5$centre_loc <- c(75, 15, 40)

mouse_md$cluster_6$cluster_cell_types <- c("Facial mesenchyme")
mouse_md$cluster_6$cluster_cell_proportions <- c(1)
mouse_md$cluster_6$centre_loc <- c(35, 25, 30)
mouse_md$cluster_6$radii <- c(10, 18, 15)
mouse_md$cluster_6$axes_rotation <- c(0, 0, 0)

mouse_md$cluster_7$cluster_cell_types <- c("Telencephalon neuroectoderm")
mouse_md$cluster_7$cluster_cell_proportions <- c(1)
mouse_md$cluster_7$centre_loc <- c(25, 10, 25)
mouse_md$cluster_7$radii <- c(18, 10, 15)
mouse_md$cluster_7$axes_rotation <- c(-60, 0, 0)

mouse_md$cluster_8$cluster_cell_types <- c("Spinal cord neuroectoderm")
mouse_md$cluster_8$cluster_cell_proportions <- c(1)
mouse_md$cluster_8$radius <- 5
mouse_md$cluster_8$start_loc <- c(20, 60, 25)
mouse_md$cluster_8$end_loc <- c(65, 64, 25)


mouse_md$cluster_9$cluster_cell_types <- c("Spinal cord neuroectoderm")
mouse_md$cluster_9$cluster_cell_proportions <- c(1)
mouse_md$cluster_9$radius <- 5
mouse_md$cluster_9$start_loc <- c(65, 64, 25)
mouse_md$cluster_9$end_loc <- c(100, 42, 25)

mouse_md$cluster_10$cluster_cell_types <- c("Spinal cord neuroectoderm")
mouse_md$cluster_10$cluster_cell_proportions <- c(1)
mouse_md$cluster_10$radius <- 5
mouse_md$cluster_10$start_loc <- c(100, 40, 25)
mouse_md$cluster_10$end_loc <- c(100, 25, 25)

mouse_spe <- simulate_spe_metadata3D(mouse_md, plot_image = F)
spatialCoords(mouse_spe) <- spatialCoords(mouse_spe) + 1000
plot_cells3D(mouse_spe,
             feature_colname = "Cell.Type",
             plot_cell_types = cells,
             plot_colours = c(
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
               "#d99dff"
             ))
