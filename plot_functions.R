plot_def_angle <- function(f_size = 6){
  theme_minimal() +
    theme_classic() +
    theme(axis.text.x = element_text(color="black",
                                     size= f_size, angle=45, vjust = 1, hjust = 1, family = "Helvetica"),
          axis.text.y = element_text(color="black",
                                     size= f_size, angle=0, family = "Helvetica"),
          axis.line = element_line(colour = "black",
                                   size = 0.5, linetype = "solid"),
          axis.ticks = element_line(colour = "black",
                                    size = 0.5, linetype = "solid"),
          axis.title.x = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"),
          axis.title.y = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"))
}


plot_def <- function(f_size = 6){
  theme_minimal() +
    theme_classic() +
    theme(axis.text.x = element_text(color="black",
                                     size= f_size, angle=0, vjust = 0, hjust = 0.5, family = "Helvetica"),
          axis.text.y = element_text(color="black",
                                     size= f_size, angle=0, family = "Helvetica"),
          axis.line = element_line(colour = "black",
                                   size = 0.5, linetype = "solid"),
          axis.ticks = element_line(colour = "black",
                                    size = 0.5, linetype = "solid"),
          axis.title.x = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"),
          axis.title.y = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"))
}


plot_def_angle_no_axis_dotted <- function(f_size = 6){
  theme_minimal() +
    theme(panel.grid = element_line(linetype = "dotted"),
          axis.text.x = element_text(color="black",
                                     size= f_size,
                                     angle=45,
                                     vjust = 1,
                                     hjust = 1,
                                     family = "Helvetica"),
          axis.text.y = element_text(color="black",
                                     size= f_size,
                                     angle=0,
                                     family = "Helvetica"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}


plot_def_no_axis <- function(f_size = 6){
  theme_minimal() +
    theme_classic() +
    theme(axis.text.x = element_text(color="black",
                                     size= f_size,
                                     angle=0,
                                     vjust = 0,
                                     hjust = 0.5,
                                     family = "Helvetica"),
          axis.text.y = element_text(color="black",
                                     size= f_size,
                                     angle=0,
                                     family = "Helvetica"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}


plot_def_no_axis_dotted <- function(f_size = 6){
  theme_minimal() +
    theme(panel.grid = element_line(linetype = "dotted"),
          axis.text.x = element_text(color="black",
                                     size= f_size,
                                     angle=0,
                                     vjust = 0,
                                     hjust = 0.5,
                                     family = "Helvetica"),
          axis.text.y = element_text(color="black",
                                     size= f_size,
                                     angle=0,
                                     family = "Helvetica"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}

plot_def_dotted <- function(f_size = 6){
  theme_minimal() +
    theme(panel.grid = element_line(linetype = "dotted"),
          axis.text.x = element_text(color="black",
                                     size= f_size, angle=0, vjust = 0, hjust = 0.5, family = "Helvetica"),
          axis.text.y = element_text(color="black",
                                     size=f_size, angle=0, family = "Helvetica"),
          axis.line = element_line(colour = "black",
                                   size = 0.5, linetype = "solid"),
          axis.ticks = element_line(colour = "black",
                                    size = 0.5, linetype = "solid"),
          axis.title.x = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"),
          axis.title.y = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"))
}


plot_def_dotted_angle <- function(f_size = 6){
  theme_minimal() +
    theme(panel.grid = element_line(linetype = "dotted"),
          axis.text.x = element_text(color="black",
                                     size= f_size, angle=45, vjust = 1, hjust = 1, family = "Helvetica"),
          axis.text.y = element_text(color="black",
                                     size= f_size, angle=0, family = "Helvetica"),
          axis.line = element_line(colour = "black",
                                   size = 0.5, linetype = "solid"),
          axis.ticks = element_line(colour = "black",
                                    size = 0.5, linetype = "solid"),
          axis.title.x = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"),
          axis.title.y = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"))
}


plot_def_dotted_vertical <- function(f_size = 6){
  theme_minimal() +
    theme(panel.grid = element_line(linetype = "dotted"),
          axis.text.x = element_text(color="black",
                                     size= f_size, angle=90, vjust = 0.5, hjust = 1, family = "Helvetica"),
          axis.text.y = element_text(color="black",
                                     size= f_size, angle=0, family = "Helvetica"),
          axis.line = element_line(colour = "black",
                                   size = 0.5, linetype = "solid"),
          axis.ticks = element_line(colour = "black",
                                    size = 0.5, linetype = "solid"),
          axis.title.x = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"),
          axis.title.y = element_text(face="bold", color="black",
                                      size= f_size, family = "Helvetica"))
}



getPalette <- function(n){
  pal <- colorRampPalette(brewer.pal(n, "Spectral"), interpolate = "spline")
  return(pal)
}

