winsorize <- function(x, level = 0.05){
  q_low <- quantile(x, level)
  q_high <- quantile(x, 1 - level)
  x[x < q_low] <- q_low
  x[x > q_high] <- q_high
  x
}


truncate <- function(x, level = 0.05){
  q_low <- quantile(x, level)
  q_high <- quantile(x, 1 - level)
  x[x < q_low] <- NA
  x[x > q_high] <- NA
  x
}

theme_hierarchy <- function(base_size = 12,
                                              dark_text = "#1A242F") {
  
  mid_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[2]
  light_text <-  monochromeR::generate_palette(dark_text, "go_lighter", n_colours = 5)[3]
  
  theme_minimal(base_size = base_size) +
    theme(text = element_text(colour = mid_text, family = 'Brandon Text',  lineheight = 1.1),
          plot.title = element_text(colour = dark_text, family = 'Enriqueta', face = 'bold', size = rel(1.6), margin = margin(12, 0, 8, 0)),
          plot.subtitle = element_text(size = rel(1.1), margin = margin(4, 0, 4, 0)),
          axis.text.y = element_text(colour = light_text, size = rel(0.8)),
          axis.title.y = element_text(size = 12, margin = margin(0, 4, 0, 0)),
          axis.text.x = element_text(colour = mid_text, size = rel(1.1)),
          # axis.title.x = element_blank(),
          legend.position = "top",
          legend.justification = 1,
          panel.grid = element_line(colour = "#F3F4F5", linetype = 'dashed'),
          plot.caption = element_text(size = rel(0.8), margin = margin(8, 0, 0, 0)),
          plot.margin = margin(20, 20, 20, 20))
}
