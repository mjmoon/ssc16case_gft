library(ggplot2)
library(gridExtra)
library(grid)

mytheme <- theme_minimal()+
  theme(axis.line = element_line(),
        # panel.grid.major.x = element_blank(),
        plot.title = element_text(size = rel(2)),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2)),
        axis.title.y = element_text(margin=margin(0,15,0,0)),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.background = element_rect(color="white"))

pltWidth <- 960
pltHeight <- 480

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

format1000 <- function(x) {
  round(x/1000,1)
}
