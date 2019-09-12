library(htmlwidgets)
library(tidyverse)
library(ggplot2)


#fix for saveWidget (leaflet map) bug
saveWidgetFix <- function (widget,file,...) {
   ## A wrapper to saveWidget which compensates for arguable BUG in
   ## saveWidget which requires `file` to be in current working
   ## directory.
   wd<-getwd()
   on.exit(setwd(wd))
   outDir<-dirname(file)
   file<-basename(file)
   setwd(outDir);
   saveWidget(widget,file=file,...)
}


#ggplot theme
theme_agile <- function(base_size = 12, base_family = "MetaNormal-Roman", plot.type = "formal", lines.lwd = 0.25, ticks.type = "outer", plot.grid = TRUE, axis.font = base_family, title.size = base_size*2, legend.size = base_size,
                        bg.col = ifelse(plot.type == "formal", "white", "#F0F0F0"), title.font = base_family , base.col = "black", axis.lines = TRUE,
                        minor.grid = ifelse(plot.grid, TRUE, FALSE), vert.grid = ifelse(plot.grid, TRUE, FALSE), ticks.length = ifelse(ticks.type == "outer", 0.2, -0.2), horz.grid = ifelse(plot.grid, TRUE, FALSE), alpha.leg = 0.1, bord.size = 0,
                        legend.bg = ifelse(plot.type == "formal", "white", "#F0F0F0"), strip.bg = ifelse(plot.type == "formal", "white", "grey80")){
   theme_bw(base_family = "MetaNormal-Roman")
}
