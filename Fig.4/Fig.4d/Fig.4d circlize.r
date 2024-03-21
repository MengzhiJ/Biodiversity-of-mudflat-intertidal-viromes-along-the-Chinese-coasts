#load packages
library(circlize)
library(viridis)
library(reshape2)
setwd("Fig.4")
#info dataset
df <- read.csv("Fig.4d.csv", header=TRUE,stringsAsFactors = FALSE,check.names = FALSE)
df_melt<-melt(df,id.vars = 'Region')
colnames(df_melt)<-c('from','to','value')
df_melt$to<-as.character(df_melt$to)

#define factor
df_sum<-apply(df[,2:ncol(df)],2,sum)+apply(df[,2:ncol(df)],1,sum)
order<-sort(df_sum,index.return=TRUE,decreasing =TRUE)
df_melt$from<-factor(df_melt$from,levels=df$Region[order$ix],order=TRUE)

df_melt<-dplyr:: arrange (df_melt, from)

#define color
mycolor = c(Intertidal_zone= "#02C874", Marine = "#C4A8FF", Soil = "#F7BF90",
            Human = "#FBEA50", Freshwater = "#4D94F0", RefSeq = "gray")
names(mycolor) <-df$Region

#circos plot
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

chordDiagram(
  x = df_melt,order = c("Intertidal_zone","Marine","Soil","Human","Freshwater","RefSeq"),
  grid.col = mycolor,
  transparency = 0.6,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)

#circos add labels
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    circos.text(
      x = mean(xlim),
      y = 3.5,
      labels = sector.index,
      facing = "bending",
      cex = 1.2
    )
    circos.axis(
      h = "top",
      major.at = c(0,5,10,15,20,25,30,35,40),
      minor.ticks = 1,
      major.tick.length = 0.5,
      labels.niceFacing = FALSE)
  }
)
