library(tidyverse)
library(scales)
library(ggforce)
library(ggsci)
sales <- c(26.3,73.7)
names<-c("a","b")
share<-sales/sum(sales)*100
data <- data.frame(
  sales,share,names)
ggplot()+
  geom_arc_bar(data=data,aes(x0 = 0, y0 = 0, r0 = 0, r = 1,amount=sales,explode=c(0,0.1),fill=names,color=names),stat="pie")+
  coord_fixed()+theme_void()+theme(legend.position = "none")+scale_color_manual(breaks=c("a","b"),values=c("lightgray","black"))+
  scale_fill_manual(breaks=c("a","b"),values=c("lightgray","black"))
ggsave("Fig.4a.tiff",width=5,height=5,path="D:/")
