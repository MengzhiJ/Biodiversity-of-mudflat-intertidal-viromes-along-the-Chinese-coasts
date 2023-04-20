library(ggplot2)
data<-data.frame(variable=c(13253,
                           687,
                            247,
                            177,
                            85,
                            84,
                            64,
                            60,
                            59,
                            57,
                            51), group = paste0("a", 1:11))
ggplot(data, aes(x = 3, y = variable, fill = group))+ geom_col() +
  coord_polar(theta = "y") +xlim(c(0.5, 4.5))+theme_void()+theme(legend.position = "none")+
  scale_fill_manual(breaks=c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10","a11"),values=c("#7AC5CD","#ff9d6f",
                                                                                                "#A0E632","#D8B0FF",
                                                                                                "#A67A13","#8E9CFF","#9D3CFF",
                                                                                                "#00A0FF","#FB6501","#E6AF2D",
                                                                                                "#EDE76D"))
ggsave("Fig.1d.tiff",width=5.5,height=5,path="I:/Tidal/R_data/Fig.1d") 
