data<-read.delim('I:/Tidal/R_data/Fig.3d/HOST-virus-correlation.txt',header = T,row.names = 1)
library(Hmisc)
cor<-rcorr(as.matrix(data),type='spearman')
r<-cor$r
p<-cor$P

p_adjust<-p.adjust(p,method="BH")
for (i in 1:ncol(p)){
  for (j in 1:nrow(p)){
    p[j,i]=p_adjust[(i-1)*nrow(p)+j]}}

cols<-colorRampPalette(c(rgb(15,153,183,max=255),rgb(255,255,255,max=255),rgb(198,113,113,max=255)))(200)
library(corrplot)
corrplot(data,type='upper',tl.srt=90,tl.col = 'black',method='square',
         p.mat = p,insig = 'label_sig',sig.level = c(0.001,0.01,0.05),pch.cex = 1,pch.col = 'white',
         col=cols) 

write.table (r,file ="I:/Tidal/R_data/Fig.3d/r_TOC_spearman.txt", row.names = T, col.names =T, quote =FALSE) 
write.table (p,file ="I:/Tidal/R_data/Fig.3d/p_TOC_spearman.txt", row.names = T, col.names =T, quote =F)
r<-read.delim('I:/Tidal/R_data/Fig.3d//r_VHR_spearman.txt',header = T,row.names = 1)
p<-read.delim('I:/Tidal/R_data/Fig.3d//p_VHR_spearman.txt',header = T,row.names = 1)
p<-as.matrix(t(p))
r<-as.matrix(t(r))
cols<-colorRampPalette(c(rgb(255,128,64,max=255),rgb(255,255,255,max=255),rgb(91,194,205,max=255)))(200)
library(corrplot)
corrplot(r,tl.srt=45,tl.col = 'black',method='square',tl.cex=0.8,
         p.mat = p,insig = 'label_sig',sig.level = c(0.001,0.01,0.05),pch.cex = 0.8,pch.col = 'black',cl.cex = 0.8,
         col=cols,tl.pos="b",cl.pos = "b",cl.ratio = 0.23)
ggsave("Alp.tiff",width=5,height=5,path="F:/")
tl.pos="n"