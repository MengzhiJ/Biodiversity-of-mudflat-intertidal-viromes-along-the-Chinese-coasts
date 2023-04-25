library(vegan)
env<-read.csv('I:/Tidal/R_data/Fig. 5/Environment.CSV',row.names = 1) # sample as row, env as column
mOTUs<-t(read.delim('I:/Tidal/R_data/Fig. 5/mOTUs.txt',row.names = 1)) # sample as row, env as column
vOTUs<-t(read.delim('I:/Tidal/R_data/Fig. 5/vOTUs.txt',row.names = 1))
host<-t(read.delim('I:/Tidal/R_data/Fig. 5/host.txt',row.names = 1))
virus<-t(read.delim('I:/Tidal/R_data/Fig. 5/virus.txt',row.names = 1))
geo<-read.csv('I:/Tidal/R_data/Fig. 5/distance.CSV',row.names = 1) # sample as row, env as column
print(row.names(env)==row.names(geo)) #check
env<-apply(env[,1:11],2,as.numeric) #transfer numeric
#calculate distance
vOTUs<-vegdist(vOTUs,method = "bray")
mOTUs<-vegdist(mOTUs,method = "bray")
host<-vegdist(host,method = "bray")
virus<-vegdist(virus,method = "bray")
geo<-vegdist(geo,method = 'euclidean') 


#mantel test between env,geo,OTUs
#calculate distance
env<-vegdist(env,method = 'euclidean',na.rm = T) 
#partial mantel test
mantel.partial(mOTUs,env,geo,permutations = 999,method="pearson",na.rm=T)


#mantel test for each env with OTUs
mantel<-list()
for (i in 1:11){
  mantel[[i]]<-vegdist(env[,i],method='euclidean',upper = FALSE,na.rm = T)
}
names(mantel)<-colnames(env)[1:11]

#mantel test for env and vOTU
r<-c()
p<-c()
for (i in 1:11){
  r[i]<-mantel.partial(vOTUs,mantel[[i]],geo,permutations = 999,method="pearson",na.rm=T)$statistic
  p[i]<-mantel.partial(vOTUs,mantel[[i]],geo,permutations = 999,method="pearson",na.rm=T)$signif
}
p<-as.data.frame(p)
r<-as.data.frame(r)
rownames(p)<-colnames(env)
rownames(r)<-colnames(env)


#mantel test for env and mOTU
r1<-c()
p1<-c()
for (i in 1:11){
  r1[i]<-mantel.partial(mOTUs,mantel[[i]],geo,permutations = 999,method="pearson",na.rm=T)$statistic
  p1[i]<-mantel.partial(mOTUs,mantel[[i]],geo,permutations = 999,method="pearson",na.rm=T)$signif
}
p1<-as.data.frame(p1)
r1<-as.data.frame(r1)
rownames(p1)<-colnames(env)
rownames(r1)<-colnames(env)
#mantel test for env and host
r2<-c()
p2<-c()
for (i in 1:11){
  r2[i]<-mantel.partial(host,mantel[[i]],geo,permutations = 999,method="pearson",na.rm=T)$statistic
  p2[i]<-mantel.partial(host,mantel[[i]],geo,permutations = 999,method="pearson",na.rm=T)$signif
}
p2<-as.data.frame(p2)
r2<-as.data.frame(r2)
rownames(p2)<-colnames(env)
rownames(r2)<-colnames(env)
#mantel test for env and virus(host)
r3<-c()
p3<-c()
for (i in 1:11){
  r3[i]<-mantel.partial(virus,mantel[[i]],geo,permutations = 999,method="pearson",na.rm=T)$statistic
  p3[i]<-mantel.partial(virus,mantel[[i]],geo,permutations = 999,method="pearson",na.rm=T)$signif
}
p3<-as.data.frame(p3)
r3<-as.data.frame(r3)
rownames(p3)<-colnames(env)
rownames(r3)<-colnames(env)

write.csv(p,file ="I:/Tidal/R_data/Fig. 5/p.csv", row.names = T, quote =FALSE)    
write.csv(r,file ="I:/Tidal/R_data/Fig. 5/r.csv", row.names = T, quote =FALSE) 
write.csv(p1,file ="I:/Tidal/R_data/Fig. 5/p1.csv", row.names = T, quote =FALSE)    
write.csv(r1,file ="I:/Tidal/R_data/Fig. 5/r1.csv", row.names = T, quote =FALSE) 
write.csv(p2,file ="I:/Tidal/R_data/Fig. 5/p2.csv", row.names = T, quote =FALSE)    
write.csv(r2,file ="I:/Tidal/R_data/Fig. 5/r2.csv", row.names = T, quote =FALSE) 
write.csv(p3,file ="I:/Tidal/R_data/Fig. 5/p3.csv", row.names = T, quote =FALSE)    
write.csv(r3,file ="I:/Tidal/R_data/Fig. 5/r3.csv", row.names = T, quote =FALSE) 


mantel<-read.delim('I:/Tidal/R_data/Fig. 5/mantel.txt')

library(linkET)
install.packages('linkET')
qcorrplot(rcorr(env), type = "lower", diag = F, grid_size = 0.4,grid_col = "lightgray") +
  geom_square(linetype=0) +
  geom_couple(aes(colour = p_value, size = r_value), data = mantel, curvature = 0.1) +
  set_corrplot_style(colours  =c("#FF8040","white","#5BC2CD")) +
  scale_size_manual(values = c(0.2, 0.4, 1))+ 
  scale_colour_manual(values=c("#87CEEB","gray","#9ACD32")) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 1.5), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))+
  geom_mark(
    only_mark = T,
    size = 3.2, 
    sig_level = c(0.05, 0.01, 0.001), 
    sig_thres = 0.05
  ) 




