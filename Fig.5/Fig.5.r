#R_code (v4.2.0) for Figure 5
#load all need packages
library(vegan)
library(linkET)
library(Hmisc)
library(ggplot2)
library(vegan)
library(minpack.lm)
library(stats4)
library(grid)
library(ape)
library(parallel)
library(bigmemory)
library(NST)
library(permute)
library(DirichletReg)
library(readxl)

setwd('D:/Tidal_spatial/R_data/github/Fig.5/raw_data_for_figure5/')
#Fig.5a
env<-read.csv('Environment.CSV',row.names = 1) # sample as row, env as column
mOTUs<-t(read.delim('mOTUs.txt',row.names = 1)) # sample as row, env as column
vOTUs<-t(read.delim('vOTUs.txt',row.names = 1))
host<-t(read.delim('host.txt',row.names = 1))
virus<-t(read.delim('virus.txt',row.names = 1))
geo<-read.csv('distance.CSV',row.names = 1) # sample as row, env as column
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

mantel<-read.delim('mantel.txt')

qcorrplot(rcorr(env), type = "upper", diag = F, grid_size = 0.4,grid_col = "lightgray") +
  geom_square(linetype=0) +
  geom_couple(aes(colour = p_value, size = r_value), data = mantel, curvature = 0.1) +
  set_corrplot_style(colours  =c("#FF8040","white","#5BC2CD")) +
  scale_size_manual(breaks = c("<0.1","0.1-0.2",">0.2"),
                    values = c(0.2, 0.5, 0.8))+ 
  scale_colour_manual(breaks = c(">0.05","0.001-0.05","<0.001"),
    values=c("gray","#9ACD32","#87CEEB")) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 2), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))+
  geom_mark(
    only_mark = T,
    size = 6, 
    sig_level = c(0.05, 0.01, 0.001), 
    sig_thres = 0.05) 

#Fig.5b VPA
votu<-read.delim('virus.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
votu_matrix<-t(votu)
votu_matrix<- decostand(votu_matrix, method = 'hellinger') 
env<-read.csv('Environment.csv',row.names = 1) #screen env<0.05
print(row.names(votu_matrix)==row.names(env))
res_all<- cca(votu_matrix~.,data=env)
res_null<-cca(votu_matrix~1,data=env)
env_frwd<-ordistep(res_null,scope=formula(res_all),direction = 'forward',permutations = how(nperm = 999)) #fit model<0.05
geo<-read.csv('Distance.csv',row.names = 1)  #calculate PCNM for geo
geo<-vegdist(geo,method = "euclidean")
geo<-pcnm(geo)
geo<-as.data.frame(geo$vectors)
res_all<- cca(votu_matrix~.,data=geo)
res_null<-cca(votu_matrix~1,data=geo)
env_frwd<-ordistep(res_null,scope=formula(res_all),direction = 'forward',permutations = how(nperm = 999)) #fit model<0.05
envdat_raw<-read.csv('VPA.csv',row.names = 1) #factors in no-linear geo/env
print(row.names(votu_matrix)==row.names(envdat_raw))
env<-envdat_raw[,1:10]
geo<-envdat_raw[,12:18]
cca_all<-varpart(votu_matrix,env,geo,chisquare = T) #obtain one or two factors common proportion

#Fig.5c
#Reference:Chen W, Ren K, Isabwe A, et al. Microbiome, 2019, 7: 1-16.
#using Non-linear least squares (NLS) to calculate R2:
#spp: A community table with taxa as rows and samples as columns
spp<-read.csv('host.txt',head=T,stringsAsFactors=F,row.names=1,sep = "\t")
spp<-t(spp)
N <- mean(apply(spp, 1, sum))
p.m <- apply(spp, 2, mean)
p.m <- p.m[p.m != 0]
p <- p.m/N
spp.bi <- 1*(spp>0)
freq <- apply(spp.bi, 2, mean)
freq <- freq[freq != 0]
C <- merge(p, freq, by=0)
C <- C[order(C[,2]),]
C <- as.data.frame(C)
C.0 <- C[!(apply(C, 1, function(y) any(y == 0))),]
p <- C.0[,2]
freq <- C.0[,3]
names(p) <- C.0[,1]
names(freq) <- C.0[,1]
d = 1/N

##Fit model parameter m (or Nm) using Non-linear least squares (NLS)
m.fit <- nlsLM(freq ~ pbeta(d, N*m*p, N*m*(1 -p), lower.tail=FALSE),start=list(m=0.1))
m.fit #get the m value
m.ci <- confint(m.fit, 'm', level=0.95)
freq.pred <- pbeta(d, N*coef(m.fit)*p, N*coef(m.fit)*(1 -p), lower.tail=FALSE)
pred.ci <- binconf(freq.pred*nrow(spp), nrow(spp), alpha=0.05, method="wilson", return.df=TRUE)
Rsqr <- 1 - (sum((freq - freq.pred)^2))/(sum((freq - mean(freq))^2))
Rsqr# get the R2 value

#Drawing the figure using grid package:
#p is the mean relative abundance
#freq is occurrence frequency
#freq.pred is predicted occurrence frequency
bacnlsALL <-data.frame(p,freq,freq.pred,pred.ci[,2:3])
inter.col<-rep('gray',nrow(bacnlsALL))
inter.col[bacnlsALL$freq <= bacnlsALL$Lower]<-'#FF9966'#define the color of below points
inter.col[bacnlsALL$freq >= bacnlsALL$Upper]<-'#95D1D7'#define the color of up points
grid.newpage()
pushViewport(viewport(h=0.6,w=0.5))
pushViewport(dataViewport(xData=range(log10(bacnlsALL$p)), yData=c(0,1.02),extension=c(0.03,0)))
grid.rect()
grid.points(log10(bacnlsALL$p), bacnlsALL$freq,pch=20,gp=gpar(col=inter.col,cex=0.5))
grid.yaxis()
grid.xaxis()
grid.lines(log10(bacnlsALL$p),bacnlsALL$freq.pred,gp=gpar(col='#4DB2F0',lwd=2),default='native')

grid.lines(log10(bacnlsALL$p),bacnlsALL$Lower ,gp=gpar(col='#4DB2F0',lwd=2,lty=2),default='native') 
grid.lines(log10(bacnlsALL$p),bacnlsALL$Upper,gp=gpar(col='#4DB2F0',lwd=2,lty=2),default='native')  
grid.text(y=unit(0,'npc')-unit(2.5,'lines'),label='Mean Relative Abundance (log10)', gp=gpar(fontface=1)) 
grid.text(x=unit(0,'npc')-unit(3,'lines'),label='Occurrence frequency',gp=gpar(fontface=1),rot=90) 

#add legends
draw.text <- function(just, i, j) {
  grid.text(paste("Rsqr=",round(Rsqr,3),"\n","Nm=",round(coef(m.fit)*N)), x=x[j], y=y[i], just=just)
}
x <- unit(1:4/5, "npc")
y <- unit(1:4/5, "npc")
draw.text(c("centre", "bottom"), 4, 1)

#Fig.5d
#Reference: #Ning D, Deng Y, Tiedje J M, et al. Proceedings of the National Academy of Sciences, 2019, 116(34): 16892-16898.
#Stegen J C, Lin X, Fredrickson J K, et al. The ISME journal, 2013, 7(11): 2069-2079.

#calculate obs similarity
tax<-read.delim('host.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE) 
tax<-t(tax) #site as row, OTU as column
Dij<-vegan::vegdist(tax,method = "bray")

#multi-threads
nworker=10
c1<-parallel::makeCluster(nworker,type="PSOCK")

#export global environment
clusterExport(c1,"tax",envir = environment())

#define null fun(random:k(k=1000)) 
##calculate null Disimilarity:null model can adjust according to NST help.Also, you can use shuffle in picante packages.
fun.tax <-function(i){
  null.taxo<-NST::taxo.null(tax,sp.freq = "prop",samp.rich = "fix",abundance = "region") #site as row, OTU as column
  taxo<-vegan::vegdist(null.taxo,method = "bray")
  list<- c(taxo)
  return(list)
}

#random 1000 (k=1:1000)
list<- parLapply(c1, 1:1000, fun.tax)

#save null (OPTION)
write.csv(Gij,file ="list.csv", row.names = T,  quote =FALSE) 
write.csv(Dij,file ="Dij.csv", row.names = T,  quote =FALSE) 

#define (Dmax=1)
Gij<-as.matrix(as.data.frame(list))
Eij<-1-Gij
Cij<-1-Dij
Eij.mean=apply(Eij, 1, mean,na.rm=TRUE)
Gij.mean=apply(Gij, 1, mean,na.rm=TRUE)
GD=Gij.mean/cbind(Gij,Dij) # G/Gk and G/D
EC=Eij.mean/cbind(Eij,Cij) # E/Ek and E/C
EC[is.nan(EC)]=1
ECGD=EC
ECGD[which(EC>1)]=GD[which(EC>1)]

#calculate RC
obs=matrix(Dij,nrow=nrow(Gij),ncol=ncol(Gij))
a1=rowSums(Gij>obs,na.rm = TRUE)/ncol(Gij)
a2=rowSums(Gij==obs,na.rm = TRUE)/ncol(Gij)
rc=(0.5-(a1+(a2/2)))*2

#save RC
write.csv(rc,file ="RCbray.csv", row.names = T,  quote =FALSE) 

#map
data<-read_excel("RCbray.xlsx")
data$class <- factor(data$class,level=c("vOTUs","mOTUs","Virus (host)","Host"))
ggplot(data,aes(x=class,y=Ratio,fill=level))+
  geom_bar(stat="identity", color="black",width=0.7,size=0.2,position="stack", aes(fill=level),alpha=0.8)+
  theme_bw()+labs(x="",y="Raup-Crick proportion")+
  scale_fill_manual(breaks=c("more","mid","less"),values = c("#95D1D7","#FF9966","#d0d0d0"))+
  theme_test()+theme(legend.position ="none")+
  theme( axis.title.x = element_text(size=5.5),
         axis.title.y = element_text(size=5.5),
         axis.text.x = element_text(hjust =0.5,size=5,colour = 'black'),
         axis.text.y=element_text(size=5,colour = 'black'),
         axis.ticks = element_line(size = 0.25),
         axis.ticks.length = unit(0.03, "cm"),
         panel.border = element_rect(size=0.35))

#Fig.5e
#calculate NST
wd="D:/Tidal_spatial/R_data/github/Fig.5/raw_data_for_figure5/" # change to folder path in your computer, where the input files are saved
save.wd="D:/Tidal_spatial/R_data/github/Fig.5/raw_data_for_figure5/" # folder path, where to save the output files
com.file="" # file path of the communty matrix, e.g. OTU table. Each row represents a taxon, and each column represents a sample. Thus, row names are taxon IDs, and column names are sample IDs.
group.file="" # treatment information table. Each row represents a sample.

rand.time=1000 # randomization time for null model analysis. usually set 1000 for real datasets. here use 20 for test.
prefix="Test"

setwd(wd)
comm=t(read.delim(com.file, row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE))
group=read.delim(group.file, row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

samp.ck=NST::match.name(rn.list=list(comm=comm,group=group))
comm=samp.ck$comm
comm=comm[,colSums(comm)>0,drop=FALSE]
group=samp.ck$group

groupi=group[,1,drop=FALSE] # if you have multiple ways to group samples, select one way each time.
prefixi=paste0(prefix,".Treat") # re-define the prefix in output filenames, specific to the grouping type.
meta.groupi=NULL # if treatment and control are from different metacommunities, you may set meta.groupi=groupi

dist.method="bray" 

setwd(save.wd)
tnst=tNST(comm=comm, group=groupi, meta.group=meta.groupi, meta.com=NULL,
          dist.method=dist.method, abundance.weighted=TRUE, rand=rand.time,
          output.rand=TRUE, nworker=25, LB=FALSE, null.model="PF",dirichlet=F,
          between.group=F, SES=F, RC=F)

write.table(tnst$index.grp,file = paste0(prefixi,".tNST.summary.csv"), quote = FALSE,sep = ",")
write.table(tnst$index.pair.grp,file = paste0(prefixi,".tNST.pairwise.csv"),quote = FALSE,sep = ",")
write.table(tnst$index.pair,file = paste0(prefixi,".tNST.pair.csv"),quote = FALSE,sep = ",")

#map
data<-read_excel("NST.xlsx") 
data$class=factor(data$class, levels=c("vOTUs","mOTUs","Virus (host)","Host"))

ggplot(data,aes(x=class,y=pairwise_NST))+
  stat_summary(aes(fill=class), alpha=0.8,fun = mean,geom="bar",color="NA",width=0.55)+
  stat_summary(fun.data = mean_se,geom="errorbar",width=.08,size=0.15)+
  labs(x="",y="Normalized stochastic ratio (%)")+
  scale_fill_manual(breaks=c("Host","vOTUs","mOTUs","Virus (host)"),
                    values=c("#95D1D7","#FF9966","gray","#4DB2F0"))+
  theme_test()+theme(legend.position ="none")+
  theme( axis.title.x = element_text(size=5.5),
         axis.title.y = element_text(size=5.5),
         axis.text.x = element_text(hjust =0.5,size=5,colour = 'black'),
         axis.text.y=element_text(size=5,colour = 'black'),
         axis.ticks = element_line(size = 0.25),
         axis.ticks.length = unit(0.03, "cm"),
         panel.border = element_rect(size=0.34))+ 
  coord_cartesian(ylim = c(0,100))+
  geom_hline(yintercept=c(50), linetype=2,color="#FF8040",size=0.25) #add Horizontal Line 

ggsave("Fig.5e.pdf", width= 1.92,height= 1.9,path="D:/")                    

