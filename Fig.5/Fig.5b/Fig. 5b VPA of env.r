library(vegan)
library(ggplot2)
votu<-read.delim('H:/Tidal/vOTUs.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
votu_matrix<-t(votu)
votu_matrix<- decostand(votu_matrix, method = 'hellinger') 

env<-read.csv('H:/Tidal/R_data/Fig.4/Environment.csv',row.names = 1) #screen env<0.05

print(row.names(votu_matrix)==row.names(env))
res_all<- cca(votu_matrix~.,data=env)
res_null<-cca(votu_matrix~1,data=env)
env_frwd<-ordistep(res_null,scope=formula(res_all),direction = 'forward',permutations = how(nperm = 199)) #fit model<0.05
env_frwd #no-linear envs

geo<-read.csv('H:/Tidal/R_data/Fig.4/Distance.csv',row.names = 1)  #calculate PCNM for geo
geo<-vegdist(geo,method = "euclidean")
geo<-pcnm(geo)
geo<-as.data.frame(geo$vectors)
res_all<- cca(votu_matrix~.,data=geo)
res_null<-cca(votu_matrix~1,data=geo)
env_frwd<-ordistep(res_null,scope=formula(res_all),direction = 'forward',permutations = how(nperm = 999)) #fit model<0.05
env_frwd #no-linear envs

write.csv(geo,file='H:/Tidal/R_data/Fig.4/PCNM.csv',row.names = T)


envdat_raw<-read.csv('H:/Tidal/R_data/Fig.4/Environment.csv',row.names = 1) #factors in no-linear geo/env/VMR
print(row.names(votu_matrix)==row.names(envdat_raw))
env<-envdat_raw[,1:10]
geo<-envdat_raw[,12:18]
cca_all<-varpart(votu_matrix,env,geo,chisquare = T) #obtain one or two factors common proportion
cca_all
decorana(votu_matrix)
