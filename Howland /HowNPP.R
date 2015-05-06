#Script for Howland tree ring NPP

library(dplR)
library(sp)
library(foreign)
library(lattice)
library(ggplot2)
library(sciplot)
library(data.table)


setwd("C:/Paleon/Howland/NPP/Howland-biomass")
hw.field.data<-read.table("HWall_FieldData.csv", header=T,sep=",")


bm.eqs<-read.table("biomass_coeff.csv", header=T,sep=",")

dim(unique(subset(hw.field.data, select=trees, site=="HOW1")))
unique(subset(ab.hw.all, select=trees, site=="HOW1"))
subset(hw.field.data, site=="HOW1")


setwd("C:/Paleon/Howland/NPP/Howland-biomass/RawRW_Data_by_sp")
files_to_read<-list.files()
# files_to_read<-"PM_TulipPoplar.rw"

core.means.all<-NULL
for (i in files_to_read){
  file_read<-read.rwl(i)
  assign(i,file_read)
  
  cores<-data.frame("code"=names(file_read))
  cores$trees<- substr(as.character(cores$code),1,7)
  core.means<-data.frame("year"=row.names(file_read))
  core.means<-NULL
  n=1
  file_read$year<-row.names(file_read)
  for (j in unique(cores$trees)){
    print(j)
    print(n)
    tree1<-data.frame(file_read[,c(as.character(cores$code[cores$trees==j]),"year")])
    if(dim(tree1)[2]>1){
      tree2<-data.frame(rowMeans(subset(tree1, select=-c(year)),na.rm=T))
    } else {tree2=tree1}
    tree2<-na.omit(tree2)
    tree2$tree<-j
    tree2$year<-as.numeric(as.character(row.names(tree2)))
    tree2$rACUM<-0
    tree2$rACUM.inv<-0
    for(k in 1:dim(tree2)[1]){
      tree2$rACUM[k]<-sum(tree2[c(1:k),1])
      tree2$rACUM.inv[k-1]<-sum(tree2[c((k):(dim(tree2)[1])),1])
    }
    names(tree2)<-c("meanRW","trees","year","rACUM","rACUM.inv")
    if(n==1)(core.means<-tree2)else(core.means<-rbind(core.means, tree2))
    n=n+1}
  plot(core.means$rACUM.inv~core.means$year, type="p")
  title(i)
  
  core.means.all<-rbind(core.means.all,core.means)
}
core.means.all$site<-substr(as.character(core.means.all$trees),1,4)
dim(unique(subset(core.means.all, select=trees, site=="HOW1")))


core.means.all$trees<-as.numeric(as.character(substr(as.character(core.means.all$trees),5,7)))
write.csv(core.means.all,file="core.means.all.csv")
ab.hw.all<-merge(core.means.all,hw.field.data, by=c("site","trees"))
bm.eqs.sp<-bm.eqs[bm.eqs$eq==1,]
ab.hw.all<-merge(ab.hw.all,bm.eqs.sp,by="species")

#dbhest1==> dbh at the BEGINNING of the growing season
ab.hw.all$dbhest1<-ab.hw.all$dbh-0.2*(ab.hw.all$rACUM.inv+ab.hw.all$meanRW)
warnings()
ab.hw.all$dbhest1[ab.hw.all$dbhest1<0]<-NA
#dbhest2==> dbh at the END of the growing season
ab.hw.all$dbhest2<-ab.hw.all$dbh-0.2*(ab.hw.all$rACUM.inv)
ab.hw.all$dbhest2[ab.hw.all$dbhest2<0]<-NA

#equation form  (M=a*b^D)
ab.hw.all$AB<-ab.hw.all$a*ab.hw.all$dbhest2^ab.hw.all$b*0.001
#write.csv(ab.hw.all, file="dendroAB")

#annAB==>biomass acculumated during the year.
# that is, biomass at the END of the growing season
# minus the biomass at the BEGINNING of the growing season
ab.hw.all$annAB<-ab.hw.all$a*(ab.hw.all$dbhest2^ab.hw.all$b-ab.hw.all$dbhest1^ab.hw.all$b)
                   
write.csv(ab.hw.all,file="ab.hw.all.csv")
ab.hw.all<-read.csv("ab.hw.all.csv",header=TRUE)




#function adding annAB and AB of inner and outer nests 
bioAnn<-bioNest(ab.hw.all)
bioAB<-bioNestAB(ab.hw.all)

bioAnn$NestSum<-bioAnn$annAB.ha1+bioAnn$annAB.ha2
bioAB$ABnestSum<-bioAB$AB1+bioAB$AB2
bioAnn.year<-aggregate(bioAnn$NestSum,list(year=bioAnn$years),mean, na.rm=TRUE)
bioAB.year<-aggregate(bioAB$ABnestSum,list(year=bioAB$years),mean,na.rm=TRUE)
colnames(bioAnn.year)<-c("year","annAB")
colnames(bioAB.year)<-c("year","AB")
#HowNPPnest$AB<-bioAB.year$AB

HOW1<-subset(bioAnn,bioAnn$sites=="HOW1")
HOW2<-subset(bioAnn, bioAnn$sites=="HOW2")
HOW3<-subset(bioAnn, bioAnn$sites=="HOW3")
bioAnn.Plot<-data.frame(year=HOW1$years,How1Mean=HOW1$NestSum,How2Mean=HOW2$NestSum,How3Mean=HOW3$NestSum)
bioAnn.Plot$maxMean<-apply(bioAnn.Plot[,2:4],1,max,na.rm=TRUE)
bioAnn.Plot$minMean<-apply(bioAnn.Plot[,2:4],1,min,na.rm=TRUE)
write.csv(bioAnn.Plot,file="plot annAB summed not meaned.csv")

HOW1AB<-subset(bioAB,bioAB$sites=="HOW1")
HOW2AB<-subset(bioAB,bioAB$sites=="HOW2")
HOW3AB<-subset(bioAB,bioAB$sites=="HOW3")
bioAB.plot<-data.frame(year=HOW1AB$years,How1MeanAB=HOW1AB$ABnestSum,How2MeanAB=HOW2AB$ABnestSum,
                       How3MeanAB=HOW3AB$ABnestSum)
bioAB.plot$maxAB<-apply(bioAB.plot[,2:4],1, max, na.rm=TRUE)
bioAB.plot$minAB<-apply(bioAB.plot[,2:4],1,min,na.rm=TRUE)
HowNPPplot<-merge(HowNPPplot,bioAB.plot,by="year")
HowNPPplot$meanAnn<-HowNPPnest$annAB
HowNPPplot$meanAB<-HowNPPnest$AB
write.csv(HowNPPplot,file="HowNPP.csv")


#species
bioSp<-bio.sp(ab.hw.all)
bioSpAB<-bio.spAB(ab.hw.all)
bioSp$annAB<-rowSums(bioSp[,4:5],na.rm=TRUE)
bioSpAB$AB<-rowSums(bioSpAB[,4:5],na.rm=TRUE)
bioSp$annAB[bioSp$annAB==0]<-NA
bioSpAB$AB[bioSpAB$AB==0]<-NA
bioSp<-subset(bioSp,bioSp$years>=1960)
bioSpAB<-subset(bioSpAB,bioSpAB$years>=1960)
bioSp$AB<-bioSpAB$AB

bioSpecies<-aggregate(cbind(AB,annAB)~years+species,sum,na.rm=TRUE,data=bioSp)
how.sp.plot<-aggregate(cbind(annAB,AB)~years+species+sites,sum,na.rm=TRUE,data=bioSp)
write.csv(how.sp.plot,file="how.sp.plot.csv")
sp.max<-aggregate(annAB~years+species,max,na.rm=TRUE,data=bioSp)
sp.max$annAB<-sp.max$annAB*3
maxAB<-aggregate(AB~years+species,max,na.rm=TRUE,data=bioSp)
maxAB$AB<-maxAB$AB*3
sp.min<-aggregate(annAB~years+species,min,na.rm=TRUE,data=bioSp)
sp.min$annAB<-sp.min$annAB*3
minAB<-aggregate(AB~years+species,min,na.rm=TRUE,data=bioSp)
minAB$AB<-minAB$AB*3

bioSpecies<-data.frame(years=sp.max$years,species=sp.max$species,meanAnn=bioSpecies$annAB,
                       maxAnn=sp.max$annAB,minAnn=sp.min$annAB,meanAB=bioSpecies$AB,maxAB=maxAB$AB,minAB=minAB$AB)

write.csv(bioSpecies,file="bioSpecies.csv")


#read in files ab.hw.all.csv, How nested NPP.csv, and HOW nested NPP by plot.csv.....won't have to re run analysis
#every time
ab.hw.all<-read.csv("ab.hw.all.csv",header=TRUE)
HowNPPnest<-read.csv("How nested NPP.csv",header=TRUE)
HowNPPplot<-read.csv("How nested NPP by plot.csv",header=TRUE)








#