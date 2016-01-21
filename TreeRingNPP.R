library(dplR)
core.means<-coreMeans(core_recode=c(1,6),site_recode=c(1,3),tree_recode=c(4,6))

#core_recode: character strings of core name: e.g. HOW1001N is c(1,6)
#site_recode: character string of site name: above example is c(1,4). Cuts to "HOW1"
#tree_recode: character string of tree number: e.g. HOW1001 is c(4,6) and cuts to "001"

coreMeans<-function(core_recode,site_recode,tree_recode){
  files_to_read<-list.files()
  core.means.all<-NULL
  for (i in files_to_read){
    file_read<-read.rwl(i)
    assign(i,file_read)
    cores<-data.frame("code"=names(file_read)) 
    cores$coreID<- substr(as.character(cores$code),core_recode[1],core_recode[2])
    core.means<-data.frame("Year"=row.names(file_read))
    core.means<-NULL
    n=1
    file_read$Year<-row.names(file_read)
    for (j in unique(cores$coreID)){
      print(j)
      print(n)
      tree1<-data.frame(file_read[,c(as.character(cores$code[cores$coreID==j]),"Year")])
      if(dim(tree1)[2]>1){
        tree2<-data.frame(rowMeans(subset(tree1, select=-c(Year)),na.rm=T))
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
      names(tree2)<-c("meanRW","coreID","Year","rACUM","rACUM.inv")
      if(n==1)(core.means<-tree2)else(core.means<-rbind(core.means, tree2))
      n=n+1}
     core.means.all<-rbind(core.means.all,core.means)
  }
  core.means.all$Site<-substr(as.character(core.means.all$coreID),site_recode[1],site_recode[2])
  core.means.all$Tree<-as.numeric(as.character(substr(as.character(core.means.all$coreID),tree_recode[1],tree_recode[2])))
  return(core.means.all)
}


field.data<-read.csv("site.field.data.csv") #how,lyf,tow,fer
bm.eqs<-read.csv("biomass_coeff.csv")
#ab.lyf.all data frame includes everything(field data, core means, annual biomass increment, bio/ha)
ab.all<-merge(core.means,field.data, by=c("Site","Tree"))
bm.eqs.sp<-bm.eqs[bm.eqs$eq==1,]
ab.all<-merge(ab.all,bm.eqs.sp,by="Species")

#dbhest1==> dbh at the BEGINNING of the growing season
#****alternative code for Fernow at bottom of script
ab.all$dbhest1<-ab.all$DBH-0.2*(ab.all$rACUM.inv+ab.all$meanRW) #0.2 is a conversion from radius to diameter
ab.all$dbhest1[ab.all$dbhest1<0]<-NA
#dbhest2==> dbh at the END of the growing season
ab.all$dbhest2<-ab.all$DBH-0.2*(ab.all$rACUM.inv)
ab.all$dbhest2[ab.all$dbhest2<0]<-NA

#calculates total aboveground biomass(AB) of each tree for each year using allometric equation a*DBH^b
ab.all$AB<-(ab.all$a*ab.all$dbhest2^ab.all$b)*0.001 #0.001 is a conversion from kg to Mg. a and b from allometric equations.
ab.all$BAI<-((ab.all$dbhest2/2)^2*pi)-((ab.all$dbhest1/2)^2*pi)
#annAB==>biomass acculumated during the year.
# that is, biomass at the END of the growing season
# minus the biomass at the BEGINNING of the growing season
ab.all$annAB<-(ab.all$a*(ab.all$dbhest2^ab.all$b-ab.all$dbhest1^ab.all$b))*0.001


#For nested
bioSum<-bioNest.Sum(ab.all)
#bioSum<-subset(bioSum,bioSum$years<2013)
bioSum$NestSum<-bioSum$annAB.ha1+bioSum$annAB.ha2
meanAnn<-aggregate(NestSum~years,mean,na.rm=TRUE,data=bioSum)
colnames(meanAnn)<-c("year","meanAnn")
minAnn<-aggregate(NestSum~years,min,na.rm=TRUE,data=bioSum)
colnames(minAnn)<-c("year","minAnn")
maxAnn<-aggregate(NestSum~years,max,na.rm=TRUE,data=bioSum)
colnames(maxAnn)<-c("year","maxAnn")

#AB
bioSumAB<-bioNestAB(ab.all)
#bioSumAB<-subset(bioSumAB,bioSumAB$years<2013)
bioSumAB$NestSum<-bioSumAB$AB1+bioSumAB$AB2
meanAB<-aggregate(NestSum~years,mean,na.rm=TRUE,data=bioSumAB)
colnames(meanAB)<-c("year","meanAB")
minAB<-aggregate(NestSum~years,min,na.rm=TRUE,data=bioSumAB)
colnames(minAB)<-c("year","minAB")
maxAB<-aggregate(NestSum~years,max,na.rm=TRUE,data=bioSumAB)
colnames(maxAB)<-c("year","maxAB")


NPP<-data.frame(year=meanAnn$year,meanAnn=meanAnn$meanAnn,minAnn=minAnn$minAnn,maxAnn=maxAnn$maxAnn,
                meanAB=meanAB$meanAB,maxAB=maxAB$maxAB,minAB=minAB$minAB)
write.csv(NPP,file="site_NPP.csv")


#for species:
#bioSp<-bio.spAB(ab.hw.all) #for species aboveground biomass
#bioSp$AB<-rowSums(bioSpAB[,4:5],na.rm=TRUE)
#bioSp$AB[bioSp$AB==0]<-NA
#bioSpecies<-aggregate(AB~years+species+sites,sum,na.rm=TRUE,data=bioSp)
#write.csv(bioSpecies, file="bioSpPlot.csv")

#***CODE FOR FERNOW
#Do not run nested functions bioNest.Sum on Fernow-no nests were used in sampling design

#equation form  (M=a*b^D)
form1<-subset(ab.all,ab.all$form==1)
form1$AB<-form1$a*form1$dbhest2^form1$b*0.001*(10000/(pi*(10)^2))
form1$annAB<-form1$a*(form1$dbhest2^form1$b-form1$dbhest1^form1$b)*0.01*(10000/(pi*(10)^2))

#equation form
form3<-subset(ab.all,ab.all$form==3)
form3$AB<-log((form3$a+form3$b*(log(form3$dbhest2))))*0.001*(10000/(pi*(10)^2))
form3$annAB<-(log((form3$a+form3$b*(log(form3$dbhest2))))-log((form3$a+form3$b*(log(form3$dbhest1)))))*0.01*(10000/(pi*(10)^2))
ab.all<-rbind(form1,form3)
ab.all$annAB<-ab.all$annAB*(10000/(pi*(10)^2))

meanAnn<-aggregate(annAB~year,sum,na.rm=TRUE,data=ab.all)
maxAnn<-aggregate(annAB~year,max,na.rm=TRUE,data=ab.all)
minAnn<-aggregate(annAB~year,min,na.rm=TRUE,data=ab.all)
NPP<-data.frame(year=meanAnn$year,meanAnn=meanAnn$meanAnn,minAnn=minAnn$minAnn,maxAnn=maxAnn$maxAnn)

#bioSpecies<-aggregate(AB~years+species+sites,sum,na.rm=TRUE,data=ab.all)



#FUNCTIONS
bioNest.Sum<-function(x){
  convHA <- 10000/(pi*(c(13,20)^2))
  nestDBH <- c(20,99)
  nestDist <- c(9.9999,19.999)
  years <- rep(sort(unique(x$Year)),2)
  sites <- rep(unique(x$Site),each=length(years)/2)
  plotBio <- data.frame(years,sites)
  for(i in 1:2){
    plotHec <- aggregate(x$annAB[x$DBH<nestDBH[i] & x$DBH>nestDist[i]],by=list(x$Year[x$DBH<nestDBH[i] & x$DBH>nestDist[i]],x$Site[x$DBH<nestDBH[i] & x$DBH>nestDist[i]]),sum,na.rm=TRUE)
    names(plotHec) <- c("years","sites",paste(c("annAB.ha",i),collapse=""))
    plotHec[,3] <- plotHec[,3]*convHA[i]
    
    plotBio <- merge(plotBio,plotHec,by=intersect(names(plotBio),names(plotHec)),all.x=TRUE)
    
  }
  return(plotBio)
}
bioNestAB<-function(x){
  convHA <- 10000/(pi*(c(13,20)^2))
  nestDBH <- c(20,99)
  nestDist <- c(9.9999,19.999)
  years <- rep(sort(unique(x$Year)),2)
  sites <- rep(unique(x$Site),each=length(years)/2)
  plotBio <- data.frame(years,sites)
  for(i in 1:2){
    plotHec <- aggregate(x$AB[x$DBH<nestDBH[i] & x$DBH>nestDist[i]],by=list(x$Year[x$DBH<nestDBH[i] & x$DBH>nestDist[i]],x$Site[x$DBH<nestDBH[i] & x$DBH>nestDist[i]]),sum,na.rm=TRUE)
    names(plotHec) <- c('years',"sites",paste(c("AB",i),collapse=""))
    plotHec[,3] <- plotHec[,3]*convHA[i]
    
    plotBio <- merge(plotBio,plotHec,by=intersect(names(plotBio),names(plotHec)),all.x=TRUE)
    
  }
  return(plotBio)
}
