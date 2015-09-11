#FUNCTIONS:

bioNest.Sum<-function(x){
  convHA <- 10000/(pi*(c(13,20)^2))
  nestDBH <- c(20,99)
  nestDist <- c(9.9999,19.999)
  years <- rep(sort(unique(x$year)),3)
  sites <- rep(unique(x$site),each=length(years)/3)
  plotBio <- data.frame(years,sites)
  for(i in 1:2){
    plotHec <- aggregate(x$annAB[x$dbh<nestDBH[i] & x$dbh>nestDist[i]],by=list(x$year[x$dbh<nestDBH[i] & x$dbh>nestDist[i]],x$site[x$dbh<nestDBH[i] & x$dbh>nestDist[i]]),sum,na.rm=TRUE)
    names(plotHec) <- c('years',"sites",paste(c("annAB.ha",i),collapse=""))
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

bio.sp<-function(x){
  convHA <- 10000/((pi*(c(13,20)^2))*3)
  nestDBH <- c(20,99)
  nestDist <- c(9.9999,19.9999)
  years <- rep(sort(unique(x$year)),length(unique(x$species))*length(unique(x$site)))
  sites <- rep(unique(x$site),each=length(years)/3)
  species <- rep(rep(unique(x$species),each=length(years)/(3*length(unique(x$species)))),3)
  plotBio <- data.frame(years,sites,species)
  for(i in 1:2){
    plotHec <- aggregate(x$annAB[x$dbh<nestDBH[i] & x$dbh>nestDist[i]],by=list(x$year[x$dbh<nestDBH[i] & x$dbh>nestDist[i]],x$site[x$dbh<nestDBH[i] & x$dbh>nestDist[i]],x$species[x$dbh<nestDBH[i] & x$dbh>nestDist[i]]),sum,na.rm=TRUE)
    plotHec[,4] <- plotHec[,4]*convHA[i]
    names(plotHec) <- c('years',"sites","species",paste(c("annAB.ha",i),collapse=""))
    plotBio <- merge(plotBio,plotHec,by=intersect(names(plotBio),names(plotHec)),all.x=TRUE)
  }
 # plotBio <- data.frame(plotBio[,1:6],rowsum(plotBio[5:6],na.rm=TRUE))
  #names(plotBio)[7] <- "annAB.ha"
  return(plotBio)
}

bio.spAB<-function(x){
  convHA <- 10000/((pi*(c(13,20)^2))*2)
  nestDBH <- c(20,99)
  nestDist <- c(9.9999,19.9999)
  years <- rep(sort(unique(x$Year)),length(unique(x$Species))*length(unique(x$Site)))
  sites <- rep(unique(x$Site),each=length(years)/2)
  species <- rep(rep(unique(x$Species),each=length(years)/(2*length(unique(x$Species)))),2)
  plotBio <- data.frame(years,sites,species)
  for(i in 1:2){
    plotHec <- plotHec <- aggregate(x$AB[x$DBH<nestDBH[i] & x$DBH>nestDist[i]],by=list(x$Year[x$DBH<nestDBH[i] & x$DBH>nestDist[i]],x$Site[x$DBH<nestDBH[i] & x$DBH>nestDist[i]],x$Species[x$DBH<nestDBH[i] & x$DBH>nestDist[i]]),sum,na.rm=TRUE)
    plotHec[,4] <- plotHec[,4]*convHA[i]
    names(plotHec) <- c('years',"sites","species",paste(c("AB.ha",i),collapse=""))
    plotBio <- merge(plotBio,plotHec,by=intersect(names(plotBio),names(plotHec)),all.x=TRUE)
  }
  #plotBio <- data.frame(plotBio[,1:3],rowMeans(plotBio[4:6],na.rm=TRUE))
  #names(plotBio)[4] <- "annAB.ha"
  return(plotBio)
}

plotAB<-function(x){
  Total.ha<-aggregate(x$annAB,by=list(year=x$year),mean)
  colnames(Total.ha)<-c("year","AB.ha")
  Total.ha$year<-as.numeric(as.character(Total.ha$year))
  Total.ann<-(Total.ha$AB.ha[-1]-Total.ha$AB.ha[-length(Total.ha$AB.ha)])/(Total.ha$year[-1]-Total.ha$year[-length(Total.ha$year)])
  Total.ann<-c(Total.ann[1],Total.ann)
  Total.ha$annAB<-Total.ann
  
  return(Total.ha)
}
plotSp<-function(x){
  Total.ha<-aggregate(annAB~year+species,sum, data=x)
  colnames(Total.ha)<-c("year","species","AB.ha")
  Total.ha$year<-as.numeric(as.character(Total.ha$year))
  Total.ann<-(Total.ha$AB.ha[-1]-Total.ha$AB.ha[-length(Total.ha$AB.ha)])/(Total.ha$year[-1]-Total.ha$year[-length(Total.ha$year)])
  Total.ann<-c(Total.ann[1],Total.ann)
  Total.ha$annAB<-Total.ann
  
  return(Total.ha)
}

plotSp<-function(x){
  annAB<-(unlist(by(x$AB,x$unique,diff2)))/(unlist(by(x$year,x$unique,diff2)))
  return(annAB)
}

totAB<-function(x){
  Total.ha<-aggregate(x$AB,by=list(year=x$year),mean)
  colnames(Total.ha)<-c("year","AB")
  Total.ha$year<-as.numeric(as.character(Total.ha$year))
  return(Total.ha)
}

#resample raw intervals
rawAB<-function(x){
  rawTotal<-aggregate(x$AB,by=list(year=x$year),mean)
  colnames(rawTotal)<-c("year","AB")
  rawTotal$year<-as.numeric(as.character(rawTotal$year))
  rawAB<-(rawTotal$AB[-1]-rawTotal$AB[-length(rawTotal$AB)])
  rawAB<-c(rawAB[1],rawAB)
  rawTotal$rawAB<-rawAB
  return(rawTotal)
}

censusAB<-function(x){
  AB<-melt(data=x,id=c("plot"),measure=c("1989","1998","2002","2010"))
  colnames(AB)<-c("plot","year","AB")
  AB$AB.ha<-AB$AB*(10000/3750)
  AB$year<-as.numeric(as.character(AB$year))
  Total.ha<-aggregate(AB$AB.ha,by=list(year=AB$year),sum)
  colnames(Total.ha)<-c("year","AB.ha")
  Total.ann<-(Total.ha$AB.ha[-1]-Total.ha$AB.ha[-length(Total.ha$AB.ha)])/(Total.ha$year[-1]-Total.ha$year[-length(Total.ha$year)])
  Total.ann<-c(Total.ann[1],Total.ann)
  Total.ha$annAB<-Total.ann
  return(Total.ha)
}

censusAB<-function(x){
  AB<-melt(data=x,id=c("plot","species"),measure=c("1989","1998","2002","2010"))
  colnames(AB)<-c("plot","year","AB")
  AB$AB.ha<-AB$AB*(10000/3750)
  AB$year<-as.numeric(as.character(AB$year))
  Total.ha<-aggregate(AB$AB.ha,by=list(year=AB$year),sum)
  colnames(Total.ha)<-c("year","AB.ha")
  Total.ann<-(Total.ha$AB.ha[-1]-Total.ha$AB.ha[-length(Total.ha$AB.ha)])/(Total.ha$year[-1]-Total.ha$year[-length(Total.ha$year)])
  Total.ann<-c(Total.ann[1],Total.ann)
  Total.ha$annAB<-Total.ann
  return(Total.ha)
}




plotSp<-function(x){
  
}
  
  
  
  
