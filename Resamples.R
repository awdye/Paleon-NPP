#randomly sample same area as dendro
plot.cen<-read.csv("Site Census.csv", header=TRUE) #Howland, Fernow

randomCen<-replicate(10000,plotAB(subset(plot.cen,plot %in% sample(unique(plot.cen$plot),6)))) #enter desired number of suplots
randomUnlistyear<-data.frame(year=unlist(randomCen[1,]))
randomUnlistAB.ann<-data.frame(annAB=unlist(randomCen[2,]))
randomDraws<-cbind(year=randomUnlistyear,AB.ha=randomUnlistAB.ann)

randomSeq<-seq(1,10000)
randomRep<-rep(randomSeq,each=3)
randomDraws$unique<-randomRep


plotAB<-function(x){
  Total.ha<-aggregate(annAB~year,mean,data=x)
  colnames(Total.ha)<-c("year","AB.ha")
  Total.ha$AB.ha<-Total.ha$AB.ha*(10000/(8*(pi*100))) #adjust to plot size
  Total.ha$year<-as.numeric(as.character(Total.ha$year))
  Total.ann<-(Total.ha$AB.ha[-1]-Total.ha$AB.ha[-length(Total.ha$AB.ha)])/(Total.ha$year[-1]-Total.ha$year[-length(Total.ha$year)])
  Total.ann<-c(Total.ann[1],Total.ann)
  Total.ha$annAB<-Total.ann
  return(Total.ha)
}