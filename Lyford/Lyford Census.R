Lyf.cen<-read.csv("Lyford raw census.csv",header=TRUE)
Lyf.cen$dbh69<-ifelse(Lyf.cen$can69=="NA", 0,Lyf.cen$dbh69) #run for AB
Lyf.cen$dbh69<-as.numeric(as.character(Lyf.cen$dbh69)) #run for AB
Lyf.cen$dbh11<-as.numeric(as.character(Lyf.cen$dbh11))
Lyf.cen[is.na(Lyf.cen)]<-0


#run for annAB only#
Lyf.cen$dbh75<-ifelse(Lyf.cen$dbh75==0 & Lyf.cen$dbh69>0,Lyf.cen$dbh69,Lyf.cen$dbh75)
Lyf.cen$dbh91<-ifelse(Lyf.cen$dbh91==0 & Lyf.cen$dbh75>0,Lyf.cen$dbh75,Lyf.cen$dbh91)
Lyf.cen$dbh01<-ifelse(Lyf.cen$dbh01==0 & Lyf.cen$dbh91>0,Lyf.cen$dbh91,Lyf.cen$dbh01)
Lyf.cen$dbh11<-ifelse(Lyf.cen$dbh11==0 & Lyf.cen$dbh01>0,Lyf.cen$dbh01,Lyf.cen$dbh11)
Lyf.cen<-subset(Lyf.cen,Lyf.cen$dbh69>=10|Lyf.cen$dbh75>=10|Lyf.cen$dbh91>=10|Lyf.cen$dbh01>=10|Lyf.cen$dbh11>10)


L.bm.eqs<-read.table("biomass_coeff.csv", header=T,sep=",")
L.bm.eqs.sp<-L.bm.eqs[L.bm.eqs$eq==1,]
lyf.AB<-merge(Lyf.cen,L.bm.eqs.sp,by="species")

lyf.AB$"1969"<-(lyf.AB$a*lyf.AB$dbh69^lyf.AB$b)*0.001
lyf.AB$"1975"<-(lyf.AB$a*lyf.AB$dbh75^lyf.AB$b)*0.001
lyf.AB$"1991"<-(lyf.AB$a*lyf.AB$dbh91^lyf.AB$b)*0.001
lyf.AB$"2001"<-(lyf.AB$a*lyf.AB$dbh01^lyf.AB$b)*0.001
lyf.AB$"2011"<-(lyf.AB$a*lyf.AB$dbh11^lyf.AB$b)*0.001

write.csv(lyf.AB, file="lyf.AB.csv")





lyfordAB<-function(x){
  Total.ha<-aggregate(x$annAB,by=list(year=x$year),mean)
  colnames(Total.ha)<-c("year","AB.ha")
  Total.ha$year<-as.numeric(as.character(Total.ha$year))
  Total.ann<-(Total.ha$AB.ha[-1]-Total.ha$AB.ha[-length(Total.ha$AB.ha)])/(Total.ha$year[-1]-Total.ha$year[-length(Total.ha$year)])
  Total.ann<-c(Total.ann[1],Total.ann)
  Total.ha$annAB<-Total.ann
  return(Total.ha)
}
lyf.plot<-melt(data=lyf.AB,id=c("block","species"),measure=c("1969","1975","1991","2001","2011"))
colnames(lyf.plot)<-c("plot","species","year","AB")
write.csv(lyf.plot,file="lyf.plot.csv")
lyf.plot$year<-as.numeric(as.character(lyf.plot$year))
lyf.plot<-aggregate(AB~year+plot,sum,data=lyf.plot)
lyf.plot$AB<-lyf.plot$AB*(10000/929)
lyf.sp.cen<-aggregate(AB~year+species,sum,data=lyf.plot)
lyf.sp.cen$AB<-lyf.sp.cen$AB*(10000/929)
write.csv(lyf.sp.cen,file="lyf.sp.cen.csv")


lyf.plot$annAB<-unlist(by(lyf.plot$AB,lyf.plot$plot,diff2))/unlist(by(lyf.plot$year,lyf.plot$plot,diff2))

write.csv(lyf.plot,file="plot.lyfAnn.csv")
plot.lyfAB<-subset(lyf.plot,lyf.plot$year!=1969)
write.csv(plot.lyfAB,file="plot.lyfAB.csv")
lyf.plot<-read.csv("Lyford Census AB by plot.csv",header=TRUE)



#random for annAB
randomLyf<-replicate(1000,lyfordAB(subset(lyf.plot,plot %in% sample(unique(lyf.plot$plot),4))))
randomUnlistyearL<-data.frame(year=unlist(randomLyf[1,]))
randomUnlistAB.annL<-data.frame(annAB=unlist(randomLyf[2,]))
LrandomDraws<-cbind(year=randomUnlistyearL,annAB=randomUnlistAB.annL)
LrandomDraws<-subset(LrandomDraws,LrandomDraws$year!=1969)
write.csv(LrandomDraws,file="Lyf Draws annAB.csv")


#for AB
lyfABdraws<-replicate(1000,totAB(subset(lyf.plot,plot %in% sample(unique(lyf.plot$plot),6))))
lyfAByear<-data.frame(year=unlist(lyfABdraws[1,]))
lyfUnlistAB<-data.frame(AB=unlist(lyfABdraws[2,]))
lyfABDraws<-cbind(year=lyfAByear,AB=lyfUnlistAB)
write.csv(lyfABDraws,file="Lyf Draws AB.csv")


