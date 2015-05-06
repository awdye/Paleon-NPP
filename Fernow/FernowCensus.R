#Script for dealing with Fernow USFS census data

fer.census<-read.csv("usfs_Fernow.csv",header=T)

#data clean up...only run 1st time
spp<-read.csv("spp codes.csv")
fer.census<-merge(fer.census,spp,by="species")
fer.census$species<-NULL
fer.census$common<-NULL
fer.census$DBH1979<-fer.census$DBH1979*2.54
fer.census$DBH1983<-fer.census$DBH1983*2.54
fer.census$DBH1989<-fer.census$DBH1989*2.54
fer.census$DBH1994<-fer.census$DBH1994*2.54
fer.census$DBH1999<-fer.census$DBH1999*2.54
fer.census$DBH2009<-fer.census$DBH2009*2.54
unique(fer.census$species)
write.csv(fer.census,file="Fernow Census_Alex.csv")

#import 
AB<-read.csv("fernowAB.csv",header=TRUE)
Total.ha<-read.csv("fernowAB at census dates.csv",header=TRUE)
species.ha<-read.csv("fernowAB by species.csv", header=TRUE)
#Mortality<-read.csv("Fernow mortality rates.csv", header=TRUE)

#analysis
fer.AB<-read.csv("Fernow Census_Alex.csv")
fer.AB<-subset(fer.AB,fer.AB$DBH1979>=10|fer.AB$DBH1983>=10|fer.AB$DBH1989>=10|fer.AB$DBH1994>=10|fer.AB$DBH1999>=10|fer.AB$DBH2009>=10)

#only for annAB:
fer.AB$DBH1983<-ifelse(fer.AB$DBH1983==0&fer.AB$DBH1979>0,fer.AB$DBH1979,fer.AB$DBH1983)
fer.AB$DBH1989<-ifelse(fer.AB$DBH1989==0&fer.AB$DBH1983>0,fer.AB$DBH1983,fer.AB$DBH1989)
fer.AB$DBH1994<-ifelse(fer.AB$DBH1994==0&fer.AB$DBH1989>0,fer.AB$DBH1989,fer.AB$DBH1994)
fer.AB$DBH1999<-ifelse(fer.AB$DBH1999==0&fer.AB$DBH1994>0,fer.AB$DBH1994,fer.AB$DBH1999)
fer.AB$DBH2009<-ifelse(fer.AB$DBH2009==0&fer.AB$DBH1999>0,fer.AB$DBH1999,fer.AB$DBH2009)


bm.eqs<-read.table("biomass_coeff.csv", header=T,sep=",")
bm.eqs.sp<-bm.eqs[bm.eqs$eq==1,]
fer.AB<-merge(fer.AB,bm.eqs.sp,by="species")

convha<-10000/2023

#allometric eqs of form 1
form1<-subset(fer.AB,fer.AB$form==1)
form1$"1979"<-(form1$a*form1$DBH1979^form1$b)*0.001
form1$"1983"<-(form1$a*form1$DBH1983^form1$b)*0.001
form1$"1989"<-(form1$a*form1$DBH1989^form1$b)*0.001
form1$"1994"<-(form1$a*form1$DBH1994^form1$b)*0.001
form1$"1999"<-(form1$a*form1$DBH1999^form1$b)*0.001
form1$"2009"<-(form1$a*form1$DBH2009^form1$b)*0.001



#allometric eqs of form 3:
form3<-subset(fer.AB,fer.AB$form==3)
e<-sum(1/factorial(1:100))
form3$"1979"<-log((form3$a+form3$b*(log(form3$DBH1979))))*0.001
form3$"1983"<-log((form3$a+form3$b*(log(form3$DBH1983))))*0.001
form3$"1989"<-log((form3$a+form3$b*(log(form3$DBH1989))))*0.001
form3$"1994"<-log((form3$a+form3$b*(log(form3$DBH1994))))*0.001
form3$"1999"<-log((form3$a+form3$b*(log(form3$DBH1999))))*0.001
form3$"2009"<-log((form3$a+form3$b*(log(form3$DBH2009))))*0.001

fer.AB<-rbind(form1,form3)
write.csv(fer.AB,file="fernow basic census bio.csv")
fer.AB<-read.csv("fernow basic census bio.csv",header=T)
colnames(fer.AB)[30:35]<-c("1979","1983","1989","1994","1999","2009")


#AB by plot
plot.fer<-melt(data=fer.AB,id=c("plot"),measure=c("1979","1983","1989","1994","1999","2009"))
colnames(plot.fer)<-c("plot","year","AB")
plot.fer$year<-as.numeric(as.character(plot.fer$year))
plot.fer<-aggregate(AB~year+plot,sum,data=plot.fer)
plot.fer$AB<-plot.fer$AB*convha
plot.fer$annAB<-(unlist(by(plot.fer$AB,plot.fer$plot,diff2)))/unlist(by(plot.fer$year,plot.fer$plot,diff2))

write.csv(plot.fer,file="plot.ferAnn.csv")
write.csv(plot.fer,file="plot.ferAB.csv")
plot.fer<-read.csv("plot.fer.csv",header=T)

#1 census plot=approximately dendro plots, so don't random sample like others
plot.fer<-subset(plot.fer, plot.fer$year!=1979)
plot.fer<-subset(plot.fer,plot.fer$plot!="B0")
plot.fer<-subset(plot.fer,plot.fer$plot!="A0")
ferMean<-aggregate(cbind(Mean.ann=plot.fer$annAB),by=list(year=plot.fer$year),mean)


library(reshape)

AB<-melt(data=AB,id=c("plot","species"),measure=c("1979","1983","1989","1994","1999","2009"))
colnames(AB)<-c("plot","species","year","AB")
AB$AB.ha<-AB$AB/2.63
AB$year<-as.numeric(as.character(AB$year))
write.csv(AB,file="fernowAB.csv")
#Total
Total.ha<-aggregate(AB$AB.ha,by=list(year=AB$year),sum)
colnames(Total.ha)<-c("year","AB.ha")
Total.ann<-(Total.ha$AB.ha[-1]-Total.ha$AB.ha[-length(Total.ha$AB.ha)])/(Total.ha$year[-1]-Total.ha$year[-length(Total.ha$year)])
Total.ann<-c(Total.ann[1],Total.ann)
Total.ha$annAB<-Total.ann
write.csv(Total.ha,file="fernowAB at census dates.csv")
Fer.census<-read.csv("fernowAB.csv",header=TRUE)



#by species:
fer.cen.sp<-aggregate(cbind(AB.ha,AB)~year+species+plot,sum,data=Fer.census)
colnames(species.ha)<-c("year","species","annAB","AB")
write.csv(fer.cen.sp,file="fer.sp.cen.csv")
fer.ACSA.cen<-subset(fer.cen.sp,fer.cen.sp$species=="ACSA")
write.csv(fer.ACSA.cen,file="fer.ACSA.cen.csv")
fer.QUMO.cen<-subset(fer.cen.sp,fer.cen.sp$species=="QUMO")
write.csv(fer.QUMO.cen,file="fer.QUMO.cen.csv")
fer.QUCO.cen<-subset(fer.cen.sp,fer.cen.sp$species=="QUCO")
write.csv(fer.QUCO.cen,file="fer.QUCO.cen.csv")
fer.TIAM.cen<-subset(fer.cen.sp,fer.cen.sp$species=="TIAM")
write.csv(fer.TIAM.cen,file="fer.TIAM.cen.csv")



