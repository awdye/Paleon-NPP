#original data set. Plots 3,4,5,6 removed due to extreme missing data. Dataset thus represents 2.75 ha
how.census<-read.csv("Howland Stem Map file_Alex.csv",header=T)
how.census[is.na(how.census)]<--99.0

#files to read:
how.census<-read.csv("Howland Census.csv",header=TRUE)
how.censusFill<-read.csv("all plots prorated DBH.csv",header=TRUE)
how.annAB<-read.csv("HOW census AB with Miss values.csv", header=TRUE)
how.compAB<-read.csv("HOW census AB full plots.csv")

#convert AB of dead trees (canopy code 7) to 0 to remove from analysis of living trees
how.census$DBH2010<-ifelse(how.census$Cnpy2010==7,0,how.census$DBH2010)
how.census$DBH2002<-ifelse(how.census$Cnpy2002==7,0,how.census$DBH2002)
how.census$DBH1998<-ifelse(how.census$Cnpy1998==7,0,how.census$DBH1998)
how.census$DBH1989<-ifelse(how.census$Cnpy1989==7,0,how.census$DBH1989)

how.census$DBH1998<-ifelse(how.census$DBH1989==0 &how.census$DBH1998==-99.0,0,how.census$DBH1998)
how.census$DBH2002<-ifelse(how.census$DBH1998==0 &how.census$DBH2002==-99.0,0,how.census$DBH2002)
how.census$DBH2010<-ifelse(how.census$DBH1998==0 &how.census$DBH2010==-99.0,0,how.census$DBH2010)

#fill in missing values for '98 and '02-if dead in 89 and '10, also dead years in between 
how.census$DBH2002<-ifelse(how.census$DBH2010==0 & how.census$DBH1989==0,0,how.census$DBH2002)
how.census$DBH1998<-ifelse(how.census$DBH2010==0 & how.census$DBH1989==0,0,how.census$DBH1998)
how.census$Cnpy2002<-ifelse(how.census$Cnpy2010==7 & how.census$Cnpy1989==7,7,how.census$Cnpy2002)
how.census$Cnpy1998<-ifelse(how.census$Cnpy2010==7 & how.census$Cnpy1989==7,7,how.census$Cnpy1998)

#fill in missing values with 0 for trees that were living but are now dead
how.census$DBH2002<-ifelse(how.census$Cnpy2010==7 & how.census$DBH2002==-99.0,0,how.census$DBH2002)
how.census$DBH1998<-ifelse(how.census$Cnpy2010==7 & how.census$DBH1998==-99.0,0,how.census$DBH1998)



#fill in 0 for dbh of trees marked "gone"
how.census$DBH2010<-ifelse(how.census$Notes=="gone"|how.census$Notes=="missing"|how.census$Notes=="not found"|how.census$Notes=="gone/missing"|how.census$Notes=="missing or untagged"|how.census$Notes=="no tag"|how.census$Notes=="could not find"|how.census$Notes=="standing dead/no tag"|how.census$Notes=="standing dead"|how.census$Notes=="down and dead"|how.census$Notes=="dead and down" & how.census$DBH2010==-99.0,0,how.census$DBH2010)
how.census$DBH2002<-ifelse(how.census$Notes=="gone"|how.census$Notes=="missing" |how.census$Notes=="not found"|how.census$Notes=="gone/missing"|how.census$Notes=="missing or untagged"|how.census$Notes=="no tag"|how.census$Notes=="could not find"|how.census$Notes=="standing dead/no tag"|how.census$Notes=="standing dead"|how.census$Notes=="down and dead"|how.census$Notes=="dead and down"& how.census$DBH2002==-99.0,0,how.census$DBH2002)
how.census$DBH1998<-ifelse(how.census$Notes=="gone"|how.census$Notes=="missing" |how.census$Notes=="not found"|how.census$Notes=="gone/missing"|how.census$Notes=="missing or untagged"|how.census$Notes=="no tag"|how.census$Notes=="could not find"|how.census$Notes=="standing dead/no tag"|how.census$Notes=="standing dead"|how.census$Notes=="down and dead"|how.census$Notes=="dead and down" & how.census$DBH1998==-99.0,0,how.census$DBH1998)

how.census$DBH2010<-as.numeric(as.integer(how.census$DBH2010))
how.census$DBH2002<-as.numeric(as.integer(how.census$DBH2002))
how.census$DBH1998<-as.numeric(as.integer(how.census$DBH1998))
how.census$DBH1989<-as.numeric(as.integer(how.census$DBH1989))
how.census<-as.data.frame(how.census)
write.csv(how.census,file="census Cleanup1.csv")

#file to read in to start analysis:
how.census<-read.csv("census Cleanup1.csv",header=TRUE)
levels(how.census$species)[levels(how.census$species)=="-99"]<-"unknown"
levels(how.census$species)[levels(how.census$species)==""]<-"unknown"
levels(how.census$species)[levels(how.census$species)=="4"]<-"unknown"


#complete in last time step
#prorate missing values in between, assuming constant annual growth between time steps
how.prorate<-subset(how.census,how.census$DBH2010!=-99)
how.prorate$DBH2002<-ifelse(how.prorate$DBH2002==-99&how.prorate$DBH1998!=-99,(how.prorate$DBH1998+
         ((how.prorate$DBH2010-how.prorate$DBH1998)/12)*4),how.prorate$DBH2002)
how.prorate$DBH1998<-ifelse(how.prorate$DBH1998==-99 & how.prorate$DBH2002!=-99,(how.prorate$DBH1989+
         ((how.prorate$DBH2002-how.prorate$DBH1989)/13)*9),how.prorate$DBH1998)
how.prorate$DBH1998<-ifelse(how.prorate$DBH1998==-99,(how.prorate$DBH1989+
        ((how.prorate$DBH2010-how.prorate$DBH1989)/21)*9),how.prorate$DBH1998)
how.prorate$DBH2002<-ifelse(how.prorate$DBH2002==-99,(how.prorate$DBH1998+
        ((how.prorate$DBH2010-how.prorate$DBH1998)/12)*4),how.prorate$DBH2002)



#taking average DBH increment for trees of each species/size class to interpolate missing end values
how.prorate$inc10<-(how.prorate$DBH2010-how.prorate$DBH2002)/how.prorate$DBH2002
how.prorate$inc02<-(how.prorate$DBH2002-how.prorate$DBH1998)/how.prorate$DBH1998
how.prorate$inc98<-(how.prorate$DBH1998-how.prorate$DBH1989)/how.prorate$DBH1989
how.prorate$size98<-cut(how.prorate$DBH1998,breaks=c(0,9.99999,12.99999,19.99999,24.99999,29.99999,39.9999,99.999),right=T,
                        labels=c(1,2,3,4,5,6,7),include.lowest=TRUE)
how.prorate$size02<-cut(how.prorate$DBH2002,breaks=c(0,9.99999,12.99999,19.99999,24.99999,29.99999,39.9999,99.999),right=T,
                        labels=c(1,2,3,4,5,6,7),include.lowest=TRUE)
how.prorate$size10<-cut(how.prorate$DBH2010,breaks=c(0,9.99999,12.99999,19.99999,24.99999,29.99999,39.9999,99.999),right=T,
                        labels=c(1,2,3,4,5,6,7),include.lowest=TRUE)

prorate.agg98<-aggregate(inc98~species+size98,mean,data=how.prorate)
prorate.agg98$inc98[is.infinite(prorate.agg98$inc98)]<-0

prorate.agg02<-aggregate(inc02~species+size02,mean,data=how.prorate)
prorate.agg02$inc02[is.infinite(prorate.agg02$inc02)]<-0
prorate.agg$inc02[is.infinite(prorate.agg$inc02)]<-0

prorate.agg10<-aggregate(inc10~species+size10,mean,data=how.prorate)
prorate.agg10$inc10[is.infinite(prorate.agg10$inc10)]<-0


#correct missing in final time step
how.lastTime<-subset(how.census,how.census$DBH2010==-99)
how.lastTime$size98<-cut(how.lastTime$DBH1989,breaks=c(0,9.99999,12.99999,19.99999,24.99999,29.99999,39.9999,99.999),right=T,
                         labels=c(1,2,3,4,5,6,7),include.lowest=TRUE)
how.lastTime<-merge(how.lastTime,prorate.agg98,by=c("species","size98"))
how.lastTime$DBH1998<-ifelse(how.lastTime$DBH1998==-99,how.lastTime$DBH1989+(how.lastTime$DBH1989*how.lastTime$inc98),
                             how.lastTime$DBH1998)

how.lastTime$size02<-cut(how.lastTime$DBH1998,breaks=c(0,9.99999,12.99999,19.99999,24.99999,29.99999,39.9999,99.999),right=T,
                         labels=c(1,2,3,4,5,6,7),include.lowest=TRUE)
how.lastTime<-merge(how.lastTime,prorate.agg02,by=c("species","size02"))
how.lastTime$DBH2002<-ifelse(how.lastTime$DBH2002==-99,how.lastTime$DBH1998+(how.lastTime$DBH1998*how.lastTime$inc02),
                               how.lastTime$DBH2002)

how.lastTime$size10<-cut(how.lastTime$DBH2002,breaks=c(0,9.99999,12.99999,19.99999,24.99999,29.99999,39.9999,99.999),right=T,
                         labels=c(1,2,3,4,5,6,7),include.lowest=TRUE)
how.lastTime<-merge(how.lastTime,prorate.agg10,by=c("species","size10"))
how.lastTime$DBH2010<-ifelse(how.lastTime$DBH2010==-99,how.lastTime$DBH2002+(how.lastTime$DBH2002*how.lastTime$inc10),
                             how.lastTime$DBH2010)

how.censusFill<-rbind(how.prorate,how.lastTime)
write.csv(how.censusFill,file="all plots prorated DBH.csv")
how.censusFill<-read.csv("all plots prorated DBH.csv",header=TRUE)


#fill in missing canopy statuses
how.censusFill<-subset(how.censusFill,how.censusFill$DBH1989>=10|how.censusFill$DBH1998>=10|how.censusFill$DBH2002>=10)


how.censusFill$Cnpy1998<-ifelse(how.censusFill$Cnpy1998==-99,how.censusFill$Cnpy1989*1,how.censusFill$Cnpy1998)
how.censusFill$Cnpy2002<-ifelse(how.censusFill$Cnpy2002==-99,how.censusFill$Cnpy1998*1,how.censusFill$Cnpy2002)
how.censusFill$Cnpy2010<-ifelse(how.censusFill$Cnpy2010==-99,how.censusFill$Cnpy2002*1,how.censusFill$Cnpy2010)


#calculate AB for each time point
#do for how.censusFill(all measurements prorated for missing values)
#and how.complete(3 plots complete through all measurements)

bm.eqs<-read.table("biomass_coeff.csv", header=T,sep=",")
bm.eqs.sp<-bm.eqs[bm.eqs$eq==1,]
how.AB<-how.censusFill
how.AB$DBH1989<-as.numeric(as.character(how.AB$DBH1989))
how.AB$DBH1998<-as.numeric(as.character(how.AB$DBH1998))
how.AB$DBH2002<-as.numeric(as.character(how.AB$DBH2002))
how.AB$DBH2010<-as.numeric(as.character(how.AB$DBH2010))

#only do for annAB:
how.AB$DBH1998<-ifelse(how.AB$DBH1998==0&how.AB$DBH1989>0,how.AB$DBH1989,how.AB$DBH1998)
how.AB$DBH2002<-ifelse(how.AB$DBH2002==0&how.AB$DBH1998>0,how.AB$DBH1998,how.AB$DBH2002)
how.AB$DBH2010<-ifelse(how.AB$DBH2010==0&how.AB$DBH2002>0,how.AB$DBH2002,how.AB$DBH2010)

how.AB<-merge(how.AB,bm.eqs.sp,by="species")

how.AB$"1989"<-(how.AB$a*how.AB$DBH1989^how.AB$b)*0.001
how.AB$"1998"<-(how.AB$a*how.AB$DBH1998^how.AB$b)*0.001
how.AB$"2002"<-(how.AB$a*how.AB$DBH2002^how.AB$b)*0.001
how.AB$"2010"<-(how.AB$a*how.AB$DBH2010^how.AB$b)*0.001
write.csv(how.AB,file="censusAB.csv")


how.annAB<-censusAB(how.AB)
write.csv(how.annAB,file="HOW census AB with Miss values.csv")



#AB by plot
how.AB<-read.csv("HowcensusAB.csv",header=TRUE)
colnames(how.AB)[30:33]<-c("1989","1998","2002","2010")

plot.cen<-melt(data=how.AB,id=c("plot","species"),measure=c("1989","1998","2002","2010"))

plot.sp<-melt(data=how.AB,id=c("plot","species"),measure=c("1989","1998","2002","2010"))
colnames(plot.cen)<-c("plot","species","year","AB")
colnames(plot.outer)<-c("plot","species","year","AB")
colnames(plot.sp)<-c("plot","species","year","AB")
plot.cen$AB<-plot.cen$AB*(10000/625)
plot.outer$AB<-plot.outer$AB*(10000/625)
plot.sp$AB<-plot.sp$AB*(10000/625)
plot.cen$year<-as.numeric(as.character(plot.cen$year))
plot.outer$year<-as.numeric(as.character(plot.outer$year))
plot.sp$year<-as.numeric(as.character(plot.sp$year))
plot.cen<-aggregate(AB~year+plot,sum,data=plot.cen)
plot.outer<-aggregate(AB~year+plot,sum,data=plot.outer)
plot.sp<-aggregate(AB~year+species+plot,sum,data=plot.sp)
plot.cen$annAB<-(unlist(by(plot.cen$AB,plot.cen$plot,diff2)))/unlist(by(plot.cen$year,plot.cen$plot,diff2))
plot.outer$annAB<-(unlist(by(plot.outer$AB,plot.outer$plot,diff2)))/unlist(by(plot.outer$year,plot.outer$plot,diff2))
plot.cenAnn<-subset(plot.cen,plot.cen$year!=1989)
write.csv(plot.cenAnn,file="plot.cenAnn.csv")
plot.cenAB<-data.frame(year=plot.cen$year,plot=plot.cen$plot,AB=plot.cen$AB)
write.csv(plot.cenAB,file="plot.cenAB.csv")



sp.ident<-seq(1,239)
sp.rep<-rep(sp.ident,each=4)
plot.sp$unique<-sp.rep
plot.sp$annAB<-(unlist(by(plot.sp$AB,plot.sp$unique,diff2)))/unlist(by(plot.sp$year,plot.sp$unique,diff2))
plot.sp$unique<-NULL
plot.sp<-subset(plot.sp,plot.sp$year!=1989)
write.csv(plot.sp, file="How census Sp.csv")
plot.sp<-read.csv("How census Sp.csv",header=TRUE)

#randomly sample same area as dendro
plot.cen<-read.csv("Howland census by plot.csv", header=TRUE)
#6 subplots
randomCen<-replicate(10000,plotAB(subset(plot.cen,plot %in% sample(unique(plot.cen$plot),6))))
randomUnlistyear<-data.frame(year=unlist(randomCen[1,]))
randomUnlistAB.ann<-data.frame(annAB=unlist(randomCen[2,]))
randomDraws<-cbind(year=randomUnlistyear,AB.ha=randomUnlistAB.ann)
randomSeq<-seq(1,10000)
randomRep<-rep(randomSeq,each=3)
randomDraws$unique<-randomRep

#random resample for total AB
randomAB<-replicate(10000,totAB(subset(plot.cenAB,plot %in% sample(unique(plot.cenAB$plot),6))))
randomAByear<-data.frame(year=unlist(randomAB[1,]))
randomUnlistAB<-data.frame(AB=unlist(randomAB[2,]))
randomABDraws<-cbind(year=randomAByear,AB=randomUnlistAB)
randomDraws$AB<-randomABDraws$AB
write.csv(randomDraws, file="HowDraws.csv")

