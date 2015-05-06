
#Script for plotting biomass and census data

bioSp<-read.csv("bioSpecies.csv")
bioSpPlot<-read.csv()
field.all<-read.csv("dendro size distributions.csv")
bioSpPlot<-read.csv("bioSpPlot.csv")
cenDist<-read.csv("census size distributions.csv")
CenSpAll2<-read.csv("CenSpAll2.csv")
howCen<-read.csv("censusAB.csv",header=TRUE)

#HOWLAND
randomStats<-read.csv("Howland census Stats.csv", header=TRUE)
HowNPP<-read.csv("HowNPP.csv",header=TRUE)
HowDrawsAnn<-read.csv("HowDraws.csv",header=TRUE)
HowDrawsAB<-read.csv("How Draws AB.csv",header=TRUE)
plot.cenAnn<-read.csv("plot.cenAnn.csv",header=TRUE)
plot.cenAB<-read.csv("plot.cenAB.csv",header=TRUE)
plot.sp<-read.csv("How census Sp.csv",header=TRUE)
How.sp<-read.csv("bioSpecies.csv",header=TRUE)


MCtheme<-theme_bw()+theme(panel.border=element_blank(),plot.title=element_text(size=20),axis.title.x=element_text(size=15),
                          axis.title.y=element_text(size=15),
                          panel.background=element_blank(),axis.line=element_line(color="black"))

#annAB
HowAnn<-ggplot(data=HowNPP,aes(year,meanAnn))+
  scale_x_continuous(limits=c(1995,2013))+
  ylab("")+
  xlab("")+
  labs(title="HOW NPP")+
  geom_violin(data=HowDraws6,aes(year,annAB,group=year),alpha=0.5,fill="burlywood")+
  annotate("text",x=1996,y=3.4,label="1998")+
  annotate("text",x=2000,y=3.3,label="2002")+
  annotate("text",x=2008,y=3.9,label="2010")+
  #stat_summary(data=plot.cenAnn,aes(year,annAB,group=year),fun.data=ErrorB, geom="errorbar",width=1)+
 # geom_point(data=howsubMeanAnn,aes(year,annAB))+
  geom_boxplot(data=plot.cenAnn,aes(year,annAB,group=year),width=.7,fill=NA)+
  geom_line()+
  #geom_text(data=HowDraws8,aes(year,annAB,label=sd(annAB)))+
  #stat_summary(aes(label=..y..), fun.y=sd, geom="text", size=8)
  #geom_line(aes(year,maxMean))+
  #geom_line(aes(year,minMean))+
  geom_ribbon(aes(ymin=minMean,ymax=maxMean),alpha=0.2,fill="forestgreen")+
  theme_bw()+theme(panel.border=element_blank(),plot.title=element_text(size=20),axis.title.x=element_text(size=15),
                   axis.title.y=element_text(size=15),
                   panel.background=element_blank(),axis.line=element_line(color="black"))


#total AB
HowTot<-ggplot(data=HowNPP,aes(year,meanAB))+
  scale_x_continuous(limits=c(1985,2013))+
  ylab("Aboveground biomass (Mg/ha)")+
  xlab("Year")+
  labs(title="HOW AB",x="",y="")+
  geom_violin(data=HowDrawsAB,aes(year,AB,group=year),alpha=0.7,fill="burlywood")+
  annotate("text",x=1987,y=80,label="1989")+
  annotate("text",x=1996.5,y=95,label="1998")+
  annotate("text",x=2000.5,y=95,label="2002")+
  annotate("text",x=2008.2,y=101,label="2010")+
  #stat_summary(data=plot.cen,aes(year,AB,group=year),fun.data=ErrorB, geom="errorbar",width=1)+
  #geom_point(data=howsubMean,aes(year,AB))+
  geom_boxplot(data=plot.cenAB,aes(year,AB,group=year),width=0.7,fill=NA)+
  geom_line()+
  #geom_line(aes(year,maxAB))+
  #geom_line(aes(year,minAB))+
  geom_ribbon(aes(ymin=minAB,ymax=maxAB),alpha=0.2,fill="forestgreen")+
  MCtheme


#LYFORD

Lyf.randomStats<-read.csv("Lyford census Stats.csv",header=TRUE)
LyfordNPP<-read.csv("Lyford NPP.csv",header=TRUE)
plot.lyfAnn<-read.csv("plot.lyfAnn.csv",header=TRUE)
plot.lyfAB<-read.csv("plot.lyfAB.csv",header=TRUE)
LyfDrawsAnn<-read.csv("Lyf Draws annAB.csv",header=TRUE)
LyfDrawsAB<-read.csv("Lyf Draws AB.csv",header=TRUE)

LyfAnn<-ggplot(data=LyfordNPP,aes(year,meanAnn))+
  scale_x_continuous(limits=c(1970,2017))+
  ylab("NPP(Mg/ha/yr)")+
  xlab("Year")+
  labs(title="LYF NPP")+
  annotate("text",x=1972,y=2.1,label="1975")+
  annotate("text",x=1987,y=2.1,label="1991")+
  annotate("text",x=1997,y=2.1,label="2001")+
  annotate("text",x=2008,y=2.2,label="2011")+
  geom_violin(data=LyfDrawsAnn,aes(year,annAB,group=year),alpha=0.5,fill="burlywood")+
  geom_boxplot(data=plot.lyfAnn,aes(year,annAB,group=year),width=1.5,fill=NA)+
  geom_line()+
  #geom_line(aes(year,maxAnn))+
  #geom_line(aes(year,minAnn))+
  #stat_summary(data=plot.lyfAnn,aes(year,annAB,group=year),fun.data=ErrorB, geom="errorbar",width=2)+
  #geom_point(data=lyfsubMeanAnn,aes(year,annAB))+
  geom_ribbon(aes(ymin=minAnn,ymax=maxAnn),alpha=0.2,fill="forestgreen")+
 MCtheme

#total AB
LyfTot<-ggplot(data=LyfordNPP,aes(year,meanAB))+
  scale_x_continuous(limits=c(1960,2017))+
  ylab("Aboveground biomass (Mg/ha)")+
  xlab("Year")+
  labs(title="LYF AB",x="")+
  #geom_violin(data=LyfDrawsAB,aes(year,AB,group=year),alpha=0.7,fill="burlywood")+
  annotate("text",x=1965,y=160,label="1969")+
  annotate("text",x=1972,y=225,label="1975")+
  annotate("text",x=1988,y=255,label="1991")+
  annotate("text",x=1998,y=300,label="2001")+
  annotate("text",x=2008,y=330,label="2011")+
  geom_violin(data=LyfDrawsAB,aes(year,AB,group=year),alpha=0.5,fill="burlywood")+
  geom_boxplot(data=lyf.plot,aes(year,AB,group=year),width=1.5,outlier.size=0,fill=NA)+
  geom_line()+
  #geom_line(aes(year,maxAB))+
  #geom_line(aes(year,minAB))+
  #stat_summary(data=lyf.plot,aes(year,AB,group=year),fun.data=ErrorB, geom="errorbar",width=2)+
  geom_ribbon(aes(ymin=minAB,ymax=maxAB),alpha=0.2,fill="forestgreen")+
  #geom_point(data=lyfsubMeanAnn,aes(year,AB))+
 MCtheme



#FERNOW
fer.field.data<-read.table("Fernow field data.csv", header=T,sep=",")
ferCen<-read.csv("fernow basic census bio.csv",header=TRUE)
ferNPP<-read.csv("Fer ANPP.csv",header=T)
plot.ferAnn<-read.csv("plot.ferAnn.csv",header=TRUE)
plot.ferAB<-read.csv("plot.ferAB.csv",header=TRUE)
fer.sp.dendro<-read.csv("fer.sp.csv",header=TRUE)
fer.sp.cen<-read.csv("fer.sp.cen.csv",header=TRUE)
fer.sp.cen.all<-aggregate(AB~year+species,mean,data=fer.sp.cen)

#annAB

FerAnn<-ggplot(data=ferNPP,aes(year,meanAnn))+
  scale_x_continuous(limits=c(1980,2013))+
  ylab("")+
  xlab("Year")+
  labs(title="FER NPP")+
  annotate("text",x=1980.75,y=8.3,label="1983")+
  annotate("text",x=1986.75,y=7.3,label="1989")+
  annotate("text",x=1991.75,y=7.3,label="1994")+
  annotate("text",x=1997,y=8.0,label="1999")+
  annotate("text",x=2007,y=8.6,label="2009")+
  #stat_summary(data=plot.ferAnn,aes(year,annAB,group=year),fun.data=ErrorB, geom="errorbar",width=2)+
  #geom_point(data=fersubMeanAnn,aes(year,annAB))+
  geom_boxplot(data=plot.ferAnn,aes(year,annAB,group=year),width=1,fill=NA)+
  geom_line()+
  #geom_line(aes(year,maxAnn))+
  #geom_line((aes(year,minAnn)))+
  geom_ribbon(aes(ymin=minAnn,ymax=maxAnn),alpha=0.2,fill="forestgreen")+
 MCtheme

FerTot<-ggplot(data=ferNPP,aes(year,meanAB))+
  scale_x_continuous(limits=c(1975,2013))+
  ylab("Aboveground biomass (Mg/ha)")+
  xlab("Year")+
  labs(title="FER AB",y="")+
  annotate("text",x=1976,y=460,label="1979")+
  annotate("text",x=1981,y=540,label="1983")+
  annotate("text",x=1986.5,y=570,label="1989")+
  annotate("text",x=1991.7,y=640,label="1994")+
  annotate("text",x=1996.7,y=680,label="1999")+
  annotate("text",x=2006,y=700,label="2009")+
  #stat_summary(data=plot.ferAB,aes(year,AB,group=year),fun.data=ErrorB, geom="errorbar",width=2)+
  #geom_point(data=fersubMean,aes(year,AB))+
  geom_boxplot(data=plot.ferAB,aes(year,AB,group=year),width=1.5,fill=NA)+
  geom_line()+
  #geom_line(aes(year,maxAB))+
  #geom_line(aes(year,minAB))+
  geom_ribbon(aes(ymin=minAB,ymax=maxAB),alpha=0.2,fill="forestgreen")+
  MCtheme


#all
field.how<-data.frame(dbh=hw.field.data$dbh,site=hw.field.data$site)
field.lyf<-data.frame(dbh=lyf.field.data$DBH,site=lyf.field.data$Site)
field.fer<-data.frame(dbh=fer.field.data$dbh,site=fer.field.data$site)
field.fer$site<-as.character(as.numeric(field.fer$site))
field.all<-rbind(field.how,field.lyf,field.fer)
write.csv(field.all,file="dendro size distributions.csv")

cen.how<-data.frame(dbh=howCen$DBH2010,site="Howland")
cen.lyf<-subset(Lyf.cen,Lyf.cen$dbh11>=10)
cen.lyf<-data.frame(dbh=cen.lyf$dbh11,site="Lyford")
cen.fer<-subset(ferCensus,ferCensus$DBH1999>=10)
cen.fer<-data.frame(dbh=cen.fer$DBH1999,site="Fernow")
cenDist<-rbind(cen.how,cen.lyf,cen.fer)
library(gridExtra)
grid.arrange(HowAnn,LyfAnn,FerAnn,ncol=1,widths=unit(c(15,20,20),"cm"),heights=unit(c(8,8,8),"cm"))
grid.arrange(HowTot,LyfTot,FerTot,ncol=1,widths=unit(c(15,20,20),"cm"),heights=unit(c(8,8,8),"cm"))
grid.arrange(howCenSp,lyfCensp,ferCensp,ncol=1,widths=unit(c(15,15,15),"cm"),heights=unit(c(15,15,15),"cm"))
grid.arrange(howDist,ferDist,lyfDist,ncol=2,widths=unit(c(15,15,15),"cm"),heights=unit(c(8,8,8),"cm"))

#rose chart testing


bioSpPlot<-subset(bioSp,(bioSp$year==2010&(bioSp$site=="HOW1"|bioSp$site=="HOW2"|bioSp$site=="HOW3"|bioSp$site=="LF1"|
                                             bioSp$site=="LF2"|bioSp$site=="LF3"))|(bioSp$year==1999&(bioSp$site=="1301"|
                                                                                                        bioSp$site=="1302"|bioSp$site=="1303"|bioSp$site=="1304"|bioSp$site
write.csv(bioSpPlot,file="bioSpPlot.csv")                                                                                                   =="1305"|bioSp$site=="1306")))



allPlot.how<-subset(bioSpPlot,bioSpPlot$plot=="HOW1"|bioSpPlot$plot=="HOW2"|bioSpPlot$plot=="HOW3")
allPlot.how<-aggregate(AB~species+plot,data=allPlot.how,sum)

#pie chart:
cbPalette <- c("#F0E442", "#E69F00", "#999999", "#CC79A7", "#D55E00", "#0072B2", "forestgreen")
palette2<-c("#F0E442", "#E69F00", "#999999", "#CC79A7", "#D55E00","goldenrod4", "#0072B2", "#009E73")
ggplot(bioSpPlot,aes(x=PlotAB/2,y=AB,fill=factor(species),width=PlotAB),color=NA)+
  geom_bar(stat="identity",position="fill")+
  coord_polar(theta="y")+
  facet_wrap(~plot,ncol=3)+
  scale_fill_manual(values=cbPalette,guide=guide_legend(title="species"))+
  labs(x="",y="",title="")+
  #geom_text(aes(x=0,y=80,label=paste(("AB = "),TotalAB,(" Mg/ha"))),size=3)+
  theme_bw()+theme(legend.title=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),axis.text.x=element_blank(),plot.title=element_text(size=25),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),strip.text.x=element_text(size=20),strip.background=element_blank())

CenSp<-ggplot(CenSpAll2,aes(x=1,y=AB,fill=factor(species)),color=NA)+
  geom_bar(stat="identity",position="fill")+
  coord_polar(theta="y")+
  facet_wrap(~site,ncol=1)+
  scale_fill_manual(values=palette2,guide=guide_legend(title="species"))+
  labs(x="",y="",title="")+
  #geom_text(aes(x=0,y=80,label=paste(("AB = "),TotalAB,(" Mg/ha"))),size=3)+
  theme_bw()+theme(legend.title=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),axis.text.x=element_blank(),plot.title=element_text(size=25),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),strip.text.x=element_text(size=20),strip.background=element_blank())

#aggregating to change labels:
bioSpPlot3<-aggregate(cbind(annAB,AB)~species+plot,sum,data=bioSpPlot2)
bioSpPlot4<-merge(bioSpPlot3,StandingBio,by="plot")

#changing order of factors in "plot" variable allows me to manually change order of ggplot facets
neworder<-c("HOW1","HOW2","HOW3","LYF1","LYF2","LYF3","FER1301","FER1302","FER1303","FER1304","FER1305","FER1306")
library("plyr")
bioSpPlot<-arrange(transform(bioSpPlot,plot=factor(plot,neworder)),plot)


#Diameter distributions:
Dist<-ggplot(field.all,aes(dbh))+geom_histogram(aes(y=..density..),color="NA",fill="forestgreen",alpha=0.2)+geom_density(color="black")+
  facet_wrap(~site,ncol=2)+
  labs(title="")+
  scale_y_continuous(breaks=c(0.0,0.05,0.10))+#limits=c(0,0.),labels=c(0,0.5,1.0))+
  theme_bw()+theme(panel.border=element_blank(),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),strip.background=element_blank(),strip.text.x=element_text(size=20),
                   axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
                   axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),plot.title=element_text(size=25))

CenDist<-ggplot(cenDist,aes(dbh))+geom_histogram(aes(y=..density..),color="NA",fill="forestgreen",alpha=0.2)+geom_density(color="black")+
  facet_wrap(~site,ncol=1)+
  labs(title="")+
  theme_bw()+theme(panel.border=element_blank(),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),strip.background=element_blank(),strip.text.x=element_text(size=20),
                   axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
                   axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),plot.title=element_text(size=25))


#order factors for graphing
library("plyr")
field.all<-arrange(transform(field.all,site=factor(site,neeworder)),site)

ggplot(HowSampSize,aes(annAB,color=SamplingLim))+geom_line(stat="density")+
  facet_wrap(~year,ncol=1)+
  labs(x="NPP",title="How sampling size")+
 scale_color_manual(values=c("burlywood","red","forestgreen"),name="Sampling Quantity",breaks=c("annHalf","annNorm","annDoub"),labels=c("3 plots","6 plots","12 plots"))+
  theme_bw()+theme(panel.border=element_blank(),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),strip.background=element_blank(),strip.text.x=element_text(size=20),
                   axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
                   axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),plot.title=element_text(size=25))



allPlot.lyf<-subset(bioSp,bioSp$years==2011&(bioSp$site=="LF1"|bioSp$site=="LF2"|bioSp$site=="LF3"))
allPlot.lyf<-subset(bioSpPlot,bioSpPlot$plot=="LF1"|bioSpPlot$plot=="LF2"|bioSpPlot$plot=="LF3")
lyfABsp<-ggplot(allPlot.lyf,aes(species,AB,width=rel.freq))+
  geom_bar(color="black",fill="forestgreen",stat="identity",position="fill")+
  coord_polar()+
  facet_wrap(~plot)+
  labs(x="",y="Mg/ha")+
  #geom_text(aes(x=11,y=10,label=TotalAB),size=3)+
  theme_bw()+theme(panel.border=element_blank(),panel.grid.major=element_line(color="black"),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),strip.background=element_rect(color="black",fill="white"))

lyfCenSp<-subset(lyfCenSp,lyfCenSp$year==2011)
lyfCenSp$site<-"Lyford"
lyfCenSp$rel.freq<-lyfCenSp$freq/sum(lyfCenSp$freq)

ggplot(lyfSizefreq,aes(SizeClass, Sizeclassfreq))+geom_point()+facet_wrap(~Site,ncol=1)+geom_line()

lyf.field.data<-read.csv("LyfordAllPlots.csv",header=TRUE)


allPlot.fer<-subset(bioSpPlot,bioSpPlot$plot=="1301"|bioSpPlot$plot=="1302"|bioSpPlot$plot=="1303"|
                                              bioSpPlot$plot=="1304"|bioSpPlot$plot=="1305"|bioSpPlot$plot=="1306")







