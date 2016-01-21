par(mfrow=c(3,1))
par(cex.lab=1.6)
par(mar=c(5,5,4,2))
library(ggplot2)
#READ FILES FOR GRAPHING
randomStats<-read.csv("Howland census Stats.csv", header=TRUE)
HowNPP<-read.csv("HowNPP.csv",header=TRUE)
HowDrawsAnn<-read.csv("HowDraws.csv",header=TRUE)
HowDraws30<-read.csv("HowDraws30m.csv",header=TRUE)
HowDrawsAB<-read.csv("How Draws AB.csv",header=TRUE)
plot.cenAnn<-read.csv("plot.cenAnn.csv",header=TRUE)
plot.cenAB<-read.csv("plot.cenAB.csv",header=TRUE)
plot.sp<-read.csv("How census Sp.csv",header=TRUE)
How.sp<-read.csv("bioSpecies.csv",header=TRUE)
#How.EC<-read.csv("How.EC.NPP.csv")
#How.EC.ave<-read.csv("How.EC.ave.csv")
#Har.EC<-read.csv("EMS.NEP.csv")
#ModNPP<-read.csv("ModisNPP.csv")
#HowMod<-subset(ModNPP,site=="Howland")
#HarMod<-subset(ModNPP,site=="Harvard")
#FerMod<-subset(ModNPP,site=="Fernow")
LyfCensusTotal<-read.csv("LyfordCensusTotal.csv")
HowCensusTotal<-read.csv("HowlandCensusTotal.csv")
FerCensusTotal<-read.csv("FernowCensusTotal.csv")
#Lyf.randomStats<-read.csv("Lyford census Stats.csv",header=TRUE)
LyfordNPP<-read.csv("LyfordNPP.csv",header=TRUE)
plot.lyfAnn<-read.csv("plot.lyfAnn.csv",header=TRUE)
plot.lyfAB<-read.csv("plot.lyfAB.csv",header=TRUE)
LyfDrawsAnn<-read.csv("Lyf Draws annAB.csv",header=TRUE)
LyfDrawsAnn30m<-read.csv("Lyf Draws annAB_30m.csv",header=TRUE)
LyfDrawsAB<-read.csv("Lyf Draws AB.csv",header=TRUE)
fer.field.data<-read.table("Fernow field data.csv", header=T,sep=",")
ferCen<-read.csv("fernow basic census bio.csv",header=TRUE)
ferNPP<-read.csv("Fer ANPP.csv",header=T)
plot.ferAnn<-read.csv("plot.ferAnn.csv",header=TRUE)
plot.ferAB<-read.csv("plot.ferAB.csv",header=TRUE)
#fer.sp.dendro<-read.csv("fer.sp.csv",header=TRUE)
#fer.sp.cen<-read.csv("fer.sp.cen.csv",header=TRUE)
#fer.sp.cen.all<-aggregate(AB~year+species,mean,data=fer.sp.cen)
Tow.NPP<-read.csv("TowerNPP.csv")
TowDraws<-read.csv("Tow Draws annAB.csv")

bioSp<-read.csv("bioSpecies.csv")
field.all<-read.csv("dendro size distributions.csv")
bioSpPlot<-read.csv("bioSpPlot.csv")
cenDist<-read.csv("census size distributions.csv")
CenSpAll2<-read.csv("CenSpAll2.csv")
howCen<-read.csv("censusAB.csv",header=TRUE)

MCtheme<-theme_bw()+theme(panel.border=element_blank(),plot.title=element_text(size=20),axis.title.x=element_text(size=25),
                          axis.title.y=element_text(size=25),
                          panel.background=element_blank(),axis.line=element_line(color="black"))+theme(legend.key=element_blank())


#all together
ggplot()+
  scale_x_continuous(limits=c(1990,2014))+
  scale_y_continuous(limits=c(0,3.2))+
  #ylab("MgC/ha/yr")+
  xlab("")+
  labs(title="")+
  #annotate("text",x=1972,y=2.1,label="1975")+
  #annotate("text",x=1987,y=2.1,label="1991")+
  #annotate("text",x=1997,y=2.1,label="2001")+
  #annotate("text",x=2008,y=2.2,label="2011")+
  geom_violin(data=HowDrawsAnn,aes(year,wNPP,group=year),alpha=0.5,fill="burlywood")+
  geom_boxplot(data=plot.cenAnn,aes(Year,wNPP_C,group=Year),width=1.5,fill=NA,outlier.shape=NA)+
  #geom_line()+
  #geom_line(data=LyfCensusTotal,aes(Year,wNPP_C),linetype=2,size=1)+
  #geom_line(data=Har.EC,aes(year,wNPP_est),linetype=2,size=1,color="dodgerblue1")+
  #geom_line(data=HarMod,aes(year,wNPP),linetype=2,size=1,color="violetred4")+
  #geom_line(data=HarMod,aes(year,wNPP),color="orange2")+
  #geom_line(data=Har.Towers.Ann,aes(year,NPP_CUE),color="blue")+
  #geom_line(aes(year,maxAnn))+
  #geom_line(aes(year,minAnn))+
  #stat_summary(data=plot.lyfAnn,aes(year,annAB,group=year),fun.data=ErrorB, geom="errorbar",width=2)+
  #geom_point(data=lyfsubMeanAnn,aes(year,annAB))+
 #geom_ribbon(data=How.EC.ave,aes(x=year,ymin=NPP_CUELowerSD,ymax=NPP_CUEUpperSD),alpha=0.4,fill="dodgerblue1")+
  #geom_ribbon(data=FerMod,aes(x=Year,ymin=wNPP_lowerSD,ymax=wNPP_upperSD),alpha=0.4,fill="violetred4")+
  #geom_ribbon(data=Tow.NPP,aes(x=year,ymin=minNPP_C,ymax=maxNPP_C),alpha=0.3,fill="seashell4")+
  geom_ribbon(data=HowNPP,aes(x=year,ymin=minNPP_C,ymax=maxNPP_C),alpha=0.3,fill="forestgreen")+
  #geom_line(data=How.EC.ave,aes(year,NEP),linetype=2,color="dodgerblue",size=1)+
  MCtheme

#annAB
HowAnn<-ggplot(data=HowNPP,aes(year,meanAnn))+
  scale_x_continuous(limits=c(1995,2013))+
  ylab("")+
  xlab("")+
  #labs(title="HOW NPP")+
  geom_violin(data=HowDrawsAnn,aes(year,annAB,group=year),alpha=0.5,fill="burlywood")+
  #annotate("text",x=1996,y=3.4,label="1998")+
  #annotate("text",x=2000,y=3.3,label="2002")+
  #annotate("text",x=2008,y=3.9,label="2010")+
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


#Species charts

#bioSpPlot<-subset(bioSp,(bioSp$year==2010&(bioSp$site=="HOW1"|bioSp$site=="HOW2"|bioSp$site=="HOW3"|bioSp$site=="LF1"|
 #                                            bioSp$site=="LF2"|bioSp$site=="LF3"))|(bioSp$year==1999&(bioSp$site=="1301"|
  #                                                                                                      bioSp$site=="1302"|bioSp$site=="1303"|bioSp$site=="1304"|bioSp$site
   #                                                                                              =="1305"|bioSp$site=="1306"))
#

library(grid)
#changing order of factors in "plot" variable allows me to manually change order of ggplot facets
neworder<-c("HOW1","HOW2","HOW3","LYF1","LYF2","LYF3","TP1","TP2","FER1301","FER1302","FER1303","FER1304","FER1305","FER1306")
neworder2<-c("Howland","Lyford","Tower","Fernow")
neworder3<-c("Howland","Harvard","Fernow")
neworder4<-c("Tree Rings","Tree Rings(Lyford)","Tree Rings(EMS)","Permanent Plots")
#neworder5<-c("eastern hemlock","red spruce","other hardwood","other conifer","oak","maple","American beech",
           #  "black cherry")


library("plyr")
bioSpPlot<-arrange(transform(bioSpPlot,plot=factor(plot,neworder)),plot)
bioSpPlot<-arrange(transform(bioSpPlot,site=factor(site,neworder3)),site)

CenSpAll3<-arrange(transform(CenSpAll3,site=factor(site,neworder3)),site)
CenSpAll3<-arrange(transform(CenSpAll3,Method=factor(Method,neworder4)),Method)
CenSpAll3<-arrange(transform(CenSpAll3,species=factor(species,neworder5)),species)
TRsite<-arrange(transform(TRsite,site=factor(site,neworder2)),site)
cenDist<-arrange(transform(cenDist,site=factor(site,neworder2)),site)
#pie chart:
cbPalette <- c("#F0E442", "#E69F00", "#999999", "#0072B2", "forestgreen", "#CC79A7", "#D55E00")
palette2<-c("#F0E442", "forestgreen","orange2", "#0072B2","#999999","burlywood", "#D55E00")
palette3<-c("#F0E442", "#CC79A7","forestgreen","orange2", "#0072B2","#999999","burlywood", "#D55E00")
ggplot(bioSpPlot,aes(x=PlotAB/2,y=AB,fill=factor(species),width=100),width=50,color=NA)+
  geom_bar(stat="identity",position="fill")+
  #coord_polar(theta="y")+
  facet_wrap(~plot)#,ncol=4)+
  scale_fill_manual(values=palette2,guide=guide_legend(title="species"))+
  labs(x="",y="",title="")+
  #geom_text(aes(x=0,y=80,label=paste(("AB = "),TotalAB,(" Mg/ha"))),size=3)+
  theme_bw()+theme(legend.title=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),axis.text.x=element_blank(),plot.title=element_text(size=25),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),strip.text.x=element_text(size=20),strip.background=element_blank(),
                    panel.margin = unit(2, "lines"))#,legend.position=c(.9,.1))

#stacked bar chart
ggplot(bioSpPlot)+
  geom_bar(aes(x=plot,y=AB,fill=factor(species)),stat="identity",position="fill")+#scale_fill_manual(values=alpha(palette,.3))+
  scale_fill_manual(values=alpha(palette2,0.75),guide=guide_legend(title="species"))+
  facet_grid(~site,scales="free",space="free")+MCtheme+
  ylab("Percentage of plot biomass")+
  labs(title="")+
  theme(strip.background=element_blank(),panel.margin = unit(1.5, "lines"),axis.title.x=element_blank(),
        strip.text.x=element_text(size=12,color="black"),legend.key=element_blank())



TRsite<-aggregate(AB~site+species+Method,data=bioSpPlot,sum)
TRsite$Method<-"Tree Rings"
CenSpAll3<-rbind(CenSpAll2,TRsite)
CenSpAll3<-read.csv("CenSpAll3.csv")

ggplot(CenSpAll3,aes(x=Method,y=AB,fill=factor(species)),color=NA)+
  geom_bar(stat="identity",position="fill")+
  scale_fill_manual(values=alpha(palette3,0.75),guide=guide_legend(title="species"))+
  #coord_polar(theta="y")+
  #facet_wrap(~site,ncol=1)+
  facet_grid(~site,scales="free",space="free")+MCtheme+
  labs(x="Method",y="Percentage of Total Biomass",title="")+
  #geom_text(aes(x=0,y=80,label=paste(("AB = "),TotalAB,(" Mg/ha"))),size=3)+
  theme(strip.background=element_blank(),panel.margin = unit(1.5, "lines"),axis.title.x=element_blank(),
        strip.text.x=element_text(size=12,color="black"),legend.key=element_blank())


ggplot(TRsite,aes(x=1,y=AB,fill=factor(species)),color=NA)+
  geom_bar(stat="identity",position="fill")+
  coord_polar(theta="y")+
  facet_wrap(~site,ncol=1)+
  scale_fill_manual(values=palette2,guide=guide_legend(title="species"))+
  labs(x="",y="",title="Tree ring plots")+
  #geom_text(aes(x=0,y=80,label=paste(("AB = "),TotalAB,(" Mg/ha"))),size=3)+
  theme_bw()+theme(legend.title=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),axis.text.x=element_blank(),plot.title=element_text(size=25),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),strip.text.x=element_text(size=20),strip.background=element_blank(),
                   legend.position="none")


#aggregating to change labels:
bioSpPlot3<-aggregate(cbind(annAB,AB)~species+plot,sum,data=bioSpPlot2)
bioSpPlot4<-merge(bioSpPlot3,StandingBio,by="plot")




#Diameter distributions:


#all
field.how<-data.frame(dbh=hw.field.data$dbh,site=hw.field.data$site)
field.lyf<-data.frame(dbh=lyf.field.data$DBH,site=lyf.field.data$Site)
field.fer<-data.frame(dbh=fer.field.data$dbh,site=fer.field.data$site)
field.fer$site<-as.character(as.numeric(field.fer$site))
field.tp<-data.frame(dbh=TP.field$DBH,site=TP.field$Site)
field.all<-rbind(field.how,field.lyf,field.fer)
field.all<-rbind(field.all,field.tp)
write.csv(field.all,file="dendro size distributions.csv")

cen.how<-data.frame(dbh=howCen$DBH2010,site="Howland")
cen.lyf<-subset(Lyf.cen,Lyf.cen$dbh11>=10)
cen.lyf<-data.frame(dbh=cen.lyf$dbh11,site="Lyford")
cen.fer<-subset(ferCensus,ferCensus$DBH1999>=10)
cen.fer<-data.frame(dbh=cen.fer$DBH1999,site="Fernow")
cenDist<-rbind(cen.how,cen.lyf,cen.fer)


ggplot(field.all,aes(dbh))+geom_histogram(aes(y=..density..),color="NA",fill="forestgreen",alpha=0.2)+geom_density(color="darkgreen")+
  facet_wrap(~plot,ncol=2)+#,scales="free")+
  labs(title="")+
  scale_y_continuous(breaks=c(0.0,0.05,0.10))+#limits=c(0,0.),labels=c(0,0.5,1.0))+
  theme_bw()+theme(panel.border=element_blank(),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),strip.background=element_blank(),strip.text.x=element_text(size=20),
                   axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
                   axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),plot.title=element_text(size=25))

ggplot(cenDist,aes(dbh))+geom_histogram(aes(y=..density..),color="NA",fill="burlywood",alpha=0.2)+geom_density(color="burlywood")+
  geom_histogram(data=field.all,aes(y=..density..,fill=Tree.Ring.Plots),color="NA",alpha=0.1)+geom_density(data=field.all,aes(color=Tree.Ring.Plots),fill="NA")+
  facet_wrap(~site,ncol=1)+
  scale_fill_manual(values=c("forestgreen","blue"))+
  scale_color_manual(values=c("forestgreen","blue"))+
  scale_y_continuous(breaks=c(0.0,0.025,0.050,0.075))+
  scale_x_continuous(limits=c(10,75))+
  labs(title="")+#guide_legend(title="Tree")
  theme_bw()+theme(panel.border=element_blank(),
                   panel.background=element_rect(fill="transparent"),plot.background=element_rect(fill="transparent"),
                   axis.line=element_blank(),strip.background=element_blank(),strip.text.x=element_text(size=20),
                   axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
                   axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),
                   legend.position="none",plot.title=element_text(size=25))


#order factors for graphing
library("plyr")
field.all<-arrange(transform(field.all,plot=factor(plot,neworder)),plot)
field.all<-arrange(transform(field.all,site=factor(site,neworder3)),site)
cenDist<-arrange(transform(cenDist,site=factor(site,neworder3)),site)
cenDist$site<-ifelse(cenDist$site=="Lyford","Harvard",cenDist$site)



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

ferCen2<-subset(ferCen,ferCen$year==1999)
ferCen2$site<-"Fernow"
ferCen2$rel.freq<-ferCen2$freq/sum(ferCen2$freq)






#Boxplot comparisons
NPPall<-read.csv("NPPall2.csv")
Lyf<-read.csv("Lyf Draws annAB.csv")
Lyf$Method<-"Permanent Plot"
Lyf$site<-"Lyford"
EMS<-read.csv("Lyf Draws annAB.csv")
EMS$Method<-"Permanent Plot"
EMS$site<-"EMS"
How<-read.csv("HowDraws2.csv")
How$Method<-"Permanent Plot"
How$site<-"Howland"
Fer<-read.csv("plot.ferAnn.csv")
Fer$Method<-"Permanent Plot"
Fer$site<-"Fernow"
NPPall<-rbind(NPPall,NPPall2,How,Lyf,EMS,Fer)

library("plyr")
library("grid")
library("scales")
siteOrd<-c("Howland","Lyford","EMS","Fernow")
methOrd<-c("Tree Rings","Permanent Plot") #,"Eddy Covariance","MODIS")
compOrd<-c("Tree Rings","Permanent Plot")#,"Eddy Covariance","MODIS")
NPPall<-arrange(transform(NPPall,site=factor(site,siteOrd)),site)
NPPall<-arrange(transform(NPPall,Method=factor(Method,methOrd)),Method)
#NPPall<-arrange(transform(NPPall,Comparison=factor(Comparison,compOrd)),Comparison)

palette<-c("forestgreen","burlywood","dodgerblue1","violetred4")

ggplot(NPPall)+
  geom_boxplot(aes(Method,wNPP,fill=Method),outlier.shape=NA,position=position_dodge(0))+scale_fill_manual(values=alpha(palette,.3))+
  facet_grid(~site,scales="free",space="free")+MCtheme+
  ylab("MgC/ha/yr")+
  #labs(title="Interannual variability in woody NPP")+
  theme(strip.background=element_blank(),panel.margin = unit(1.5, "lines"),axis.title.x=element_blank(),
        strip.text.x=element_text(size=12,color="black"),legend.key=element_blank())


