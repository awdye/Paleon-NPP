
#Note: this script is set up for Lyford tree ring data. The basic framework will be used across all PalEON sites
#with some small changes. I tried to indicate where these changes are most likely to occur.

#Dendro field data. 
# for Howland: How.field.data.csv
#for Lyford:Lyf.field.data.csv
#for Tower:Tow.field.data.csv
#for Fernow:Fer.field.data.csv

field.data<-read.table("LyfordAllPlots.csv", header=T,sep=",")

  #allometric biomass equations. The file biomass_coeff.csv is included in the zip file.
  #You will not need all equations. Mark "1" in eq column to specify equations to be used (based on region) 
  #Add equations as necessary
  #see Appendix A for comprehensive list of species eqs and references
bm.eqs<-read.table("biomass_coeff.csv", header=T,sep=",")

#Create distance class based on 13,20,and 30m nests. Only relevant for nested plots
field.data$distclass<-cut(lyf.field.data$Distance,breaks=c(0,12.999,19.999,99.999), right=T,
                             labels=c(13,20,30),include.lowest=T)
  
lyf.field.data$convha<-as.numeric(as.character(lyf.field.data$convha))
#Used this for Lyford for simplicity since no nests:
lyf.field.data$convha<-10000/(pi*(13)^2)


#Step 3:Read in ring width files. RW files are in Tucson format, .001 precision.
#Howland folder: HowRW
#Lyford folder: LyfRW
#Fernow folder: FerRW

setwd("C:/Paleon/Lyford/Lyford_Data_13m/RW/Combined")
library(dplR)

#lists .rwl files to be read. Check to make sure everything is where it should be
files_to_read<-list.files()
 
core.means.all<-NULL

#creates data frame core.means.all, averaging annual ring widths from cores of same tree
for (i in files_to_read){
  file_read<-read.rwl(i)
  assign(i,file_read)
  
  #codes in name of core, e.g."LF101a"
  cores<-data.frame("code"=names(file_read)) 
  
  #truncates core name to combine cores from same tree, e.g. cuts LF101a and LF101b to "LF101". 
  #Adjust for different sample id lengths
  cores$recode<- substr(as.character(cores$code),1,5)
  
  #creates new data frame for core means
  core.means<-data.frame("year"=row.names(file_read))
  core.means<-NULL
  n=1
  file_read$year<-row.names(file_read)
  for (j in unique(cores$recode)){
    print(j)
    print(n)
    tree1<-data.frame(file_read[,c(as.character(cores$code[cores$recode==j]),"year")])
    if(dim(tree1)[2]>1){
      tree2<-data.frame(rowMeans(subset(tree1, select=-c(year)),na.rm=T))
    } else {tree2=tree1}
    tree2<-na.omit(tree2)
    tree2$tree<-j
    tree2$year<-as.numeric(as.character(row.names(tree2)))
    
   #rACUM calculates inside-out biomass, rACUM.inv calculates outside-in biomass. 
   #Only using rACUM.inv for now.
   tree2$rACUM<-0
    tree2$rACUM.inv<-0
    for(k in 1:dim(tree2)[1]){
      tree2$rACUM[k]<-sum(tree2[c(1:k),1])
      tree2$rACUM.inv[k-1]<-sum(tree2[c((k):(dim(tree2)[1])),1])
    }
    names(tree2)<-c("meanRW","recode","year","rACUM","rACUM.inv")
    if(n==1)(core.means<-tree2)else(core.means<-rbind(core.means, tree2))
    n=n+1}
  plot(core.means$rACUM.inv~core.means$year, type="p")
  title(i)
  
  core.means.all<-rbind(core.means.all,core.means)
}

#truncates core id to site name, e.g. LF101 to "LF1" Adjust accordingly for specific site.
core.means.all$Site<-substr(as.character(core.means.all$recode),1,3)
dim(unique(subset(core.means.all, select="Tree.Number", Site=="LF1")))

#truncates core id to tree number, e.g., LF101 to "01". Adjust accordingly to sample id of site.
core.means.all$Tree.Number<-as.numeric(as.character(substr(as.character(core.means.all$recode),4,5)))

#ab.lyf.all data frame includes everything(field data, core means, annual biomass increment, bio/ha)
ab.all<-merge(core.means.all,field.data, by=c("Site","Tree.Number"))
bm.eqs.sp<-bm.eqs[bm.eqs$eq==1,]
ab.all<-merge(ab.all,bm.eqs.sp,by="species")

#dbhest1==> dbh at the BEGINNING of the growing season
#****alternative code for Fernow at bottom of script
ab.all$dbhest1<-ab.all$DBH-0.2*(ab.all$rACUM.inv+ab.all$meanRW) #0.2 is a conversion from radius to diameter
ab.all$dbhest1[ab.all$dbhest1<0]<-NA
#dbhest2==> dbh at the END of the growing season
ab.all$dbhest2<-ab.all$DBH-0.2*(ab.all$rACUM.inv)
ab.all$dbhest2[ab.all$dbhest2<0]<-NA

#calculates total aboveground biomass(AB) of each tree for each year using allometric equation a*DBH^b
ab.all$AB<-(ab.all$a*ab.all$dbhest2^ab.all$b)*0.001 #0.001 is a conversion from kg to Mg. a and b from allometric equations.

#annAB==>biomass acculumated during the year.
# that is, biomass at the END of the growing season
# minus the biomass at the BEGINNING of the growing season
ab.all$annAB<-(ab.all$a*(ab.all$dbhest2^ab.all$b-ab.all$dbhest1^ab.all$b))*0.001



#Additional code for biomass at nested plots (Howland, Lyford, Tower)
#Functions located in file PaleonFunctions.R
bioSum<-bioNest.Sum(ab.all)
bioSum$NestSum<-bioSum$annAB.ha1+bioSum$annAB.ha2
meanAnn<-aggregate(bioSum$NestSum,list(year=bioSum$years),mean, na.rm=TRUE)
colnames(NPP)<-c("year","meanAnn")
minAnn<-aggregate(NestSum~years,min,na.rm=TRUE,data=bioSum)
colnames(minAnn)<-c("year","minAnn")
maxAnn<-aggregate(NestSum~years,max,na.rm=TRUE,data=bioSum)
colnames(maxAnn)<-c("year","maxAnn")
NPP<-data.frame(year=meanAnn$year,meanAnn=meanAnn$meanAnn,minAnn=minAnn$minAnn,maxAnn=maxAnn$maxAnn)


#for species:
bioSp<-bio.spAB(ab.hw.all) #for species aboveground biomass
bioSp$AB<-rowSums(bioSpAB[,4:5],na.rm=TRUE)
bioSp$AB[bioSp$AB==0]<-NA
bioSpecies<-aggregate(AB~years+species+sites,sum,na.rm=TRUE,data=bioSp)


#***Code for Fernow
#Do not run nested functions bioNest.Sum on Fernow-no nests were used in sampling design

#equation form  (M=a*b^D)
form1<-subset(ab.all,ab.all$form==1)
#form1$AB<-form1$a*form1$dbhest2^form1$b*0.001*(10000/(pi*(10)^2))
form1$annAB<-form1$a*(form1$dbhest2^form1$b-form1$dbhest1^form1$b)*0.01*(10000/(pi*(10)^2))

#equation form
form3<-subset(ab.all,ab.all$form==3)
#form3$AB<-log((form3$a+form3$b*(log(form3$dbhest2))))*0.001*(10000/(pi*(10)^2))
form3$annAB<-(log((form3$a+form3$b*(log(form3$dbhest2))))-log((form3$a+form3$b*(log(form3$dbhest1)))))*0.01*(10000/(pi*(10)^2))
ab.all<-rbind(form1,form3)
ab.all$annAB<-ab.all$annAB*(10000/(pi*(10)^2))

meanAnn<-aggregate(annAB~year,sum,na.rm=TRUE,data=ab.all)
maxAnn<-aggregate(annAB~year,max,na.rm=TRUE,data=ab.all)
minAnn<-aggregate(annAB~year,min,na.rm=TRUE,data=ab.all)
NPP<-data.frame(year=meanAnn$year,meanAnn=meanAnn$meanAnn,minAnn=minAnn$minAnn,maxAnn=maxAnn$maxAnn)
bioSpecies<-aggregate(AB~years+species+sites,sum,na.rm=TRUE,data=ab.all)
