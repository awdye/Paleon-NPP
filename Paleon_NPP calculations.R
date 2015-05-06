library(dplR)
install.packages("dplR", repos = "http://R-Forge.R-project.org") #has new code for treating edge zeros
library(sp)

#Example script for how to deal with NPP data from tree rings

#Note: this script is set up for Lyford tree ring data. The basic framework will be used across all PalEON sites
#with some small changes. I tried to indicate where these changes are most likely to occur and areas of confusion.
#code contributors Dario Martin-Benito, Alex Dye, Dan Bishop

#Step 1: Read in data
setwd("C:/Paleon/Lyford/Lyford_Data_13m")

  #Dendro field data. All data frame columns match those in the LyfordAllPlots.csv file, which can be accessed on Paleon wiki.
lyf.field.data<-read.table("LyfordAllPlots.csv", header=T,sep=",")

  #allometric biomass equations. The file biomass_coeff.csv is included.
  #You will not need all equations. Mark "1" in eq column to specify equations to be used. 
  #Equations are of form M=a*DBH^b where M is biomass in kg, a and b are species-specific coefficients. 
  #Add equations as necessary
  #see Ter-Mikealian & Korzhukin 1997 or Jenkins 2004 for comprehensive list of species eqs and references
bm.eqs<-read.table("biomass_coeff.csv", header=T,sep=",")
  

#Step 3:Read in ring width files. RW files are in Tucson format, .001 precision.
setwd("C:/Paleon/Lyford/Lyford_Data_13m/RW/Combined")

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
ab.lyf.all<-merge(core.means.all,lyf.field.data, by=c("Site","Tree.Number"))
bm.eqs.sp<-bm.eqs[bm.eqs$eq==1,]
ab.lyf.all<-merge(ab.lyf.all,bm.eqs.sp,by="Species")

#dbhest1==> dbh at the BEGINNING of the growing season
ab.lyf.all$dbhest1<-ab.lyf.all$DBH-0.2*(ab.lyf.all$rACUM.inv+ab.lyf.all$meanRW) #0.2 is a conversion from radius to diameter
ab.lyf.all$dbhest1[ab.lyf.all$dbhest1<0]<-NA
#dbhest2==> dbh at the END of the growing season
ab.lyf.all$dbhest2<-ab.lyf.all$DBH-0.2*(ab.lyf.all$rACUM.inv)
ab.lyf.all$dbhest2[ab.lyf.all$dbhest2<0]<-NA

#calculates total aboveground biomass(AB) of each tree for each year using allometric equation a*DBH^b
ab.lyf.all$AB<-(ab.lyf.all$a*ab.lyf.all$dbhest2^ab.lyf.all$b)*0.001 #0.001 is a conversion from kg to Mg. a and b from allometric equations.

#annAB==>biomass acculumated during the year.
# that is, biomass at the END of the growing season
# minus the biomass at the BEGINNING of the growing season
ab.lyf.all$annAB<-(ab.lyf.all$a*(ab.lyf.all$dbhest2^ab.lyf.all$b-ab.lyf.all$dbhest1^ab.lyf.all$b))*0.001

#biomass per hectare assuming the plot represents the entire hectare. annAB is bio/year/ha, AB is total bio/ha
ab.lyf.all$annAB.ha<-ab.lyf.all$annAB*ab.lyf.all$convha
ab.lyf.all$AB.ha<-ab.lyf.all$AB*ab.lyf.all$convha


#ab.lyf.site data frame splits annAB.ha measurements between each site
#LyfordMean column is the average annAB of all 3 sites.
ab.lyf.site<-data.frame(tapply(X=ab.lyf.all$annAB.ha,INDEX=list(ab.lyf.all$year,ab.lyf.all$Site),sum.fn))
ab.lyf.site$year<-as.numeric(as.character(row.names(ab.lyf.site)))
ab.lyf.site[is.na(ab.lyf.site)]=0
ab.lyf.site$LyfordMean<-(ab.lyf.site$LF1+ab.lyf.site$LF2+ab.lyf.site$LF3)/3
ab.lyf.site<-subset(ab.lyf.site,year>=1960&year<=2011)


