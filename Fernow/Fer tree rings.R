#Script for Fernow tree ring NPP analysis

fer.field.data<-read.table("Fernow field data.csv", header=T,sep=",")
fer.field.data<-subset(fer.field.data,fer.field.data$species!="unk")
fer.field.data$convha<-10000/(pi*(10)^2)
fer.field.data$convha<-as.numeric(as.character(fer.field.data$convha))

bm.eqs<-read.table("biomass_coeff.csv", header=T,sep=",")

bm.eqs.sp<-bm.eqs[bm.eqs$eq==1,]
ab.fer<-merge(fer.field.data,bm.eqs.sp,by="species")
form1<-subset(ab.fer,ab.fer$form==1)
form3<-subset(ab.fer,ab.fer$form==3)
form1$AB<-(form1$a*form1$dbh^form1$b)*0.001*form1$convha
form3$AB<-log((form3$a+form3$b*(log(form3$dbh))))*0.001*form3$convha
ab.fer<-rbind(form1,form3)
ab.fer<-aggregate(AB~site,sum,data=ab.fer)

files_to_read<-list.files()
# files_to_read<-"PM_TulipPoplar.rw"

core.means.all<-NULL
for (i in files_to_read){
  file_read<-read.rwl(i)
  assign(i,file_read)
  
  cores<-data.frame("code"=names(file_read))
  cores$trees<- substr(as.character(cores$code),1,6)
  core.means<-data.frame("year"=row.names(file_read))
  core.means<-NULL
  n=1
  file_read$year<-row.names(file_read)
  for (j in unique(cores$trees)){
    print(j)
    print(n)
    tree1<-data.frame(file_read[,c(as.character(cores$code[cores$trees==j]),"year")])
    if(dim(tree1)[2]>1){
      tree2<-data.frame(rowMeans(subset(tree1, select=-c(year)),na.rm=T))
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
    names(tree2)<-c("meanRW","trees","year","rACUM","rACUM.inv")
    if(n==1)(core.means<-tree2)else(core.means<-rbind(core.means, tree2))
    n=n+1}
  plot(core.means$rACUM.inv~core.means$year, type="p")
  title(i)
  
  core.means.all<-rbind(core.means.all,core.means)
}
core.means.all$site<-substr(as.character(core.means.all$trees),1,4)
#dim(unique(subset(core.means.all, select=trees, site=="HOW1")))


core.means.all$trees<-as.numeric(as.character(substr(as.character(core.means.all$trees),5,6)))
write.csv(core.means.all,file="core.means.all.csv")

core.means.all<-read.csv("core.means.all.csv",header=TRUE)

ab.fer.all<-merge(core.means.all,fer.field.data, by=c("site","trees"))
bm.eqs.sp<-bm.eqs[bm.eqs$eq==1,]
ab.fer.all<-merge(ab.fer.all,bm.eqs.sp,by="species")
ab.fer.all$DateCreated<-NULL

#dbhest1==> dbh at the BEGINNING of the growing season
ab.fer.all$dbhest1<-ab.fer.all$dbh-0.2*(ab.fer.all$rACUM.inv+ab.fer.all$meanRW)
warnings()
ab.fer.all$dbhest1[ab.fer.all$dbhest1<0]<-NA
#dbhest2==> dbh at the END of the growing season
ab.fer.all$dbhest2<-ab.fer.all$dbh-0.2*(ab.fer.all$rACUM.inv)
ab.fer.all$dbhest2[ab.fer.all$dbhest2<0]<-NA

#equation form  (M=a*b^D)
form1<-subset(ab.fer.all,ab.fer.all$form==1)
form1$AB<-form1$a*form1$dbhest2^form1$b*0.001*31.83099
form1$annAB<-form1$a*(form1$dbhest2^form1$b-form1$dbhest1^form1$b)*0.001*31.83099

#equation form
form3<-subset(ab.fer.all,ab.fer.all$form==3)
form3$AB<-log((form3$a+form3$b*(log(form3$dbhest2))))*0.001*31.83099
form3$annAB<-(log((form3$a+form3$b*(log(form3$dbhest2))))-log((form3$a+form3$b*(log(form3$dbhest1)))))*0.001*31.83099
ab.fer<-rbind(form1,form3)
write.csv(ab.fer,file="ab.fer.all.csv")

fer.sp.plot<-aggregate(cbind(annAB,AB)~year+species+site,sum,na.rm=TRUE,data=ab.fer)
fer.sp.mean<-aggregate(annAB~year+species,sum,na.rm=TRUE,data=fer.sp)
fer.sp.max<-aggregate(annAB~year+species,max,na.rm=TRUE,data=fer.sp)
fer.sp.max$annAB<-fer.sp.max$annAB*6
fer.sp.min<-aggregate(annAB~year+species,min,na.rm=TRUE,data=fer.sp)
fer.sp.min$annAB<-fer.sp.min$annAB*6
fernowSpecies<-data.frame(year=fer.sp.mean$year,species=fer.sp.mean$species,meanAnn=fer.sp.mean$annAB,
                          maxAnn=fer.sp.max$annAB,minAnn=fer.sp.min$annAB)
write.csv(fernowSpecies,"fer.sp.csv")

ab.fer.all<-read.csv("ab.fer.all.csv",header=TRUE)
ab.ferAB<-aggregate(cbind(annAB,AB)~year+site,sum,na.rm=TRUE,data=ab.fer)
write.csv(ab.ferAB,file="ferAB by plot.csv")



maxFer<-aggregate(cbind(annAB,AB)~year,max,data=ab.fer)
minFer<-aggregate(cbind(annAB,AB)~year,min,data=ab.fer)
FerNPP<-data.frame(year=maxFer$year,meanAnn=ANPP$annAB,meanAB=meanFer$AB,maxAnn=ANPP$max,
                   maxAB=maxFer$AB,minAnn=ANPP$min,minAB=minFer$AB)
write.csv(FerNPP,file="Fer ANPP.csv")
ANPP<-read.csv("Fer ANPP.csv",header=T)
ANPP$AB<-FerNPP$AB
write.csv(ANPP,file="Fer ANPP.csv")
