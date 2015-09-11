library(dplR)
library(ggplot2)
library(sciplot)
library(data.table)
       

Towers.Ann<-read.csv("HFEMS.csv") 
#files are HFEMS.csv for Harvard EMS and How.Towers.csv for Main and West Howland towers.
Towers.Ann[Towers.Ann==-9999]<-NA
        
Towers.Ann$year<-ifelse(Towers.Ann$year>=1991&Towers.Ann$year<1992,1991,Towers.Ann$year)
Towers.Ann$year<-ifelse(Towers.Ann$year>=1992&Towers.Ann$year<1993,1992,Towers.Ann$year)
Towers.Ann$year<-ifelse(Towers.Ann$year>=1993&Towers.Ann$year<1994,1993,Towers.Ann$year)
Towers.Ann$year<-ifelse(Towers.Ann$year>=1994&Towers.Ann$year<1995,1994,Towers.Ann$year)
Towers.Ann$year<-ifelse(Towers.Ann$year>=1995&Towers.Ann$year<1996,1995,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=1996&Towers.Ann$year<1997,1996,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=1997&Towers.Ann$year<1998,1997,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=1998&Towers.Ann$year<1999,1998,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=1999&Towers.Ann$year<2000,1999,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2000&Towers.Ann$year<2001,2000,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2001&Towers.Ann$year<2002,2001,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2002&Towers.Ann$year<2003,2002,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2003&Towers.Ann$year<2004,2003,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2004&Towers.Ann$year<2005,2004,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2005&Towers.Ann$year<2006,2005,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2006&Towers.Ann$year<2007,2006,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2007&Towers.Ann$year<2008,2007,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2008&Towers.Ann$year<2009,2008,Towers.Ann$year)
        Towers.Ann$year<-ifelse(Towers.Ann$year>=2009,2009,Towers.Ann$year)
Towers.Ann$year<-ifelse(Towers.Ann$year>=2010&Towers.Ann$year<2011,2010,Towers.Ann$year)
Towers.Ann$year<-ifelse(Towers.Ann$year>=2011&Towers.Ann$year<2012,2011,Towers.Ann$year) 
Towers.Ann$year<-ifelse(Towers.Ann$year>=2012&Towers.Ann$year<2013,2012,Towers.Ann$year)
        write.csv(Towers.Ann, "Towers.Ann.csv")
        

#flux tower reading are in micromoles/m2/sec, and readings are taken every 30 minutes
        GPP<-aggregate(GPP~year,mean,data=Towers.Ann)
        RE<-aggregate(RE~year,mean,data=Towers.Ann)
        Towers.Ann<-aggregate(NPP~year,mean,data=Towers.Ann)
        Towers.Ann<-data.frame(year=GPP$year,GPP=GPP$GPP,RE=RE$RE,NPP=GPP$GPP-RE$RE)
        
        
        Towers.Ann$NEP<-Towers.Ann$NEP*-1    #negative NEE = positive NEP, so take x -1 to make positive
        Towers.Ann$NPP<-Towers.Ann$NPP/1000000*0.0120107*10000*0.001*31556900  #conversion to Mg/ha/yr
