###############################################################
###############################################################
### Read and import 
### Need to clean up data and get species count totals by site

dat1 <- read.csv("Fish_Data.csv")
str(dat1)

###############################################################
### SUMMARIZE for species count totals by site

library(plyr)
summary1 <- ddply(dat1,.(SPECIES,SiteID), 
               summarise, 
               N		= length(SPECIES),
               .drop=FALSE
           )

###############################################################
### SUMMARIZE for density, CUE for SITE by/AND SPECIES

shocker <- read.csv("Fish_effort_data.csv")         # load effort data
dat2 <- merge(summary1,shocker,by="SiteID")         # merge effort and fish catch data by SiteID

boxplot(dat2$N~dat2$xingTYPE*dat2$LOC)


dat2 <- transform(dat2, EFF_M=EFF_S/60)             # convert seconds to minutes
dat2 <- transform(dat2, AREA=mWET*LENGTH)           # calculate area 
dat2 <- transform(dat2, CUE=N/EFF_M)                # CUE as number fish per minute e-fishing
dat2 <- transform(dat2, DEN_CUE=CUE/AREA)           # Density as CUE per m^2
dat2 <- transform(dat2, DEN_N=N/AREA)               # Density as N per m^2

###   Use this table to make long to wide conversion to get matrix for NMDS

fish1=dat2[,c("SiteID","SPECIES","N")]                      # pull variable we want
wide=reshape(fish1, v.names="N", timevar="SPECIES",
             idvar=c("SiteID"), direction="wide")           # reshape to wide
write.csv(wide, "fish_wide.csv", na="", row.names=F)        # write to file

fish2=dat2[,c("SiteID","SPECIES","DEN_N")]                  # pull variable we want
wide2=reshape(fish2, v.names="DEN_N", timevar="SPECIES",
              idvar=c("SiteID"), direction="wide")           # reshape to wide
write.csv(wide2, "fish_wide_den.csv", na="", row.names=F)   # write to file

### Calculate richness and diversity

library(vegan)                          #load vegan package
rich <- wide[,2:17]
specnumber(rich)
fish_sp<-specnumber(fish)
write.csv(fish_sp, "fish_rich.csv")
shan<-diversity(rich,index="shannon")
write.csv(shan, "fish_shan.csv")

### ammend these two results to summary for analysis table (below)

###############################################################
#### CALCULATE density, CUE  by SITE ###########

summary2 <- ddply(dat2,.(SiteID, xingTYPE, LOC), 
		          summarise, 
		   	      N       = sum(N)
          )                                             # summarize N (no. fish caught) for each site (n=66)

dat3 <- merge(summary2,shocker,by="SiteID")             # merge effort data

dat3 <- transform(dat3, EFF_M=EFF_S/60)                 # convert seconds to minutes
dat3 <- transform(dat3, AREA=mWET*LENGTH)               # calculate area 

dat3 <- transform(dat3, CUE=N/EFF_M)                    # CUE as number fish per minute e-fishing  
dat3 <- transform(dat3, DEN_CUE=CUE/AREA)               # Density as CUE per m^2
dat3 <- transform(dat3, DEN_N=N/AREA)                   # Density as N per m^2

### This is the dataset for analyses to be run on

boxplot(dat3$N~dat3$xingTYPE.x*dat3$LOC.x)
boxplot(dat3$CUE~dat3$xingTYPE.x*dat3$LOC.x)
boxplot(dat3$DEN_CUE~dat3$xingTYPE.x*dat3$LOC.x)
boxplot(dat3$DEN_N~dat3$xingTYPE.x*dat3$LOC.x)

write.csv(dat3, "summary_forAnalysis1.csv", 
		row.names=F)
#
##
### This is the data set for analyses to be run on

## Calcuate Shannon, evenness, richness diversity from
# 	Vegan package on converted tabled
#	and ammend to the above table for analyses
#
###############################################################
###############################################################



