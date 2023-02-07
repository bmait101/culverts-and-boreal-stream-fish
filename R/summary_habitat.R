dat1=read.csv("Habitat_Data_RAW_noPP2.csv") ##noPP1 has fixed depth values, temp and DO
attach(dat1)
str(dat1)
names(dat1)

#nrow(dat1)
#dat2=dat1[LOC=="down"|LOC=="up",]		###REMOVE THE PLUNGE POOLS
#write.csv(dat2, "Habitat_Data_RAW_noPP.csv",
        #  row.names=F)

### check for entry errors ###
boxplot(COND~xingTYPE*LOC)
boxplot(TEMP~xingTYPE*LOC)  
boxplot(DO~xingTYPE*LOC)  
boxplot(TURBID~xingTYPE*LOC)
boxplot(VELO~xingTYPE*LOC)          #error, one reading over 8 m/s - FIXED
boxplot(mean_DEPTH~xingTYPE*LOC)    #error, culvert down @ ~1.7; ref up @ >2.0 - FIXED
boxplot(WET_W~xingTYPE*LOC)         
boxplot(BANK_W~xingTYPE*LOC)
boxplot(FINES~xingTYPE*LOC)
boxplot(GRAVEL~xingTYPE*LOC)
boxplot(COBBLE~xingTYPE*LOC)
boxplot(POOL~xingTYPE*LOC)
boxplot(RIFF~xingTYPE*LOC)
boxplot(RUN~xingTYPE*LOC)


### SUMMARIZE BY STREAM REACH


library(plyr)
summary1=ddply(dat1,.(SER,xingTYPE,LOC), 
               summarise, 
               
               mWET		= mean(WET_W, na.rm=T),
               sdWET  = sd(WET_W, na.rm=T),
               mBANK	= mean(BANK_W, na.rm=T),
               sdBANK	= sd(BANK_W, na.rm=T),
               mDEPTH	= mean(mean_DEPTH, na.rm=T),
               sdDEPTH= sd(mean_DEPTH, na.rm=T),
               mVELO	= mean(VELO, na.rm=T),
               sdVELO	= sd(VELO, na.rm=T),
               mTEMP	= mean(TEMP, na.rm=T),
               sdTEMP	= sd(TEMP, na.rm=T),
               mDO		= mean(DO, na.rm=T),
               sdDO		= sd(DO, na.rm=T),
               mPH		= mean(pH, na.rm=T),
               sdPH		= sd(pH, na.rm=T),
               mCOND	= mean(COND, na.rm=T),
               sdCOND	= sd(COND, na.rm=T),
               mTURBID= mean(TURBID, na.rm=T),
               sdTURBID= sd(TURBID, na.rm=T),		   	
               mFINES	= mean(FINES, na.rm=T),
               sdFINES	= sd(FINES, na.rm=T),
               mROCK	= mean(sumROCK, na.rm=T),
               sdROCK	= sd(sumROCK, na.rm=T),
               mPOOL		= mean(POOL, na.rm=T),
               sdPOOL	= sd(POOL, na.rm=T),
               mRIFF		= mean(RIFF, na.rm=T),
               sdRIFF	= sd(RIFF, na.rm=T),
               mRUN		= mean(RUN, na.rm=T),
               sdRUN		= sd(RUN, na.rm=T)
               
)

write.csv(summary1, "summary_habitat_byReach_pp2.csv")

####################################################################################
####################################################################################

SHOCKER SETTINGS

shock=read.csv("Shocker_Settings_RAW.csv")
attach(shock)

####################################################################################
####################################################################################
### Shocker effort ANOVA

shock.sub=shock[LOC=="down"|LOC=="up",]  
write.csv(shock.sub, "shock_analysis.csv", row.names=F)
shock.new=read.csv("shock_analysis.csv")
attach(shock.new)
boxplot(SECONDS~xingTYPE*LOC)
RES=residuals(lm(log(SECONDS)~xingTYPE*LOC))
hist(RES)
shapiro.test(RES)   ### barely significant, 0.4
anova(lm(SECONDS~xingTYPE*LOC)) ### so no difference in ANOVA - consistant effert
####################################################################################
####################################################################################


shock2=shock[LOC=="down"|LOC=="up",]  	###REMOVE THE PLUNGE POOLS
attach(shock2)

library(plyr)
summary2=ddply(shock2,.(SER,xingTYPE,LOC), 
               summarise, 
               
               seconds		= mean(SECONDS))

write.csv(summary2, "summary_shock.csv")

####################################################################################
####################################################################################

### SUMMARIZE BY CROSSING TYPE AND LOCATION


summary2=ddply(dat2,.(xingTYPE,LOC), 
		          summarise, 

			mWET		= mean(WET_W, na.rm=T),
			sdWET		= sd(WET_W, na.rm=T),		
			mBANK		= mean(BANK_W, na.rm=T),
			sdBANK	= sd(BANK_W, na.rm=T),
		   	mDEPTH	= mean(mean_DEPTH, na.rm=T),
			sdDEPTH	= sd(mean_DEPTH, na.rm=T),
		   	mVELO		= mean(VELO, na.rm=T),
			sdVELO	= sd(VELO, na.rm=T),
		   	mTEMP		= mean(TEMP, na.rm=T),
			sdTEMP	= sd(TEMP, na.rm=T),
		   	mDO		= mean(DO, na.rm=T),
			sdDO		= sd(DO, na.rm=T),
		   	mPH		= mean(pH, na.rm=T),
			sdPH		= sd(pH, na.rm=T),
		   	mCOND		= mean(COND, na.rm=T),
			sdCOND	= sd(COND, na.rm=T),
		   	mTURBID	= mean(TURBID, na.rm=T),
			sdTURBID	= sd(TURBID, na.rm=T),		   	
		   	mFINES	= mean(FINES, na.rm=T),
			sdFINES	= sd(FINES, na.rm=T),
		   	mGRAVEL	= mean(GRAVEL, na.rm=T),
		   	sdGRAVEL	= sd(GRAVEL, na.rm=T),
			mCOBBLE	= mean(COBBLE, na.rm=T),
			sdCOBBLE	= sd(COBBLE, na.rm=T),			
		   	mBOULDER	= mean(BOULDER, na.rm=T),
			sdBOULDER	= sd(BOULDER, na.rm=T),
			mPOOL		= mean(POOL, na.rm=T),
			sdPOOL	= sd(POOL, na.rm=T),
		   	mRIFF		= mean(RIFF, na.rm=T),
			sdRIFF	= sd(RIFF, na.rm=T),
		   	mRUN		= mean(RUN, na.rm=T),
			sdRUN		= sd(RUN, na.rm=T)

		
)

nrow(summary2)
write.csv(summary2, "summary_stats_ByXING.csv")


### SUMMARIZE BY CROSSING TYPE ONLY


summary3=ddply(dat2,.(xingTYPE), 
               summarise, 
               
               mWET		= mean(WET_W, na.rm=T),
               sdWET		= sd(WET_W, na.rm=T),		
               mBANK		= mean(BANK_W, na.rm=T),
               sdBANK	= sd(BANK_W, na.rm=T),
               mDEPTH	= mean(mean_DEPTH, na.rm=T),
               sdDEPTH	= sd(mean_DEPTH, na.rm=T),
               mVELO		= mean(VELO, na.rm=T),
               sdVELO	= sd(VELO, na.rm=T),
               mTEMP		= mean(TEMP, na.rm=T),
               sdTEMP	= sd(TEMP, na.rm=T),
               mDO		= mean(DO, na.rm=T),
               sdDO		= sd(DO, na.rm=T),
               mPH		= mean(pH, na.rm=T),
               sdPH		= sd(pH, na.rm=T),
               mCOND		= mean(COND, na.rm=T),
               sdCOND	= sd(COND, na.rm=T),
               mTURBID	= mean(TURBID, na.rm=T),
               sdTURBID	= sd(TURBID, na.rm=T),		   	
               mFINES	= mean(FINES, na.rm=T),
               sdFINES	= sd(FINES, na.rm=T),
               mGRAVEL	= mean(GRAVEL, na.rm=T),
               sdGRAVEL	= sd(GRAVEL, na.rm=T),
               mCOBBLE	= mean(COBBLE, na.rm=T),
               sdCOBBLE	= sd(COBBLE, na.rm=T),			
               mBOULDER	= mean(BOULDER, na.rm=T),
               sdBOULDER	= sd(BOULDER, na.rm=T),
               mPOOL		= mean(POOL, na.rm=T),
               sdPOOL	= sd(POOL, na.rm=T),
               mRIFF		= mean(RIFF, na.rm=T),
               sdRIFF	= sd(RIFF, na.rm=T),
               mRUN		= mean(RUN, na.rm=T),
               sdRUN		= sd(RUN, na.rm=T)
               
               
)


write.csv(summary3, "summary_stats_ByXINGonly.csv")


### SUMMARIZE BY LOC only


summary4=ddply(dat2,.(LOC), 
               summarise, 
               
               mWET  	= mean(WET_W, na.rm=T),
               sdWET		= sd(WET_W, na.rm=T),		
               mBANK		= mean(BANK_W, na.rm=T),
               sdBANK	= sd(BANK_W, na.rm=T),
               mDEPTH	= mean(mean_DEPTH, na.rm=T),
               sdDEPTH	= sd(mean_DEPTH, na.rm=T),
               mVELO		= mean(VELO, na.rm=T),
               sdVELO	= sd(VELO, na.rm=T),
               mTEMP		= mean(TEMP, na.rm=T),
               sdTEMP	= sd(TEMP, na.rm=T),
               mDO		= mean(DO, na.rm=T),
               sdDO		= sd(DO, na.rm=T),
               mPH		= mean(pH, na.rm=T),
               sdPH		= sd(pH, na.rm=T),
               mCOND		= mean(COND, na.rm=T),
               sdCOND	= sd(COND, na.rm=T),
               mTURBID	= mean(TURBID, na.rm=T),
               sdTURBID	= sd(TURBID, na.rm=T),		   	
               mFINES	= mean(FINES, na.rm=T),
               sdFINES	= sd(FINES, na.rm=T),
               mGRAVEL	= mean(GRAVEL, na.rm=T),
               sdGRAVEL	= sd(GRAVEL, na.rm=T),
               mCOBBLE	= mean(COBBLE, na.rm=T),
               sdCOBBLE	= sd(COBBLE, na.rm=T),			
               mBOULDER	= mean(BOULDER, na.rm=T),
               sdBOULDER	= sd(BOULDER, na.rm=T),
               mPOOL		= mean(POOL, na.rm=T),
               sdPOOL	= sd(POOL, na.rm=T),
               mRIFF		= mean(RIFF, na.rm=T),
               sdRIFF	= sd(RIFF, na.rm=T),
               mRUN		= mean(RUN, na.rm=T),
               sdRUN		= sd(RUN, na.rm=T)
               
               
)

write.csv(summary4, "summary_stats_ByLOConly.csv")
