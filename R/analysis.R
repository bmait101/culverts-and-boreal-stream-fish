
# Import and check data

# This data is a combo of the summarized fish data by site (each fish as variable), 
# summarized habitat data by site (as variables)
# species richness and diversity by site

library(tidyverse)
library(here)

library(lme4)
library(nlme)
library(glmmADMB)

dat1 <- read_csv(here("data", "summary_forAnalysis2.csv"))

# add log offset
dat2 <- dat1 %>% mutate(logAREA=log(AREA))

# check
boxplot(dat2$N ~ dat2$xingTYPE*dat1$LOC)


#dat2 <- dat2[-c(15,16),] # remove extreme outliers
dat2 <- transform(dat2, N.PHOX = N.NRDC + N.FNDC + N.NFDC)
dat2 <- transform(dat2, N.SALMO = N.MNWH + N.BLTR + N.ARGR)
dat2 <- transform(dat2, N.SUCK = N.WHSC + N.LNSC)


hab <- read.csv("Habitat_Data_RAW_noPP2.csv") # test habitat with all measures (7 per reach - correct df)




### See if habitat differs among sites 
###-----------------------------------------------------------
### See if habitat differs among sites 

names(dat2)
attach(dat2)
str(dat2)

# Wetted width
fitWet <- lme(mWET ~ xingTYPE*LOC, data=dat2, random = ~1|SER)  		    
summary(fitWet)
RES <- residuals(fitWet)
plot(RES)   

# Depth
fitDEP <- lme(log(mDEPTH) ~ xingTYPE*LOC, data=dat2, random = ~1|SER) 
summary(fitDEP)
RES <- residuals(fitDEP)
plot(RES)   

#Fines
fitFINES <- lme(mFINES ~ xingTYPE*LOC, data=dat2, random = ~1|SER)  
summary(fitFINES)
RES <- residuals(fitFINES)
plot(RES)   

# Gravel
fitGRAV <- lme(GRAVEL ~ xingTYPE*LOC, data=dat2, random = ~1|SER)
summary(fitGRAV)
RES <- residuals(fitGRAV)
plot(RES)   

# Cobble
boxplot(mCOBBLE~xingTYPE*LOC)
dat2 <- transform(dat2, logCob = log(mCOBBLE))
dat10 <- dat1[-c(22),]
fitCOB <- lme(logCob ~ xingTYPE*LOC, data=dat10, random = ~1|SER) 
summary(fitCOB)
RES <- residuals(fitCOB)
plot(RES)   

#Boulder
boxplot(mBOULDER~xingTYPE*LOC)
fitBOL <- lme(sqrt(mBOULDER) ~ xingTYPE*LOC, data=dat2, random = ~1|SER) 
summary(fitBOL)
RES <- residuals(fitBOL)
plot(RES)   

# Rock

fitROC <- lme(mROCK ~ xingTYPE*LOC, data=dat2, random = ~1|SER)  

summary(fitROC)
RES <- residuals(fitROC)
plot(RES)   

# TEMP
fitTEMP <- lme(mTEMP ~ xingTYPE*LOC, data=dat2, random = ~1|SER, na.action = na.exclude)  
summary(fitTEMP)
RES <- residuals(fitTEMP)
plot(RES)   

# DO
fitDO <- lme(mDO ~ xingTYPE*LOC, data=dat2, random = ~1|SER, na.action = na.exclude)  
summary(fitDO)
RES <- residuals(fitDO)
plot(RES)  

# pH
fitPH <- lme(mPH ~ xingTYPE*LOC, data=dat2, random = ~1|SER, na.action = na.exclude)  
summary(fitPH)
RES <- residuals(fitPH)
plot(RES)   

# COND
fitCOND <- lme(mCOND ~ xingTYPE*LOC, data=dat2, random = ~1|SER, na.action = na.exclude)  
summary(fitCOND)
RES <- residuals(fitCOND)
plot(RES)   

# Turbid
boxplot(dat2$TURBID~dat2$xingTYPE*dat2$LOC)
fitTURBID <- lme(mTURBID ~ xingTYPE*LOC, data=dat2, random = ~1|SER, na.action = na.exclude) 
summary(fitTURBID)
RES <- residuals(fitTURBID)
plot(RES)   

# Velocity
fitVELO <- lme(log(mVELO) ~ xingTYPE*LOC, data=dat2, random = ~1|SER, na.action = na.exclude)  
summary(fitVELO)
RES <- residuals(fitVELO)
plot(RES)   

# Pool
?lme
fitPOOL <- lme(mPOOL ~ xingTYPE*LOC, data=dat2, random = ~1|SER, na.action = na.exclude) 
summary(fitPOOL)
RES <- residuals(fitPOOL)
plot(RES)   

# RIFF
fitRIFF <- lme(mRIFF ~ xingTYPE*LOC, data=dat2, random = ~1|SER, na.action = na.exclude)  
summary(fitRIFF)
RES <- residuals(fitRIFF)
plot(RES)   

# RUN
fitRUN <- lme(mRUN ~ xingTYPE*LOC, data=dat2, random = ~1|SER, na.action = na.exclude)  
summary(fitRUN)
RES <- residuals(fitRUN)
plot(RES)   

# RUN
fitRUN <- lme(m)  
summary(fitRUN)
RES <- residuals(fitRUN)
plot(RES)   

### Fish responce to habitat variables

### Responces as a function of habitat variables

### Responce as a function of habitat
###-----------------------------------------------------------
### Responce as a function of habitat

# overall density
fit1 <- glmmadmb(formula = N ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(fit1)
RES <- residuals(fit1)
plot(RES)   

# sculpin density
fit2 <- glmmadmb(formula = N.SLSC ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(fit2)
RES <- residuals(fit2)
plot(RES)   

# Lake Chub density
fit3 <- glmmadmb(formula = N.LKCH ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(fit3)
RES <- residuals(fit3)
plot(RES)   

# Phoxinus density
names(dat2)
fit4 <- glmmadmb(formula = N.PHOX ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(fit4)
RES <- residuals(fit4)
plot(RES)   

# Brook Stickleback density
fit5 <- glmmadmb(formula = N.BRST ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(fit5)
RES <- residuals(fit5)
plot(RES) 

# Redside Shiner density
fit6 <- glmmadmb(formula = N.RDSH ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER) + offset(logAREA) , data=dat1, family="nbinom")        

summary(fit6)
RES <- residuals(fit6)
plot(RES) 

# Sucker density
fit7 <- glmmadmb(formula = N.SUCK ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(fit7)
RES <- residuals(fit7)
plot(RES) 

# LN DC density
fit8 <- glmmadmb(formula = N.LNDC ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(fit8)
RES <- residuals(fit8)
plot(RES) 

# Salmo density
names(dat1)
fit9 <- glmmadmb(formula = N.SALMO ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(fit9)
RES <- residuals(fit9)
plot(RES) 

# Richness
boxplot(dat1$RICH ~ dat1$xingTYPE*dat1$LOC) # check
fit10 <- glmmadmb(formula = RICH ~ mWET + mDEPTH + mVELO + mFINES + mROCK
                 + (1|SER), data=dat2, family="poisson")        

summary(fit10)
RES <- residuals(fit10)
plot(RES)  


### Responce across stream types accounting for habitat
###-----------------------------------------------------------
### Responce across stream types accounting for habitat

names(dat1)

# overall density
mod1 <- glmmadmb(formula = N ~ xingTYPE * LOC + mWET + mDEPTH
                 + (1|SER) +  offset(logAREA) , data=dat2, family="nbinom")        

summary(mod1)
RES <- residuals(mod1)
plot(RES) 

# Richness

mod2 <- glmmadmb(formula = RICH ~ xingTYPE*LOC + mWET + mDEPTH 
                 + (1|SER), data=dat2, family="poisson")        

summary(mod2)
RES <- residuals(mod2)
plot(RES)

# SLSC density

mod3 <- glmmadmb(formula = N.SLSC ~ xingTYPE*LOC + mWET  +mDEPTH
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod3)
RES <- residuals(mod3)
plot(RES) 

# LKCH density

mod4 <- glmmadmb(formula = N.LKCH ~ xingTYPE*LOC + mWET  +mDEPTH
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod4)
RES <- residuals(mod4)
plot(RES)

# PHOX density

mod5 <- glmmadmb(formula = N.PHOX ~ xingTYPE*LOC + mWET  +mDEPTH
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod5)
RES <- residuals(mod5)
plot(RES)

# Brook sticklback density

mod6 <- glmmadmb(formula = N.BRST ~ xingTYPE*LOC + mWET  +mDEPTH
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod6)
RES <- residuals(mod6)
plot(RES)

# BRedside Shiner density

mod7 <- glmmadmb(formula = N.RDSH ~ xingTYPE*LOC + mWET  +mDEPTH
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod7)
RES <- residuals(mod7)
plot(RES)

# LN sucker Shiner density

mod8 <- glmmadmb(formula = N.LNSC ~ xingTYPE*LOC + mWET  +mDEPTH
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod8)
RES <- residuals(mod8)
plot(RES)

# WH sucker Shiner density

mod9 <- glmmadmb(formula = N.WHSC ~ xingTYPE*LOC + mWET  +mDEPTH
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod9)
RES <- residuals(mod9)
plot(RES)

# Salmo  density

mod10 <- glmmadmb(formula = N.SALMO ~ xingTYPE*LOC + mWET  +mDEPTH
                 + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod10)
RES <- residuals(mod10)
plot(RES)

# LNDC densitry

mod11 <- glmmadmb(formula = N.LNDC ~ xingTYPE*LOC + mWET  +mDEPTH
                  + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod11)
RES <- residuals(mod11)
plot(RES)

# Sucker

mod12 <- glmmadmb(formula = N.SUCK ~ xingTYPE*LOC + mWET  +mDEPTH
                  + (1|SER) + offset(logAREA) , data=dat2, family="nbinom")        

summary(mod12)
RES <- residuals(mod12)
plot(RES)
