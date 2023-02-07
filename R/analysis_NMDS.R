##--------------------------------------------------------------------
##--------------------------------------------------------------------
## Import and check

## This data is a combo of the summarized fish data by site, 
##   metrics calculated from vegan diversity,
##   and species rel abund as variables,
##   and habitat data

dat1=read.csv("summary_forAnalysis2.csv")
boxplot(dat1$N ~ dat1$xingTYPE*dat1$LOC) # check
dat2 <- transform(dat1, DEN.SALMO = DEN_N.ARGR + DEN_N.MNWH + DEN_N.BLTR)
dat2 <- transform(dat2, DEN.PHOX = DEN_N.NRDC + DEN_N.FNDC + DEN_N.NFDC)
dat2 <- transform(dat2, DEN.SUCK = DEN_N.WHSC + DEN_N.LNSC)

#dat2 <- dat2[-c(15,16),] # remove extreme outliers

#write.csv(dat2, "fishden.csv")

data.frame(names(dat2))
rownames(dat2) <- dat2$SiteID         # creates rownames
labels <- dat2$SiteID		            	# creates site labels
types <- dat2$xingTYPE  	            # creates site types
loc <- dat2$LOC
strata <- dat2$SER

fish.means <- dat2[,c(13:28)]
fish.means.all <- dat2[,c(13:28,62:64)]
fish.means.combo <- dat2[,c(14,16,20:21,24,26:28,62:64)]

fish.comm <- dat2[,9:10]
hab.means <- dat2[,c(45,47:54, 58:61)]



##------------------------------------------------------------------------------------------

?adonis
library(ecodist)
library(vegan) ### for {envfit}

### PERMONOVA

adonis(fish.means.combo ~ xingTYPE*LOC, 
                method = "bray",data=dat2, permutations=9999, strata = strata)


## NMDS {ecodist}
BC        <-  distance(fish.means.combo, "bray-curtis")      # calculates BS matrix

nmds_out  <- nmds(BC, mindim=2, maxdim=2)               # Runs NMDS
scores    <- nmds.min(nmds_out)                         # generate scores
nmds_out$stress                                         # 0.2509138

### add dimentions
nmds_out1  <- nmds(BC, mindim=2, maxdim=3)               # Runs NMDS
scores1    <- nmds.min(nmds_out1)                         # generate scores
nmds_out1$stress                                         # 0.1601039

#nmds_out2  <- nmds(BC, mindim=2, maxdim=4)               # Runs NMDS
#scores2    <- nmds.min(nmds_out1)                         # generate scores
#nmds_out1$stress                                         # 0.1601039

### 3 dimentions produced best fit
#plot(scores1$X1~scores1$X2, pch=c(17,15)[TYPE$LOC], 
 #    col=c("blue","red","#666666")[TYPE$xingTYPE], cex=1.3)
#plot(scores1$X1~scores1$X3, pch=c(17,15)[TYPE$LOC], 
#     col=c("blue","red","#666666")[TYPE$xingTYPE], cex=1.3)
#plot(scores1$X2~scores1$X3, pch=c(17,15)[TYPE$LOC], 
#     col=c("blue","red","#666666")[TYPE$xingTYPE], cex=1.3)

score12=scores1[,c("X1","X2")]
score13=scores1[,c("X1","X3")]
score23=scores1[,c("X2","X3")]
##------------------------------------------------------------------------------------------
## Fit vectors for the main variables to the NMDS configuration
?envfit

vec.fish <- envfit(scores, fish.means.combo, k=2, 1000, na.rm = TRUE) 
vec.comm <- envfit(scores, fish.comm, k=2, 1000, na.rm = TRUE)
vec.hab <- envfit(scores, hab.means, k=2, 1000, na.rm = TRUE)


vectors12_1=envfit(score12, fish.means, k=2, 1000, na.rm = TRUE) 
vectors12_2=envfit(score12, fish.comm, k=2, 1000, na.rm = TRUE)
vectors12_3=envfit(score12, hab.means, k=2, 1000, na.rm = TRUE)

vectors13_1=envfit(score13, fish.means, k=2, 1000, na.rm = TRUE) 
vectors13_2=envfit(score13, fish.comm, k=2, 1000, na.rm = TRUE)
vectors13_3=envfit(score13, hab.means, k=2, 1000, na.rm = TRUE)

vectors23_1=envfit(score23, fish.means.combo, k=2, 1000, na.rm = TRUE) 
vectors23_2=envfit(score23, fish.comm, k=2, 1000, na.rm = TRUE)
vectors23_3=envfit(score23, hab.means, k=2, 1000, na.rm = TRUE)


### Vector correlations with NMDS ordination
vec.fish
vec.comm 
vec.hab 

vectors23_1
vectors23_2
vectors23_3

##------------------------------------------------------------------------------------------
plot(scores, pch=c(15,17)[dat2$LOC], 
     col=c("red","blue","#666666")[dat2$xingTYPE], cex=1.3)
plot(vec.fish, col="black", cex=0.5)
plot(vec.hab, col="blue", cex=0.5)
plot(vec.comm, col="green", cex=0.5)


graphics.off()
windows(width=7, height=10) 
par (mfrow=c(2,1))
par (cex=1, ps=12, family="sans", mar=c(1,1,1,1)) 

plot(scores, col="white", 
     type="n", axes=F, ann=F,
     xlim=c(-0.9,0.75), ylim=c(-0.9,0.9))
box(which="plot", lty="solid")

points(scores, pch=c(15,17)[dat2$LOC], 
       col=c("red","blue","#666666")[dat2$xingTYPE], cex=1.3)
plot(scores, col="white", 
     type="n", axes=F, ann=F,
     xlim=c(-0.8,0.9), ylim=c(-0.8,0.8))
box(which="plot", lty="solid")
plot(vec.fish, col="black", cex=0.5)
plot(vec.hab, col="blue", cex=0.5)
plot(vec.comm, col="green", cex=0.5)

#text(scores,  labels=labels)


# Multiple dimensions

plot(score12, pch=c(15,17)[dat2$LOC], 
     col=c("red","blue","#666666")[dat2$xingTYPE], cex=1.3)
plot(vectors12_1, col="black", cex=0.5)
plot(vectors12_3, col="grey", cex=0.5)

plot(score13, pch=c(15,17)[dat2$LOC], 
     col=c("red","blue","#666666")[dat2$xingTYPE], cex=1.3)
plot(vectors13_1, col="black", cex=0.5)
plot(vectors13_3, col="grey", cex=0.5)

plot(score23, pch=c(15,17)[dat2$LOC], 
     col=c("red","blue","#666666")[dat2$xingTYPE], cex=1.3)
plot(vectors23_1, col="black", cex=0.5)
plot(vectors23_3, col="grey", cex=0.5)


graphics.off()
windows(width=7, height=10) 
par (mfrow=c(2,1))
par (cex=1, ps=12, family="sans", mar=c(1,1,1,1)) 

plot(score12, col="white", 
     type="n", axes=F, ann=F,
     xlim=c(-0.85,0.6), ylim=c(-0.8,0.7))
box(which="plot", lty="solid")

points(score23, pch=c(15,17)[dat2$LOC], 
       col=c("red","blue","#666666")[dat2$xingTYPE], cex=1.3)
plot(score23, col="white", 
     type="n", axes=F, ann=F,
     xlim=c(-0.8,0.9), ylim=c(-0.8,0.8))
box(which="plot", lty="solid")
plot(vectors23_1, col="black", cex=0.5)
plot(vectors23_3, col="blue", cex=0.5)
plot(vec.comm, col="green", cex=0.5)

#------------------------------------------------------------------------------------
#for(i in seq(groupz)) {                                               # ellispes x6
#  ordiellipse(score12, groups, conf=0.68, label = TRUE,
 #             font=2, cex=2, col=i, show.groups=groupz[i]) 
#} 

legend(0.6~0.3, cex=0.7, bty="y", 
       pch=c(2,0,0,0,0),
       col=c("black","black","red","blue","#666666"), 
       legend=c("Upstream","Downstream","Culvert","Bridge","Reference"))
text(-0.85,0.8, "R≤ = 0.70", pos=4)
text(-0.85,0.7, "Stress = 0.25", pos=4)

## bottom panel

plot(score23, col="white", 
     type="n", axes=F, ann=F,
     xlim=c(-0.6,0.6), ylim=c(-0.8,0.6))
box(which="plot", lty="solid")

plot(vectors23_1, col="black", cex=0.5)
plot(vectors23_2, col="blue", cex=0.5)
plot(vectors23_3, col="grey", cex=0.5)

title(ylab="NMDS Axis 2", xlab="NMDS Axis 1")
#text(scores, labels=labels)


#--------------------------------------------------------------------
# metaMDS {vegan}

require("vegan")
nmds <- metaMDS(fish.means.combo, k=3, wascores=TRUE)
plot(nmds, display = c("sites", "species"))

plot(nmds$points, col=c("blue","red","#666666")[dat2$xingTYPE], cex=1.3)
plot(nmds$species)

stressplot(nmds)

ordiplot(nmds,type="n")
orditorp(nmds,display="species",col="red",air=0.01)
orditorp(nmds,display="sites",cex=0.8,air=0.01)
# Fit vectors for the main variables to the NMDS configuration


vectors1=envfit(nmds$points, hab.means, k=2, 1000, na.rm = TRUE) 
vectors2=envfit(nmds$points,fish.means, k=2, 1000, na.rm = TRUE) 
vectors1
vectors2
plot(vectors1, col="blue")
plot(vectors2, col="red")
nmds
