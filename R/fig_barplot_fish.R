
library(sciplot)

dat1=read.csv("summary_forAnalysis2.csv") # this just has D > zD for grouping for plots
dat2 <- transform(dat1, DEN.SALMO = DEN_N.ARGR + DEN_N.MNWH + DEN_N.BLTR)
dat2 <- transform(dat2, DEN.PHOX = DEN_N.NRDC + DEN_N.FNDC + DEN_N.NFDC)
dat2 <- transform(dat2, DEN.SUCK = DEN_N.WHSC + DEN_N.LNSC)

dat2 <- dat2[-c(15,16),] # remove extreme outliers
attach(dat2)
names(dat2)

### Density and Richness
#----------------------------------------------------------------------------------------

boxplot(DEN_N ~ xingTYPE * LOC)

graphics.off()
windows(width=10, height=5)
par(mfrow=c(1,1), cex=1.2, ps=12, family="serif",mar=c(5,4,2,2))

bargraph.CI(x.factor=xingTYPE, group=LOC, response=DEN_N,
            legend=F,
            xlab="Crossing Type", 
            names=c("Cul","Bri","Ref"),
            main="Density (n.m-2)", 
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,0.2), las=1,
            x.leg=1, y.leg=0.21, cex.leg=1.2, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.1, 
            err.col="black", err.lty=1,
            bty="n"
)

text(2, 0.11, "a", cex=1.5)

par(cex=1.2, ps=12, family="sans", mar=c(5,5,2,2))

bargraph.CI(x.factor=xingTYPE, group=LOC, response=RICH,
            legend=F,
            xlab="Crossing Type", 
            names=c("Cul","Bri","Ref"),
            main="Richness", 
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,8), las=1,
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.1, 
            err.col="black", err.lty=1
)

text(2, 6.3, "b", cex=1.5)
text(5, 4.2, "a", cex=1.5)

#----------------------------------------------------------------------------------

### Bar plots of most common species
#----------------------------------------------------------------------------------

graphics.off()
windows(width=10,height=5)
par(mar=c(3,3,2,2),cex=1, ps=12, family="serif")
par(mfrow=c(2,4))
#Sculpin
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.SLSC,
            legend=F,
            names=c("C","B","R"),
            main="(a) Slimy Sculpin", 
            cex.lab=1.2,
            ylim=c(0,0.06), las=1,
            x.leg=1, y.leg=0.06, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 0.02, "b")
text(5, 0.05, "a")
text(8, 0.05, "a")
#Lake Chub
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.LKCH,
            legend=T,
            names=c("C","B","R"),
            main="(b) Lake Chub", 
            cex.lab=1.2,
            ylim=c(0,0.06), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 0.02, "b")
#Phoxinus
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN.PHOX,
            legend=T,
            names=c("C","B","R"),
            main="(c) Phoxinus Sp.", 
            cex.lab=1.2,
            ylim=c(0,0.06), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 0.04, "b")
text(5, 0.03, "a")
#Brook stickleback
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.BRST,
            legend=F,
            names=c("C","B","R"),
            main="(d) Brook Stickleback", 
            cex.lab=1.2,
            ylim=c(0,0.016), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 0.015, "b")
#Redside Shiner
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.RDSH,
            legend=T,
            names=c("C","B","R"),
            main="(e) Redside Shiner", 
            cex.lab=1.2,
            ylim=c(0,0.016), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 0.005, "b")

#White Sucker
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN.SUCK,
            legend=T,
            names=c("C","B","R"),
            main="(f) Sucker Sp.", 
            cex.lab=1.2,
            ylim=c(0,0.016), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)

text(5, 0.003, "a")

# LNDC
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.LNDC,
            legend=F,
            names=c("C","B","R"),
            main="(g) Longnose Dace",
            cex.lab=1.2,
            ylim=c(0,0.016), las=1,
            x.leg=1, y.leg=0.001, 
            cex.leg=1.2, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)

text(2, 0.007, "a")

#Salmonids
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN.SALMO,
            legend=F,
            names=c("C","B","R"),
            main="(h) Salmonids",
            cex.lab=1.2,
            ylim=c(0,0.016), las=1,
            x.leg=1, y.leg=0.003, 
            cex.leg=1.2, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)






