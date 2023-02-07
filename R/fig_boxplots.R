newdat=read.csv("summary_forAnalysis.csv")

attach(newdat)
str(newdat)

### Fish community metrics

par(mfrow=c(1,1))

boxplot(DEN_N~LOC*xingTYPE)
boxplot(RICH~LOC*xingTYPE)
boxplot(SHANNON~LOC*xingTYPE)
boxplot(EVENNESS~LOC*xingTYPE)

graphics.off()
windows(width=10, height=5)
par(mfrow=c(1,4))
par(mar=c(4,3,1,1),cex=1, ps=12, family="serif")

boxplot(DEN_N~LOC*xingTYPE, las=1,
        at=c(1,2, 4,5, 7,8),
        ylim=c(0,0.02),main="A) Fish density (n*m-2)",  xlab="Stream Type",
        col=c("#FFFFFF","#999999","#FFFFFF","#999999","#FFFFFF","#999999"),
        names=c("B", "", "C", "", "R", "")
)
legend(0.02~0.5, cex=1, pch=c(22,22),
       col=c("#999999","#999999"),
       legend=c("Upstream","Downstream"), bty="n"
)
boxplot(RICH~LOC*xingTYPE, las=1,
        at=c(1,2, 4,5, 7,8),main="B) Richness",
        ylim=c(0,12),   xlab="Stream Type",
        col=c("#FFFFFF","#999999","#FFFFFF","#999999","#FFFFFF","#999999"),
        names=c("B", "", "C", "", "R", "")
)
boxplot(SHANNON~LOC*xingTYPE, las=1,
        at=c(1,2, 4,5, 7,8),main="C) Shannon index (H)",
        ylim=c(0,2.5),     xlab="Stream Type",
        col=c("#FFFFFF","#999999","#FFFFFF","#999999","#FFFFFF","#999999"),
        names=c("B", "", "C", "", "R", "")
)
boxplot(EVENNESS~LOC*xingTYPE, las=1,
        at=c(1,2, 4,5, 7,8),main="D) Evenness",
        ylim=c(0,1),    xlab="Stream Type",
        col=c("#FFFFFF","#999999","#FFFFFF","#999999","#FFFFFF","#999999"),
        names=c("B", "", "C", "", "R", "")
)

#----------------------------------------------------------------------------------

### Bar plots of most common species
library(sciplot)
?bargraph.CI()
graph=read.csv("summary_forAnalysis.csv")
attach(graph)
data.frame(names(graph))

max(graph$DEN_N.RDSH)

par(mfrow=c(1,1))

graphics.off()
windows(width=10,height=5)
par(mar=c(5,5,2,2),cex=1, ps=12, family="serif")
par(mfrow=c(2,4))
#Salmonids
bargraph.CI(x.factor=xingTYPE, group=LOC, response= SALMO.DEN,
            legend=T,
            xlab="Crossing Type", 
            names=c("B","C","R"),
            ylab="Salmonid Density (n*m-2)", 
            cex.lab=1.2,
            ylim=c(0,0.003), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FFFFFF", "#999999"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
#Sculpin
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.SLSC,
            legend=T,
            xlab="Crossing Type", 
            names=c("B","C","R"),
            ylab="Slimy Sculpin", 
            cex.lab=1.2,
            ylim=c(0,0.055), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FFFFFF", "#999999"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
#Lake Chub
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.LKCH,
            legend=T,
            xlab="Crossing Type", 
            names=c("B","C","R"),
            ylab="Lake Chub", 
            cex.lab=1.2,
            ylim=c(0,0.05), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FFFFFF", "#999999"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
#Phoxinus
bargraph.CI(x.factor=xingTYPE, group=LOC, response= PHOX.DEN,
            legend=T,
            xlab="Crossing Type", 
            names=c("B","C","R"),
            ylab="Phoxinus Sp.", 
            cex.lab=1.2,
            ylim=c(0,0.05), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FFFFFF", "#999999"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
#Brook stickleback
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.BRST,
            legend=T,
            xlab="Crossing Type", 
            names=c("B","C","R"),
            ylab="Brook Stickleback", 
            cex.lab=1.2,
            ylim=c(0,0.016), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FFFFFF", "#999999"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
#Redside Shiner
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.RDSH,
            legend=T,
            xlab="Crossing Type", 
            names=c("B","C","R"),
            ylab="Redside Shiner", 
            cex.lab=1.2,
            ylim=c(0,0.016), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FFFFFF", "#999999"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
#Longnose Sucker
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.LNSC,
            legend=T,
            xlab="Crossing Type", 
            names=c("B","C","R"),
            ylab="Longnose Sucker", 
            cex.lab=1.2,
            ylim=c(0,0.016), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FFFFFF", "#999999"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
#White Sucker
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DEN_N.WHSC,
            legend=T,
            xlab="Crossing Type", 
            names=c("B","C","R"),
            ylab="White Sucker", 
            cex.lab=1.2,
            ylim=c(0,0.016), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FFFFFF", "#999999"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)