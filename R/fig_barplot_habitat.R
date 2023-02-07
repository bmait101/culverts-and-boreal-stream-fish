

library(sciplot)
?bargraph.CI
dat1=read.csv("summary_forAnalysis2.csv") # this just has D > zD for grouping for plots
dat1=read.csv("Habitat_Data_RAW_noPP2.csv") # this gives correct


dat2 <- dat1[-c(15,16),] # remove extreme outliers
attach(dat2)
names(dat2)


graphics.off()
windows(width=12,height=6)
par(mar=c(5,5,2,2),cex=1.5, ps=12, family="Times")
par(mfrow=c(3,4))

#mean_DEPTH
par(mar=c(4,4,4,2))
bargraph.CI(x.factor=xingTYPE, group=LOC, response= mean_DEPTH,
            leg=F,
            main="Mean Water Depth (m)",
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,0.5), las=1,
            x.leg=1, y.leg=0.53, cex.leg=1.5, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 0.35, "b", cex=1.2)
text(5, 0.25, "a", cex=1.2)
#Velocity
bargraph.CI(x.factor=xingTYPE, group=LOC, response= VELO,
            main="Water Velocity (m.s-1)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1,cex.names=1.5,
            ylim=c(0,0.3), las=1,
            x.leg=1, y.leg=21, cex.leg=1.5, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(5, 0.25, "A", cex=1.2)
text(8, 0.25, "A", cex=1.2)
#FINES
bargraph.CI(x.factor=xingTYPE, group=LOC, response= FINES,
            main="Fines Substrate (<2 mm, %)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,100), las=1,
            x.leg=1, y.leg=20, cex.leg=1.5, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 80, "a", cex=1.2)
text(5, 60, "a", cex=1.2)
text(8, 45, "A", cex=1.2)
#ROCK
bargraph.CI(x.factor=xingTYPE, group=LOC, response= sumROCK,
            main="Course Substrate (??? 2 mm, %)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,100), las=1,
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 75, "b", cex=1.2)
text(8, 85, "a", cex=1.2)
#POOL
bargraph.CI(x.factor=xingTYPE, group=LOC, response= POOL,
            main="Pool (%)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,80), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 70, "b", cex=1.2)

#RIFF
bargraph.CI(x.factor=xingTYPE, group=LOC, response= RIFF,
            main="Riffle (%)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,80), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 45, "b", cex=1.2)

#RUN
bargraph.CI(x.factor=xingTYPE, group=LOC, response= RUN,
            main="Run (%)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,80), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 45, "b", cex=1.2)

#TURBID
bargraph.CI(x.factor=xingTYPE, group=LOC, response= TURBID,
            main="Turbidity (NTUs)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,10), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 8.5, "b", cex=1.2)
text(5, 5.5, "a", cex=1.2)
text(8, 5, "a", cex=1.2)

#TEMP
bargraph.CI(x.factor=xingTYPE, group=LOC, response= TEMP,
            legend=F,
            main="Stream Temperature (°C)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,20), las=1,
            x.leg=1, y.leg=21, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 19, "a", cex=1.2)
text(5, 15, "A", cex=1.2)
text(8, 12, "A", cex=1.2)
#DO
bargraph.CI(x.factor=xingTYPE, group=LOC, response= DO,
            main="Dissolved Oxygen (mg.L-1)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,12), las=1,
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)

text(2, 11, "b", cex=1.2)

#pH
bargraph.CI(x.factor=xingTYPE, group=LOC, response= pH,
            main="pH", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,12), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)
text(2, 10, "b", cex=1.2)
text(5, 10, "a", cex=1.2)
text(8, 10, "a", cex=1.2)
#COND
bargraph.CI(x.factor=xingTYPE, group=LOC, response= COND,
            main="Conductivity (?s.cm-1)", 
            names=c("Cul","Bri","Ref"),
            cex.lab=1.5, cex.axis=1.1,cex.names=1.5,
            ylim=c(0,400), las=1,
            x.leg=1, y.leg=6, cex.leg=1, 
            leg.lab=c("Upstream","Downstream"),
            col=c("#FF9999", "#FF3300","#33CCFF", "#0000FF","#00CC33", "#006633"),
            uc=T, lc=T, err.width=0.03, 
            err.col="black", err.lty=1,
)



