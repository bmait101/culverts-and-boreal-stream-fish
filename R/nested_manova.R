
library(vegan)

data(warcom)
data(warenv)
# use larger number of permutations for real studies
nested.npmanova(warcom~rift.valley+popshort, data=warenv, method="jac", 
                permutations=5)
nested.anova.dbrda(warcom~rift.valley+popshort, data=warenv, method="jac", 
                   permutations=5)
## End(Not run)


str(warenv)
str(datfact)
str(datcom)
str(warcom)


datcom=read.csv("fishden.csv")
datfact=read.csv("manova_factors.csv")

rownames(datfact) <- datfact$SITE       # creates rownames
rownames(datcom) <- datcom$SiteID       # creates rownames

datcom <- datcom[,c(2:12)]
datfact <- datfact[,c(2:4)]

nested.npmanova(datcom~ TYPE + LOC,data=datfact,
                method="bray", permutations=1000)
