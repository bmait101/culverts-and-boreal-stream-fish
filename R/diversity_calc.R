#This was all run through an metric of interest were added to sunmmary_by Sites.csv


library(vegan) #load vegan package

fish1 <- read.csv("fish_wide.csv") #load fish data, it is now a named object (matrix) called 'fish'
fish <- fish1[,3:17]

cul <- fish1[fish1$TYPE=="cul",3:17]
bri <- fish1[fish1$TYPE=="bri",3:17]
ref <- fish1[fish1$TYPE=="ref",3:17]

#rows are sites (66 total), columns are species, numbers are abundances (raw number fish)

#----------------------------
#how to calculate richness. I will create a function here, that uses a data matrix (x).

rich<-function(x){
  x[x>0]<-1
  return(rowSums(x))
}
#try it
rich(fish)
#we can also use this funciton in vegan
specnumber(fish)	

#now to use it
fish_sp<-rich(fish)
#or
fish_sp<-specnumber(fish)
write.csv(fish_sp, "fish_rich.csv")

#what is the fewest and most species in a site?
min(fish_sp)  # 1
max(fish_sp)	# 10

#unfortunately vegan does not calculate Margalef's or Menhinick's diversity. Let's do Margalef's. Here I will create a function. It will call the 'rich' function above, so you need to run that first.

D.mg<-function(x){
  r<-rich(x)
  a<-rowSums(x)
  return((r-1)/log(a))
}

#now lets calculate it for fish
fish_Dmg<-D.mg(fish)	
#write.csv(fish_Dmg, "fish_Dmg.csv")

#now lets plot Dmg versus richness
plot(fish_sp,fish_Dmg,xlab="Species richness",ylab="Margalef's diversity")


####now let's estimate diversity with Chao's 
#luckily there is a function to do this in R

#across the 66 plots, we have recorded 16 species, but many are rare, 
#so we estimate the number we may have missed using 'specpool'

est.all<-specpool(fish)
est.all

#we can also do this for each site using 'estimateR'

est.plot<-estimateR(fish)

#now lets plot observed richness against estimated, but lets make two panels, 
# one for the previous figure

par(mfrow=c(1,2))
plot(fish_sp,fish_Dmg,xlab="Species richness",ylab="Margalef's diversity")

plot(est.plot[1,],est.plot[2,],xlab="Observed richness",ylab="Estimated richness (Chao)")
#---------------------------------------------------
######next let's look at rarefaction for all
#what is the number of fish in the lowest abundance plot?
min(rowSums(fish))
#2
#and which plot is it?
rowSums(fish)[rowSums(fish)==2]
#since there are different numbers of individuals across plots, 
#let's use rarefaction to comparethe plots as if they all had 2 species
rare_rich<-rarefy(fish,min(rowSums(fish)))
#now what is the min and max values
min(rare_rich)  # 1
max(rare_rich)  # 1.888
#lets draw a rarefaction curve for one plot (#1). to do this, we need to 
# calculate estimated number of species from 1 to the number of trees in plot 1
#number of trees in plot i
sum(fish[2,])
#or
rowSums(fish)[2]
#now we need to use a loop to calculate richness from 1 to 56
tmp.rich<-1:sum(fish[2,])
est.rich<-NULL
for (i in 1:length(tmp.rich)){
  est.rich[i]<-rarefy(fish[1,],i)
}
#let's plot this
par(mfrow=c(1,1))
plot(tmp.rich,est.rich,xlab="Number of individuals",ylab="Estimated richness")
#let's draw a line for the previous rarefaction based on 0 individuals
rare_rich[1] #estimated richness for plot 1
abline(v=1.838961,lty="dashed")
abline(h=rare_rich[1],lty="dashed")
#---------------------------------------------------
######next let's look at rarefaction for cul

#what is the number of fish in the lowest abundance plot?

min(rowSums(cul))
#2

#and which plot is it?

rowSums(cul)[rowSums(cul)==2]

#since there are different numbers of individuals across plots, 
#let's use rarefaction to comparethe plots as if they all had 2 species

rare_richC<-rarefy(cul,min(rowSums(cul)))

#now what is the min and max values

min(rare_richC)  # 1
max(rare_richC)  # 1.888

#lets draw a rarefaction curve for one plot (#1). to do this, we need to 
# calculate estimated number of species from 1 to the number of trees in plot 1

#number of trees in plot i
sum(cul[1,])
#or
rowSums(cul)[1]

#now we need to use a loop to calculate richness from 1 to 56

tmp.richC<-1:sum(cul[1,])
est.richC<-NULL

for (i in 1:length(tmp.richC)){
  est.richC[i]<-rarefy(cul[1,],i)
}

#let's plot this
par(mfrow=c(1,1))
plot(tmp.richC,est.richC,xlab="Number of individuals",ylab="Estimated richness")

#let's draw a line for the previous rarefaction based on 0 individuals

rare_richC[1] #estimated richness for plot 1

abline(v=1.838961,lty="dashed")
abline(h=rare_richC[1],lty="dashed")
#---------------------------------------------------
######next let's look at rarefaction for bri

#what is the number of fish in the lowest abundance plot?

min(rowSums(bri))
#12

#and which plot is it?

rowSums(bri)[rowSums(bri)==12]

#since there are different numbers of individuals across plots, 
#let's use rarefaction to comparethe plots as if they all had 12 species

rare_richB<-rarefy(bri,min(rowSums(bri)))

#now what is the min and max values

min(rare_richB)  # 1
max(rare_richB)  # 4.94

#lets draw a rarefaction curve for one plot (#1). to do this, we need to 
# calculate estimated number of species from 1 to the number of trees in plot 1

#number of trees in plot i
sum(bri[1,])
#or
rowSums(bri)[1]

#now we need to use a loop to calculate richness from 1 to 64

tmp.richB <- 1:sum(bri[1,])
est.richB <- NULL

for (i in 1:length(tmp.richB)){
  est.richB[i]<-rarefy(bri[1,],i)
}

#let's plot this
par(mfrow=c(1,1))
plot(tmp.richB,est.richB)
abline(est.richC~tmp.richC)

#let's draw a line for the previous rarefaction based on 0 individuals

rare_rich[1] #estimated richness for plot 1

abline(v=4.937146,lty="dashed")
abline(h=rare_rich[1],lty="dashed")
#---------------------------------------------------
######next let's look at rarefaction for ref

#what is the number of fish in the lowest abundance plot?

min(rowSums(ref))
#28

#and which plot is it?

rowSums(ref)[rowSums(ref)==28]

#since there are different numbers of individuals across plots, 
#let's use rarefaction to comparethe plots as if they all had 28 species

rare_rich<-rarefy(ref,min(rowSums(ref)))

#now what is the min and max values

min(rare_rich)  # 2
max(rare_rich)  # 7.66

#lets draw a rarefaction curve for one plot (#1). to do this, we need to 
# calculate estimated number of species from 1 to the number of trees in plot 1

#number of trees in plot i
sum(ref[1,])
#or
rowSums(ref)[1]

#now we need to use a loop to calculate richness from 1 to 51

tmp.rich<-1:sum(ref[1,])
est.rich<-NULL

for (i in 1:length(tmp.rich)){
  est.rich[i]<-rarefy(ref[1,],i)
}

#let's plot this
par(mfrow=c(1,1))
plot(tmp.rich,est.rich,xlab="Number of individuals",ylab="Estimated richness")

#let's draw a line for the previous rarefaction based on 0 individuals

rare_rich[1] #estimated richness for plot 1

abline(v=2.997882,lty="dashed")
abline(h=rare_rich[1],lty="dashed")
#---------------------------------
######diversity indices

#the first two are easy

shan<-diversity(fish,index="shannon")
simp<-diversity(fish,index="invsimpson")
write.csv(shan, "fish_shan.csv")
#but diversity numbers does not exist in R. I'll write the function. This time we also need to specify alpha (a).

D.Hill<-function(x,a){
  tot<-apply(x,1,sum)
  x<-sweep(x,1,tot,"/")
  x<-x^a
  D<-apply(x,1,sum,na.rm=TRUE)
  D<-D^(1/(1-a))
}

Dhill<-D.Hill(fish,0.4)

#Now lets plot the three with richness on the bottom axis.

par(mfrow=c(1,3))
plot(fish_sp,shan,xlab="Number of species",ylab="Shannon diversity")
plot(fish_sp,simp,xlab="Number of species",ylab="Simpson diversity")
plot(fish_sp,Dhill,xlab="Number of species",ylab="Diversity numbers")

#now eveness for all three

E.shan<-shan/log(fish_sp)
E.simp<-simp/fish_sp
E.numb<-Dhill/fish_sp
write.csv(E.shan, "fish_shanE.csv")
#Lets plot all three on the same plot

par(mfrow=c(1,1))
plot(fish_sp,E.shan,xlab="Number of species",ylab="Evenness",
     ylim=c(0,1), pch=19)
points(fish_sp,E.simp,pch=19,col="red")
points(fish_sp,E.numb,pch=19,col="blue")

#make legend
points(8,0.1,pch=19)
text(8,0.1,pos=4,"Shannon")

points(8,0.05,pch=19,col="red")
text(8,0.05,pos=4,"Simpson")

points(8,0,pch=19,col="blue")
text(8,0,pos=4,"Diversity numbers")

###next measures of dissimilarity. We'll use two fuctions. 
#'betadiver' for Sorensen and Jaccard, and 'vegdist' for Bray-Curtis.


sor<-betadiver(fish,method="sor")
jac<-betadiver(fish,method="j")
BC<-vegdist(fish, method="bray")

par(mfrow=c(1,3))
plot(jac,sor,xlab="Jaccards",ylab="Sorensens")
plot(jac,BC,xlab="Jaccards",ylab="Bray-Curtis")
plot(sor,BC,xlab="Sorensens",ylab="Bray-Curtis")

#but wait! jac and sor are expressed as similarity values and B-C as dissimilarity, so we need to get 1-BC for dissimilarity.

BC.dis<-1-BC

par(mfrow=c(1,3))
plot(jac,sor,xlab="Jaccards",ylab="Sorensens")
plot(jac,BC.dis,xlab="Jaccards",ylab="Bray-Curtis (dis)")
plot(sor,BC.dis,xlab="Sorensens",ylab="Bray-Curtis (dis)")



#finally, let's calculate whittaker's and Lande's beta diversity. For these, I'll create the functions. Whittaker's is shown in the vegan manual (p.11)

ncol(fish)/mean(fish_sp)-1

#here is Lande's

ncol(fish)-mean(fish_sp)

#now let's make a function that allows you to calculate both, plus returns gamma and alpha

abc<-function(x){
  gam<-ncol(x)
  alph<-mean(specnumber(x))
  whit<-gam/alph-1
  lande<-gam-alph
  out<-c(gam,alph,whit,lande)
  names(out)<-c("Gamma", "Mean Alpha", "Whittaker's Beta", "Lande's Beta")
  return(out)
}

abc(fish)	

