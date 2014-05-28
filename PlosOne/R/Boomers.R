# Author: triffe
###############################################################################

#Dx <- read.csv("/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Data/USCBdeaths.csv", stringsAsFactors = FALSE)
#
## Male = 1
## Female = 2
## Total = 0
## make male and female matrices:
#Dxm           <- as.matrix(Dx[Dx$SEX == 1, 5:ncol(Dx)])
#Dxf           <- as.matrix(Dx[Dx$SEX == 2, 5:ncol(Dx)])
#rownames(Dxf) <- rownames(Dxm) <- sort(unique(Dx$YEAR))
#colnames(Dxf) <- colnames(Dxm) <- 0:85
## transpose to age by year:
#Dxm           <- t(Dxm)
#Dxf           <- t(Dxf)

# Need to break up 85+, but where to put them? 
# Instead we should thanatologically redistribute the 85+ population?

library(DemogBerkeley)
username      <- userInput()
password      <- userInput()
mlt           <- readHMDweb("USA", "mltper_1x1", username = username, password = password)
flt           <- readHMDweb("USA", "fltper_1x1", username = username, password = password)
P             <- readHMDweb("USA", "Population", username = username, password = password)

# make mx matrices:
library(reshape2)
mxmHMD <- acast(mlt, Age~Year, value.var = "mx")
mxfHMD <- acast(flt, Age~Year, value.var = "mx")
#colnames(mxmHMD) # 2010 most recent

# I think the hybrid data-mungy procedure is far too messy and difficult to explain. It'd
# make much more sense to simply assume certain annual rates of improvement: 0, .5%, 1%
# ----------------------------------------------------
# so what have been some empirical rates of improvement, just to make sure we're in the usual range:
# ----------------------------------------------------
# looked at different year-ranges.
# avg improvement varies greatly over age, and only recently has 100+ mortality even begun to improve.
# ranges between -1% and 4% improvement per age per year, and most vaues are in the 1% to 3% range.
# therefore a conservative assumption would be sustaied .5% increases per age per year. Very modest.
# if the pattern continues, large gains are in store for ages 90+, which will be our boomers down the road.
yr1 <- as.character(1933:2009)
yr2 <- as.character(1934:2010)
#
chgm <- (2*(mxmHMD[,yr2] - mxmHMD[,yr1])) / 
        (mxmHMD[,yr1] + mxmHMD[,yr2]) 

#
chgf <- (2*(mxfHMD[,yr2] - mxfHMD[,yr1])) / 
        (mxfHMD[,yr1] + mxfHMD[,yr2]) 
# function from http://druedin.com/2012/08/11/moving-averages-in-r/
mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}
AnnImprov   <- -(colMeans(chgm) + colMeans(chgf))/2
yrs         <- 1933:2009
#plot(yrs,cumprod(1+AnnImprov)/((1934:2010)-1934),xlim=c(1950,2010),ylim=c(0,.05)) # not same
# graphics.off()
plot(yrs, AnnImprov, pch = 19, col="gray",main = "Sustained improvements\nbetween .5% and 3% per annum on avg")
lines(yrs, mav(AnnImprov, 8), col = "blue")
lines(predict(smooth.spline(AnnImprov~yrs)), col = "green")
abline(h=0)
legend("topright",
        col=c("gray","blue","green"),
        lty=c(NA,1,1),
        pch=c(19,NA,NA),
        legend=c("Annual avg.","8-yr avg.","smooth spline"))

# so my assumptions will be more simplified than that, 
# but in a similar, if not more conservative range.
# assumptions:
# December 31 Male and Female populations:
Pm2011  <- P$Male2[P$Year == 2010]
Pf2011  <- P$Female2[P$Year == 2010]

horizon <- 65                    # how many years out do we need to jump in order to have full attrition
years   <- 2011:(2011 + horizon - 1) # concretely,which years do we project through?
# different improvement possibilities

iota.assumptions       <- matrix(nrow = horizon, ncol = 6, dimnames = list(years, 1:6))
iota.assumptions[,1]   <- rep(0, horizon)    # 1) iota = 0 = constant from jump off year
iota.assumptions[,2]   <- rep(.005, horizon) # 2) iota = .005 = 1/2 percent better per age per year = conservative
iota.assumptions[,3]   <- rep(.01, horizon)  # 3) iota = .01 = 1 percent improvement per age per year = very possible.
iota.assumptions[,4]   <- .01 / (1 + exp(-(seq(5, -5, length = horizon)))) # 4) iota = logistic with max 1% improvement
iota.assumptions[,5]   <- .02 / (1 + exp(-(seq(5, -5, length = horizon)))) # 5) iota = logistic with max 2% improvement
iota.assumptions[,6]   <- rep(.02, horizon)   # 6) iota = .02 = 2 percent improvement per age per year, rather optimistic
#
matplot(years,iota.assumptions,type='l',main="rate improvement assumptions")
# applied of the form:
# mx.improv = mx.prev * prod(1-iota.i[x:iteration])

# ----------------------------------------------------------
# now for the actual data exercise: how diffuse will the attrition of the boomers be?
# ----------------------------------------------------------
# take 2010 as jump off mortality, using 2011 Jan 1 pop:

mxm <- mxmHMD[,"2010"]
mxf <- mxfHMD[,"2010"]

# assume constant hazards within intervals, which means we can do:
sum(c(1,exp(-cumsum(mxm)))) # estimate e0
sum(c(1,exp(-cumsum(mxf)))) # estimate e0
# pretty much the same as the HMD estimate (2-digit accuracy, makes our lives easier)

# lifetable utils:
mx2lx <- function(mx){
    c(1,exp(-cumsum(mx[-length(mx)])))
}
# automatic close-out
mx2dx <- function(mx){
    -diff(c(mx2lx(mx), 0))
}
mx2Lx <- function(mx){
    lx <- mx2lx(mx)
    (lx + c(lx[2:length(lx)],0))/2
}
mx2ex <- function(mx){
    Lx <- mx2Lx(mx)
    rev(cumsum(rev(Lx)))
}

mxadj <- function(mx, iota){
    mult <- cumprod(1 - iota)
    outer(mx, mult, "*")
}
#image(log(t(mxadj(mxf,iota3))))

# --------------------------------------------------------------
# this is just exploratory: what are the e0 consequences of our
# various improvement scenarios?
e0f <- apply(iota.assumptions,2,function(iota, .mx){
            apply(mxadj(.mx,iota),2,function(mx){
                        mx2ex(mx)[1]
                    })
        },.mx=mxf)
e0m <- apply(iota.assumptions,2,function(iota, .mx){
            apply(mxadj(.mx,iota),2,function(mx){
                        mx2ex(mx)[1]
                    })
        },.mx=mxm)
# these pretty much define the bounds of plausibility. 2% (assumption 6)
# is quite high, but doesn't buck the trend either -- it would have males
# and females well over 90 by 2075: high, but not out of this world.

# constant 2010 is pessimistic. We have improvements in the pipeline.

# the logisitc decrease in improvement (2% to 0%) is a kind of old-school
# scenario. I don't actually think we'll max-out so easily, but who's to say
# how things will be in 60 years. Further, the ending-e0 of this scenario
# implies attaining Japans CURRENT e0 after 60 years. So if I had to choose
# between the 2% and the logisitc tapering, I'd almost bet on the constant
# improvement, because 1) the US is a lagger, and should catch up with the 
# leader one day and 2) the current leader (Japan) will also continue to
# improve. i.e., there is *plenty* of room for improvement in the US.

# Take a look at all e0 trajectories:

matplot(years, e0f, type='l',ylim=c(70,100))
matplot(years, e0m, type='l',ylim=c(70,100), add= TRUE, lty=2)
abline(h=c(86.43,   79.96), col = "green", lwd = 2) # Japan 2012
e0f[,6] - 86.43 # 2035
e0m[,6] - 79.96 # 2025
e0f[,5] - 86.43 # 2037
e0m[,5] - 79.96 
# constant rates of improvement entail convergence between the sexes (truism)
matplot(years, e0f-e0m, type='l') 

# these are the 3 trajectories that will go into the boomer plot:
matplot(years, e0f[,c(1,5,6)], type='l',ylim=c(75,98), lty=1)
matplot(years, e0m[,c(1,5,6)], type='l',ylim=c(75,98), add= TRUE, lty=2)

# ---------------------------------------------------------

# ---------------------------------------------------------
# the redistribution (death projection) function to be used here.
# spits out object Fx
# fx is like lifetable dx, but conditional on reaching a certain age
# fx sums to 1.
# Fx is the same thing, but calculated for *each* individual cohort of the boomer generation
# i.e., it's staggered to match up 'year of death'.
# ---------------------------------------------------------
# args: mx is the jump-off mx vector (complete, ages 0-110+)
#       iota is the improvement assumption
#       boomers is a vector of the boomer years, for labelling mostly
#       boomer index vector (referring to jumo-off year)
getFx <- function(mx, iota, boomers, b.i){
    horizon     <- length(iota)
    N           <- length(mx)
    Fx          <- matrix(0,
                          nrow = length(boomers),
                          ncol = horizon, 
                          dimnames = list(boomers, years))
    # loop over the individual boomer cohorts
    for (i in 1:length(boomers)){
        # what mx pattern lies ahead for this cohort
        mx.i        <- mx[b.i[i]:N]
        # discount for cumulative improvements:
        mx.i.adj    <- mx.i * cumprod(1 - iota[1:length(mx.i)])
        # whats a partial lx that corresponds to this?
        lx          <- c(exp(-cumsum(mx.i.adj)), 0) # not good for age 0, but we don't work with age 0 in this exercise
        # and what kind of attrition dose this imply?
        fx          <- -diff(lx) / lx[1]
        # fill in, from front (stacked by year)
        Fx[i,1:length(fx)] <- fx
    }    
    Fx
}

# ---------------------------------------------
# the boomer cohorts
boomers     <- 1946:1964
# Jan 1, 2011. They were this old
(ages       <- 2011 - boomers - 1)
# boomer index in jump off data:
b.i         <- ages + 1
# OK, let's do an easy-peasy projection of decrement
DF          <- DM <- matrix(nrow = 6, ncol = horizon, 
                                dimnames = list(1:6, years))
# the main loop (for each improvement assumption)
for (i in 1:6){
    # each column would be an annual cohort within the boomers, which we don't separate.
    DM[i, ] <- colSums(getFx(mxm, iota.assumptions[, i], boomers, b.i) * Pm2011[b.i])
    DF[i, ] <- colSums(getFx(mxf, iota.assumptions[, i], boomers, b.i) * Pf2011[b.i])
}
(B <- sum(Pm2011[b.i]) + sum(Pf2011[b.i])) # 78 million boomers on Jan 1, 2011! Wow, that's a lot!
#rowSums(DM + DF)-B # sums match!
# males and females: females dashed, colors correspond to iota assumptions
matplot(years,t(DF), type = 'l', lty = 2,
        ylab = "Deaths", xlab="Year",main = "Male and Female boomer decrement\ndifferent assumptions of annual mortality improvement")
matplot(years,t(DM), type = 'l', lty = 1, add = TRUE)

# we plot them together:

matplot(t(DM + DF), type = 'l')

# a quantile function for these data. Mostly to find the middle bulk and the right tail
getQ <- function(DD,q,years){
    years[which(cumsum(DD)/sum(DD) - q > 0)[1]]
}
#plot(2011:2074,diff(iota.assumptions[,5]))
# total boomer deaths:
DD <- DM + DF
Quantiles <- matrix(nrow=6,ncol=5,dimnames=list(1:6,c(.25,.5,.75,.9,.98)))
for (i in 1:6){
    Quantiles[i, ] <- c(getQ(DD[i,],.25,years),
            getQ(DD[i,],.5,years),
            getQ(DD[i,],.75,years),
            getQ(DD[i,],.9,years),
            getQ(DD[i,],.98,years))     
}


# interquartile range:
Quantiles[,"0.75"] - Quantiles[,"0.25"]
# i.e. 1/2 of the boomers that were alive in 2011 will 
# take a period of time roughly equal to the full boomer
# cohort width to exit the population.
# assumption #5 as likely the most palatable of these, and
# its interquartile range is 19...
# this is due to 
# 1) The lifetable deaths distribution is diffuse for a single-year cohort, but the boomers are actuall 19 cohorts
# 2) Improvements move the distribution right, but since they come gradually, it also stretches (widens) the distribution.
#    We don't know how improvements will continue, but no matter how you move around improvement you get a similar
#    outcome
# 3) Females are further to the right than males, so we have two distributions with different centers, which
#    widens the distribution even more.
# 4) Note that these rates are averages, and that the storey is more complex when changes are allowed
#    to vary over age. A look back over time will have us think that even bigger improvements are in
#    store for the older age groups that the boomers will pass through.

save(DD, file="/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Data/BoomerDx.Rdata")
save(Quantiles, file="/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Data/BoomerQuantiles.Rdata")

# ---------------------------------------------------------------------------
cMx <- readHMDweb("USA","cMx_1x1",username,password)
Bt <- readHMDweb("USA","Births",username,password)

bommerc <- as.character(boomers)
BommercMxm <- acast(cMx, Age~Year, value.var = "Male")[,bommerc]
BommercMxf <- acast(cMx, Age~Year, value.var = "Female")[,bommerc]

ml2010 <- apply(BommercMxm, 2, function(mx){
            rev(mx2lx(mx[!is.na(mx)]))[1]
        })
fl2010 <- apply(BommercMxf, 2, function(mx){
            rev(mx2lx(mx[!is.na(mx)]))[1]
        })

Morig <- Bt$Male[Bt$Year >= 1946 & Bt$Year <= 1964]
Forig <- Bt$Female[Bt$Year >= 1946 & Bt$Year <= 1964]
sum(Morig+Forig)
sum(Morig*ml2010+Forig*fl2010)

