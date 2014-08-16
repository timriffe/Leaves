# this script does not need to be re-run, since its output data object,
# PopulationBrChMa.csv
# is sitting in
# ~/PlosOne/Data already.
# rerunning this will not change that object.
# ----------------------------------------------

# change this!
setwd("/home/triffe/git/Leaves")

###############################################################################
# this does a spline interpolation to graduate.
# plotted results are reasonable enough. 
# ------------------------------------
# could be improved by constraining and maintaining smoothness,
# but that'sa bigger problem than it sounds like. These results
# will give back essentially the same e0, and more importantly
# there will be no visual difference in the pyramids / leaves
# produced. These results are for visualizations and the estimation
# of means only...
# ------------------------------------

Interp <- function(DAT){
    if (nrow(DAT) == 0){
        return(NULL)
    }
    ages           <- 0:110 # used at end
    
    # abridgde ages (lower end of interval)
    x              <- DAT$AgeInt
    
    # male and female Mx
    ym             <- DAT$Mxm
    yf             <- DAT$Mxf
    N              <- length(yf)
    
    # spline over logged Mx, extrapolating (no need for m0, which stays the same)
    yf2            <- c(yf[1], exp(spline(x[2:N], log(yf[2:N]), xout = 1:110)$y))
    ym2            <- c(ym[1], exp(spline(x[2:N], log(ym[2:N]), xout = 1:110)$y))
    
    # make sure high ages not decreasing
    f.ind          <- c(diff(yf2) < 0)
    f.ind          <- c(f.ind, rev(f.ind)[1])
    f.ind[1:80]    <- FALSE
    
    m.ind          <- c(diff(ym2) < 0)
    m.ind          <- c(m.ind, rev(m.ind)[1])
    m.ind[1:80]    <- FALSE
    
    yf2[f.ind]     <- yf2[sum(!f.ind)]
    ym2[m.ind]     <- ym2[sum(!m.ind)]
    
    # max rate 1.1 (arbitrary, HMD uses 1, this will be a flat line if imputed)
    yf2[yf2 > 1.1] <- 1.1
    ym2[ym2 > 1.1] <- 1.1
    
    dxm <- mx2dxHMD(ym2, "m")
    dxf <- mx2dxHMD(yf2, "f")
    
    # spit out df
    data.frame(Country = unique(DAT$Country), Year = unique(DAT$Year), Age = ages, 
            mxm = ym2, mxf = yf2, dxm = dxm, dxf = dxf, stringsAsFactors = FALSE)
}

# read in WHO abdridged rates
WHOabridgedRates <- read.csv("PlosOne/Data/WHOabridgedRates.csv", 
        stringsAsFactors = FALSE)

# now for ages 0-110 for China, Brazil and Malawi (2 years, because we need to interpolate to 2008)
GraduatedRates <- 
        do.call(rbind,
                lapply(
                        split(WHOabridgedRates,list(WHOabridgedRates$Country, WHOabridgedRates$Year)), 
                        Interp)
        )
# save out 
write.table(GraduatedRates, "PlosOne/Data/WHOgraduatedRates.csv",
        sep = ",", col.names = colnames(GraduatedRates), row.names = FALSE)


## get population counts
library(xlsx)
Population <- read.xlsx("PlosOne/Data/population.xls", sheetIndex = 1, 
        colClasses = c("character","integer","character","integer","numeric","character"))
# (6th column has the source)
Population <- Population[,1:5]

# keep Pop for Malawi, Brazil, China 2011
Population <- Population[Population$Country %in% c("Malawi", "Brazil", "China"),]
Population <- Population[Population$Year != 2000, ]
# tabulation weights off by a factor due to old fashioned digit rounding conventions.
# this actually makes no difference, since we plot structure as a percentage.

Population$Pop[Population$Country == "Malawi"] <- Population$Pop[Population$Country == "Malawi"] / 100

# stadardize, for coherent selection later
Population$Sex <- as.character(Population$Sex)
Population$Sex <- ifelse(Population$Sex == "Male", "m", "f")

Population$Source <- ifelse(Population$Country == "Malawi", "IPUMS",
        ifelse(Population$Country == "China", "USCB","IBGE"))

# write out
write.table(Population, "PlosOne/Data/PopulationBrChMa.csv",
        sep = ",", col.names = colnames(Population), row.names = FALSE)
