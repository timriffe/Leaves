# change this!
setwd("/home/triffe/git/Leaves")

# 1) 
## we'll need a few functions:
#library(RiffeetalFunctions)
library(data.table)
library(devtools)
install_github("DemogBerkeley", subdir = "DemogBerkeley", username = "UCBdemography")
install_github("Leaves", subdir = "PlosOne/R/RiffeetalFunctions", username = "timriffe")
library(RiffeetalFunctions)
library(DemogBerkeley)
# -------------------------------------------------------------------------------
# warning: the HMD part of this code takes around 15 minutes to execute
#          on my 2009 2.26GHz laptop

HMDcountries <- getHMDcountries()

us      <- userInput() # INTERACTIVE! enter HMD username in console, no quotes, then press enter
pass    <- userInput() # INTERACTIVE! enter HMD password in console, no quotes, then press enter

# the rest can be naively selected and run in one pass
Data <- do.call(rbind, lapply(HMDcountries, function(XXX,us,pass){
                    DATm      <- readHMDweb(XXX, "mltper_1x1", username = us, password = pass)
                    DATm$Code <- XXX
                    DATm$Sex  <- "m"
                    DATf      <- readHMDweb(XXX, "fltper_1x1", username = us, password = pass)
                    DATf$Code <- XXX
                    DATf$Sex  <- "f"
                    POP       <- readHMDweb(XXX, "Population", username = us, password = pass)
                    DATm$Pop1 <- POP$Male1
                    DATm$Pop2 <- POP$Male2
                    DATf$Pop1 <- POP$Female1
                    DATf$Pop2 <- POP$Female2
                    rbind(DATm, DATf)
                }, us = us, pass = pass))

# -------------------------------------------------------------------------------
# since we use dx, etc, lets standardize to radix 1
Data$lx <- Data$lx / 1e5
Data$Lx <- Data$Lx / 1e5
Data$Tx <- Data$Tx / 1e5

Data <- data.table(Data)

# dx needs to be done in chunks
st <- function(x){
    x / sum(x, na.rm = TRUE)
}
Data$dx <- as.double(Data$dx)
Data[, dx := st(dx), by = list(Code, Year, Sex)]
# now dx sums to 1..

# -----------------------------------
# Py is population classified by remaining years of life (thanatological age)
# the main goods!
Data[,Py := 
                Mna0(Minf0(rowSums(Thano(Pop1, dx))))   
        ,by=list(Code,Year,Sex)]

#-----------------------------------------
rownames(Data) <- NULL
#-----------------------------------------

# for funsies, not used, we also calculate median remaining years of life by age.
getMedianRYL <- function(lx){
    # distribution
    dx   <- c(-diff(lx),0)
    # repeated in a matrix
    DX   <- replicate(length(dx),dx)
    # zero out earlier ages as we move up in age
    DX[upper.tri(DX)] <- 0
    # standardize for each age (column) to sum to 1
    EDX  <- t(t(DX) / colSums(DX))
    # the cumulative sum give the proportion, we want to know when it hits .5
    EDXC <- apply(EDX,2,cumsum)
    # approximates an exact median remaing lifespan for each age
    MRYL <- apply(EDXC,2,function(llx){
                if (all(is.na(llx))){
                    return(NA)
                }
                splinefun(0:110~llx)(.5)
            }) - 0:110
    # in old ages you can get junk...
    MRYL[is.na(MRYL) | MRYL < 0] <- 0
    # return the goods to compare with ex.
    MRYL
}
# sorry might take a while to calculate.
Data[,MRYL := getMedianRYL(lx),by=list(Code,Year,Sex)]

# save it out for later use
save(Data,file = "PlosOne/Data/HMDAll.Rdata")


