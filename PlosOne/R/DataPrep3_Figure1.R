# this script must be run prior to Figure1.R
# the final data object produced for figure 1 is:
# Fig1Data, a data.frame

# -----------------------------------------

# change this!
setwd("/home/triffe/git/Leaves")

# ------------------------------------------------------
# read in Pop data that came from IPUMS, USCB and IBGE (processed in DataPrep2_WHO.R)
PopBrChMa <- read.csv("PlosOne/Data/PopulationBrChMa.csv", stringsAsFactors = FALSE)
# *redistribute counts of unknown age (NA) for Malawi:
MALm                <- PopBrChMa[PopBrChMa$Sex == "m" & PopBrChMa$Country == "Malawi", ]
MALf                <- PopBrChMa[PopBrChMa$Sex == "f" & PopBrChMa$Country == "Malawi", ]
m.unk.i             <- !is.na(MALm$Age)
f.unk.i             <- !is.na(MALf$Age)
# redistribute proportionally:
MALm$Pop[m.unk.i]   <- MALm$Pop[m.unk.i] + MALm$Pop[!m.unk.i] * MALm$Pop[m.unk.i] / sum(MALm$Pop[m.unk.i])
MALf$Pop[f.unk.i]   <-  MALf$Pop[f.unk.i] + MALf$Pop[!f.unk.i] * MALf$Pop[f.unk.i] / sum(MALf$Pop[f.unk.i])
# cut down:
MALm                <- MALm[m.unk.i, ]
MALf                <- MALf[f.unk.i, ]
# reappend:
PopBrChMa           <- PopBrChMa[!PopBrChMa$Country == "Malawi", ]
PopBrChMa           <- rbind(PopBrChMa, MALm, MALf)

# the  IPUMS, USCB and IBGE populations are in-order.
# -----------------------------------------------------
# read in HMD pop counts of interest:

HMDall <- local(get(load("PlosOne/Data/HMDAll.Rdata")))
HMDall <- as.data.frame(HMDall)
# need USA 2010, RUS 2010 and JAP 2009:

PopHMD <- rbind(HMDall[with(HMDall, Code == "USA" & Year == 2010), ],
                HMDall[with(HMDall, Code == "RUS" & Year == 2010), ],
                HMDall[with(HMDall, Code == "JPN" & Year == 2010), ]
          )
#PopHMD <- as.data.frame(PopHMD)
PopHMD           <- PopHMD[,c("Code","Year","Sex","Age","Pop1")]
PopHMD$Source    <- "HMD"
colnames(PopHMD) <- colnames(PopBrChMa)

# stick together for figure 1
PopFig1          <- rbind(PopBrChMa,PopHMD)

# ------------------------------------------------------------------------------------
# now the goal is to get dx for males and females for the same years. 
# For Malawi, we interpolate mx between 2000 and 2011 (linear is OK), 
# and then rederive dx- everything else is close enough to the population year
# ------------------------------------------------------------------------------------
# read in WHO graduated data:
WHOmx  <- read.csv("/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Data/WHOgraduatedRates.csv", 
         stringsAsFactors = FALSE)
 
 MAL   <- WHOmx[WHOmx$Country == "Malawi", ]
 
 i2000 <- MAL$Year == 2000
 mxm1  <- MAL[i2000, "mxm"]
 mxm2  <- MAL[!i2000, "mxm"]
 mxm   <- mxm1 + (mxm2 - mxm1) / 11 * 8
 
 mxf1  <- MAL[i2000, "mxf"]
 mxf2  <- MAL[!i2000, "mxf"]
 mxf   <- mxf1 + (mxf2 - mxf1) / 11 * 8

 MAL <- data.frame(Country = "Malawi", Year = 2008, Age = 0:110, 
         mxm = mxm, mxf = mxf, 
         dxm = mx2dxHMD(mxm, "m"),
         dxf = mx2dxHMD(mxf, "f"),
         stringsAsFactors = FALSE)
 
 WHOmx <- rbind(WHOmx[!WHOmx$Country == "Malawi", ], MAL)
# --------------------------------------------------------------------
# now stack values for sex, radlly only need dx

mWHO        <- WHOmx[, c("Country","Year","Age","dxm")]
fWHO        <- WHOmx[, c("Country","Year","Age","dxf")]
mWHO$Sex    <- "m"
fWHO$Sex    <- "f"
colnames(fWHO)[4] <- colnames(mWHO)[4] <- "dx"
WHO <- rbind(fWHO, mWHO)
WHO <- WHO[,c("Country","Year","Sex","Age","dx")]
# --------------------------------------------------------------------
# now get the HMD dx values:

dxHMD <- rbind(HMDall[with(HMDall, Code == "USA" & Year == 2010), ],
        HMDall[with(HMDall, Code == "RUS" & Year == 2010), ],
        HMDall[with(HMDall, Code == "JPN" & Year == 2010), ]
)
#head(dxHMD)
dxHMD <- dxHMD[,c("Code","Year","Sex","Age","dx")]
colnames(dxHMD) <- colnames(WHO)
# ---------------------------------------------------------------------
# stick together:
dxFig1 <- rbind(dxHMD, WHO) 

# ---------------------------------------------------------------------
# now we have dx and Pop for our six Figure 1 countries

# Fig1Data should be generated as such to come into memory
# at least that way we keep the steps in place.
Fig1Data <- lapply(unique(dxFig1$Country), function(xxx, dxFig1, PopFig1){
            
            list(Male = Thano(PopFig1$Pop[with(PopFig1, Sex == "m" & Country == xxx)],
                              dxFig1$dx[with(dxFig1, Sex == "m" & Country == xxx)]),
                 Female = Thano(PopFig1$Pop[with(PopFig1, Sex == "f" & Country == xxx)],
                              dxFig1$dx[with(dxFig1, Sex == "f" & Country == xxx)])
                )
            
        }, dxFig1 = dxFig1, PopFig1 = PopFig1)

names(Fig1Data) <- unique(dxFig1$Country)

# names, for consistency
names(Fig1Data)[names(Fig1Data)=="RUS"] <- "Russia"
names(Fig1Data)[names(Fig1Data)=="JPN"] <- "Japan"












