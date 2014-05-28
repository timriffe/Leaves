# Author: triffe
###############################################################################
# this does a spline interpolation to graduate.
# plotted results are reasonable enough
Interp <- function(DAT){
    if (nrow(DAT) == 0){
        return(NULL)
    }
    ages <- 0:110
    x   <- DAT$AgeInt
    ym  <- DAT$Mxm
    yf  <- DAT$Mxf
    N   <- length(yf)
    yf2 <- c(yf[1], exp(spline(x[2:N],log(yf[2:N]),xout = 1:110)$y))
    ym2 <- c(ym[1], exp(spline(x[2:N],log(ym[2:N]),xout = 1:110)$y))
    
    # make sure high ages not decreasing
    f.ind       <- c(diff(yf2)<0)
    f.ind       <- c(f.ind,rev(f.ind)[1])
    f.ind[1:80] <- FALSE
    
    m.ind       <- c(diff(ym2)<0)
    m.ind       <- c(m.ind,rev(m.ind)[1])
    m.ind[1:80] <- FALSE

    yf2[f.ind] <- yf2[sum(!f.ind)]
    ym2[m.ind] <- ym2[sum(!m.ind)]
    
    # max rate 1.1 (arbitrary)
    yf2[yf2 > 1.1] <- 1.1
    ym2[ym2 > 1.1] <- 1.1
    
    dxm <- mx2dxHMD(ym2,"m")
    dxf <- mx2dxHMD(yf2,"f")
#   
#    plot(0:10,rep(1,11),type= 'n', xlim = c(0,110),ylim = c(1e-4,1), log = "y")
#    points(x, ym, col = "blue")
#    points(x, yf, col = "red")
#    lines(ages, ym2, col = "blue")
#    lines(ages, yf2, col = "red")
#    locator(1)
#    dev.off()
    data.frame(Country = unique(DAT$Country), Year = unique(DAT$Year), Age = ages, 
            mxm = ym2, mxf = yf2, dxm = dxm, dxf = dxf, stringsAsFactors = FALSE)
}

# read in WHO abdridged rates
WHOabridgedRates <- read.csv("/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Data/WHOabridgedRates.csv", 
        stringsAsFactors = FALSE)

# now for ages 0-110 for China, Brazil and Malawi (2 years, because we need to interpolate to 2008)
GraduatedRates <- 
        do.call(rbind,
            lapply(
                split(WHOabridgedRates,list(WHOabridgedRates$Country, WHOabridgedRates$Year)), 
                Interp)
        )
# save out 
write.table(GraduatedRates, "/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Data/WHOgraduatedRates.csv",
        sep = ",", col.names = colnames(GraduatedRates), row.names = FALSE)


## get population counts
#library(xlsx)
#Population <- read.xlsx(file.path(dropbox.data, "population.xls"), sheetIndex = 1, 
#        colClasses = c("character","integer","character","integer","numeric","character"))
#head(Population)
#Population <- Population[,1:5]
#
## keep Pop for Malawi, Brazil, China 2011
#Population <- Population[Population$Country %in% c("Malawi", "Brazil", "China"),]
#table(Population$Country, Population$Year)
#Population <- Population[Population$Year != 2000, ]
#plot(Population$Age[Population$Country == "Malawi"], Population$Pop[Population$Country == "Malawi"])
#Population$Pop[Population$Country == "Malawi"] <- Population$Pop[Population$Country == "Malawi"] / 100
#Population$Sex <- as.character(Population$Sex)
#Population$Sex <- ifelse(Population$Sex == "Male","m","f")
#head(Population)
#Population$Source <- ifelse(Population$Country == "Malawi", "IPUMS",
#        ifelse(Population$Country == "China", "USCB","IBGE"))

#write.table(Population, "/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Data/PopulationBrChMa.csv",
#        sep = ",", col.names = colnames(Population), row.names = FALSE)
