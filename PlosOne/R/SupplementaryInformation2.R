# Author: triffe
###############################################################################
# library(devtools)
# load_all("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/R/RiffeetalFunctions")

# do a time-series animation of US structure
# define some functions:


getxpurty <- function(xlim){
    xpurty <- pretty(xlim,n=10)
    xpurty <- xpurty[xpurty > min(xlim) & xpurty < max(xlim)]
    xpurty
}
makeBG <- function(x, y, xlim = c(-.02, .02), tl = 5,l = TRUE,xpurty = getxpurty(xlim),cex=.8){
    xlabs  <- paste0(abs(xpurty * 100),"%")
    xlabs[xlabs == "0%"] <- ""
    x.at <- xpurty + x
    rect(xlim[1] + x, 0 + y, xlim[2] + x, 111 + y, col = gray(.97), border = NA)
    segments(x.at, 0 + y, x.at, 111 + y, col = "white", lwd=.5)
    segments(xlim[1] + x, seq(0, 110, by = 10) + y, xlim[2] + x, seq(0, 110, by = 10) + y, col = "white", lwd = .5)
    # ticks
    segments(x.at, y, x.at, 0 - tl + y, lwd = .5 )
    y.at <- seq(0, 100, by = 20) + y  
    tly<- diff(xlim) * tl / 111
    if(l){ 
        segments(xlim[1] + x, y.at, xlim[1] - tly + x, y.at, lwd = .5 )
    } else {
        segments(xlim[2] + x, y.at, xlim[2] + tly + x, y.at, lwd = .5 )
    }
    text(x.at, -tl, xlabs, cex = cex, pos = 1)
}

prepare4plot <- function(XXX,colsFun1,colsFun2){
    Mf  <- XXX$f
    Mm  <- XXX$m
    TOT <- sum(Mf) + sum(Mm)
    
    Het1f <- apply(Mf ,2, aggN, N = 10) / TOT             # age with ey het
    Het1m <- apply(Mm ,2, aggN, N = 10) / TOT             # age with ey het
    Het2f <- apply(Mf ,1, aggN, N = 10) / TOT             # age with ey het
    Het2m <- apply(Mm ,1, aggN, N = 10) / TOT 
    
# mark 0s for downstream renmoval
    ind01f <- Het1f == 0
    ind02f <- Het2f == 0
    ind01m <- Het1m == 0
    ind02m <- Het2m == 0
    
# cumulative sum for cleaner plotting
    cumHet1f <- t(apply(Het1f,2,cumsum))
    cumHet2f <- t(apply(Het2f,2,cumsum))
    cumHet1m <- -t(apply(Het1m,2,cumsum))
    cumHet2m <- -t(apply(Het2m,2,cumsum))
    
    cols1 <- colsFun1(ncol(cumHet1m))
    cols2 <- colsFun2(ncol(cumHet1m))
    y <- c(0,rep(1:110,each=2),111)
    y <- c(y,rev(y))
    list(Cf=cumHet1f, Tf=cumHet2f,Cm=cumHet1m,Tm=cumHet2m,Ccol = cols1, Tcol = cols2, y=y)
}

colsFun1    <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9,"YlGnBu"),space="Lab")
colsFun2    <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9,"RdPu")),space="Lab")

# -----------------------------------------------------------------------
# End function defs
Data        <- local(get(load("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Data/HMDAll.Rdata")))
US          <- Data[Data$Code == "USA", ] 
# make a 111x111 matrix for each Year-Sex (Sex within Year)
USL         <- lapply(split(US,list(US$Year)), function(yr){
                   lapply(split(yr,list(yr$Sex)), function(yrsex){
                        Thano(yrsex$Pop1, yrsex$dx)
                    })
        })
# this step groups each year's data appropriately for plotting. i.e. 10-year heterogeneity bars for each 
# age / remaining years. Makes for cleaner plotting that displays better in most devices
USP <- lapply(USL, prepare4plot, colsFun1 = colsFun1, colsFun2 = colsFun2)


y2x <- function(lmobj,y){
    coefs <- lmobj$coef
    int   <- coefs[1]
    slope <- coefs[2]
    
    (y-int)/slope
}
plotFrame <- function(XXX,x1=0,x2=.03,y=0,tl=2, cex,year,lmm,aC,aT){
    plot(NULL, type = "n", xlim=c(-.014,.04),ylim=c(-30,120), axes=FALSE, xlab="",ylab="")
    xpurty <- getxpurty(c(-.012, .012))
    
    makeBG(x=x1,y=y,xlim=c(-.012, .012), tl = tl, l = FALSE, xpurty = xpurty, cex=cex)
    makeBG(x=x2,y=y,xlim=c(-.012, .012), tl = tl, l = TRUE, xpurty = xpurty, cex=cex)
    
    mid <- mean(c(x1,x2))
    text(mid,seq(0,100,by=20),seq(0,100,by=20),cex = cex)
    N <- ncol(XXX[["Cm"]])
    for (i in N:1){
        Cm <- rep(XXX[["Cm"]][, i], each = 2)
        Cf <- rep(XXX[["Cf"]][, i], each = 2)
        Tm <- rep(XXX[["Tm"]][, i], each = 2)
        Tf <- rep(XXX[["Tf"]][, i], each = 2)
        polygon(c(Cm, rev(Cf)) + x1, XXX[["y"]] + y, border = NA, col = XXX[["Ccol"]][i])
        polygon(c(Tm, rev(Tf)) + x2, XXX[["y"]] + y, border = NA, col = XXX[["Tcol"]][i])
    }
    PyramidOutline(abs(XXX[["Cm"]][,N]),XXX[["Cf"]][,N],x=x1,y=y, scale=1,border=gray(.1), lwd = .2)
    PyramidOutline(abs(XXX[["Tm"]][,N]),XXX[["Tf"]][,N],x=x2,y=y, scale=1,border=gray(.1), lwd = .2)
    
    segments(mid,110,mid,120,xpd=TRUE)
    text(mid,115,"Years left",pos=4,cex=cex*1.4,xpd=TRUE)
    text(mid,115,"Age", pos = 2, cex = cex*1.4,xpd=TRUE)
    
    ncols <- length(XXX[["Ccol"]])
    wid <- .015
    xpos <- seq(x1-wid/2,x1+wid/2,length=ncols+1)
    rect(xpos[1:ncols],-15,xpos[2:(ncols+1)],-25,border=NA,col=XXX[["Ccol"]])
    rect(x1-wid/2,-15,x1+wid/2,-25,border=gray(.2),lwd=.5)
    segments(xpos[1:ncols],-25,xpos[1:ncols],-27)
    text(xpos[1:ncols][seq(ncols) %% 2 == 1], -27, seq(0,100,by=20),pos=1,cex=cex,xpd=TRUE)
    text(x1-wid/2,-20,"Years left",pos=2,cex=cex*1.4)
    
    xpos <- seq(x2-wid/2,x2+wid/2,length=ncols+1)
    rect(xpos[1:ncols],-15,xpos[2:(ncols+1)],-25,border=NA,col=XXX[["Tcol"]])
    segments(xpos[1:ncols],-25,xpos[1:ncols],-27)
    rect(x2-wid/2,-15,x2+wid/2,-25,border=gray(.2),lwd=.5)
    text(xpos[1:ncols][seq(ncols) %% 2 == 1], -27, seq(0,100,by=20),pos=1,cex=cex,xpd=TRUE)
    text(x2+wid/2,-20,"Age",pos=4,cex=cex*1.4)
    
    # male female
    text(x1,114,"Male",pos=2,cex=1.4*cex)
    text(x2,114,"Male",pos=2,cex=1.4*cex)
    text(x1,114,"Female",pos=4,cex=1.4*cex)
    text(x2,114,"Female",pos=4,cex=1.4*cex)
    
    # yr
    text(mid,135,year,cex=cex*4,col=gray(.5),xpd=TRUE)
    
    if (year %in% c(1944:1946)){
        segments(y2x(lmm,21),21,y2x(lmm,38),38)
        text(-.01,30,"WWII\nadj.",cex=cex*1.8,col=gray(.4))
    }
    
    # mean ages:
    segments(x1+.012,aC,x1+.011,aC,col=gray(.1),lwd=3)
    text(x1+.011,aC,expression(bar(x)),pos=2)
    segments(x2-.012,aT,x2-.011,aT,col=gray(.1),lwd=3)
    text(x2-.011,aT,expression(bar(y)),pos=4)
}

cex     <- .6
years   <- 1933:2010
x       <- -(US$Pop1[with(US, Sex == "m" & Year == 1943)][18:41]) / sum(US$Pop1[US$Year == 1943])
y       <- 19:42
lmm     <- lm(y~x) # keep this
# some mean ages
USC     <- reshape2::acast(US, Age~Year, sum, value.var = "Pop1")
UST     <- reshape2::acast(US, Age~Year, sum, value.var = "Py")
w       <- .5:110.5

aC      <- colSums(USC*w) / colSums(USC)
aT      <- colSums(UST*w) / colSums(UST)

library(animation)
saveSWF({
            par(mai=c(.2,.2,.8,.2),xaxs="i",yaxs="i")
            for (i in 1:length(USP)){
                plotFrame(USP[[i]], x1=0,x2=.027,cex=.6,year=years[i],lmm=lmm,aC=aC[i],aT=aT[i]) 
            }
        }, 
        swf.name = "animationS1.swf", 
        img.name = "Rplot", 
        swftools = NULL,
        interval = .5,
        ani.height = 4.7,
        ani.width = 7.8, 
        ani.dev = "pdf",
        ani.type = "pdf",
        autoplay = FALSE,
        loop = FALSE,
        outdir = "/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures")


#saveVideo({
#            par(mai=c(.2,.2,.8,.2),xaxs="i",yaxs="i")
#            for (i in 1:length(USP)){
#                plotFrame(USP[[i]], x1=0,x2=.027,cex=.6,year=years[i],lmm=lmm,aC=aC[i],aT=aT[i]) 
#            }
#        }, video.name = "animationS1.mp4", 
#        other.opts = "-b 300k",
#        outdir = "/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures")

# ----------------------------------------------------------------------
# Figure 1 of ED
pdf("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures/Riffe_EDfig1.pdf",height = 3.50394, width = 3.50394)
par(mai=c(.7,.7,.5,.5)/2,xaxs="i",yaxs="i")
plot(1933:2010, aC, type='l', ylim=c(28,48), xlab = "", ylab = "",axes=FALSE,lwd=2,
        panel.first=list(rect(1933,28,2010,48,col="#ABABAB20",border=NA),
                segments(seq(1940,2010,by=10),28,seq(1940,2010,by=10),48,col="white",lwd=.5),
                segments(seq(1940,2010,by=10),28,seq(1940,2010,by=10),27.8,col="black",lwd=.5,xpd=TRUE),
                text(seq(1940,2010,by=10),27.8,seq(1940,2010,by=10),pos=1,cex=.6,xpd=TRUE),
                segments(1933,seq(30,47.5,by=2.5),2010,seq(30,47.5,by=2.5),col="white",lwd=.5),
                segments(1933,seq(30,45,by=5),1932,seq(30,45,by=5),col="black",lwd=.5,xpd=TRUE),
                text(1932,seq(30,45,by=5),seq(30,45,by=5),pos=2,cex=.6,xpd=TRUE)
             
        ))
lines(1933:2010, aT, col=gray(.6),lwd=3)
text(c(1980,1980),c(44.5,34.3),c("Mean remaining lifetime","Mean age"),pos=2,cex=.7)
text(1932,48.8,"Age (Years)",xpd=TRUE,cex=.7)
text(1972,26.1,"Year",xpd=TRUE,cex=.7)
dev.off()

# png for initial submission
png("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures/Riffe_figS1.png",
        height=4.5*150,width=4.5*150,res=150,pointsize=12)
par(mai=c(1,1,.5,.5)/2,xaxs="i",yaxs="i")
plot(1933:2010, aC, type='l', ylim=c(28,48), xlab = "", ylab = "",axes=FALSE,lwd=2,
        panel.first=list(rect(1933,28,2010,48,col="#ABABAB20",border=NA),
                segments(seq(1940,2010,by=10),28,seq(1940,2010,by=10),48,col="white",lwd=.5),
                segments(seq(1940,2010,by=10),28,seq(1940,2010,by=10),27.8,col="black",lwd=.5,xpd=TRUE),
                text(seq(1940,2010,by=10),27.8,seq(1940,2010,by=10),pos=1,cex=.6,xpd=TRUE),
                segments(1933,seq(30,47.5,by=2.5),2010,seq(30,47.5,by=2.5),col="white",lwd=.5),
                segments(1933,seq(30,45,by=5),1932,seq(30,45,by=5),col="black",lwd=.5,xpd=TRUE),
                text(1932,seq(30,45,by=5),seq(30,45,by=5),pos=2,cex=.6,xpd=TRUE)
        
        ))
lines(1933:2010, aT, col=gray(.6),lwd=3)
text(c(1980,1980),c(44.5,34.3),c("Mean years left","Mean years lived"),pos=2,cex=.7)
text(1932,48.8,"Age (Years)",xpd=TRUE,cex=.7)
text(1972,26.1,"Year",xpd=TRUE,cex=.7)
dev.off()










plot(1933:2010, aC+aT, ylim=c(60,85), type='l')
lines(1933:2010, e0vec)
SRB <- 1.05
e0mat <- matrix(US$ex[US$Age==0],byrow=TRUE,ncol=2)
e0vec <- colSums(t(e0mat) * c(1/2.05,1.05/2.05))



