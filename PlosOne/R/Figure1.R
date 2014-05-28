# Author: triffe
###############################################################################
# function definitions for this script

# packages required:
# install.packages("RColorBrewer")
# install.packages("grDevices")

library(devtools) 
load_all("/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/R/RiffeetalFunctions")
setwd("/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne")
source("R/DataPrep3_Figure1.R")
# ------------------------------------------------------------------------------------
# these are very specific functions needed for Figure 1. Not general enough for package
plotRow <- function(XXX,x1,x2,y){
    N <- ncol(XXX[["Cm"]])
    for (i in N:1){
        Cm <- rep(XXX[["Cm"]][, i], each = 2)
        Cf <- rep(XXX[["Cf"]][, i], each = 2)
        Tm <- rep(XXX[["Tm"]][, i], each = 2)
        Tf <- rep(XXX[["Tf"]][, i], each = 2)
        polygon(c(Cm, rev(Cf)) + x1, XXX[["y"]] + y, border = NA, col = XXX[["Ccol"]][i])
        polygon(c(Tm, rev(Tf)) + x2, XXX[["y"]] + y, border = NA, col = XXX[["Tcol"]][i])
        
    }
    
    PyramidOutline(abs(XXX[["Cm"]][, N]), XXX[["Cf"]][, N], 
            x = x1, y = y, scale = 1, border = gray(.1), lwd = .2)
    PyramidOutline(abs(XXX[["Tm"]][, N]), XXX[["Tf"]][, N], 
            x = x2, y = y, scale = 1, border = gray(.1), lwd = .2)
}

# OK data object in order
# transform to plot-specific data
prepare4plot <- function(XXX, colsFun1, colsFun2){
    Mf          <- XXX$Female
    Mm          <- XXX$Male
    TOT         <- sum(Mf) + sum(Mm)
    
    Het1f       <- apply(Mf ,2, aggN, N = 10) / TOT             # age with ey het
    Het1m       <- apply(Mm ,2, aggN, N = 10) / TOT             # age with ey het
    Het2f       <- apply(Mf ,1, aggN, N = 10) / TOT             # age with ey het
    Het2m       <- apply(Mm ,1, aggN, N = 10) / TOT 
    
# cumulative sum for cleaner plotting
    cumHet1f    <- t(apply(Het1f, 2, cumsum))
    cumHet2f    <- t(apply(Het2f, 2, cumsum))
    cumHet1m    <- -t(apply(Het1m, 2, cumsum))
    cumHet2m    <- -t(apply(Het2m, 2, cumsum))
    
    cols1       <- colsFun1(ncol(cumHet1m))
    cols2       <- colsFun2(ncol(cumHet1m))
    y           <- c(0, rep(1:110, each = 2), 111)
    y           <- c(y, rev(y))
    list(Cf = cumHet1f, Tf = cumHet2f, Cm = cumHet1m, Tm = cumHet2m, Ccol = cols1, Tcol = cols2, y = y)
}

# white grid over light gray background with ticks:
makeBG <- function(x,y,xlim=c(-.02,.02),tl = 5,l=TRUE,col=gray(.94),border=NA,lwd=1){
    rect(xlim[1] + x, y, xlim[2] + x, 111 + y, col = col, border = border,lwd=lwd)
    # grid lines, turn off 
    #segments(seq(xlim[1],xlim[2],by=.0025)+x,0+y,seq(xlim[1],xlim[2],by=.0025)+x,111+y,col = "white",lwd=lwd)
    #segments(xlim[1]+x,seq(0,110,by=10)+y,xlim[2]+x,seq(0,110,by=10)+y,col = "white",lwd=lwd)
    # ticks
    x.at <- seq(xlim[1], xlim[2], by = .005) + x
    segments(x.at, y, x.at, -tl + y, lwd = lwd )
    y.at <- seq(0, 100, by = 20) + y  
    if(l){ 
        segments(xlim[1] + x, y.at, xlim[1] - .0007 + x, y.at, lwd = lwd )
    } else {
        segments(xlim[2] + x, y.at, xlim[2] + .0007 + x, y.at, lwd = lwd )
    }
}
# color functions
# colsFun1 are for the left-side (chronological age, where color highlights remaining years)
# colsFun2 is for right side, thano age where color is for birth cohort (approx)
colsFun1        <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"), space = "Lab")
colsFun2        <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "RdPu")), space = "Lab")

PlotData        <- lapply(Fig1Data, prepare4plot, colsFun1 = colsFun1, colsFun2 = colsFun2 )
FigureCountries <- c("USA","Japan","Russia","Brazil","China","Malawi")

{
## ----------------------------------------------------------------
## this was the first draft of Figure 1
#{
## ---------------------------------------
## we do layout in standard plot(), using all base plotting functions, manual shifting and scaling.
#    y.spacing       <- 150
#    y.grid          <- seq(0, 5 * y.spacing, by = y.spacing)
#    
#    
#    
##dev.new(height=7,width=5) # for practice
#    pdf("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures/Figure1.pdf",height=7,width=5)
#    
#    par(mai = c(.4,.5,.6,.5), xaxs = "i", yaxs = "i",xpd=TRUE)
#    plot(NULL, type = "n", xlim = c(-.03,.066), ylim = c(0, 900), 
#            axes = FALSE, xlab = "",ylab="")
#    sapply(y.grid,function(y){
#                makeBG(x=0,y=y)  
#                makeBG(x=0.04,y=y,xlim=c(-.015,.015),l=FALSE)  
#            })
#    for (i in 1:6){
#        plotRow(PlotData[[FigureCountries[i]]],x1=0,x2=.04,y=rev(y.grid)[i])
#    }
#    
## label countries
#    text(-.021,y.grid+115,c("Malawi 2008","China 2011","Brazil 2010","Russia 2010","Japan 2009","USA 2010"),cex=.7,pos=4)
#    
## x axis labels
#    text(seq(-.02,.02,by=.005),0,c("2.0%","1.5%","1.0%","0.5%","0","0.5%","1.0%","1.5%","2.0%"),pos=1,cex=.4)
#    text(seq(-.015,.015,by=.005)+.04,y.grid[6]+111,c("1.5%","1.0%","0.5%","0","0.5%","1.0%","1.5%"),pos=3,cex=.4)
#    segments(seq(-.015, .015, by = .005) + .04,y.grid[6]+111,seq(-.015, .015, by = .005) + .04,y.grid[6]+111+5,lwd=.5 )
## y axis labels
#    text(-.02,outer(seq(0,100,by=20),y.grid,"+"),seq(0,100,by=20),pos=2,cex=.4)
#    text(.055,outer(seq(0,100,by=20),y.grid,"+"),seq(0,100,by=20),pos=4,cex=.4)
## male female labels:
#    text(-.01,-40,"Male",cex=.8)
#    text(.01,-40,"Female",cex=.8)
#    text(-.01+.04,790+111,"Male",cex=.8)
#    text(.01+.04,790+111,"Female",cex=.8)
#    
## now for color strip legends
#    RYcols <- colsFun1(12)
#    Acols  <- rev(colsFun2(12))
## left side legend (remaining years colors)
#    y.at <- seq(400,700,length.out=13)
#    rect(-.035,y.at[1:12],-.04,y.at[2:13],border = "white", col = RYcols)
#    text(-.035,y.at[1:12],seq(0,110,by=10),pos=4,cex = .5)
#    text(-.043,730,"Remaining\nYears",pos=4,cex=.7)
## right side legend (age colors)
#    rect(.065,y.at[1:12],.07,y.at[2:13],border = "white", col = rev(Acols))
#    text(.07,y.at[1:12],seq(0,110,by=10),pos=4,cex = .5)
#    text(.072,730,"Age",pos=2,cex=.7)
#    
## main title
#    text(-0.04,940,"Age Pyramids\n(shade indicates remaining years)",pos=4)
#    text(0.025,940,"Remaining-years leaves\n(shade indicates age)",pos=4)
##text(c(-.01,-.01,.04,.04),c(200,700,200,700),"Draft",cex=11,srt=45,col = "#CCCCCC40")
#    
#    dev.off()
#}
## ---------------------------------------------------------
## Figure 1 standard double column:
## double col width 7.20472 (183 mm) 
## another version....
#{
## changes: 
##          Years lived, left instead for labels, less confusing
##          country labels in middle, on level with 100?
#
#y.spacing       <- 130
#y.grid          <- seq(0, 5 * y.spacing, by = y.spacing)
#fac <- 7.20472 / 5
#x1 <- -.005
#x2 <- .028
#xlim1 <- c(-.02,.02)
#xlim2 <- c(-.01,.01)
##dev.new(height=7*fac,width=5*fac) # for practice
#pdf("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures/Riffe_fig1.pdf",height=7*fac,width=5*fac)
#par(mai = c(0,0,0,0), xaxs = "i", yaxs = "i",xpd=TRUE)
#plot(NULL, type = "n", xlim = c(-.035,.055), ylim = c(-60, 810), 
#        axes = FALSE, xlab = "",ylab="")
#sapply(y.grid,function(y){
#            makeBG(x=x1,y=y,xlim=xlim1,col=NA, border = gray(.8), lwd = .5)  
#            makeBG(x=x2,y=y,xlim=xlim2,l=FALSE,col=NA,border = gray(.8), lwd = .5)  
#        })
#for (i in 1:6){
#    plotRow(PlotData[[FigureCountries[i]]],x1=x1,x2=x2,y=rev(y.grid)[i])
#}
## label countries
#text(x1+xlim1[1]-.0025,y.grid+55,
#        c("Malawi\n(2008)","China\n(2011)","Brazil\n(2010)","Russia\n(2010)","Japan\n(2009)","US\n(2010)"),
#        cex=.8,pos=2)
#
## x axis labels
#text(seq(xlim1[1],xlim1[2],by=.005)+x1,0,c("2.0%","1.5%","1.0%","0.5%","0","0.5%","1.0%","1.5%","2.0%"),pos=1,cex=.4)
#text(seq(xlim2[1],xlim2[2],by=.005)+x2,0,c("1.0%","0.5%","0","0.5%","1.0%"),pos=1,cex=.4)
#
## y axis labels
#text(xlim1[1]+x1, outer(seq(0, 100, by = 20), y.grid, "+"), seq(0, 100, by = 20), pos = 2, cex = .4)
#text(xlim2[2]+x2, outer(seq(0, 100, by = 20), y.grid, "+"), seq(0, 100, by = 20), pos = 4, cex = .4)
## male female labels:
#text(-.0025 + x1, -25, "Male", cex = .8, pos = 2)
#text(.0025 + x1, -25, "Female", cex = .8, pos = 4)
#text(-.0025 + x2, -25, "Male", cex = .8, pos = 2)
#text(.0025 + x2, -25, "Female", cex = .8, pos = 4)
#
## now for color strip legends
#RYcols <- colsFun1(12)
#Acols  <- rev(colsFun2(12))
## left side legend (remaining years colors)
#y.at <- seq(0,300,length.out=13)
#
#xl <- .045
#lw <- .004
#rect(xl,y.at[1:12],xl+lw,y.at[2:13],border = "white", col = RYcols)
#text(xl+lw,y.at[1:12],seq(0,110,by=10),pos=4,cex = .5)
#text(xl+lw/2,max(y.at)+15,"Years\nleft",cex=.8)
## right side legend (age colors)
#rect(xl,y.at[1:12]+400,xl+lw,y.at[2:13]+400,border = "white", col = rev(Acols))
#text(xl+lw,y.at[1:12]+400,seq(0,110,by=10),pos=4,cex = .5)
#text(xl+lw/2,max(y.at)+400+15,"Years\nlived",cex=.8)
#
## main title
#text(x1+xlim1[2],max(y.grid )+30+111,"Pyramids",pos=2,cex=1)
#text(x1+xlim1[2],max(y.grid )+20+111,"(shade indicates years left)",pos=2,cex=.7)
#text(x2+xlim2[1],max(y.grid )+30+111,"Leaves",pos=4)
#text(x2+xlim2[1],max(y.grid )+20+111,"(shade indicates years lived)",pos=4,cex=.7)
##text(c(-.01,-.01,.04,.04),c(200,700,200,700),"Draft",cex=11,srt=45,col = "#CCCCCC40")
#
#text(x1+xlim1[1],max(y.grid) + 10 + 111, "Years\nlived",pos=2, cex = .8)
#text(x2+xlim2[2],max(y.grid) + 10 + 111, "Years\nleft",pos=4, cex = .8)
#arrows(c(x2+xlim2[2]+.0032,x1+xlim1[1]-.0032), rep(max(y.grid) + 107,2), 
#        c(x2+xlim2[2]+.0032,x1+xlim1[1]-.0032), rep(max(y.grid) + 70,2), length=.06, lwd = .5)
#dev.off()
#}
## -----------------------------------------------------------------------------------------
## png adjustments for text:
## for in-text, later not used
#{
png("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures/Riffe_fig1.png",
        height=7*150,width=5*150,res=150,pointsize=8)
par(mai = c(0,0,0,0), xaxs = "i", yaxs = "i",xpd=TRUE)
plot(NULL, type = "n", xlim = c(-.035,.055), ylim = c(-60, 810), 
        axes = FALSE, xlab = "",ylab="")
sapply(y.grid,function(y){
            makeBG(x=x1,y=y,xlim=xlim1,col=NA, border = gray(.8), lwd = .5)  
            makeBG(x=x2,y=y,xlim=xlim2,l=FALSE,col=NA,border = gray(.8), lwd = .5)  
        })
for (i in 1:6){
    plotRow(PlotData[[FigureCountries[i]]],x1=x1,x2=x2,y=rev(y.grid)[i])
}
# label countries
text(x1+xlim1[1]-.0025,y.grid+55,
        c("Malawi\n(2008)","China\n(2011)","Brazil\n(2010)","Russia\n(2010)","Japan\n(2009)","US\n(2010)"),
        cex=.8,pos=2)

# x axis labels
text(seq(xlim1[1],xlim1[2],by=.005)+x1,0,c("2.0%","1.5%","1.0%","0.5%","0","0.5%","1.0%","1.5%","2.0%"),pos=1,cex=.4)
text(seq(xlim2[1],xlim2[2],by=.005)+x2,0,c("1.0%","0.5%","0","0.5%","1.0%"),pos=1,cex=.4)

# y axis labels
text(xlim1[1]+x1, outer(seq(0, 100, by = 20), y.grid, "+"), seq(0, 100, by = 20), pos = 2, cex = .4)
text(xlim2[2]+x2, outer(seq(0, 100, by = 20), y.grid, "+"), seq(0, 100, by = 20), pos = 4, cex = .4)
# male female labels:
text(-.0025 + x1, -25, "Male", cex = .8, pos = 2)
text(.0025 + x1, -25, "Female", cex = .8, pos = 4)
text(-.0025 + x2, -25, "Male", cex = .8, pos = 2)
text(.0025 + x2, -25, "Female", cex = .8, pos = 4)

# now for color strip legends
RYcols <- colsFun1(12)
Acols  <- rev(colsFun2(12))
# left side legend (remaining years colors)
y.at <- seq(0,300,length.out=13)

xl <- .045
lw <- .004
rect(xl,y.at[1:12],xl+lw,y.at[2:13],border = "white", col = RYcols)
text(xl+lw,y.at[1:12],seq(0,110,by=10),pos=4,cex = .5)
text(xl+lw/2,max(y.at)+15,"Years\nleft",cex=.8)
# right side legend (age colors)
rect(xl,y.at[1:12]+400,xl+lw,y.at[2:13]+400,border = "white", col = rev(Acols))
text(xl+lw,y.at[1:12]+400,seq(0,110,by=10),pos=4,cex = .5)
text(xl+lw/2,max(y.at)+400+15,"Years\nlived",cex=.8)

# main title
text(x1+xlim1[2],max(y.grid )+33+111,"Pyramids",pos=2,cex=1)
text(x1+xlim1[2],max(y.grid )+20+111,"(shade indicates years left)",pos=2,cex=.7)
text(x2+xlim2[1],max(y.grid )+33+111,"Leaves",pos=4)
text(x2+xlim2[1],max(y.grid )+20+111,"(shade indicates years lived)",pos=4,cex=.7)
#text(c(-.01,-.01,.04,.04),c(200,700,200,700),"Draft",cex=11,srt=45,col = "#CCCCCC40")

text(x1+xlim1[1],max(y.grid) + 10 + 111, "Years\nlived",pos=2, cex = .8)
text(x2+xlim2[2],max(y.grid) + 10 + 111, "Years\nleft",pos=4, cex = .8)
arrows(c(x2+xlim2[2]+.0032,x1+xlim1[1]-.0032), rep(max(y.grid) + 107,2), 
        c(x2+xlim2[2]+.0032,x1+xlim1[1]-.0032), rep(max(y.grid) + 70,2), length=.06, lwd = .5)
dev.off()
}
}
##############################
# for PlosOne
##############################
y.spacing       <- 130
y.grid          <- seq(0, 5 * y.spacing, by = y.spacing)
fac             <- 7.20472 / 5
x1 <- -.006
x2 <- .029
xlim1 <- c(-.02,.02)
xlim2 <- c(-.01,.01)
# getting Arial
#install_github("Rttf2pt1", "wch")
#install.packages("extrafont",dependencies=TRUE)
library(extrafont)
#font_import()
loadfonts(device = "postscript")
postscript("/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Figures/Figure1.eps", 
        height = 9.19, width = 6.83,
        family = "Arial", paper = "special", onefile = FALSE, bg = "white",
        horizontal = FALSE, pointsize = 12)
#dev.new(family = "Arial",height = 9.19, width = 6.83)
par(mai = c(0,0,0,0), xaxs = "i", yaxs = "i",xpd=TRUE)
plot(NULL, type = "n", xlim = c(-.036,.054), ylim = c(-60, 810), 
        axes = FALSE, xlab = "",ylab="")
invisible(sapply(y.grid,function(y){
                    makeBG(x=x1,y=y,xlim=xlim1,col=NA, border = gray(.8), lwd = .5)  
                    makeBG(x=x2,y=y,xlim=xlim2,l=FALSE,col=NA,border = gray(.8), lwd = .5)  
                }))
for (i in 1:6){
    plotRow(PlotData[[FigureCountries[i]]],x1=x1,x2=x2,y=rev(y.grid)[i])
}
# label countries
text(-.0355,y.grid+75,
        c("Malawi\n(2008)","China\n(2011)","Brazil\n(2010)","Russia\n(2010)","Japan\n(2010)","US\n(2010)"),
        cex=(8/12),pos=4)
text(-.0355,y.grid+100,
        c("F","E","D","C","B","A"),
        cex=1,pos=4,font=2)# bold
# x axis labels
text(seq(xlim1[1],xlim1[2],by=.005)+x1,0,c("2.0%","1.5%","1.0%","0.5%","0","0.5%","1.0%","1.5%","2.0%"),pos=1,cex=8/12)
text(seq(xlim2[1],xlim2[2],by=.005)+x2,0,c("1.0%","0.5%","0","0.5%","1.0%"),pos=1,cex=8/12)

# y axis labels
text(xlim1[1]+x1, outer(seq(0, 100, by = 20), y.grid, "+"), seq(0, 100, by = 20), pos = 2, cex = 8/12)
text(xlim2[2]+x2, outer(seq(0, 100, by = 20), y.grid, "+"), seq(0, 100, by = 20), pos = 4, cex = 8/12)
# male female labels:
text(-.002 + x1, -30, "Male", cex = 10/12, pos = 2)
text(.002 + x1, -30, "Female", cex = 10/12, pos = 4)
text(-.002 + x2, -30, "Male", cex = 10/12, pos = 2)
text(.002 + x2, -30, "Female", cex = 10/12, pos = 4)

# now for color strip legends
RYcols <- colsFun1(12)
Acols  <- rev(colsFun2(12))
# left side legend (remaining years colors)
y.at <- seq(0,300,length.out=13)

xl <- .045
lw <- .004
rect(xl,y.at[1:12],xl+lw,y.at[2:13],border = "white", col = RYcols)
text(xl+lw,y.at[1:12],seq(0,110,by=10),pos=4,cex = 8/12)
text(xl+lw/2,max(y.at)+18,"Years\nleft",cex=10/12)
# right side legend (age colors)
rect(xl,y.at[1:12]+400,xl+lw,y.at[2:13]+400,border = "white", col = rev(Acols))
text(xl+lw,y.at[1:12]+400,seq(0,110,by=10),pos=4,cex =8/12)
text(xl+lw/2,max(y.at)+400+18,"Years\nlived",cex=10/12)

# main title
text(x1+xlim1[2],max(y.grid )+33+111,"Pyramids",pos=2,cex=1, font=2)
text(x1+xlim1[2],max(y.grid )+16+111,"(shade indicates years left)",pos=2,cex=10/12)
text(x2+xlim2[1],max(y.grid )+33+111,"Leaves",pos=4, font = 2)
text(x2+xlim2[1],max(y.grid )+16+111,"(shade indicates years lived)",pos=4,cex=10/12)
#text(c(-.01,-.01,.04,.04),c(200,700,200,700),"Draft",cex=11,srt=45,col = "#CCCCCC40")

#text(x1+xlim1[1]-.001,max(y.grid) + 16 + 111, "Years\nlived", pos = 2, cex = 10/12)
#text(x2+xlim2[2]+.001,max(y.grid) + 16 + 111, "Years\nleft", pos = 4, cex = 10/12)
#arrows(c(x2+xlim2[2]+.0032,x1+xlim1[1]-.0032), rep(max(y.grid) + 107,2), 
#        c(x2+xlim2[2]+.0032,x1+xlim1[1]-.0032), rep(max(y.grid) + 70,2), length=.06, lwd = .5)
#

#text(x1+xlim1[1]-.001,min(y.grid) -25, "Years\nlived", pos = 2, cex = 10/12)
#text(x2+xlim2[2]+.001,min(y.grid) -25, "Years\nleft", pos = 4, cex = 10/12)
#arrows(c(x2+xlim2[2]+.0032,x1+xlim1[1]-.0032), rep(min(y.grid) + 70,2), 
#        c(x2+xlim2[2]+.0032,x1+xlim1[1]-.0032), rep(min(y.grid) + 107,2), length=.06, lwd = .5)


text(x1+xlim1[1]-.003,62,"Years lived",srt=90,cex=10/12,pos=2)
text(x2+xlim2[2]+.003,62,"Years left",srt=270,cex=10/12,pos=4)
dev.off()









