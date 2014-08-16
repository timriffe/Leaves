# Author: triffe
###############################################################################


# change this!
setwd("/home/triffe/git/Leaves")

# will need pseudo package for paper
# istall if necessary:
#library(devtools) # FYI, this may need tweaking on Windows or MAC, instructions
#                   available by searching in the devtools instructions on github.
#install_github("Leaves", subdir = "PlosOne/R/RiffeetalFunctions", username = "timriffe") # belongs to paper

library(RiffeetalFunctions)

# this needs to run in order to load "Fig1Data" data.frame
source("R/DataPrep3_Figure1.R")

# ------------------------------------------------------------------------------------
# these are very specific functions needed for Figure 1. Not general enough for package
# sorry, I know there might be demand for that sort of thing. In general, all you need
# to plot a leaf, or pyramid with heterogeneity is a function that will plot stacked
# bars, see the Pyramid package under my name on github. It can do this easily. However,
# we wanted even crisper graphics for this paper, and have chosen to laboriously repesent
# color regions as single complex polygons. It takes some mind-bending to get the coordinates
# right, and that's part of why I myself consider the code that follows to be among the ugliest
# I've written! For this reason, I'll annotate as best as possible.

plotRow <- function(XXX,x1,x2,y){
    
    # XXX is a special data object containing the info needed to plot
    # the left an right figures. prepared by function prepare4plot()
    
    # N heterogeneity classes.
    # Cm is chronological age with remaining years classes,
    # Tm is thanatological age with time-since birth clases.
    # these heterogeneity classes are already aggregated to 10 year groups..
    N <- ncol(XXX[["Cm"]])
    for (i in N:1){
        # each value need repetition because each bar ends with 2 points...
        Cm <- rep(XXX[["Cm"]][, i], each = 2)
        Cf <- rep(XXX[["Cf"]][, i], each = 2)
        Tm <- rep(XXX[["Tm"]][, i], each = 2)
        Tf <- rep(XXX[["Tf"]][, i], each = 2)
        # and this draws the color regions
        polygon(c(Cm, rev(Cf)) + x1, XXX[["y"]] + y, border = NA, col = XXX[["Ccol"]][i])
        polygon(c(Tm, rev(Tf)) + x2, XXX[["y"]] + y, border = NA, col = XXX[["Tcol"]][i])
        
    }
    
    # handy function, draws profile, good for comparing age structures also
    PyramidOutline(abs(XXX[["Cm"]][, N]), XXX[["Cf"]][, N], 
            x = x1, y = y, scale = 1, border = gray(.1), lwd = .2)
    PyramidOutline(abs(XXX[["Tm"]][, N]), XXX[["Tf"]][, N], 
            x = x2, y = y, scale = 1, border = gray(.1), lwd = .2)
}

# data object in order
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
    
    # color ramp functions, produced by colorRampPalette()
    cols1       <- colsFun1(ncol(cumHet1m))
    cols2       <- colsFun2(ncol(cumHet1m))
    y           <- c(0, rep(1:110, each = 2), 111)
    y           <- c(y, rev(y))
    
    # these are the objects needed by plotRow()
    list(Cf = cumHet1f, Tf = cumHet2f, Cm = cumHet1m, Tm = cumHet2m, Ccol = cols1, Tcol = cols2, y = y)
}

# special grid for background. Try not to make distracting
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
# other ramps would be possible, as long as they are sort of continous. Too many colors
# is confusing... RColorBrewer are great palettes

# produce figure data list
PlotData        <- lapply(Fig1Data, prepare4plot, colsFun1 = colsFun1, colsFun2 = colsFun2 )
FigureCountries <- c("USA","Japan","Russia","Brazil","China","Malawi")

# there was a large amount of figure experimentation, not presented here. playing with axes, legends, placement
# proportions, color schemes. 
##############################
# for PlosOne
##############################

# these are old fashioned-coordinate place holders and scalers
# trust me there are better ways to do this, but for the present
# case it afforded me control over the canvas.
y.spacing       <- 130
y.grid          <- seq(0, 5 * y.spacing, by = y.spacing)
fac             <- 7.20472 / 5
x1              <- -.006
x2              <- .029
xlim1           <- c(-.02,.02)
xlim2           <- c(-.01,.01)

# ---------------------------------------------------
# Now, PlosOne requires Arial fonts embedded in eps.
# we NEED eps because this figure needs to be crisp. Otherwise
# I'd have produced a vector pdf. Embedding fonts in eps is
# non-trivial in R
# ---------------------------------------------------

# getting Arial
#install_github("Rttf2pt1", "wch")
#install.packages("extrafont", dependencies = TRUE)
library(extrafont)
font_import() # only need to run once
loadfonts(device = "postscript") # this needs to be run every time I think
postscript("PlosOne/Figures/Figure1.eps", 
        height = 9.19, width = 6.83,
        family = "Arial", paper = "special", onefile = FALSE, bg = "white",
        horizontal = FALSE, pointsize = 12)
#dev.new(family = "Arial",height = 9.19, width = 6.83) # for experimenting, text moves
par(mai = c(0,0,0,0), xaxs = "i", yaxs = "i",xpd=TRUE)
plot(NULL, type = "n", xlim = c(-.036,.054), ylim = c(-60, 810), 
        axes = FALSE, xlab = "",ylab="")
# background
invisible(sapply(y.grid,function(y){
                    makeBG(x=x1,y=y,xlim=xlim1,col=NA, border = gray(.8), lwd = .5)  
                    makeBG(x=x2,y=y,xlim=xlim2,l=FALSE,col=NA,border = gray(.8), lwd = .5)  
                }))
# pyramids/leaves
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

# ---------------------------------------------------
# whew. primitive plotting is tedious.
# as Hadley Wickham says, using base to plot is like drawing,
# whereas ggplot2 is for understanding your data. This I admit
# was like old-school drafting. I have a bias for artisanal figures
# in articles.
# ---------------------------------------------------
