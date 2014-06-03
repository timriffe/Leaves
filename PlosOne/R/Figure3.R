# figure 3 compares some historical leaves with contemporary leaves
# uses data object produced in DataPrep1_HMD.R,
# which is sitting in ~/PlosOne/Data/
# HMDall.Rdata
# script includes many other interactive results. Figure 3 is around line 300

# change this!
setwd("/home/triffe/git/Leaves")

library(RiffeetalFunctions) # see DataPrep1_HMD.R for installation
library(data.table) # speedy
library(reshape2)   # handy
#
Data <- local(get(load("PlosOne/Data/HMDAll.Rdata")))

# remove Belgium gap WWI
Data <- Data[! (Data$Code == "BEL" & Data$Year >= 1914 & Data$Year <= 1918),]

Data <- data.table(Data)
# define indicator functions:
# old age dependency ratio, rather limited use, won't be plotted, as we know what it's all about.
getOADR <- function(Pop,Age){ 
    sum(Pop[Age >= 65], na.rm = TRUE) / 
            sum(Pop[Age >= 15 & Age < 65], na.rm = TRUE)
}

# not sure what paper to cite, but this is the general idea.
# tells us: what proportion is of an age with a remaining life expectancy of 15 or less
getPu15Sanderson <- function(Pop, ex, Age, Sex){
    rle15m <- splinefun(Age[Sex == "m"]~ex[Sex == "m"])(15)
    rle15f <- splinefun(Age[Sex == "m"]~ex[Sex == "f"])(15)
    fracm  <- rle15m - floor(rle15m)
    fracf  <- rle15f - floor(rle15f)
    (sum(Pop[Age > rle15m & Sex == "m"]) + 
                sum(Pop[Age > rle15f & Sex == "f"]) + 
                (1-fracm) * Pop[Age == floor(rle15m) & Sex == "m"] +
                (1-fracf) * Pop[Age == floor(rle15f) & Sex == "f"]
                ) / sum(Pop)
}

# tells us what proportion of a population has a remaining lifespan of 15 or less, though they
# could come from any age. Rather different from the above.
getPu15Riffe <- function(Py,Age){
    # Py <- Data$Py[1:111] 
    sum(Py[Age <= 15]) / sum(Py)
}

#getOADR(Data$Pop1[1:222],Data$Age[1:222],Data$Sex[1:222])
#getPu15Sanderson(Data$Pop1[1:222],Data$ex[1:222],Data$Age[1:222],Data$Sex[1:222])
#getPu15Riffe(Data$Py[1:222],Data$Age[1:222])


OADR      <- Data[, getOADR(Pop1,Age),by=list(Code,Year)]
Pu15Sand  <- Data[,getPu15Sanderson(Pop1,ex,Age,Sex),by=list(Code,Year)]
Pu15Riffe <- Data[,getPu15Riffe(Py,Age),by=list(Code,Year)]

# change V1 to variable name
setnames(OADR, 3, "OADR") 
setnames(Pu15Sand, 3, "Pu15S")
setnames(Pu15Riffe, 3, "Pu15R")

# turn into Year by Country matrices
Pu15S  <- acast(Pu15Sand,Year~Code,value.var = "Pu15S")
Pu15R  <- acast(Pu15Riffe,Year~Code,value.var = "Pu15R")
OADRs  <- acast(OADR,Year~Code,value.var = "OADR")

# years are x for plotting
years  <- as.integer(rownames(OADRs))
# spaghetti! ( not shown in paper)
matplot(years, Pu15R, type = 'l', lty = 1, col = "#00000050", ylim = c(0, .45))
matplot(years, Pu15S, type = 'l', lty = 1, add = TRUE, col = "#FF000050")
matplot(years, OADRs, type = 'l', lty = 1, add = TRUE, col = "#0000FF50")
legend("bottomleft",col=c("black","red","blue"),lty=1,legend = c("RLS<=15","RLE<=15","OADR"))

# how do these compare in the recent period?
matplot(years,Pu15R,type='l',lty=1,col="#00000050",ylim=c(0,.3), xlim = c(1950,2012))
matplot(years,Pu15S,type='l',lty=1,add=TRUE,col="#FF000050")

# check for stationary population. Use identity that cy = ca for a=y in stationary population.
# hence, we can just use Lx as a perfect proxy for Ly...

sum(Data$Lx[1:16]) / sum(Data$Lx[1:111]) # stationary prop with remaining lifespan of 16 or less...

# at what exact age does ex cross 15?
rle15 <- splinefun(0:110~Data$ex[1:111])(15)
# we split that age in two parts using frac:
frac  <- rle15 - floor(rle15)

# stationary prop in ages for which the remaining life expectancy is 15 or less: (different value!)
(sum(Data$Lx[1:111][(0:110) > rle15]) + 
            (1-frac) * Data$Lx[1:111][(0:110) == floor(rle15)]) / 
        sum(Data$Lx[1:111])

# ----------------------------------------------------
# these ended up in a poster at PAA, 2014.
# not in paper, but they tell a wonderful
# story of historical population change.

#pdf("/home/triffe/git/PAA2014poster/PAA2014Poster/Figures/TimeSeries.pdf", width = 12, height = 12)
years <- 1751:2011
ylim  <- c(0, .45)
xlim  <- range(years)
par(mai = c(.25, .5, .25, .5), xaxs = "i", yaxs = "i", mfrow=c(3,1))
# -----------------------------------------------
# Riffe et al
plot(NULL, type = "n", xlim = xlim, ylim = ylim, xlab = "" , ylab = "",
        axes = FALSE, 
        panel.first = list(
                rect(xlim[1], ylim[1], xlim[2], ylim[2], col = gray(.95), border = NA),
                segments(xlim[1], seq(ylim[1], ylim[2], by = .05), xlim[2], seq(ylim[1], ylim[2], by = .05), col = "white",lwd=.5),
                segments(years[years %% 10 == 0], ylim[1], years[years %% 10 == 0], ylim[2], col = "white",lwd=.5),
                segments(xlim[1], seq(ylim[1], ylim[2], by = .1), xlim[2], seq(ylim[1], ylim[2], by = .1), col = "white"),
                segments(years[years %% 20 == 0], ylim[1], years[years %% 20 == 0], ylim[2], col = "white"),
                text(xlim[1], seq(ylim[1], ylim[2], by = .1), paste0(seq(ylim[1], ylim[2], by = .1) * 100,"%"), 
                        pos = 2, cex = 1.8, xpd = TRUE),
                text(years[years %% 20 == 0],ylim[1], years[years %% 20 == 0], 
                        pos = 1, cex = 1.8, xpd = TRUE)
        ))
matplot(years, Pu15R, type = 'l', lty = 1, col = "#00000050", add = TRUE)
# Sanderson & Scherbov
plot(NULL, type = "n", xlim = xlim, ylim = ylim, xlab = "" , ylab = "",
        axes = FALSE, 
        panel.first = list(
                rect(xlim[1], ylim[1], xlim[2], ylim[2], col = gray(.95), border = NA),
                segments(xlim[1], seq(ylim[1], ylim[2], by = .05), xlim[2], seq(ylim[1], ylim[2], by = .05), col = "white",lwd=.5),
                segments(years[years %% 10 == 0], ylim[1], years[years %% 10 == 0], ylim[2], col = "white",lwd=.5),
                segments(xlim[1], seq(ylim[1], ylim[2], by = .1), xlim[2], seq(ylim[1], ylim[2], by = .1), col = "white"),
                segments(years[years %% 20 == 0], ylim[1], years[years %% 20 == 0], ylim[2], col = "white"),
                text(xlim[1], seq(ylim[1], ylim[2], by = .1), paste0(seq(ylim[1], ylim[2], by = .1) * 100,"%"), 
                        pos = 2, cex = 1.8, xpd = TRUE),
                text(years[years %% 20 == 0],ylim[1], years[years %% 20 == 0], 
                        pos = 1, cex = 1.8, xpd = TRUE)
        ))
matplot(years, Pu15S, type = 'l', lty = 1, col = "#FF000050", add = TRUE)
# OADR
plot(NULL, type = "n", xlim = xlim, ylim = ylim, xlab = "" , ylab = "",
        axes = FALSE, 
        panel.first = list(
                rect(xlim[1], ylim[1], xlim[2], ylim[2], col = gray(.95), border = NA),
                segments(xlim[1], seq(ylim[1], ylim[2], by = .05), xlim[2], seq(ylim[1], ylim[2], by = .05), col = "white",lwd=.5),
                segments(years[years %% 10 == 0], ylim[1], years[years %% 10 == 0], ylim[2], col = "white",lwd=.5),
                segments(xlim[1], seq(ylim[1], ylim[2], by = .1), xlim[2], seq(ylim[1], ylim[2], by = .1), col = "white"),
                segments(years[years %% 20 == 0], ylim[1], years[years %% 20 == 0], ylim[2], col = "white"),
                
                text(xlim[1], seq(ylim[1], ylim[2], by = .1), paste0(seq(ylim[1], ylim[2], by = .1) * 100,"%"), 
                        pos = 2, cex = 1.8, xpd = TRUE),
                text(years[years %% 20 == 0],ylim[1], years[years %% 20 == 0], 
                        pos = 1, cex = 1.8, xpd = TRUE)
        ))
matplot(years, OADRs, type = 'l', lty = 1, col = "#0000FF50", add = TRUE)
#dev.off()
#graphics.off()
# ----------------------------------------------------------
# Difference
#plot(NULL, type = "n", xlim = xlim, ylim = ylim, xlab = "" , ylab = "",
#        axes = FALSE, 
#        panel.first = list(
#                rect(xlim[1], ylim[1], xlim[2], ylim[2], col = gray(.95), border = NA),
#                segments(xlim[1], seq(ylim[1], ylim[2], by = .05), xlim[2], seq(ylim[1], ylim[2], by = .05), col = "white",lwd=.5),
#                segments(years[years %% 10 == 0], ylim[1], years[years %% 10 == 0], ylim[2], col = "white",lwd=.5),
#                segments(xlim[1], seq(ylim[1], ylim[2], by = .1), xlim[2], seq(ylim[1], ylim[2], by = .1), col = "white"),
#                segments(years[years %% 20 == 0], ylim[1], years[years %% 20 == 0], ylim[2], col = "white"),
#                
#                text(xlim[1], seq(ylim[1], ylim[2], by = .1), paste0(seq(ylim[1], ylim[2], by = .1) * 100,"%"), 
#                        pos = 2, cex = .8, xpd = TRUE),
#                text(years[years %% 20 == 0],ylim[1], years[years %% 20 == 0], 
#                        pos = 1, cex = .8, xpd = TRUE)
#        ))
#matplot(years, Pu15R-Pu15S, type = 'l', lty = 1, col = "#00000050", add = TRUE)
#
#
#
#yrs <- 1970:2011
#hist(apply(Pu15S[as.character(yrs), ], 2, function(y){
#                    lm(y~yrs)$coef[2]
#                }),xlab = "slope")
#hist(apply(Pu15R[as.character(yrs), ], 2, function(y){
#                    lm(y~yrs)$coef[2]
#                }),xlab = "slope")
#
#
#dim(Pu15R)
#yearsc <- rownames(Pu15R)[200:261]


# Mean Age, Mean Sanderson  and Mean Riffe
# assume all mid interval:

getMeanAge <- function(Age, Px){
    sum(((Age + .5) * Px) / sum(Px, na.rm = TRUE), na.rm = TRUE)
}

getMeanYL_Riffe <- function(Y, Py){
    sum(((Y + .5) * Py) / sum(Py, na.rm = TRUE), na.rm = TRUE)
}

ex  <- Data$ex[with(Data, Year == 2000 & Code == "USA")]
Age <- Data$Age[with(Data, Year == 2000 & Code == "USA")]
Px  <- Data$Pop1[with(Data, Year == 2000 & Code == "USA")]
Sex <- Data$Sex[with(Data, Year == 2000 & Code == "USA")]


getMeanYL_Sanderson <- function(ex, Age, Px, Sex){
    ex.5m <- splinefun(ex[Sex == "m"]~Age[Sex == "m"])(Age[Sex == "m"]+.5)
    ex.5f <- splinefun(ex[Sex == "f"]~Age[Sex == "f"])(Age[Sex == "f"]+.5)
    
    sum((ex.5m * Px[Sex == "m"] + ex.5f * Px[Sex == "f"]) / sum(Px, na.rm = TRUE), na.rm = TRUE)
}

MA      <- Data[, getMeanAge(Age, Pop1),by=list(Code,Year)]
MYSand  <- Data[,getMeanYL_Sanderson(ex,Age,Pop1,Sex),by=list(Code,Year)]
MYRiffe <- Data[,getMeanYL_Riffe(Age,Py),by=list(Code,Year)]

# change V1 to variable name
setnames(MA, 3, "MeanAge") 
setnames(MYSand, 3, "MYSand")
setnames(MYRiffe, 3, "MYRiffe")

# turn into Year by Country matrices
MYSandM   <- acast(MYSand,Year~Code,value.var = "MYSand")
MYRiffeM  <- acast(MYRiffe,Year~Code,value.var = "MYRiffe")
MeanAgeM  <- acast(MA,Year~Code,value.var = "MeanAge")

years  <- as.integer(rownames(OADRs))
# spaghetti!
matplot(years, MYRiffeM, type = 'l', lty = 1, col = "#00000050")
#matplot(years, MYSandM, type = 'l', lty = 1, add = TRUE, col = "#FF000050")
matplot(years, MeanAgeM, type = 'l', lty = 1, add = TRUE, col = "#0000FF50")
#legend("bottomleft",col=c("black","red","blue"),lty=1,legend = c("RLS<=15","RLE<=15","OADR"))

matplot(years, MYSandM - MYRiffeM, type = 'l', lty = 1, col = "#00000050")

matplot(years, MYRiffeM - MeanAgeM , type = 'l', lty = 1, col = "#00000050")
abline(h=0)


RM <- c("RUS","SVN","UKR","LTU","LVA","HUN","EST","DEUTE","BLR")
Ind <- !colnames(MYRiffeM) %in% RM
matplot(years, MYRiffeM[,Ind] - MeanAgeM[,Ind] , type = 'l', lty = 1, col = "#00000050")
abline(h=0)
matplot(years, MYRiffeM[,!Ind] - MeanAgeM[,!Ind] , type = 'l', lty = 1, col = "#FF000050",add=TRUE)

tail(MYRiffeM - MeanAgeM)

colnames(MYRiffeM)[order((MYRiffeM - MeanAgeM)["2009",])]

# less: for the whole population, we have the same result as for Sanderson & Scherbov,
# but for individual thanatological age classes, we have different results. We should then 
# showcase the different results. But then we're not going to mention such
# minutea in the paper, no point for this audience I don't think.

# more graphical experimentation
makePyrOutline <- function(Data, .Code = "USA", .Year = 1940,Age = TRUE,...){
    if (Age){
        Males   <- Data$Pop1[with(Data, Sex == "m" & Code == .Code & Year == .Year)]
        Females <- Data$Pop1[with(Data, Sex == "f" & Code == .Code & Year == .Year)]
    } else {
        Males   <- Data$Py[with(Data, Sex == "m" & Code == .Code & Year == .Year)]
        Females <- Data$Py[with(Data, Sex == "f" & Code == .Code & Year == .Year)]
    }

    PyramidOutline(males = Males,females = Females,...)
}
years <- seq(1935,2010,by=5)

par(mfrow=c(2,2),mai = c(.1,.1,.1,.1))
plot(NULL, type = "n", ylim = c(0,111),xlim = c(-.015,.015), axes = FALSE, xlab = "", ylab = "")
makePyrOutline(Data,"USA",1935,scale=1,border="blue", col = "#0011DD50")
makePyrOutline(Data,"USA",2010,scale=1, col = "#00000050")

years <- seq(1935,2010,by=5)
plot(NULL, type = "n", ylim = c(0,111),xlim = c(-.015,.015), axes = FALSE, xlab = "", ylab = "")
makePyrOutline(Data,"USA",1935,Age=FALSE,scale=1,border="blue", col = "#0011DD50")
makePyrOutline(Data,"USA",2010,Age=FALSE,scale=1, col = "#00000050")

plot(NULL, type = "n", ylim = c(0, 111),xlim = c(-.015, .015), axes = FALSE, xlab = "", ylab = "")
makePyrOutline(Data, "SWE", 1850, scale = 1,border = "blue", col = "#0011DD50")
makePyrOutline(Data, "SWE", 2010, scale = 1, col = "#00000050")
 
plot(NULL, type = "n", ylim = c(0, 111), xlim = c(-.015, .015), axes = FALSE, xlab = "", ylab = "")
makePyrOutline(Data, "SWE", 1850, Age = FALSE, scale = 1,border = "blue", col = "#0011DD50")
makePyrOutline(Data, "SWE", 2010, Age = FALSE, scale = 1, col = "#00000050")

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

##############################
# for PlosOne, figure 3
##############################
# clean devices
graphics.off()

y.spacing       <- 130
y.grid          <- seq(0, 1 * y.spacing, by = y.spacing)
fac             <- 7.20472 / 5
x1              <- 0
x2              <- .033
xlim1           <- c(-.015,.015)
xlim2           <- c(-.01,.01)
FigureCountries <- c("USA","SWE")

library(extrafont)
#font_import()
loadfonts(device = "postscript")
postscript("PlosOne/Figures/Figure3.eps", 
        height = 5, width = 6.83,
        family = "Arial", paper = "special", onefile = FALSE, bg = "white",
        horizontal = FALSE, pointsize = 12)
#dev.new(width = 6.83,height = 5)
par(mai = c(.1,.8,.1,.05), xaxs="i",yaxs="i",xpd=TRUE)
plot(NULL, type = "n", ylim = c(-50, 270), xlim = c(-.02, .05), axes = FALSE, xlab = "", ylab = "")
invisible(sapply(y.grid,function(y){
                    makeBG(x=x1,y=y,xlim=xlim1,col=NA, border = gray(.8), lwd = .5)  
                    makeBG(x=x2,y=y,xlim=xlim2,l=FALSE,col=NA,border = gray(.8), lwd = .5)  
                }))

# US row
# Chrono
    makePyrOutline(Data, .Code = FigureCountries[1], .Year = 1935, Age = TRUE, x = x1, y = y.grid[1], scale = 1, border = "blue", col = "#0011DD50")
    makePyrOutline(Data, FigureCountries[1], 2010, Age = TRUE, x = x1, y = y.grid[1], scale = 1, col = "#00000050")
# Thano
    makePyrOutline(Data, .Code = FigureCountries[1], .Year = 1935, Age = FALSE, x = x2, y = y.grid[1], scale = 1, border = "blue", col = "#0011DD50")
    makePyrOutline(Data, FigureCountries[1], 2010, Age = FALSE, x = x2, y = y.grid[1], scale = 1, col = "#00000050")
# Sweden row
# Chrono
    makePyrOutline(Data, .Code = FigureCountries[2], .Year = 1850, Age = TRUE, x = x1, y = y.grid[2], scale = 1, border = "blue", col = "#0011DD50")
    makePyrOutline(Data, FigureCountries[2], 2010, Age = TRUE, x = x1, y = y.grid[2], scale = 1, col = "#00000050")
# Thano
    makePyrOutline(Data, .Code = FigureCountries[2], .Year = 1850, Age = FALSE, x = x2, y = y.grid[2], scale = 1, border = "blue", col = "#0011DD50")
    makePyrOutline(Data, FigureCountries[2], 2010, Age = FALSE, x = x2, y = y.grid[2], scale = 1, col = "#00000050")
    
    
# main title
text(x1+xlim1[2],max(y.grid )+15+111,"Pyramids",pos=2,cex=1, font=2)
text(x2+xlim2[1],max(y.grid )+15+111,"Leaves",pos=4, font = 2)

# x axis labels
text(seq(xlim1[1],xlim1[2],by=.005)+x1,-2,c("1.5%","1.0%","0.5%","0","0.5%","1.0%","1.5%"),pos=1,cex=8/12)
text(seq(xlim2[1],xlim2[2],by=.005)+x2,-2,c("1.0%","0.5%","0","0.5%","1.0%"),pos=1,cex=8/12)

# y axis labels
text(xlim1[1]+x1, outer(seq(0, 100, by = 20), y.grid, "+"), seq(0, 100, by = 20), pos = 2, cex = 8/12)
text(xlim2[2]+x2, outer(seq(0, 100, by = 20), y.grid, "+"), seq(0, 100, by = 20), pos = 4, cex = 8/12)

# male female labels:
text(-.002 + x1, -25, "Male", cex = 10/12, pos = 2)
text(.002 + x1, -25, "Female", cex = 10/12, pos = 4)
text(-.002 + x2, -25, "Male", cex = 10/12, pos = 2)
text(.002 + x2, -25, "Female", cex = 10/12, pos = 4)

# label countries
text(-.027,y.grid+85,
        c("US","Sweden"),
        cex=(10/12),pos=4)
text(-.027,y.grid+100,
        c("B","A"),
        cex=1,pos=4,font=2)# bold

# year legend:
rect(-.023,y.grid+60,-.021,y.grid+70, col = c("#00000050","#00000050"), border = "black")
rect(-.023,y.grid+45,-.021,y.grid+55, col = c("#0011DD50","#0011DD50"), border = "blue")
text(-.0233,y.grid + 65, c(2010,2010), pos = 2, cex = 10/12)
text(-.0233,y.grid + 50, c(1935,1850), pos = 2, cex = 10/12)

# y axis labels:
text(x1+xlim1[1]-.003,62,"Years lived",srt=90,cex=10/12,pos=2)
text(x2+xlim2[2]+.003,62,"Years left",srt=270,cex=10/12,pos=4)

dev.off()


