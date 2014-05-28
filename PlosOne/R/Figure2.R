

# These values were produced by Jeroen Spijker in Excel and copied here for
# Figure production. The Excel spreadsheet may be reproduced here in R at
# some future date. For now, here's the Figure code.

# Figure by Tim Riffe
#
#BBDt <-c(10.431546217253900000,   11.315331001853900000,   12.150056286091400000,   13.057960843370300000,   14.049066812302600000,
#        14.924919931548400000,   16.027772718321500000,   16.929566720381500000,   17.829015348353500000,   19.294965304940900000,
#        20.073051773061100000,   20.863186852993200000,   21.651557184018300000,   22.538029499595200000,   23.488883450317900000,
#        24.506708705008900000,   25.612200195746200000,   26.787726212352800000,   28.071583466689800000,   29.428284284895500000,
#        30.777846126457400000,   32.300484648152400000,   33.924969984822600000,   35.621970567280300000,   37.319693724456200000,
#        39.100025746589800000,   40.963707975754500000,   43.093748380648800000,   44.823601627243500000,   46.993719313237300000,
#        49.103142935025800000,   51.122264589262900000,   52.994817219475400000,   54.911724100612100000,   56.753682711418500000,
#        58.440672518436300000,   60.001545277593200000,   61.252458113915900000,   62.341506962737600000,   63.126682531452600000,
#        63.585291080321800000,   63.658180049144300000,   63.423923216454900000,   62.766496962559700000,   61.727383486535500000,
#        60.365157445076400000,   58.758495336813400000,   56.558422521594400000,   54.113293498414600000,   51.307204474102100000,
#        48.160384488601300000,   44.865852927865800000,   41.551383223916900000,   38.046961141606100000,   34.451573350056600000,
#        30.733494608241000000,   27.208687575438700000,   23.530893612047300000,   20.090437854262800000,   16.869847494269700000,
#        13.940295155038800000)
#
##res <- 200
## 3.50394 = 89mm
#years <- 2000:2060
#pdf("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures/Riffe_fig2.pdf",height=3.50394,width=3.50394)
#par(mai=c(.4,.5,.5,.1)*3.50394/5)
#plot(NULL,type='n',xlim=c(2000,2060),ylim=c(0,100),axes=FALSE,xlab="",ylab="",
#        panel.first = list(rect(2000,0,2060,100,col=gray(.94),border=NA),
#                #text(c(2010,2010,2040,2040)+5,c(20,70,20,70)+5,rep("Draft",4),srt=45,cex=7,col="#00000006",xpd=TRUE),
#                abline(v=seq(2010,2050,by=10),col="white"),
#                abline(h=seq(10,90,by=10),col="white"),
#                text(seq(2000,2060,by=10),0,seq(2000,2060,by=10),cex=.6,pos=1,xpd=TRUE),
#                segments(seq(2000,2060,by=10),0,seq(2000,2060,by=10),-.7,xpd=TRUE),
#                text(2000,seq(0,100,by=10),paste0(seq(0,100,by=10),"%"),cex=.6,pos=2,xpd=TRUE),
#                segments(2000,seq(0,100,by=10),1999.5,seq(0,100,by=10),xpd=TRUE),
#                segments(2011,0,2011,100,gray(.5)),
#                segments(2012,0,2012,100,gray(.5)),
#                text(2012,97,"USCB Projection",pos=4,cex=.6),
#                text(2011,97,"HMD", pos=2,cex=.6),
#                arrows(2010.5, 94, 2006, 94, length=.08),
#                arrows(2012.5, 94, 2017, 94, length=.08)))
#lines(years, BBDt, lwd = 2)
#text(2030,-11,"Year",xpd=TRUE,cex=.7)
#text(2000,110,"% of all deaths\nto baby boomers",xpd=TRUE,cex=.7)
#dev.off()
#
#
#
#png("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures/Riffe_fig2.png",
#        height=4.5*150,width=4.5*150,res=150,pointsize=12)
#par(mai=c(.4,.5,.5,.1)*3.50394/5)
#plot(NULL,type='n',xlim=c(2000,2060),ylim=c(0,100),axes=FALSE,xlab="",ylab="",
#        panel.first = list(rect(2000,0,2060,100,col=gray(.94),border=NA),
#                #text(c(2010,2010,2040,2040)+5,c(20,70,20,70)+5,rep("Draft",4),srt=45,cex=7,col="#00000006",xpd=TRUE),
#                abline(v=seq(2010,2050,by=10),col="white"),
#                abline(h=seq(10,90,by=10),col="white"),
#                text(seq(2000,2060,by=10),0,seq(2000,2060,by=10),cex=.6,pos=1,xpd=TRUE),
#                segments(seq(2000,2060,by=10),0,seq(2000,2060,by=10),-.7,xpd=TRUE),
#                text(2000,seq(0,100,by=10),paste0(seq(0,100,by=10),"%"),cex=.6,pos=2,xpd=TRUE),
#                segments(2000,seq(0,100,by=10),1999.5,seq(0,100,by=10),xpd=TRUE),
#                segments(2011,0,2011,100,gray(.5)),
#                segments(2012,0,2012,100,gray(.5)),
#                text(2012,97,"USCB Projection",pos=4,cex=.6),
#                text(2011,97,"HMD", pos=2,cex=.6),
#                arrows(2010.5, 94, 2006, 94, length=.08),
#                arrows(2012.5, 94, 2017, 94, length=.08)))
#lines(years, BBDt, lwd = 2)
#text(2030,-10,"Year",xpd=TRUE,cex=.7)
#text(2000,110,"% of all deaths\nto baby boomers",xpd=TRUE,cex=.7)
#dev.off()
#
#
#library(extrafont)
##font_import()
#loadfonts(device = "postscript")
#postscript("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures/Figure2.eps", 
#        height = 3.27, width = 3.27,
#        family = "Arial", paper = "special", onefile = FALSE, bg = "white",
#        horizontal = FALSE, pointsize = 12)
#par(mai=c(.25,.45,.4,.05))
#plot(NULL,type='n',xlim=c(2000,2060),ylim=c(0,100),axes=FALSE,xlab="",ylab="",
#        panel.first = list(rect(2000,0,2060,100,col=gray(.94),border=NA),
#                #text(c(2010,2010,2040,2040)+5,c(20,70,20,70)+5,rep("Draft",4),srt=45,cex=7,col="#00000006",xpd=TRUE),
#                abline(v=seq(2010,2050,by=10),col="white"),
#                abline(h=seq(10,90,by=10),col="white"),
#                text(seq(2000,2060,by=10),0,seq(2000,2060,by=10),cex=8/12,pos=1,xpd=TRUE),
#                segments(seq(2000,2060,by=10),0,seq(2000,2060,by=10),-.7,xpd=TRUE),
#                text(2000,seq(0,100,by=10),paste0(seq(0,100,by=10),"%"),cex=8/12,pos=2,xpd=TRUE),
#                segments(2000,seq(0,100,by=10),1999.5,seq(0,100,by=10),xpd=TRUE),
#                segments(2011,0,2011,100,gray(.5)),
#                segments(2012,0,2012,100,gray(.5)),
#                text(2012,97,"USCB Projection",pos=4,cex=8/12),
#                text(2011,97,"HMD", pos=2,cex=8/12),
#                arrows(2010.5, 93, 2006, 93, length=.08),
#                arrows(2012.5, 93, 2017, 93, length=.08)))
#lines(years, BBDt, lwd = 2)
#text(2030,-10.5,"Year",xpd=TRUE,cex=10/12)
#text(2000,110,"% of all deaths\nto baby boomers",xpd=TRUE,cex=10/12)
#dev.off()
#

# --------------------------------------------------------------------------------------
# No longer using Jeroen's results:

DD    <- local(get(load("/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Data/BoomerDx.Rdata")))
years <- as.integer(colnames(DD))
ylim  <- range(pretty(DD))
xlim  <- range(years)
library(extrafont)
#font_import()
loadfonts(device = "postscript")
postscript("/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Figures/Figure2.eps", 
        height = 3.27, width = 3.27,
        family = "Arial", paper = "special", onefile = FALSE, bg = "white",
        horizontal = FALSE, pointsize = 12)
par(mai=c(.35,.45,.3,.2))
plot(NULL, type = 'n', xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "",
        panel.first = list(rect(xlim[1], ylim[1], xlim[2], ylim[2], col = gray(.94), border = NA),
                #text(c(2010,2010,2040,2040)+5,c(20,70,20,70)+5,rep("Draft",4),srt=45,cex=7,col="#00000006",xpd=TRUE),
                abline(v=seq(2020,2070,by=10),col="white"),
                abline(h=pretty(DD[5,]),col="white"),
                text(seq(2020,2070,by=10),0,seq(2020,2070,by=10),cex=8/12,pos=1,xpd=TRUE),
                segments(seq(2020,2070,by=10),ylim[1],seq(2020,2070,by=10),ylim[1]-2e4,xpd=TRUE),
                text(2011,pretty(DD[1,]),pretty(DD[1,]/1e3),cex=8/12,pos=2,xpd=TRUE),
                segments(2011,pretty(DD[1,]),2010.5,pretty(DD[1,]),xpd=TRUE)
        ))
lines(years, DD[5,], lwd = 2)
text(2050, DD[5, "2050"], "2% improvement \ntapering off to 0", cex = 8 / 12, pos = 4)
lines(years, DD[1,], lwd = 3, col = gray(.6))
text(2043, DD[1, "2043"], "constant 2010 mortality", cex = 8 / 12, pos = 4)
lines(years, DD[6,], lwd = 2, lty = 2)
text(2061, DD[6, "2061"], "constant 2%\nimprovement", cex = 8 / 12, pos = 4, xpd = TRUE)
text(2040, -ylim[2] * .12, "Year", xpd = TRUE, cex = 10 / 12)
text(2006, ylim[2] * 1.1, "nr. deaths\n(1000s)", xpd = TRUE, cex = 10 / 12)
dev.off()



#
pdf("/home/triffe/git/RiffeSpijkerMacInnes1/ScienceArticle/Figures/Figure2.pdf", 
        height = 3.27, width = 3.27,
        family = "Arial",bg = "white",
        pointsize = 12)
par(mai=c(.35,.45,.3,.2))
plot(NULL, type = 'n', xlim = xlim, ylim = ylim, axes = FALSE, xlab = "", ylab = "",
        panel.first = list(rect(xlim[1], ylim[1], xlim[2], ylim[2], col = gray(.94), border = NA),
                #text(c(2010,2010,2040,2040)+5,c(20,70,20,70)+5,rep("Draft",4),srt=45,cex=7,col="#00000006",xpd=TRUE),
                abline(v=seq(2020,2070,by=10),col="white"),
                abline(h=pretty(DD[5,]),col="white"),
                text(seq(2020,2070,by=10),0,seq(2020,2070,by=10),cex=8/12,pos=1,xpd=TRUE),
                segments(seq(2020,2070,by=10),ylim[1],seq(2020,2070,by=10),ylim[1]-2e4,xpd=TRUE),
                text(2011,pretty(DD[1,]),pretty(DD[1,]/1e3),cex=8/12,pos=2,xpd=TRUE),
                segments(2011,pretty(DD[1,]),2010.5,pretty(DD[1,]),xpd=TRUE)
        ))
lines(years, DD[5,], lwd = 2)
text(2050, DD[5, "2050"], "2% improvement \ntapering off to 0", cex = 8 / 12, pos = 4)
lines(years, DD[1,], lwd = 3, col = gray(.6))
text(2043, DD[1, "2043"], "constant 2010 mortality", cex = 8 / 12, pos = 4)
lines(years, DD[6,], lwd = 2, lty = 2)
text(2061, DD[6, "2061"], "constant 2%\nimprovement", cex = 8 / 12, pos = 4, xpd = TRUE)
text(2040, -ylim[2] * .12, "Year", xpd = TRUE, cex = 10 / 12)
text(2006, ylim[2] * 1.1, "nr. deaths\n(1000s)", xpd = TRUE, cex = 10 / 12)
dev.off()
#
#
#
#
#
#
#

