# Figure 2 is for the baby boomers.
# to produce BoomerDx.Rdata, first run Boomers.R
# that is not necessary, as the data object produced is 
# included in ~/PlosOne/Data/ already.

# change this!
setwd("/home/triffe/git/Leaves")

DD    <- local(get(load("PlosOne/Data/BoomerDx.Rdata")))
years <- as.integer(colnames(DD))
ylim  <- range(pretty(DD))
xlim  <- range(years)
library(extrafont)
#font_import()
loadfonts(device = "postscript")
postscript("PlosOne/Figures/Figure2.eps", 
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

