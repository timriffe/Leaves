
# ouch, just after submitting we discovered:
# Brouard, Nicolas (1986) "Structure et Dyanmique des populations. La pyramide des annees 
# a vivre, aspects nationaux et exemples regionaux"

# he shows remaining years pyramids but does not show how they were derived. So I'm going to
# try to reproduce Figure 1 B (1982) to see whether it was plausibly with the same method. 

library(DemogBerkeley)
user <- userInput()
pass <- userInput()
Pop  <- readHMDweb("FRATNP","Population",username=user,password=pass)
mlt  <- readHMDweb("FRATNP","mltper_1x1",username=user,password=pass)
flt  <- readHMDweb("FRATNP","fltper_1x1",username=user,password=pass)

library(devtools)
load_all("/home/triffe/git/Leaves/PlosOne/R/RiffeetalFunctions")

# Jan 1 1982 population counts
PM <- Pop$Male1[Pop$Year == 1982]
PF <- Pop$Female1[Pop$Year == 1982]

dxm <- mlt$dx[mlt$Year == 1982]
dxf <- flt$dx[flt$Year == 1982]
dxm <- dxm/sum(dxm)
dxf <- dxf/sum(dxf)

PMy <- rowSums(Thano(PM, dxm))
PFy <- rowSums(Thano(PF, dxf))

library(Pyramid)
Pyramid(males = PMy, females = PFy, prop = FALSE)
# matches figure 5 1982 remaining-years pyramid!

exm <- mlt$ex[mlt$Year == 1982]
exf <- flt$ex[flt$Year == 1982]

plot(0:110,exm,type='l')

wmean <- function(x,w){
    sum(x*w)/sum(w)
}
wmean(0:110 + .5,PMy + PFy) # matches closely
wmean(0:110 + .5,PM + PF)   # also, duh

# it's time to write an homage to Brouard...!




