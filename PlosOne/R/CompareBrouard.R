
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

# later located a mathematical piece of his, from a workshop in Cameroon
# and he used the same continuous formulas. Our motivations and implementations
# were similar, but we differ in a few ways.

# 1) exemplifying heterogeneity (decomposition) in both 
# chronological pyramids and population leaves. That's a big visual difference

# 2) in applying scenarios to the boomers (Brouard used strictly historical and
# period mortality)

# 3) in including a discrete, easy to implement, formula in the paper (and of course this code)

# 4) in applying the perspective to contemporary, global populations.

# Indeed, Brouard should take the credit for the method, but we do need to be explicit in
# including the method in the paper still, because the one in French does not seem
# to have made it into the canon, and the discrete formula helps with replicability.

# 5) the notion of thanatological age in a bit more of a mind-bender than simply saying
# remaining years of life. That's a subtle difference too, and worth pointing out. Not
# just nomenclature. Considering some phrases from Brouard 1986, I think he might agree.

# ----------------------------------------------




