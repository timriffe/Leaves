
# 1) HMD Data Prep:
# instructions:

# 1) download the big HMD zip files from www.mortality.org
#              1.1) lt_female.zip
#              1.2) lt_male.zip
#              1.3) population.zip
# 2) extract these to a folder called /HMD/
#    so as to create the folder structure:
#    HMD/
#       lt_female/
#       lt_male/
#       population/
# (each of these will contain more subfolders.)

## we'll need a few functions:
# library(RiffeetalFunctions)
# library(devtools)
# load_all("/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/R/RiffeetalFunctions")

# the main HMD folder path, change as necessary:
HMDpath <- "/home/triffe/DATA/HMD"

# slice off HMD country short codes from file names:
HMDcountries <- unlist(lapply(list.files(file.path(HMDpath, "lt_female","fltper_1x1")), function(x){
                    strsplit(x, split = "\\.")[[1]][[1]]
                }))

# -------------------------------------------------------------------------------
# warning: the HMD part of this code takes around 15 minutes to execute
#          on my 2009 2.26GHz laptop
# -------------------------------------------------------------------------------
# make similar long format for HMD data, Deaths, Exp, Pop Counts all smacked together. 
# Later we'll put on Fert data to make a single awesome object.
Data <- do.call(rbind,lapply(HMDcountries, function(XXX, HMDpath){
                    
                    flt <- read.table(file.path(HMDpath,"lt_female/fltper_1x1", paste0(XXX, ".fltper_1x1.txt")),
                            skip = 2, header = TRUE, as.is = TRUE, na.strings = ".")
                    mlt <- read.table(file.path(HMDpath,"lt_male/mltper_1x1", paste0(XXX, ".mltper_1x1.txt")),
                            skip = 2, header = TRUE, as.is = TRUE, na.strings = ".")
                    pop <- read.table(file.path(HMDpath,"population/Population", paste0(XXX, ".Population.txt")),
                            skip = 2, header = TRUE, as.is = TRUE, na.strings = ".")
                    
                    flt$Sex <- "f"
                    mlt$Sex <- "m"
                    flt$Code <- XXX
                    mlt$Code <- XXX
                    
                    # Jan 1 pops
                    pop$Year    <- as.character(pop$Year)
                    yrs         <- sort(unique(gsub("\\+","",gsub("\\-","",pop$Year))))
                    Nyr         <- length(yrs)
                    
                    pop1        <- pop[!grepl("\\-", pop$Year), ]
                    pop2        <- pop[!grepl("\\+", pop$Year), ]
                    pop1$Year   <- gsub("\\+","",pop1$Year)
                    pop2$Year   <- gsub("\\-","",pop2$Year)
                    pop1        <- pop1[pop1$Year != yrs[Nyr], ]
                    pop2        <- pop2[pop2$Year != yrs[1], ]
                    
                    # we can trust the ordering here, standard HMD data and dimensions
                    flt$Pop1    <- pop1$Female
                    flt$Pop2    <- pop2$Female
                    mlt$Pop1    <- pop1$Male
                    mlt$Pop2    <- pop2$Male
                    
                    # return as a single obj
                    rbind(flt, mlt)
                }, HMDpath = HMDpath))

Data$Age  <- as.integer(gsub("\\+","",Data$Age))
Data$Year <- as.integer(Data$Year)

# ------------------------------------

# since we use dx, etc, lets standardize to radix 1
Data <- do.call(rbind,lapply(split(Data, list(Data$Code, Data$Year)), function(Dat){
                    Dat$dx <- Dat$dx / sum(Dat$dx)
                    Dat$lx <- Dat$lx / 1e5
                    Dat$Lx <- Dat$Lx / 1e5
                    Dat$Tx <- Dat$Tx / 1e5
                    Dat
                }))

# now add Py, take a while to compute
Data <- do.call(rbind,lapply(split(Data, list(Data$Code, Data$Sex, Data$Year)), function(Dat){
                    if (nrow(Dat)==111){
                        Dat$Py <- Mna0(Minf0(rowSums(Thano(Dat$Pop1, Dat$dx))))   
                    }
                    Dat
                }))

#-----------------------------------------
rownames(Data) <- NULL
#-----------------------------------------
# save it out for later use
# save(Data,file = "/home/triffe/git/RiffeSpijkerMacInnes1/PlosOne/Data/HMDAll.Rdata")
#

















