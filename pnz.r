prelim.df <- read.delim("Prelim.txt")
with(prelim.df, table(Lifestage, Species))
with(prelim.df, table(Species))

ab.prelim <- list()
ab.prelim[[1]] <- allfit(datafun = glean.prelim)
pdf(file = "EFprelim.pdf", width = 255/25.4, height = 195/25.4)
flyplot(data = ab.prelim, choice = 1, pc = c(line = 99), lt.ld = "LC")
dev.off()


### Replicated serious stuff:
rep.df <- read.delim("Replicated.txt")
ab.rep <- list()
ab.rep[[1]] <- allfit(datafun = glean.repd)
pdf(file = "EFrep.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:60, data = ab.rep, choice = 1, pc = c(line = 99), lt.ld = "LC", range.strategy = "individual", lt.rnd = 2)
dev.off()

data.spot(ab.rep,1) # >> TotalTested.txt
mean.lt(ab.rep, leg.beg = 0, leg.end = 2) ## don't keep
mean.lt(ab.rep, 2, leg.beg = 0, leg.end = 2, insect = "various in ULO",
        border = TRUE, rnd = 2) ##  keep

ab.rep$reduced <- chopduds() # plotted but irrelevant since glean.repdA
mean.lt(ab.rep, 2, leg.beg = 0, leg.end = 2, insect = "various in ULO",
         border = TRUE, rnd = 2) ##  keep
##############################################################
## Make "new" species wherein moribund ALCM are deemed dead
ab.rep$finished <- allfit(datafun = glean.repdA)

pdf(file = "EFrepMDML.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:60, data = ab.rep, choice = "finished", pc = c(line = 99),
        lt.ld = "LC", range.strategy = "individual", lt.rnd = 2)
dev.off()

mean.lt(ab.rep, "finished", leg.beg = 0, leg.end = 2,
        insect = "various in ethly formate PNZ", border = TRUE, rnd = 2) ##  keep


##############################################################
## loess plots
##############################################################
## get it working with one PDF file
plot.repd(, .8) # >> EthylFormateMortalityPNZ.pdf
### needed to  rerun that line after updating OS (strange but true)

## for report
plot.report() # >> ALMC_R.png, latania_R.png, omb_R.png, thrips_R.png

##
## Additional plots using moribund as dead
plot.reportA() # >> ALMCml_R.png, ALMCmd_R.png, latania_R.png, omb_R.png, thrips_R.png
plot.presentationA()
## When doing files for paper, start with plot.reportA

#### 29/4/14 PNGs for 2-column width of Plant Protection paper.
plot.paper()## >> ALMCml_paper.png, ALMCmd_paper.png, latania_paper.png,
            ## omb_paper.png, thrips_paper.png


##############################################################################
##
## 11/7/14 June 2014 data done similarly
##
#######################################################################

june.df <- read.delim("EF_June2014.txt") # all OFF fruit
june.df <- unfactor(june.df)
## remove egg hatch  lot
June.df <- june.df # keep all spare
june.df <- june.df[!june.df$Lifestage == "Egg hatch", ]

ab.june <- list()
ab.june[[1]] <- allfit(datafun = glean.june)
pdf(file = "EFjuneMortality.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:60, data = ab.june, choice = 1, pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = FALSE, lt.rnd = 2)
dev.off()
## total insects per trial
with(ab.june[[1]], data.frame(Gtotal = id.total, legend))

mean.lt(start.list = ab.june, choose = 1, leg.beg = 0, leg.end= 2, rnd = 2, border = TRUE, omit = "Male")

## Remove information poor trials from ab list
ab.june.use <- chopduds.june()
ab.june[["use"]] <- ab.june.use

mean.lt(start.list = ab.june, choose = 2, leg.beg = 0, leg.end= 2, rnd = 2, border = TRUE)
mean.lt(start.list = ab.june, choose = 1, leg.beg = 0, leg.end= 2, rnd = 2, border = TRUE)

## example of 12 plots in 3x4 array
flyplot(1:12, data = ab.june, choice = 1, pc = c(line = 99), lt.ld = "LC",
        range.strategy = "page", byrow = FALSE, lt.rnd = 2)

## PNGs for report
plot.reportJ(titl = TRUE, span = 0.8) ## >> SJS.png, GHT.png, WAA.png, LTMB.png
## overwrite with
plot.reportJ(titl = FALSE, span = 0.8) ##

ab.june[[2]] <- allfit(datafun = glean.juneE)
pdf(file = "EFjuneMortalityE.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:33, data = ab.june, choice = 2, pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = FALSE, lt.rnd = 2)
dev.off()

mean.lt(start.list = ab.june, choose = 2, leg.beg = 0, leg.end= 2, rnd = 2, border = TRUE)

##############################################################################
##
## 21/8/14 August data is ON fruit
##
##################################


aug.df <- read.delim("EF_Aug2014.txt") # june.df was all OFF fruit
aug.df <- unfactor(aug.df)
## remove egg hatch  lot
Aug.df <- aug.df # keep all spare
aug.df <- aug.df[!aug.df$Lifestage == "Egg hatch", ]


## aug.suss <- aug.df[is.na(aug.df$Total), ]
## aug.df <- within(aug.df, Total[is.na(Total)] <- 0)
aug.df <- clear.duds(aug.df)

ab.aug <- list()
ab.aug[[1]] <- allfit(datafun = glean.aug) # abandoned

##################################
##
## 14/10/2014
##
##################################
## September Brown-headed leafroller
sep.df <- read.delim("EF_Sep2014.txt") # BHLR only
sep.df <- unfactor(sep.df)
ab.sep <- list()
ab.sep[[1]] <- allfit(datafun = glean.sep)

pdf(file = "EFsepMortality.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:33, data = ab.sep, choice = 1, pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()
tidy()
with(ab.sep[[1]], data.frame(Gtotal = id.total, legend))

plot.reportS() # >> BHLR.png

mean.lt(ab.sep, 1, leg.beg = 0, leg.end= 2, rnd = 2, border = TRUE,
        insect = "BHLR", omit = c(1:4, 12:23)) # >> BHLR_LC.ci.pdf


#############################################################
##
## 09/06/2015 
##
######################################################

june15Off.df <- read.delim("June15offFruit.txt")
ab.june15Off <- list()
ab.june15Off[[1]] <- allfit(data = gleanOffFruit)
pdf(file = "EFjune15OffMortality.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:69, data = ab.june15Off, choice = 1, pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()

## combine reps
ab.june15Off$Joined <- allfit(data = gleanOffFruitS)
pdf(file = "EFjune15OffMortalityS.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:26, data = ab.june15Off, choice = 2, pc = c(line = 99), lt.ld = "LC",
        range.strategy = "page", byrow = TRUE, lt.rnd = 2)
dev.off()

### Multiple mortality lines grouped by duration and temperature
 group.responses()  # >> Multimort_byDuration.pdf
 group.responses2() # >> Multimort_byTemperature.pdf
### CT product on X-axis
 group.responsesCT() # >> Multimort_byTemperatureCT.pdf

## presentation plots
group.responsesD1(want = 2, post = TRUE) ## > Lines_forLBAM_5__2h.pdf  
system("pdf2png Lines_forLBAM_5__2h")
group.responsesD1(want = 5, post = TRUE) ## > Lines_forLBAM_ME_2h.pdf  
system("pdf2png Lines_forLBAM_ME_2h")

group.responsesT1(want = 1, post = TRUE)  ## > Lines_forLBAM_5__5C.pdf  
group.responsesT1(want = 3, post = TRUE)  ## > Lines_forLBAM_5__15C.pdf 
group.responsesT1(want = 4, post = TRUE)  ## > Lines_forLBAM_ME_5C.pdf  
group.responsesT1(want = 6, post = TRUE)  ## > Lines_forLBAM_ME_15C.pdf 
system("pdf2png Lines_forLBAM_5__5C")    ## > Lines_forLBAM_5__5C.png  
system("pdf2png Lines_forLBAM_5__15C")   ## > Lines_forLBAM_5__15C.png 
system("pdf2png Lines_forLBAM_ME_5C")    ## > Lines_forLBAM_ME_5C.png  
system("pdf2png Lines_forLBAM_ME_15C")   ## > Lines_forLBAM_ME_15C.png

## CTs
group.responsesCT1(want = 1, post = TRUE)  ## > Lines_forLBAM_5__5CT.pdf  
group.responsesCT1(want = 3, post = TRUE)  ## > Lines_forLBAM_5__15CT.pdf 
group.responsesCT1(want = 4, post = TRUE)  ## > Lines_forLBAM_ME_5CT.pdf  
group.responsesCT1(want = 6, post = TRUE)  ## > Lines_forLBAM_ME_15CT.pdf 
system("pdf2png Lines_forLBAM_5__5CT")    ## > Lines_forLBAM_5__5CT.png  
system("pdf2png Lines_forLBAM_5__15CT")   ## > Lines_forLBAM_5__15CT.png 
system("pdf2png Lines_forLBAM_ME_5CT")    ## > Lines_forLBAM_ME_5CT.png  
system("pdf2png Lines_forLBAM_ME_15CT")   ## > Lines_forLBAM_ME_15CT.png


group.responsesD2h(store = 5)   ## > Lines_for5C_2h.pdf
group.responsesD2h(store = 15)  ## > Lines_for15C_2h.pdf
system("pdf2png Lines_for5C_2h")   ## > Lines_for5C_2h.png
system("pdf2png Lines_for15C_2h")   ## > Lines_for15C_2h.png

## What coefficient works?
ct.checkM() # > LBAM_CT_coeff.pdf


#############################################################
##
## 14/07/2015 
##
######################################################

july15Off.df <- read.delim("All_PNZ_EFoffJuly2015.txt")
## ditch strange X column
july15Off.df <- july15Off.df[, jettison("X", names(july15Off.df))]
ab.july15Off <- list()
ab.july15Off[[1]] <- allfit(data = gleanOffFruitJuly)
pdf(file = "EFjuly15OffMortality.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:123, data = ab.july15Off, choice = 1, pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()


## combine reps
ab.july15Off$Joined <- allfit(data = gleanOffFruitJulyS)
pdf(file = "EFjuly15OffMortalityS.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:39, data = ab.july15Off, choice = 2, pc = c(line = 99), lt.ld = "LC",
        range.strategy = "page", byrow = TRUE, lt.rnd = 2)
dev.off()

### Multiple mortality lines grouped by duration and temperature
## overwrite June files
 group.responses(july15Off.df, ab.list = ab.july15Off)  # >> Multimort_byDuration.pdf
 group.responses2(july15Off.df, ab.list = ab.july15Off) # >> Multimort_byTemperature.pdf

## presentation plots
group.responsesD1(july15Off.df, ab.list = ab.july15Off, want = 2) ## > Lines_forLBAM_5__2h.pdf  
system("pdf2png Lines_forLBAM_5__2h")
group.responsesD1(july15Off.df, ab.list = ab.july15Off, want = 5) ## > Lines_forLBAM_ME_2h.pdf  
system("pdf2png Lines_forLBAM_ME_2h")

group.responsesT1(july15Off.df, ab.list = ab.july15Off, want = 1)  ## > Lines_forLBAM_5__5C.pdf  
group.responsesT1(july15Off.df, ab.list = ab.july15Off, want = 3)  ## > Lines_forLBAM_5__15C.pdf 
## group.responsesT1(july15Off.df, ab.list = ab.july15Off, want = 4)  ## > Lines_forLBAM_ME_5C.pdf  
## group.responsesT1(july15Off.df, ab.list = ab.july15Off, want = 6)  ## > Lines_forLBAM_ME_15C.pdf 
system("pdf2png Lines_forLBAM_5__5C")    ## > Lines_forLBAM_5__5C.png  
system("pdf2png Lines_forLBAM_5__15C")   ## > Lines_forLBAM_5__15C.png 
## system("pdf2png Lines_forLBAM_ME_5C")    ## > Lines_forLBAM_ME_5C.png  
## system("pdf2png Lines_forLBAM_ME_15C")   ## > Lines_forLBAM_ME_15C.png

## CTs

group.responsesCT1(july15Off.df, want = 1, ab.list = ab.july15Off)  ## > Lines_forLBAM_5__5CT.pdf  
group.responsesCT1(july15Off.df, want = 3, ab.list = ab.july15Off)  ## > Lines_forLBAM_5__15CT.pdf 
system("pdf2png Lines_forLBAM_5__5CT")    ## > Lines_forLBAM_5__5CT.png  
system("pdf2png Lines_forLBAM_5__15CT")   ## > Lines_forLBAM_5__15CT.png 

## Lots of lines
group.responsesD2h(july15Off.df, ab.list = ab.july15Off, store = 5)   ## > Lines_for5C_2h.pdf
group.responsesD2h(july15Off.df, ab.list = ab.july15Off, store = 15)  ## > Lines_for15C_2h.pdf
system("pdf2png Lines_for5C_2h")   ## > Lines_for5C_2h.png
system("pdf2png Lines_for15C_2h")   ## > Lines_for15C_2h.png

## What coefficient works?
ct.checkM() # > LBAM_CT_coeff.pdf


#############################################################
##
## 03/09/2015
## On fruit (and in box, etc)
##
######################################################
##
## Lots of messing here.  Go down to line 439 where it's much tidier

sept15Off.df <- read.delim("EFOffSept2015.txt")
ab.sept15Off <- list()
ab.sept15Off[[1]] <- allfit(data = gleanOffFruitSept) # small mistake
## rerun with correction
ab.sept15Off[[2]] <- allfit(data = gleanOffFruitSept) # redone
## However made no difference
aa <- cbind(ab.sept15Off[[1]]$lt[,3], ab.sept15Off[[2]]$lt[,3])
apply(aa, 1, diff) # all zero or NA
pdf(file = "EFsept15OffMortality.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:173, data = ab.sept15Off, choice = 1, pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()


mean.lt(ab.sept15Off, 1, leg.beg = 0, leg.end= 2, rnd = 2, border = TRUE,
        insect = "everything") # 
mean.lt(ab.sept15Off, 1, leg.beg = 0, leg.end= 2, rnd = 2, 
        insect = "everything", xlout = "Sept2015CIs.xls") # 
sepCI.df <- mean.lt(ab.sept15Off, 1, leg.beg = 0, leg.end= 2, rnd = 2, df.out = TRUE) # 

## make pest, duration and temperature columns

sepCI.df$Pest <- getbit(rownames(sepCI.df), "\\|", 1)
sepCI.df$Temperature <- getbit(rownames(sepCI.df), "\\|", 2)
sepCI.df$Temperature <- as.numeric(gsub("[A-z]", "", sepCI.df$Temperature))
sepCI.df$Duration <- getbit(rownames(sepCI.df), "\\|", 3)
sepCI.df$Duration <- as.numeric(gsub("[A-z]", "", sepCI.df$Duration)) # maybe not necessary

sepCIlbamOFF.df <- sepCI.df[grep("LBAM", sepCI.df$Pest), ]

## LCT for Off fruit lot
ab.sept15Off[["CT"]] <- allfit(data = gleanOffFruitSeptCT) #
pdf(file = "EFsept15OffMortalityCT.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:173, data = ab.sept15Off, choice = "CT", pc = c(line = 99), lt.ld = "LCT",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()
sepCI.df <- mean.lt(ab.sept15Off, 1, leg.beg = 0, leg.end= 2, rnd = 2, df.out = TRUE) # 

## make pest, duration and temperature columns

sepCI.df$Pest <- getbit(rownames(sepCI.df), "\\|", 1)
sepCI.df$Temperature <- getbit(rownames(sepCI.df), "\\|", 2)
sepCI.df$Temperature <- as.numeric(gsub("[A-z]", "", sepCI.df$Temperature))
sepCI.df$Duration <- getbit(rownames(sepCI.df), "\\|", 3)
sepCI.df$Duration <- as.numeric(gsub("[A-z]", "", sepCI.df$Duration)) # maybe not necessary

sepCIlbamOFF.df <- sepCI.df[grep("LBAM", sepCI.df$Pest), ]

## LCT for Off fruit lot
ab.sept15Off[["CT"]] <- allfit(data = gleanOffFruitSeptCT) #
pdf(file = "EFsept15OffMortalityCT.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:173, data = ab.sept15Off, choice = "CT", pc = c(line = 99), lt.ld = "LCT",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()
sepCI_CT.df <- mean.lt(ab.sept15Off, "CT", leg.beg = 0, leg.end= 2, rnd = 2, df.out = TRUE) # 
sepCIlbamOFF_CT.df <- sepCI_CT.df[grep("LBAM", rownames(sepCI_CT.df)), ]


#########
## with fruit
sept15With.df <- read.delim("EFwithSept2015.txt")
## need to fix up inconsistencies with CM
with(sept15With.df, table(SLS)) # 6 different CM to describe Egg and 5*
sept15With.df <- within(sept15With.df, SLS <- as.character(SLS))
sept15With.df <- within(sept15With.df, SLS[SLS == "CM 5"] <- "CM5")
sept15With.df <- within(sept15With.df, SLS[SLS == "Cm 5"] <- "CM5")
sept15With.df <- within(sept15With.df, SLS[SLS == "CM Eggs"] <- "CM Egg")
sept15With.df <- within(sept15With.df, SLS[SLS == "CM eggs"] <- "CM Egg")
sept15With.df <- within(sept15With.df, SLS <- as.factor(SLS))

ab.sept15With <- list()
ab.sept15With[[1]] <- allfit(data = gleanWithFruitSept)


### won't work: too many inconsistencies

## Try LBAM only
table(sept15With.df$Pest) # >> avoid missing the spaces
septLBAMwith.df <- sept15With.df[with(sept15With.df, grep("LBAM", Pest)),]
septLBAMwith.df <- within(septLBAMwith.df, Date <-
                          as.Date(as.character(Date), format = "%d/%m/%Y"))

septLBAMwithFIX.df <- fix.septLBAMwith(septLBAMwith.df)
ab.sept15With[["lbam"]] <- allfit(data = gleanWithFruitSeptLBAM)
ab.sept15With$lbam_CT <- allfit(data = gleanWithFruitSept_CT)

pdf(file = "EFsept15WithMortalityLBAM.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:25, data = ab.sept15With, choice = "lbam", pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:25, data = ab.sept15With, choice = "lbam_CT", pc = c(line = 99), lt.ld = "LCT",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()

mean.lt(ab.sept15With, "lbam", leg.beg = 0, leg.end= 2, rnd = 2,
        border = TRUE, insect = "LBAM", omit = c(10), lt.ld = "LC") 

septLBAMwithCI.df <-
  mean.lt(ab.sept15With, "lbam", leg.beg = 0, leg.end= 2, rnd = 2,
          df.out = TRUE, insect = "LBAM", omit = c(10, 18:25), lt.ld = "LC")
  ## now the CT lot
septLBAMwithCI_CT.df <-
  mean.lt(ab.sept15With, "lbam_CT", leg.beg = 0, leg.end= 2, rnd = 2,
          df.out = TRUE, insect = "LBAM", omit = c(10, 18:25), lt.ld = "LCT")

### First consistent 100% points
septLBAMwithFirst100.df <- get100mort(septLBAMwithFIX.df)
ab.sept15With100 <- list()
ab.sept15With100$lbam <- df2ablist(septLBAMwithFirst100.df)

mean.lt(ab.sept15With100, "lbam", leg.beg = 0, leg.end= 2, rnd = 2, lt = 100,
        border = TRUE, insect = "LBAM", lt.ld = "LC")

septLBAMwithCI100.df <-
  mean.lt(ab.sept15With100, "lbam", leg.beg = 0, leg.end= 2, rnd = 2,
          df.out = TRUE, lt = 100, insect = "LBAM", lt.ld = "LC")

## CT version
septLBAMwithFirst100CT.df <- get100mortCT(septLBAMwithFIX.df)
ab.sept15With100$lbam_CT<- df2ablist(septLBAMwithFirst100CT.df)

mean.lt(ab.sept15With100, "lbam_CT", leg.beg = 0, leg.end= 2, rnd = 2, lt = 100,
        border = TRUE, insect = "LBAM", lt.ld = "LCT")

septLBAMwithCI100_CT.df <-
  mean.lt(ab.sept15With100, "lbam_CT", leg.beg = 0, leg.end= 2, rnd = 2,
          df.out = TRUE, lt = 100, insect = "LBAM", lt.ld = "LCT")


## off fruit again

sepCIlbamOFF.df <- sepCI.df[grep("LBAM", sepCI.df$Pest), ]# no further calculation
septLBAM15Off.df <- sept15Off.df[sept15Off.df$Pest == "LBAM",]


septLBAMOffFirst100.df <- get100mortOff(xxx = gleanOffFruitSept, max.control = TRUE)
ab.sept15Off100 <- list()
ab.sept15Off100$lbam <- df2ablist(septLBAMOffFirst100.df)

septLBAMOffFirst100CI.df <- # not many 100%
  mean.lt(ab.sept15Off100, "lbam", leg.beg = 0, leg.end= 2, rnd = 2, lt = 100,
          df.out = TRUE, insect = "LBAM", lt.ld = "LC")

## CT



## CT
septLBAMoffFirst100_CT.df <- conc2ct(septLBAMOffFirst100.df)
septLBAMoffFirst100_CT.df
######################################################################
## Don't use from line 284 above: inconsistent names and impossible to follow. 
###############################################################################################

## Much tidier
sept15Off.df <- read.delim("EFOffSept2015.txt") ## i.e. off fruit
## 
sept15With.df <- read.delim("EFwithSept2015.txt") # in containers of fruit
sept15LBAMoff.df <- sept15Off.df[with(sept15Off.df, Pest == "LBAM"), ]
sept15LBAMwith.df <- sept15With.df[with(sept15With.df, grep("LBAM", Pest)),]
sept15LBAMwith.df <- within(sept15LBAMwith.df, Date <-
                            as.Date(as.character(Date), format = "%d/%m/%Y"))
## Lots of data entry tinkering required
sept15LBAMwithFIX.df <- fix.septLBAMwith(sept15LBAMwith.df)

ab.sept15With <- ab.sept15Off <- list() # 
ab.sept15Off[["conc"]] <- allfit(data = gleanOffFruitSeptLBAM)
ab.sept15Off[["CT"]] <- allfit(data = gleanOffFruitSeptLBAM_CT)
ab.sept15With[["conc"]] <- allfit(data = gleanWithFruitSeptLBAM)
ab.sept15With[["CT"]] <- allfit(data = gleanWithFruitSeptLBAM_CT)

pdf(file = "EFsept15OffMortalityLBAM.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:75, data = ab.sept15Off, choice = "conc", pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:75, data = ab.sept15Off, choice = "CT", pc = c(line = 99), lt.ld = "LCT",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()

pdf(file = "EFsept15WithMortalityLBAM.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:25, data = ab.sept15With, choice = "conc", pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:25, data = ab.sept15With, choice = "CT", pc = c(line = 99), lt.ld = "LCT",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()


## First consistent 100% points 
ab.sept15With100 <- ab.sept15Off100 <- list()
ab.sept15Off100$conc <- df2ablist(get100mort(gleanOffFruitSeptLBAM))
ab.sept15Off100$CT <- df2ablist(get100mort(gleanOffFruitSeptLBAM_CT))
ab.sept15With100$conc <- df2ablist(get100mort(gleanWithFruitSeptLBAM))
ab.sept15With100$CT <- df2ablist(get100mort(gleanWithFruitSeptLBAM_CT))

### dataframes of CIs
##  LC and LCT
septLBAMoffCI.df <-
  mean.lt(ab.sept15Off, "conc", leg.beg = 0, leg.end= 2, rnd = 2,
          df.out = TRUE, insect = "LBAM", omit = c(11, 26), lt.ld = "LC")
septLBAMoffCI_CT.df <-
  mean.lt(ab.sept15Off, "CT", leg.beg = 0, leg.end= 2, rnd = 2,
          df.out = TRUE, insect = "LBAM", omit = c(11, 26), lt.ld = "LCT")

septLBAMwithCI.df <-
  mean.lt(ab.sept15With, "conc", leg.beg = 0, leg.end= 2, rnd = 2,
          df.out = TRUE, insect = "LBAM", omit = c(10, 18:25), lt.ld = "LC")
septLBAMwithCI_CT.df <-
  mean.lt(ab.sept15With, "CT", leg.beg = 0, leg.end= 2, rnd = 2,
          df.out = TRUE, insect = "LBAM", omit = c(10, 18:25), lt.ld = "LCT")

## Lowest consistent 100% mortality points
septLBAMoff100CI.df <-
  mean.lt(ab.sept15Off100, "conc", leg.beg = 0, leg.end= 2, rnd = 2, lt = 100,
          df.out = TRUE, insect = "LBAM", lt.ld = "LC")
septLBAMoff100CI_CT.df <-
  mean.lt(ab.sept15Off100, "CT", leg.beg = 0, leg.end= 2, rnd = 2, lt = 100,
          df.out = TRUE, insect = "LBAM", lt.ld = "LCT")

septLBAMwith100CI.df <-
  mean.lt(ab.sept15With100, "conc", leg.beg = 0, leg.end= 2, rnd = 2, lt = 100,
          df.out = TRUE, insect = "LBAM", lt.ld = "LC")
septLBAMwith100CI_CT.df <-
  mean.lt(ab.sept15With100, "CT", leg.beg = 0, leg.end= 2, rnd = 2, lt = 100,
          df.out = TRUE, insect = "LBAM", lt.ld = "LCT")

## Collect LCs and LCTs for LBAM

collectLCsLBAM() # >> PredictionLBAM_Tables_EF.xls

### Get ablist for combined reps to get predictions at various concentrations

ab.sept15Off$Joined <- ab.sept15Off$Joined <- allfit(data = gleanOffFruitSeptLBAM_J)
ab.sept15With$Joined <- ab.sept15With$Joined <- allfit(data = gleanWithFruitSeptLBAM_J)
## Draw corresponding plots
pdf(file = "LBAMjoinedEFmortality.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:25, data = ab.sept15Off, choice = "Joined", pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:25, data = ab.sept15With, choice = "Joined", pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()

## "Predicted" and "Actual" mortalities at 2 and 3% EF
collectLCsLBAM2() # >> PredictionLBAM_WithFruit_EF.xls
## Using 2&3 as target concentrations and finding what conc eventuated
##  to model the expected mortality
collectLCsLBAM4() # >> PredictionLBAM_WithFruit_EF4.xls

###################################################################################

## Semi-commercial environment
semicom.df <- read.delim("EFsemicSept2015.txt")
semicomLBAM.df <- semicom.df[with(semicom.df, Pest == "LBAM"),]
sept15LBAMsemic.df <- sieve(semicomLBAM.df) # >> PredictionLBAM_SemiCommercial_EF4.xls

###################################################################################
###################################################################################

##### LBAM all works: try the rest.

### off fruit already done above: repeated here

sept15Off.df <- read.delim("EFOffSept2015.txt")
ab.sept15OffAll <- list()
ab.sept15OffAll[["conc"]] <- allfit(data = gleanOffFruitSept) 
ab.sept15OffAll[["CT"]] <- allfit(data = gleanOffFruitSept_CT) 
ab.sept15OffAll[["concJ"]] <- allfit(data = gleanOffFruitSept_J) 
ab.sept15OffAll[["CTJ"]] <- allfit(data = gleanOffFruitSept_CTJ) 

pdf(file = "EFsept15OffMortality.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:173, data = ab.sept15OffAll, choice = "conc", pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:173, data = ab.sept15OffAll, choice = "CT", pc = c(line = 99), lt.ld = "LCT",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:73, data = ab.sept15OffAll, choice = "concJ", pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:73, data = ab.sept15OffAll, choice = "CTJ", pc = c(line = 99), lt.ld = "LCT",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off()

## confidence intervals for All off fruit
mean.lt(ab.sept15OffAll, "conc", leg.beg = 0, leg.end= 2, rnd = 2, border = TRUE,
        insect = "PNZ pests", lt.ld = "LC") # 
mean.lt(ab.sept15Off, 1, leg.beg = 0, leg.end= 2, rnd = 2, 
        insect = "everything", xlout = "Sept2015CIs.xls") # 
sepOffCI.df <- mean.lt(ab.sept15OffAll, "conc", leg.beg = 0, leg.end= 2, rnd = 2, df.out = TRUE,
                       omit = c(1:15, 25:38, 52:64, 81:82, 101:103, 121, 145,156:161)) # 
sepOffCI_CT.df <- mean.lt(ab.sept15OffAll, "CT", leg.beg = 0, leg.end= 2, rnd = 2, df.out = TRUE,
                       omit = c(1:15, 25:38, 52:64, 81:82, 101:103, 121, 145,156:161)) # 

## First consistent 100% points 
ab.sept15With100 <- ab.sept15Off100 <- list()
ab.sept15Off100$conc <- df2ablist(get100mortAll(gleanOffFruitSept))
ab.sept15Off100$CT <- df2ablist(get100mortAll(gleanOffFruitSept_CT))

sepOff100CI.df <- mean.lt(ab.sept15Off100, "conc", leg.beg = 0, lt = 100,
                          omit = c(1:15, 25:38, 52:64, 81:82, 101:103, 121, 145,156:161),
                          leg.end= 2, rnd = 2, df.out = TRUE) # 
sepOff100CI_CT.df <- mean.lt(ab.sept15Off100, "CT", leg.beg = 0, lt = 100,
                            omit = c(1:15, 25:38, 52:64, 81:82, 101:103, 121, 145,156:161),
                           leg.end= 2, rnd = 2, df.out = TRUE) # 

######################################################################################

### With fruit 

septwithFIXall.df <- fix.septWith(xx = sept15With.df) # lots of fixes
ab.sept15WithAll <- list()
ab.sept15WithAll[["conc"]] <- allfit(data = gleanWithFruitSeptAll)
ab.sept15WithAll$CT <- allfit(data = gleanWithFruitSeptAll_CT)
ab.sept15WithAll[["concJ"]] <- allfit(data = gleanWithFruitSeptAll_J)
ab.sept15WithAll$CTJ <- allfit(data = gleanWithFruitSeptAll_CTJ)

pdf(file = "EFsept15WithMortality.pdf", width = 255/25.4, height = 195/25.4)
flyplot(1:43, data = ab.sept15WithAll, choice = "conc", pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:43, data = ab.sept15WithAll, choice = "CT", pc = c(line = 99), lt.ld = "LCT",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:15, data = ab.sept15WithAll, choice = "concJ", pc = c(line = 99), lt.ld = "LC",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
flyplot(1:15, data = ab.sept15WithAll, choice = "CTJ", pc = c(line = 99), lt.ld = "LCT",
        range.strategy = "individual", byrow = TRUE, lt.rnd = 2)
dev.off() # CT lot in same file 

sepWithCI.df <- mean.lt(ab.sept15WithAll, "conc", leg.beg = 0, leg.end= 2, rnd = 2,
                        omit = c(22, 30:43), df.out = TRUE) # 
sepWithCI_CT.df <- mean.lt(ab.sept15WithAll, "CT", leg.beg = 0, leg.end= 2, rnd = 2,
                        omit = c(22, 30:43), df.out = TRUE) # 

## First consistent 100% points 
ab.sept15With100$conc <- df2ablist(get100mortAll(gleanWithFruitSeptAll))
ab.sept15With100$CT <- df2ablist(get100mortAll(gleanWithFruitSeptAll_CT))


sepWith100CI.df <- mean.lt(ab.sept15With100, "conc", leg.beg = 0, lt = 100,
                          omit = c(22, 30:43), leg.end= 2, rnd = 2, df.out = TRUE) # 
sepWith100CI_CT.df <- mean.lt(ab.sept15With100, "CT", leg.beg = 0, lt = 100,
                             omit = c(22, 30:43), leg.end= 2, rnd = 2, df.out = TRUE) #


collectLCs() ## >> PredictionAll_With.OffFruit_EF.xls

collectLCs(adjust.cont = TRUE) ## >> Predictions_With.OffFruit_EF_controlAdjust.xls
collectLCs(adjust.cont = FALSE) ## >> Predictions_With.OffFruit_EF.xls

##############################################
##
## Github repository (xterm command line)
##
##########################################

git config --global user.name "Tuxkid"
git config --global core.editor "emacs"

git init
git add .
git commit
git remote add origin https://github.com/Tuxkid/PNZ_EF.git 
git push -u origin master




