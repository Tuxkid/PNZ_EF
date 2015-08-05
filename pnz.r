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

##############################################
##
## Github repository
##
##########################################

git config --global user.name "Tuxkid"
git config --global core.editor "emacs"

git init
git add .
git commit
git remote add origin https://github.com/Tuxkid/PNZ_EF.git 
git push -u origin master




