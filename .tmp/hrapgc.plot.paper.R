plot.paper <- function(titl = FALSE, span = .8 )
{
### Purpose:- PNG files for Plant Protection paper
### ----------------------------------------------------------------------
### Modified from:- plot.reportA (change to monochrome)
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 29 Apr 2014, 09:05
### ----------------------------------------------------------------------
### Revisions:- 

  ax.cex <- .8
  lab.cex <- 1.1
  cex.pt <- .9
  leg.cex <- 0.8
  ytix <- c(0, 1, 10, 20, 50, 80, 90, 99, 100)
  ytix.at <- ang(ytix/100)
  require(RColorBrewer)
  PCHS <- (0:5)[-4] # pick from these below
  LTYS <- c("solid", "22", "2282", "42", "63") # pick from these below
 ### Use all/most of above in following plots

  rep.df <- within(rep.df, Efppm[is.na(Efppm)] <- 0)
  rep.df <- rep.df[, -9]


  alcm.df <- rep.df[rep.df$Species == "Apple leaf curling midge",]
  alcm.df$Lifestage = "diap"
  alcm.df <- df.sort(alcm.df, c("Efppm", "Rep", "Lifestage"))
  alcm.df <- within(alcm.df, Live <- Live + Moribund)
  latania.df <- rep.df[rep.df$Species == "Latania scale",]
  latania.df <- df.sort(latania.df, c("Efppm", "Rep", "Lifestage"))
  latania.df <- within(latania.df, Dead <- Dead + Moribund)
  mb.df <- rep.df[rep.df$Species == "Obscure mealybugs",]
  mb.df <- within(mb.df, Live <- Live + Moribund)
  mb.df <- df.sort(mb.df, c("Efppm", "Rep", "Lifestage"))
  thrip.df <- rep.df[rep.df$Species == "Onion thrips",]
  thrip.df <- df.sort(thrip.df, c("Efppm", "Rep", "Lifestage"))
  thrip.df <- within(thrip.df, Live <- Live + Moribund)
  ## add into one dataframe
  use.df <- rbind(alcm.df[, names(mb.df)], latania.df[, names(mb.df)],
                  mb.df , thrip.df[, names(mb.df)])
  ## shorter names:
  use.df$Species <-
    lookup(as.character(use.df$Species),
           c("Apple leaf curling midge","Onion thrips", "Latania scale", 
             "Obscure mealybugs"),
           c("ALCM", "thrips", "LS", "OMB"))
## Omit rows with "male" in Species column
  biff.males <- grep(" male", use.df$Lifestage)
  use.df <- use.df[ -biff.males,]
  use.df <- within(use.df, Mort <- Dead/Total)
  use.df <- within(use.df, AngMort <- ang(Mort))
  use.df$Efppm <- use.df$Efppm/10000
  sum.df <- aggregate(AngMort ~ Efppm + Lifestage + Species, data = use.df, mean)
  ## separate out into species again:
  alcm.df <- sum.df[sum.df$Species == "ALCM",]


  alcm.df <- rep.df[rep.df$Species == "Apple leaf curling midge",]
  alcm.df$Lifestage = "diap"
  alcm.df <- df.sort(alcm.df, c("Efppm", "Rep", "Lifestage"))
  alcmMD.df <- within(alcm.df, Dead <- Dead + Moribund) #
  alcmMD.df$Species <- "ALCM_md"
  alcmML.df <- within(alcm.df, Live <- Live + Moribund)
  latania.df <- rep.df[rep.df$Species == "Latania scale",]
  ##   latania.df$Species <- "LS"
  latania.df <- df.sort(latania.df, c("Efppm", "Rep", "Lifestage"))
  latania.df <- within(latania.df, Dead <- Dead + Moribund)

  mb.df <- rep.df[rep.df$Species == "Obscure mealybugs",]
  ##  
  ## mb.df$Lifestage <- "all" ## add lifestages
  ##   mb.sum <- unique(mb.df[, 1:6])
  ##   mb.sum$Live <- aggregate(Live~ Efppm + Lifestage + Rep, data = mb.df, sum)$Live
  ##   mb.sum$Dead <- aggregate(Dead~ Efppm+ Lifestage + Rep, data = mb.df, sum)$Dead
  ##   mb.sum$Total <-aggregate(Total~ Efppm+ Lifestage + Rep, data = mb.df, sum)$Total
  ##   mb.sum$Moribund <-aggregate(Moribund~ Efppm+ Lifestage + Rep, data = mb.df, sum)$Moribund
  mb.df <- within(mb.df, Live <- Live + Moribund)
  mb.df <- df.sort(mb.df, c("Efppm", "Rep", "Lifestage"))
  ##  names(mb.sum) <- names(mb.df)

  thrip.df <- rep.df[rep.df$Species == "Onion thrips",]
  thrip.df <- df.sort(thrip.df, c("Efppm", "Rep", "Lifestage"))
  thrip.df <- within(thrip.df, Live <- Live + Moribund)

  ## add into one dataframe
  use.df <- rbind(alcm.df[, names(mb.df)], alcmMD.df[, names(mb.df)],
                  latania.df[, names(mb.df)],
                  mb.df , thrip.df[, names(mb.df)])

  
  ## shorter names:
  use.df$Species <-
    lookup(as.character(use.df$Species),
           c("Apple leaf curling midge", "ALCM_md",
             "Onion thrips", "Latania scale", 
             "Obscure mealybugs"),
           c("ALCM_ml", "ALCM_md","thrips", "LS", "OMB"))
  duds <- grep(" male", use.df$Lifestage)
### ditch those with 'male' in lifestage (too few individuals)
  use.df <- use.df[-duds,]  
  use.df <- within(use.df, Mort <- Dead/Total)
  use.df <- within(use.df, AngMort <- ang(Mort))
  use.df$Efppm <- use.df$Efppm/10000
  sum.df <- aggregate(AngMort ~ Efppm + Lifestage + Species, data = use.df, mean)
  ## separate out into species again:
  alcmML.df <- sum.df[sum.df$Species == "ALCM_ml",]
  alcmMD.df <- sum.df[sum.df$Species == "ALCM_md",]  
  thrip.df <- sum.df[sum.df$Species == "thrips",]
  latania.df <- sum.df[sum.df$Species == "LS",]
  omb.df <- sum.df[sum.df$Species == "OMB",]
### Individual plotting functions (non-generic but close)
##  colours <- COLOURS
  ltys <- LTYS
  pchs <- PCHS
  span <- span
### ALCM
  alcm.df <- alcmML.df # then use same code with alcmMD.df
  alcm.lo <- with(alcm.df, loess(AngMort ~ Efppm, degree = 1, span = span))
  alcm.lo.pred <- predict(alcm.lo, unique(alcm.df$Efppm), se = TRUE)
  alcm.lo.pred$fit[alcm.lo.pred$fit > 90] <- 90
  sems <- NULL # silly here but works when there's more than one line
  sems <- c(sems, rms(alcm.lo.pred$se.fit)) # make vector of  sems
  alcm.lo.pred$fit[alcm.lo.pred$fit > 90] <- 90
  spline.i <- spline(unique(alcm.df$Efppm), alcm.lo.pred$fit, n = 200)
  spline.i$y[spline.i$y > 90] <-  90
   spline.i$y <- monotone(spline.i$y)# take out some more bends sometimes
   
### plotting begins
  bitmap("ALMCml_paper.png", type = "png16m", width = 155, height = 143,
         res = 600, units = "mm", pointsize = 11)
  with(alcm.df, blank.plot(Efppm, AngMort, ylim = c(0, 90)))
  axis(1, las = 1, mgp = c(1.8, .8, 0), cex.axis = .8)
  axis(2, las = 1, at = ytix.at, labels = ytix,
       mgp = c(1.8, .8, 0), cex.axis = ax.cex)
  box()
  mtext("Ethyl formate concentration (%)", side = 1, line = 1.9, cex = lab.cex)
  mtext("Mortality (%)", side = 2, line = 1.9, cex = lab.cex)
  with(spline.i, lines(x, y, lwd = 1.5, lty = ltys[1]))
  with(alcm.df, points(Efppm, AngMort, pch = pchs[1], #lty = df.i$lt[1],
                       lty = ltys[1], cex = cex.pt))
  if(titl)
    title("ALCM")
  bar.legs(.6, .8, ebv = sems, ltys = ltys,
           labs = "diapausing", pchs = pchs[1],
           point.cex = cex.pt, leg.cex = leg.cex, #ltys = ltys[1],
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)
  dev.off() # ploting of this one ends
  
### ALCM -- moribund = dead
  alcm.df <-  alcmMD.df
  alcm.lo <- with(alcm.df, loess(AngMort ~ Efppm, degree = 1, span = span))
  alcm.lo.pred <- predict(alcm.lo, unique(alcm.df$Efppm), se = TRUE)
  alcm.lo.pred$fit[alcm.lo.pred$fit > 90] <- 90
  sems <- NULL # silly here but works when there's more than one line
  sems <- c(sems, rms(alcm.lo.pred$se.fit)) # make vector of  sems
  alcm.lo.pred$fit[alcm.lo.pred$fit > 90] <- 90
  spline.i <- spline(unique(alcm.df$Efppm), alcm.lo.pred$fit, n = 200)
  spline.i$y[spline.i$y > 90] <-  90
   spline.i$y <- monotone(spline.i$y)# take out some more bends sometimes
   
### plotting begins
  bitmap("ALMCmd_paper.png", type = "png16m", width = 155, height = 143,
         res = 600, units = "mm", pointsize = 11)
  with(alcm.df, blank.plot(Efppm, AngMort, ylim = c(0, 90)))
  axis(1, las = 1, mgp = c(1.8, .8, 0), cex.axis = .8)
  axis(2, las = 1, at = ytix.at, labels = ytix,
       mgp = c(1.8, .8, 0), cex.axis = ax.cex)
  box()
  mtext("Ethyl formate concentration (%)", side = 1, line = 1.9, cex = lab.cex)
  mtext("Mortality (%)", side = 2, line = 1.9, cex = lab.cex)
  with(spline.i, lines(x, y, lwd = 1.5, lty = ltys[1]))
  with(alcm.df, points(Efppm, AngMort, pch = pchs[1], #lty = df.i$lt[1],
                       lty = ltys[1], cex = cex.pt))
  if(titl)
    title("ALCM_md")
  bar.legs(.6, .5, ebv = sems, #ltys = rep(1, 3)[1],
           labs = "diapausing", pchs = pchs[1],
           point.cex = cex.pt, leg.cex = leg.cex, ltys = ltys,
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)
  dev.off() # ploting of this one ends



### Latania scale
  sems <- NULL # add on sems as calculated
  latania.df$Lifestage <- gsub("1st instar", "1st instar whitecap",
                               latania.df$Lifestage)
  latania.df$Lifestage <- gsub("Crawler", "1st instar crawler",
                               latania.df$Lifestage)
  latania.df$Lifestage <- gsub("Mature female", "3rd instar mature",
                               latania.df$Lifestage)
  stages <- sort(unique(latania.df$Lifestage))
  ltys <- LTYS[seq(stages)] # code to work for other insects as well
  pchs <- PCHS[seq(stages)]
  spans <- rep(span, length(stages)) # probably tinker with this one
  names(pchs) <- names(ltys) <- names(spans) <- stages
  bitmap("latania_paper.png", type = "png16m", width = 155, height = 143,
         res = 600, units = "mm", pointsize = 11)
  for(i in stages){
    latania.df.i <- latania.df[latania.df$Lifestage == i,]
    latania.lo.i <- with(latania.df.i, loess(AngMort ~ Efppm,
                                             degree = 1, span = spans[i]))
    latania.lo.pred.i <- predict(latania.lo.i,
                                 unique(latania.df.i$Efppm), se = TRUE)
    latania.lo.pred.i$fit[latania.lo.pred.i$fit > 90] <- 90
    sems <- c(sems, rms(latania.lo.pred.i$se.fit)) # make vector of three sems
    latania.lo.pred.i$fit[latania.lo.pred.i$fit > 90] <- 90
    spline.i <- spline(unique(latania.df.i$Efppm), latania.lo.pred.i$fit, n = 200)
    spline.i$y[spline.i$y > 90] <-  90
    spline.i$y <- monotone(spline.i$y)# take out some more bends sometimes
### plotting begins
    if(i == stages[1]){
      with(latania.df.i, blank.plot(Efppm, AngMort, ylim = c(0, 90)))
      axis(1, las = 1, mgp = c(1.8, .8, 0), cex.axis = .8)
      axis(2, las = 1, at = ytix.at, labels = ytix,
           mgp = c(1.8, .8, 0), cex.axis = ax.cex)
      box()
      mtext("Ethyl formate concentration (%)", side = 1, line = 1.9, cex = lab.cex)
      mtext("Mortality (%)", side = 2, line = 1.9, cex = lab.cex)
    }
    with(spline.i, lines(x, y, lwd = 1.5, lty = ltys[i]))

    with(latania.df.i, points(Efppm, AngMort, pch = pchs[i], #lty = df.i$lt[1],
                              lty = ltys[i], cex = cex.pt))
  }
  if(titl)
    title("Latania scale")
  bar.legs(.6, .8, ebv = sems, #ltys = rep(1, length(sems)),
           labs = stages, pchs = pchs,
           point.cex = cex.pt, leg.cex = leg.cex, ltys = ltys,
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)
  dev.off()
### Mealybug
  sems <- NULL # add on sems as calculated
  stages <- unique(omb.df$Lifestage)
  ltys <- LTYS[seq(stages)] # code to work for other insects as well
  pchs <- PCHS[seq(stages)]
  spans <- rep(span+.1, length(stages)) # probably tinker with this one
  names(pchs) <- names(ltys) <- names(spans) <- stages
  bitmap("omb_paper.png", type = "png16m", width = 155, height = 143,
         res = 600, units = "mm", pointsize = 11)
  for(i in stages){
    omb.df.i <- omb.df[omb.df$Lifestage == i,]
    omb.lo.i <- with(omb.df.i, loess(AngMort ~ Efppm,
                                             degree = 1, span = spans[i]))
    omb.lo.pred.i <- predict(omb.lo.i,
                                 unique(omb.df.i$Efppm), se = TRUE)
    omb.lo.pred.i$fit[omb.lo.pred.i$fit > 90] <- 90
    sems <- c(sems, rms(omb.lo.pred.i$se.fit)) # make vector of three sems
    omb.lo.pred.i$fit[omb.lo.pred.i$fit > 90] <- 90
    spline.i <- spline(unique(omb.df.i$Efppm), omb.lo.pred.i$fit, n = 200)
    spline.i$y[spline.i$y > 90] <-  90
    spline.i$y <- monotone(spline.i$y)# take out some more bends sometimes
### plotting begins
    if(i == stages[1]){
      with(omb.df.i, blank.plot(Efppm, AngMort, ylim = c(0, 90)))
      axis(1, las = 1, mgp = c(1.8, .8, 0), cex.axis = .8)
      axis(2, las = 1, at = ytix.at, labels = ytix,
           mgp = c(1.8, .8, 0), cex.axis = ax.cex)
      box()
      mtext("Ethyl formate concentration (%)", side = 1, line = 1.9, cex = lab.cex)
      mtext("Mortality (%)", side = 2, line = 1.9, cex = lab.cex)
    }
    with(spline.i, lines(x, y, lwd = 1.5, lty = ltys[i]))

    with(omb.df.i, points(Efppm, AngMort, pch = pchs[i], #lty = df.i$lt[1],
                              lty = ltys[i], cex = cex.pt))
  }
  if(titl)
    title("OMB")
  bar.legs(.6, .7, ebv = sems, #ltys = rep(1, length(sems)),
           labs = stages, pchs = pchs,
           point.cex = cex.pt, leg.cex = leg.cex, ltys = ltys,
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)
  dev.off()
### Onion thrips
  sems <- NULL # add on sems as calculated
  stages <- unique(thrip.df$Lifestage)
  ltys <- LTYS[seq(stages)] # code to work for other insects as well
  pchs <- PCHS[seq(stages)]
  spans <- rep(span, length(stages)) # probably tinker with this one
  names(pchs) <- names(ltys) <- names(spans) <- stages
  bitmap("thrips_paper.png", type = "png16m", width = 155, height = 143,
         res = 600, units = "mm", pointsize = 11)
  for(i in stages){
    thrip.df.i <- thrip.df[thrip.df$Lifestage == i,]
    thrip.lo.i <- with(thrip.df.i, loess(AngMort ~ Efppm,
                                             degree = 1, span = spans[i]))
    thrip.lo.pred.i <- predict(thrip.lo.i,
                                 unique(thrip.df.i$Efppm), se = TRUE)
    thrip.lo.pred.i$fit[thrip.lo.pred.i$fit > 90] <- 90
    sems <- c(sems, rms(thrip.lo.pred.i$se.fit)) # make vector of three sems
    thrip.lo.pred.i$fit[thrip.lo.pred.i$fit > 90] <- 90
    spline.i <- spline(unique(thrip.df.i$Efppm), thrip.lo.pred.i$fit, n = 200)
    spline.i$y[spline.i$y > 90] <-  90
    spline.i$y <- monotone(spline.i$y)# take out some more bends sometimes
### plotting begins
    if(i == stages[1]){
      with(thrip.df.i, blank.plot(Efppm, AngMort, ylim = c(0, 90)))
      axis(1, las = 1, mgp = c(1.8, .8, 0), cex.axis = .8)
      axis(2, las = 1, at = ytix.at, labels = ytix,
           mgp = c(1.8, .8, 0), cex.axis = ax.cex)
      box()
      mtext("Ethyl formate concentration (%)", side = 1, line = 1.9, cex = lab.cex)
      mtext("Mortality (%)", side = 2, line = 1.9, cex = lab.cex)
    }
    with(spline.i, lines(x, y, lwd = 1.5, lty = ltys[i]))
    with(thrip.df.i, points(Efppm, AngMort, pch = pchs[i], #lty = df.i$lt[1],
                            lty = ltys[i], cex = cex.pt))
  }
  if(titl)
    title("Thrips")
  bar.legs(.6, .7, ebv = sems, #ltys = rep(1, length(sems)),
           labs = stages, pchs = pchs,
           point.cex = cex.pt, leg.cex = leg.cex, ltys = ltys,
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)
  dev.off()
}
