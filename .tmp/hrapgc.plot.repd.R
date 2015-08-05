plot.repd <- function(titl = TRUE, span = .8, post = TRUE)
{
### Purpose:- plotting function for various critters in ethyl formate
### ----------------------------------------------------------------------
### Modified from:- glean.repd (the beginning)
### ----------------------------------------------------------------------
### Arguments:- titl: in debugging stage, titles added to check
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 12 Jun 2013, 14:22
### ----------------------------------------------------------------------
### Revisions:- 

  ax.cex <- .8
  lab.cex <- 1.1
  cex.pt <- .9
  leg.cex <- 0.8
  ytix <- c(0, 1, 10, 20, 50, 80, 90, 99, 100)
  ytix.at <- ang(ytix/100)
  require(RColorBrewer)
  COLOURS <- brewer.pal(5, "Dark2") # pick from these below
  PCHS <- (0:5)[-4] # pick from these below
 ### Use all/most of above in following plots
  if(post){
    pdf(file = "EthylFormateMortalityPNZ.pdf", width = 155/25.4, height = 285/25.4)
    on.exit(dev.off(), add = TRUE)
    par(mfrow = c(2, 1))
  }

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
  thrip.df <- sum.df[sum.df$Species == "thrips",]
  latania.df <- sum.df[sum.df$Species == "LS",]
  omb.df <- sum.df[sum.df$Species == "OMB",]
### Individual plotting functions (non-generic but close)
### ALCM
  colours <- COLOURS
  pchs <- PCHS
  span <- span
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
  with(alcm.df, blank.plot(Efppm, AngMort, ylim = c(0, 90)))
  axis(1, las = 1, mgp = c(1.8, .8, 0), cex.axis = .8)
  axis(2, las = 1, at = ytix.at, labels = ytix,
       mgp = c(1.8, .8, 0), cex.axis = ax.cex)
  box()
  mtext("Ethyl formate concentration (%)", side = 1, line = 1.9, cex = lab.cex)
  mtext("Mortality (%)", side = 2, line = 1.9, cex = lab.cex)
  with(spline.i, lines(x, y, lty = 1, lwd = 1.5, col = colours[1]))
  with(alcm.df, points(Efppm, AngMort, pch = pchs[1], #lty = df.i$lt[1],
                       col = colours[1], cex = cex.pt))
if(titl)
  title("ALCM")
  bar.legs(.6, .8, ebv = sems, ltys = rep(1, 3)[1],
           labs = "diapausing", pchs = pchs[1],
           point.cex = cex.pt, leg.cex = leg.cex, cols = colours[1],
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)

### Latania scale
  sems <- NULL # add on sems as calculated
  latania.df$Lifestage <- gsub("1st instar", "1st instar whitecap",
                               latania.df$Lifestage)
  latania.df$Lifestage <- gsub("Crawler", "1st instar crawler",
                               latania.df$Lifestage)
  latania.df$Lifestage <- gsub("Mature female", "3rd instar mature",
                               latania.df$Lifestage)
  stages <- sort(unique(latania.df$Lifestage))
  colours <- COLOURS[seq(stages)] # code to work for other insects as well
  pchs <- PCHS[seq(stages)]
  spans <- rep(span, length(stages)) # probably tinker with this one
  names(pchs) <- names(colours) <- names(spans) <- stages
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
    with(spline.i, lines(x, y, lty = 1, lwd = 1.5, col = colours[i]))

    with(latania.df.i, points(Efppm, AngMort, pch = pchs[i], #lty = df.i$lt[1],
                              col = colours[i], cex = cex.pt))
  }
  if(titl)
    title("Latania scale")
  bar.legs(.6, .8, ebv = sems, ltys = rep(1, length(sems)),
           labs = stages, pchs = pchs,
           point.cex = cex.pt, leg.cex = leg.cex, cols = colours,
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)

### Mealybug
  sems <- NULL # add on sems as calculated
  stages <- unique(omb.df$Lifestage)
  colours <- COLOURS[seq(stages)] # code to work for other insects as well
  pchs <- PCHS[seq(stages)]
  spans <- rep(span+.1, length(stages)) # probably tinker with this one
  names(pchs) <- names(colours) <- names(spans) <- stages
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
    with(spline.i, lines(x, y, lty = 1, lwd = 1.5, col = colours[i]))

    with(omb.df.i, points(Efppm, AngMort, pch = pchs[i], #lty = df.i$lt[1],
                              col = colours[i], cex = cex.pt))
  }
  if(titl)
    title("OMB")
  bar.legs(.6, .7, ebv = sems, ltys = rep(1, length(sems)),
           labs = stages, pchs = pchs,
           point.cex = cex.pt, leg.cex = leg.cex, cols = colours,
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)

### Onion thrips
  sems <- NULL # add on sems as calculated
  stages <- unique(thrip.df$Lifestage)
  colours <- COLOURS[seq(stages)] # code to work for other insects as well
  pchs <- PCHS[seq(stages)]
  spans <- rep(span, length(stages)) # probably tinker with this one
  names(pchs) <- names(colours) <- names(spans) <- stages
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
    with(spline.i, lines(x, y, lty = 1, lwd = 1.5, col = colours[i]))

    with(thrip.df.i, points(Efppm, AngMort, pch = pchs[i], #lty = df.i$lt[1],
                              col = colours[i], cex = cex.pt))
  }
  if(titl)
    title("Thrips")
  bar.legs(.6, .7, ebv = sems, ltys = rep(1, length(sems)),
           labs = stages, pchs = pchs,
           point.cex = cex.pt, leg.cex = leg.cex, cols = colours,
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)





}
