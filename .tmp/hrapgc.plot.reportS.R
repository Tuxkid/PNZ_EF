plot.reportS <- function(span = .8 , titl = FALSE)
{
### Purpose:- September batch BHLR
### ----------------------------------------------------------------------
### Modified from:- plot.reportJ
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 14 Nov 2014, 16:00
### ----------------------------------------------------------------------
### Revisions:-  27/11/14 fixe handling control error
 

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
  species <- unique(sep.df$Species)
  species.abbr <- c("BHLR")
  sep.df <- sep.df[sep.df$Efg != "Handling control",]# fixed 27/11/14
  sep.df <- within(sep.df, Efppm[is.na(Efppm)] <- 0)
  sep.df <- sep.df[, -8]
  sep.df$Efppm <- sep.df$Efppm * 10000 # (not really ppm but %)


  ## ditch rows of silly data
  omit.rows <- with(sep.df, Lifestage == "Early eggs" & Rep < 5 |
                    Lifestage == "Late eggs" & Rep == 2)
   use.df <- sep.df[! omit.rows,]
 
  use.df <- within(use.df, Mort <- Dead/Total)
  use.df <- within(use.df, AngMort <- ang(Mort))
  use.df$Efppm <- use.df$Efppm/10000
  sum.df <- aggregate(AngMort ~ Efppm + Lifestage + Species, data = use.df, mean)
  
### plotting begins

###  BHLR only
  sems <- NULL # add on sems as calculated
  stages <- unique(use.df$Lifestage)
  colours <- COLOURS[seq(stages)] # code to work for other insects as well
  pchs <- PCHS[seq(stages)]
  spans <- rep(span, length(stages)) # probably tinker with this one
  names(pchs) <- names(colours) <- names(spans) <- stages
  bitmap("BHLR.png", type = "png16m", width = 155, height = 143,
         res = 900, units = "mm", pointsize = 11)
  for(i in stages){
    use.df.i <- use.df[use.df$Lifestage == i,]
    use.lo.i <- with(use.df.i, loess(AngMort ~ Efppm,
                                             degree = 1, span = spans[i]))
    use.lo.pred.i <- predict(use.lo.i,
                                 unique(use.df.i$Efppm), se = TRUE)
    use.lo.pred.i$fit[use.lo.pred.i$fit > 90] <- 90
    sems <- c(sems, rms(use.lo.pred.i$se.fit)) # make vector of three sems
    use.lo.pred.i$fit[use.lo.pred.i$fit > 90] <- 90
    spline.i <- spline(unique(use.df.i$Efppm), use.lo.pred.i$fit, n = 200)
    spline.i$y[spline.i$y > 90] <-  90
    spline.i$y <- monotone(spline.i$y)# take out some more bends sometimes
### plotting begins
    if(i == stages[1]){
      with(use.df.i, blank.plot(Efppm, AngMort, ylim = c(0, 90)))
      axis(1, las = 1, mgp = c(1.8, .8, 0), cex.axis = .8)
      axis(2, las = 1, at = ytix.at, labels = ytix,
           mgp = c(1.8, .8, 0), cex.axis = ax.cex)
      box()
      mtext("Ethyl formate concentration (%)", side = 1, line = 1.9, cex = lab.cex)
      mtext("Mortality (%)", side = 2, line = 1.9, cex = lab.cex)
    }
    with(spline.i, lines(x, y, lty = 1, lwd = 1.5, col = colours[i]))

    with(use.df.i, points(Efppm, AngMort, pch = pchs[i], #lty = df.i$lt[1],
                              col = colours[i], cex = cex.pt))
  }
  if(titl)
    title("BHLR")
  bar.legs(.7, .5, ebv = sems, ltys = rep(1, length(sems)),
           labs = stages, pchs = pchs,
           point.cex = cex.pt, leg.cex = leg.cex, cols = colours,
           eq.tex.gap = FALSE, line.lwd = 1.5, line.leng = .12)
  dev.off()            
}

