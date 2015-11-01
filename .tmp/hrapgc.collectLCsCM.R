collectLCsCM <- function(adjust.cont = FALSE,
                       xO = sepOffCI.df, xOCT = sepOffCI_CT.df,
                       xW = sepWithCM_CI.df, xWCT = sepWithCI_CT.df,
                       xO100 = sepOff100CI.df, xOCT100 = sepOff100CI_CT.df,
                       xW100 = sepWith100CI.df, xWCT100 = sepWith100CI_CT.df,
                       ab.off = ab.sept15OffAll$concJ,
                       ab.with = ab.sept15WithCM$concJ,
                       zO = sept15Off.df, zW = septwithFIXcm.df)
{
### Purpose:- Codling moth done somewhat differently
### ----------------------------------------------------------------------
### Modified from:- collectLCs2
###                 will mention a lot of objects which aren't used
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 23 Oct 2015, 09:42
### ----------------------------------------------------------------------
### Revisions:- 

  ## Standardize egg in identifiers
  rownames(xO) <- gsub("ME", "egg", rownames(xO))
  rownames(xOCT) <- gsub("ME", "egg", rownames(xOCT))
  ## Off fruit
  off.df <- data.frame(LC99est = as.numeric(xO$lt.mean),
                       ## Min100mort = as.numeric(xO100$lt.mean),
                       LCT99est = as.numeric(xOCT$lt.mean)) #,
                      ##  Min100mortCT = as.numeric(xOCT100$lt.mean))
  rownames(off.df) <- rownames(xO)
  use.off <- "CM5|15C|2h"
  use.cols <- names(off.df) # CM 5* only 15deg 2h
  off.df[use.off, use.cols]
   
  ## With fruit: have to use a matrix
  with.mat <- matrix(nrow = nrow(xW), ncol = 2)
  dimnames(with.mat) <- list(rownames(xW), names(off.df))
  with.mat[rownames(xW), "LC99est"] <- as.numeric(xW$lt.mean)
##   with.mat[rownames(xW100), "Min100mort"] <- as.numeric(xW100$lt.mean)
##  with.mat[rownames(xWCT), "LCT99est"] <- as.numeric(xWCT$lt.mean)
##   with.mat[rownames(xWCT100), "Min100mortCT"] <- as.numeric(xWCT100$lt.mean)
  with.df <- as.data.frame(with.mat)
  with.id <- rownames(with.df)
  
  ## remove A| and K| from rownames
  ## With help from Rhelp
  chopAK <- function(x)
    gsub("\\|[AK]\\|","\\|", x)
  
  glean.off <- get(ab.off$datafun)
  glean.with <- get(ab.with$datafun)
  ## data used in those ab lists
  mort.dat.off <- with(glean.off(), data.frame(id, times, dead, total))
  mort.dat.with <- with(glean.with(), data.frame(id, times, dead, total))
  ## legends to line up
  legs.off <- glean.off()$legend
  legs.off <- gsub("°", "", legs.off)
  id.off.use <- which(legs.off%in%use.off)
  mort.dat.off[mort.dat.off$id == id.off.use,]


  zO <- zO[with(zO, SLS == "CM5" & Duration == 2 & Temperature == 15), ]

  ## Which concentrations do we want predictions done?
  predict.at <- 2:3 # i.e. target concentrations
  zzO <- zO %>%
    filter(Efnom %in% predict.at, Rep > 0) %>%
      select(Efnom, Efpc, Rep) %>%
        arrange(Efnom, Rep) %>%
          group_by(Efnom) %>%
            summarise(EF = round(mean(Efpc), 3), Efmin = min(Efpc), Efmax = max(Efpc))
  zzO <- within(zzO, Efnom <- fact2num(Efnom))

  zW <- within(zW, Efpc <- fact2num(Efpc))
  zW <- within(zW, Efnom <- fact2num(Efnom))
  zzW <- zW %>% 
    filter(Efnom %in% predict.at) %>%
       mutate(Efnom = factor(Efnom)) %>% # group_by must use character or factor
        select(Placement, Rep, Efnom, Efpc) %>%
          arrange(Placement, Efnom, Rep) %>%
            group_by(Placement, Efnom) %>% # group_by must use character or factor
              summarise(EF = round(mean(Efpc), 3), Efmin = min(Efpc), Efmax = max(Efpc))

  zzW <- within(zzW, Efnom <- fact2num(Efnom)) # needs a number for subsetting

  
  prediction.df <- data.frame(Placement = rep(with.id, each = 2),
                              TargetEFconc = rep(predict.at, length(with.id)),
                              AchievedEFoff_mean = NA, AchievedEFoff_lo = NA, AchievedEFoff_hi = NA,
                              PredictedOff_mean = NA, PredictedOff_lo = NA, PredictedOff_hi = NA,
                              AchievedEFwith_mean = NA, AchievedEFwith_lo = NA, AchievedEFwith_hi = NA,
                              PredictedWith_mean = NA, PredictedWith_lo = NA, PredictedWith_hi = NA)
  for(i in seq(with.id)){
    id.i <- with.id[i]
    dat.with.i <- mort.dat.with[mort.dat.with$id == i,]
    ## slope and intercept info
    slope.i.with <- ab.with$slope[i]
    intercept.i.with <- ab.with$intercept[i]

     ## corresponding off data identifier
    lab.with.i <- with.id[i]
    lab.with.i.match <- gsub("C", "°C", lab.with.i)
    lab.off.i <- "CM5|15C|2h" 
    id.off.i <- which(legs.off == lab.off.i)
    dat.off.i <- mort.dat.off[mort.dat.off$id == id.off.i,]
    ## corresponding slope and intercept info for off data
    slope.i.off <- ab.off$slope[id.off.i]
    intercept.i.off <- ab.off$intercept[id.off.i]
    cont.dat.with.i <- dat.with.i[dat.with.i$times == 0,]
    cont.dat.off.i <- dat.off.i[dat.off.i$times == 0,]
    ## control adjustments
    cont.mort.with.i <- with(cont.dat.with.i, mean(dead/total))
    cont.mort.off.i <- with(cont.dat.off.i, mean(dead/total))
    if(adjust.cont){    ## adjust intercepts for control mort
      if(cloglog.bt(intercept.i.off) > cont.mort.off.i)
        intercept.i.off <- cloglog(cloglog.bt(intercept.i.off) - cont.mort.off.i) else
      slope.i.off <- slope.i.off * (1 - cont.mort.off.i) # adj slope instead
      
      if(cloglog.bt(intercept.i.with) > cont.mort.with.i)
        intercept.i.with <- cloglog(cloglog.bt(intercept.i.with) - cont.mort.with.i) else
      slope.i.with <- slope.i.with * (1 - cont.mort.with.i)
    }
### Relevant concentrations for this i
    achieved.i.off <- as.data.frame(zzO) # ii for off
    achieved.i.with <- as.data.frame(zzW) # i for with
    
    for(j in predict.at){ # always 2 for Off fruit
      conc.ij.off <- achieved.i.off[achieved.i.off$Efnom == j, ] %>%
        select(EF, Efmin, Efmax)
      conc.ij.with <- achieved.i.with[achieved.i.with$Efnom == j, ] %>%
        select(EF, Efmin, Efmax)
      try(prediction.df[with(prediction.df, Placement == lab.with.i & TargetEFconc == j), 
                        c("AchievedEFoff_mean", "AchievedEFoff_lo", "AchievedEFoff_hi")] <-
          conc.ij.off, silent = TRUE)
      try(prediction.df[with(prediction.df, Placement == lab.with.i & TargetEFconc == j),
                        c("AchievedEFwith_mean", "AchievedEFwith_lo", "AchievedEFwith_hi")] <-
          conc.ij.with, silent = TRUE)
      
      ## predictions for those concentrations, off and with
      pred.off.ij <- round(cloglog.bt(intercept.i.off + slope.i.off * conc.ij.off) * 100, 1)
      pred.with.ij <- round(cloglog.bt(intercept.i.with + slope.i.with * conc.ij.with) * 100, 1)
      
      try(prediction.df[with(prediction.df, Placement == lab.with.i & TargetEFconc == j),
                        c("PredictedOff_mean", "PredictedOff_lo", "PredictedOff_hi")] <-
          pred.off.ij, silent = TRUE)
      try(prediction.df[with(prediction.df, Placement == lab.with.i & TargetEFconc == j),
                        c("PredictedWith_mean", "PredictedWith_lo", "PredictedWith_hi")] <-
          pred.with.ij)      
    }    
  }
  
  require("WriteXLS")
  out.df <- prediction.df %>%
    select(Placement, TargetEFconc,
           AchievedEFoff_mean, AchievedEFoff_lo, AchievedEFoff_hi,
           PredictedOff_mean, PredictedOff_lo, PredictedOff_hi,
           AchievedEFwith_mean, AchievedEFwith_lo, AchievedEFwith_hi,
           PredictedWith_mean, PredictedWith_lo, PredictedWith_hi)
  xlout <- "Predictions_With.OffFruit_EF2CM.xls"
#browser()
  
### Write out predictions, the off/with LC99 estimates, and the confidence limit of each
###              group of replicates
##   WriteXLS(x = c("out.df", "off.df", "with.df", "xO", "xO100", "xOCT", "xOCT100", "xW", "xW100",
##            "xWCT", "xWCT100"), xlout, row.names = TRUE,
##            c("predictions @ 2 & 3%", "Off fruit", "With fruit", "Off fruit LC99CI", "Off fruit 100% LC99CI",
##            "Off fruit CT LC99CI", "Off fruit 100% CT LC99CI", "With fruit LC99CI", "With fruit 100% LC99CI",
##            "With fruit CT LC99CI", "With fruit 100% CT LC99CI"),
##             BoldHeaderRow = TRUE, FreezeRow = 3, FreezeCol = 2)
##  out.df

  WriteXLS(x = c("out.df"), xlout, row.names = FALSE, c("predictions @ 2 & 3%"), BoldHeaderRow = TRUE)

  
}
