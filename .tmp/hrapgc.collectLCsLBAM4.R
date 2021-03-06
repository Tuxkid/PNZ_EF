collectLCsLBAM4 <- function(xO = septLBAMoffCI.df, xOCT = septLBAMoffCI_CT.df,
                            xW = septLBAMwithCI.df, xWCT = septLBAMwithCI_CT.df,
                            xO100 = septLBAMoff100CI.df, xOCT100 = septLBAMoff100CI_CT.df,
                            xW100 = septLBAMwith100CI.df, xWCT100 = septLBAMwith100CI_CT.df,
                            zO = sept15LBAMoff.df, zW = sept15LBAMwithFIX.df)
{
### Purpose:- Incorporates Target concentration and achieved conc
### ----------------------------------------------------------------------
### Modified from:- collectLCsLBAM3
### ----------------------------------------------------------------------
### Arguments:- zO, zW, off and with data used by glean functions
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 28 Sep 2015, 10:56
### ----------------------------------------------------------------------
### Revisions:- 

  ## Standardize egg in identifiers
  rownames(xO) <- gsub("ME", "egg", rownames(xO))
  rownames(xOCT) <- gsub("ME", "egg", rownames(xOCT))
  
  ## Off fruit
  off.df <- data.frame(LC99est = as.numeric(xO$lt.mean),
                       Min100mort = as.numeric(xO100$lt.mean),
                       LCT99est = as.numeric(xOCT$lt.mean),
                       Min100mortCT = as.numeric(xOCT100$lt.mean))
  rownames(off.df) <- rownames(xO)
  
  ## With fruit: have to use a matrix
  with.mat <- matrix(nrow = nrow(xW100), ncol = 4)
  dimnames(with.mat) <- list(rownames(xW100), names(off.df))
  with.mat[rownames(xW), "LC99est"] <- as.numeric(xW$lt.mean)
  with.mat[rownames(xW100), "Min100mort"] <- as.numeric(xW100$lt.mean)
  with.mat[rownames(xWCT), "LCT99est"] <- as.numeric(xWCT$lt.mean)
  with.mat[rownames(xWCT100), "Min100mortCT"] <- as.numeric(xWCT100$lt.mean)
  with.df <- as.data.frame(with.mat)
  with.id <- rownames(with.df)
  
  ## remove A| and K| from rownames
  ## With help from Rhelp
  chopAK <- function(x)
    gsub("\\|[AK]\\|","\\|", x)
  
  ab.off <- ab.sept15Off$Joined
  ab.with <- ab.sept15With$Joined
  glean.off <- get(ab.off$datafun)
  glean.with <- get(ab.with$datafun)
  ## data used in those ab lists
  mort.dat.off <- with(glean.off(), data.frame(id, times, dead, total))
  mort.dat.with <- with(glean.with(), data.frame(id, times, dead, total))
  ## legends to line up
  legs.off <- glean.off()$legend
  legs.off <- gsub("ME", "egg", legs.off)
  
  ## Get concentrations from data used in glean functions
  zO <- within(zO, SLS <- gsub("ME", "egg", SLS)) # needed here too
  zO <- within(zO, Temp <- paste0(Temperature, "C"))
  zO <- within(zO, Hours <- paste0(Duration, "h"))
  zW <- within(zW, Temp <- paste0(Temperature, "C"))
  zW <- within(zW, Hours <- paste0(Duration, "h"))
  
  zO <- within(zO, Ndx <- paste(SLS, Temp, Hours, sep = "|"))
  zW <- within(zW, Ndx <- paste(SLS, substring(Fruit, 1, 1), Temp, Hours, sep = "|"))
  
  ## Which concentrations do we want predictions done?
  predict.at <- 2:3 # i.e. target concentrations
  zzO <- zO %>%
    filter(Efnom %in% predict.at, Rep > 0) %>%
      select(Ndx, Efnom, Efpc, Rep) %>%
        arrange(Ndx, Efnom, Rep) %>%
          group_by(Ndx, Efnom) %>%
            summarise(EF = mean(Efpc), Efmin = min(Efpc), Efmax = max(Efpc))
  zzO <- within(zzO, Efnom <- fact2num(Efnom))
  
  zzW <- zW %>%
    filter(Efnom %in% predict.at, Efpc > 0) %>%
      mutate(Efnom = factor(Efnom)) %>%
        select(Ndx, Efnom, Efpc, Rep) %>%
          arrange(Ndx, Efnom, Rep) %>%
            group_by(Ndx, Efnom) %>% # group_by must use character or factor
              summarise(EF = mean(Efpc), Efmin = min(Efpc), Efmax = max(Efpc))
  zzW <- within(zzW, Efnom <- fact2num(Efnom)) # needs a number for subsetting
  
  prediction.df <- data.frame(StageTreat = rep(with.id, each = 2),
                              TargetEFconc = rep(2:3, length(with.id)),
                              AchievedEFoff_lo = NA, AchievedEFoff_hi = NA,
                              PredictedOff_lo = NA, PredictedOff_hi = NA,
                              AchievedEFwith_lo = NA, AchievedEFwith_hi = NA,
                              PredictedWith_lo = NA, PredictedWith_hi = NA)
  for(i in seq(with.id)){
    id.i <- with.id[i]
    dat.with.i <- mort.dat.with[mort.dat.with$id == i,]
    ## slope and intercept info
    slope.i.with <- ab.with$slope[i]
    intercept.i.with <- ab.with$intercept[i]
    ## corresponding off data identifier
    lab.with.i <- with.id[i]
    lab.with.i.match <- gsub("C", "°C", lab.with.i)
    lab.off.i <- chopAK(lab.with.i.match) # removes the fruit info
    lab.off.ii <- gsub("°C", "C", lab.off.i) # for matching in zzO
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
    ## adjust slopes for control mort
    if(cloglog.bt(intercept.i.off) > cont.mort.off.i)
      intercept.i.off <- cloglog(cloglog.bt(intercept.i.off) - cont.mort.off.i) else
    slope.i.off <- slope.i.off * (1 - cont.mort.off.i) # adj slope instead
    
    if(cloglog.bt(intercept.i.with) > cont.mort.with.i)
      intercept.i.with <- cloglog(cloglog.bt(intercept.i.with) - cont.mort.with.i) else
    slope.i.with <- slope.i.with * (1 - cont.mort.with.i)
### Relevant concentrations for this i
    achieved.i.off <- as.data.frame(zzO[with(zzO, Ndx == lab.off.ii), ])
    achieved.i.with <- as.data.frame(zzW[with(zzW, Ndx == lab.with.i), ])
    
    for(j in predict.at){ # always 2 for Off fruit
      conc.ij.off <- achieved.i.off[achieved.i.off$Efnom == j, ] %>%
        select(Efmin, Efmax)
      conc.ij.with <- achieved.i.with[achieved.i.with$Efnom == j, ] %>%
        select(Efmin, Efmax)
      try(prediction.df[with(prediction.df, StageTreat == lab.with.i & TargetEFconc == j), 
                        c("AchievedEFoff_lo", "AchievedEFoff_hi")] <- conc.ij.off, silent = TRUE)
      try(prediction.df[with(prediction.df, StageTreat == lab.with.i & TargetEFconc == j),
                        c("AchievedEFwith_lo", "AchievedEFwith_hi")] <- conc.ij.with)
      
      ## predictions for those concentrations, off and with
      pred.off.ij <- cloglog.bt(intercept.i.off + slope.i.off * conc.ij.off) * 100
      pred.with.ij <- cloglog.bt(intercept.i.with + slope.i.with * conc.ij.with) * 100
      
      try(prediction.df[with(prediction.df, StageTreat == lab.with.i & TargetEFconc == j),
                        c("PredictedOff_lo", "PredictedOff_hi")] <- pred.off.ij, silent = TRUE)
      try(prediction.df[with(prediction.df, StageTreat == lab.with.i & TargetEFconc == j),
                        c("PredictedWith_lo", "PredictedWith_hi")] <- pred.with.ij)      
    }    
  }
  prediction.df <- within(prediction.df, StageTreat <- as.character(StageTreat))
  prediction.df <- within(prediction.df, Stage <- getbit(StageTreat,  "\\|", 1))
  prediction.df <- within(prediction.df, fruit <- getbit(StageTreat,  "\\|", 2))
  prediction.df <- within(prediction.df, temp <- getbit(StageTreat,  "\\|", 3))
  prediction.df <- within(prediction.df, duration <- getbit(StageTreat,  "\\|", 4))
  prediction.df <- within(prediction.df, Temp <- as.numeric(gsub("C", "", temp)))
  prediction.df <- within(prediction.df, Duration <- as.numeric(gsub("h", "", duration)))
  prediction.df$Fruit <- NA
  prediction.df <- within(prediction.df, Fruit[fruit == "A"] <- "apple")
  prediction.df <- within(prediction.df, Fruit[fruit == "K"] <- "kiwifruit")
  
  require("WriteXLS")
  out.df <- prediction.df %>%
    select(Stage, Fruit, Temp, Duration, TargetEFconc, AchievedEFoff_lo, AchievedEFoff_hi,
           PredictedOff_lo, PredictedOff_hi, AchievedEFwith_lo, AchievedEFwith_hi,
           PredictedWith_lo, PredictedWith_hi)
  WriteXLS(x = "out.df", "PredictionLBAM_WithFruit_EF4.xls", "predictions")
  out.df  
}
