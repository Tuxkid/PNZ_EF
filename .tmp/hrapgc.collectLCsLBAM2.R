collectLCsLBAM2 <- function(xO = septLBAMoffCI.df, xOCT = septLBAMoffCI_CT.df,
                           xW = septLBAMwithCI.df, xWCT = septLBAMwithCI_CT.df,
                           xO100 = septLBAMoff100CI.df, xOCT100 = septLBAMoff100CI_CT.df,
                           xW100 = septLBAMwith100CI.df, xWCT100 = septLBAMwith100CI_CT.df)
{
### Purpose:- Extending to use predicted mortalities
### ----------------------------------------------------------------------
### Modified from:- collectLCsLBAM
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 23 Sep 2015, 16:07
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

## remove A| and K| from rownames
with.id <- rownames(with.df)
chopAK <- function(x){
  ii <- gsub("\\|", "-", x)
  aa <- gsub("A-", "", ii)
  bb <- gsub("K-", "", aa)
  cc <- gsub("-", "\\|", bb)
  cc
}
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

predict.at <- 2:3

prediction.df <- data.frame(StageTreat = rep(with.id, each = 2),
                            Conc = rep(2:3, length(with.id)),
                            Predicted = NA, Actual = NA)
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
  slope.i.off <- slope.i.off * (1 - cont.mort.off.i)
  slope.i.with <- slope.i.with * (1 - cont.mort.with.i)
## predictions from off and with fruit
  pred.off <- cloglog.bt(intercept.i.off + slope.i.off * predict.at) * 100
  pred.with <- cloglog.bt(intercept.i.with + slope.i.with * predict.at) * 100
#
  if(is.null(pred.off))
  browser()
  prediction.df[prediction.df$StageTreat == lab.with.i, "Predicted"] <- round(pred.off, 1)
  prediction.df[prediction.df$StageTreat == lab.with.i, "Actual"] <- round(pred.with, 1)
  
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
  select(Stage, Fruit, Temp, Duration, Conc, Predicted, Actual)
WriteXLS(x = "out.df", "PredictionLBAM_WithFruit_EF.xls", "predictions")


}
