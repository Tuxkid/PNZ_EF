gleanOffFruitSept_J <- function(choice = 1)
{
### Purpose:- Replicates joined
### ----------------------------------------------------------------------
### Modified from:- gleanOffFruitSept
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  2 Oct 2015, 09:34
### ----------------------------------------------------------------------
### Revisions:-


###             23/09/2015 added selecting max of Handling and CO2 controls
###             25/11/2015 CM calculations fixed (copied from With-fruit lot

  xx <- sept15Off.df
  xx <- within(xx, EfNom <- as.numeric(as.character(Efnom))) # fixed 9/9/15
  ## Indicate which is handling control
  xx <- within(xx, HC <- is.na(EfNom))

  xx <- within(xx, Efnom[is.na(Efnom)] <- 0) # no CO2
  xx <- within(xx, Dead[is.na(Dead)] <- 0) # one empty cell should be zero

  is.egg <- grep("egg", levels(xx$Lifestage), ignore.case = TRUE,value = TRUE)
  xx <- within(xx, IsEgg <- Lifestage%in%is.egg)
  is.scale <- grep("OS", levels(xx$SLS), ignore.case = TRUE,value = TRUE)
  xx <- within(xx, IsScale <- SLS%in%is.scale)
  xx <- xx[!is.na(xx$Total), ] # won't total unless
##  
  ## Define what is dead
  xx <- within(xx, dead <- Dead)
  xx <- within(xx, dead[IsEgg] <- Unhatched[IsEgg])
  xx <- within(xx, dead[IsScale] <- Dead[IsScale] + Moribund[IsScale])
  
  ## Use higher of handling and CO2 controls
  require(dplyr)
  xx <- within(xx, Ndx <- paste(SLS, Temperature, Duration, Rep, sep = "|"))
   xx <- within(xx, Ndx <- factor(Ndx)) # otherwise screws up group_by()
  xx <- xx %>% arrange(Ndx)
  xx <- within(xx, Mort <- dead/Total)
  xx <- within(xx, Row <- seq(nrow(xx)))
  cont.df <- xx[xx$Efpc == 0,]
  treat.df <- xx[xx$Efpc > 0,]

  ## browser()
  smallest <- function(x){
    biff <- logical(length(x)) # sometimes only 1 which will be also a min
    if(length(x) > 1){
      xmin <- min(x, na.rm = TRUE)
      wx <- which(x == xmin)
      wx <- wx[1] # necessary for ties: want only one
      biff[wx] <- TRUE
    }
    if(length(biff[biff]) > 1)
      cat("funny business\n")
    biff
  }
  ## Get overall mortalites for both controls
  cont.sum.df <- cont.df %>%
    select(Ndx, HC, dead, Total) %>%
      group_by(Ndx, HC) %>%
        summarise_each(funs(sum), dead, Total) %>%
          mutate(Mort = dead/Total)
   
  cont.sum.df$Smaller <- unlist(with(cont.sum.df, tapply(Mort, Ndx, smallest)))
  cont.sum.df$Efpc <- 0
  use.cont.df <- cont.sum.df[with(cont.sum.df, !Smaller),] %>%
    select(Ndx, Efpc, dead, Total)
  use.treat.df <- treat.df %>%
    select(Ndx, Efpc, dead, Total)

  use.df <- merge(use.cont.df, use.treat.df, all = TRUE) %>%
    arrange(Ndx)
  use.df <- within(use.df, Ndx <- as.character(Ndx)) # no longer want a factor
  use.df <- within(use.df, SLS <- getbit(Ndx, "\\|", 1))
  use.df <- within(use.df, Temperature <- as.numeric(getbit(Ndx, "\\|", 2)))
  use.df <- within(use.df, Duration <- as.numeric(getbit(Ndx, "\\|", 3)))
  use.df <- within(use.df, Rep <- getbit(Ndx, "\\|", 4))
  use.df <- use.df %>% arrange(SLS, Temperature, Duration, Efpc)

 ### Then a normal glean function
  idset <- with(use.df, make.id(Efpc))
  cutx <- NULL
  
  use.df <- within(use.df, Temp <- paste0(Temperature, "°C"))
  use.df <- within(use.df, Hours <- paste0(Duration, "h"))
  ##  use.df <- within(use.df, DAT <- paste0(Assessed, "d"))
  leg.brief <- with(use.df, unique(paste(SLS, Temp, Hours, sep= "|")))
  maint <- "Mortality of PNZ pests off fruit in ethyl formate after various durations (joined)"
  xlabels <- c(0, 0)
  xaxtitle <- "Dose (%)"
  with(use.df,
       list(id = idset, times = Efpc, total = Total, dead = dead, 
            cutx = cutx, offset = 0, xaxtitle = xaxtitle, maint = maint, 
            legend = leg.brief, xlabels = xlabels, takelog = FALSE))
}
