gleanOffFruitSept <-
  function(choice = 1)
{
### Purpose:- September lot
### ----------------------------------------------------------------------
### Modified from:- gleanOffFruitJuly
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  3 Sep 2015, 15:51
### ----------------------------------------------------------------------
### Revisions:- as.character(Efnom) fixed 09/09/2015
###             23/09/2015 added selecting max of Handling and CO2 controls

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
##  browser()

  ## Define what is dead
  xx <- within(xx, dead <- Dead)
  xx <- within(xx, dead[IsEgg] <- Unhatched[IsEgg])
  xx <- within(xx, dead[IsScale] <- Dead[IsScale] + Moribund[IsScale])
  
  ## Use higher of handling and CO2 controls
  require(dplyr)
  xx <- within(xx, Ndx <- paste(SLS, Temperature, Duration, Rep, sep = "|"))
  xx <- xx %>% arrange(Ndx)
  xx <- within(xx, Mort <- dead/Total)
  xx <- within(xx, Row <- seq(nrow(xx)))
  cont.df <- xx[xx$Efpc == 0,]
  smallest <- function(x){
    biff <- logical(length(x)) # sometimes only 1 which will be also a min
    if(length(x) > 1){
      xmin <- min(x, na.rm = TRUE)
      wx <- which(x == xmin)
      wx <- wx[1] # necessary for ties: want only one
      biff[wx] <- TRUE
    }
    biff
  }
  smallest2 <- function(x){
    biff <- logical(length(x)) # sometimes only 1 which will be also a min
    if(length(x) > 1){
      xmax <- max(x, na.rm = TRUE)
      wx <- which(x != xmax)
      biff[wx] <- TRUE
    }
    biff
  }
##  cont.df$Smaller <- (with(cont.df, tapply(Mort, Ndx, smallest, simplify = FALSE))) # incorrect
##
  cont.df$Smaller2 <- logical(nrow(cont.df))
  for(i in unique(cont.df$Ndx)){
    cont.i <- cont.df[cont.df$Ndx == i,]
    small.i <- smallest2(cont.i$Mort)
    cont.df$Smaller2[cont.df$Ndx == i] <- small.i
  }
  ignore.rows <- with(cont.df, Row[Smaller2])
  xx <- xx[!xx$Row %in% ignore.rows, ] %>%
    arrange(SLS, Temperature, Duration, Rep, Efpc, HC) %>%
      select(SLS, Temperature, Duration, Rep, Efpc, dead, Total)

  use.df <- xx %>%
    tbl_df %>% arrange(SLS, Temperature, Duration, Rep, Efpc) 

### Then a normal glean function
  idset <- with(use.df, make.id(Efpc))
  cutx <- NULL
  
  use.df <- within(use.df, Temp <- paste0(Temperature, "°C"))
  use.df <- within(use.df, Hours <- paste0(Duration, "h"))
  ##  use.df <- within(use.df, DAT <- paste0(Assessed, "d"))
  leg.brief <- with(use.df, unique(paste(SLS, Temp,
                                         Hours, Rep, sep= "|")))
  maint <- "Mortality of PNZ pests off fruit in ethyl formate after various durations"
  xlabels <- c(0, 0)
  xaxtitle <- "Dose (%)"
  with(use.df,
       list(id = idset, times = Efpc, total = Total, dead = dead, 
            cutx = cutx, offset = 0, xaxtitle = xaxtitle, maint = maint, 
            legend = leg.brief, xlabels = xlabels, takelog = FALSE))
}
