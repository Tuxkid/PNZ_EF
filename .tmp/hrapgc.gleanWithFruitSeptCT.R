gleanWithFruitSeptCT <- function(choice = 1)
{
### Purpose:- CT version
### ----------------------------------------------------------------------
### Modified from:- gleanWithFruitSept
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 22 Sep 2015, 08:45
### ----------------------------------------------------------------------
### Revisions:- 

  xx <- sept15With.df
  xx <- within(xx, EfNom <- as.numeric(Efnom))
##   xx <- xx[!is.na(xx$EfNom),]
   xx <- within(xx, Efnom[is.na(Efnom)] <- 0) # no CO2
   xx <- within(xx, Dead[is.na(Dead)] <- 0) # one empty cell should be zero
  is.egg <- grep("egg", levels(xx$Lifestage), ignore.case = TRUE,value = TRUE)
  xx <- within(xx, IsEgg <- Lifestage%in%is.egg)
  is.scale <- grep("OS", levels(xx$SLS), ignore.case = TRUE,value = TRUE)
  xx <- within(xx, IsScale <- SLS%in%is.scale)
  xx <- xx[!is.na(xx$Total), ] # won't total unless

## Define what is dead
  xx <- within(xx, dead <- Dead)
  xx <- within(xx, dead[IsEgg] <- Unhatched[IsEgg])
  xx <- within(xx, Dead[IsScale] <- Dead[IsScale] + Moribund[IsScale])

  xx <- within(xx, Efpc[is.na(Efpc)] <- EfNom[is.na(Efpc)]/2) # might fix later
  xx <- within(xx, Efpc[EfNom ==  0] <- 0) # no controls sometimes otherwise
  xx$Row <- seq(nrow(xx))

require(dplyr)
  
  use.df <- xx %>%
    tbl_df %>% arrange(SLS, Temperature, Duration, Rep, Efpc) 

##   use.df2 <- df.sort(xx, rev(c("Pest", "Lifestage", "Temperature",
##                               "Duration", "Rep", "Efpc")))
##   with(use.df, table(Lifestage))  
  ## Check if there's any difference between "Controls"


### Then a normal glean function
  use.df$Idset <- idset <- with(use.df, make.id(Efpc))
  cutx <- NULL

 ##  prp <- use.df %>% #filter(Idset == 15) %>%
##     select(SLS, Temperature, Duration, Rep, Idset, Efpc, Row, Total, dead)
##  browser()
##   dtsm <- use.df %>% filter(SLS == "dTSM")
  
  use.df <- within(use.df, Temp <- paste0(Temperature, "°C"))
  use.df <- within(use.df, Hours <- paste0(Duration, "h"))
##  use.df <- within(use.df, DAT <- paste0(Assessed, "d"))
  leg.brief <- with(use.df, unique(paste(SLS, Temp,
                              Hours, Rep, sep= "|")))
  maint <- "Mortality of PNZ targets with fruit in ethyl formate after various durations (CT)"
  xlabels <- c(0, 0)
  xaxtitle <- "CT (%-h)"
  with(use.df,
       list(id = idset, times = Efpc * Duration, total = Total, dead = dead, 
            cutx = cutx, offset = 0, xaxtitle = xaxtitle, maint = maint, 
            legend = leg.brief, xlabels = xlabels, takelog = FALSE))

  
}
