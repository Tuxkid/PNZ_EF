gleanWithFruitSeptCM <- function(choice = 1)
{
### Purpose:- Codling moth sectiion of sept15With.df
### ----------------------------------------------------------------------
### Modified from:- gleanWithFruitSeptAll
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 22 Oct 2015, 11:49
### ----------------------------------------------------------------------
### Revisions:- 25/11/2015 fixed control mortality calculations

  require(dplyr)
  xx <- septwithFIXcm.df
  
  ## which control mortality is greater?  
  xx <- within(xx, Mort <- dead/Total)
  xx <- within(xx, Row <- seq(nrow(xx)))
  xx <- within(xx, Ndx <- paste(Placement, Rep, sep = "|"))
  xx <- within(xx, Ndx <- factor(Ndx)) # otherwise screws up group_by()
  xx <- within(xx, Efpc <- fact2num(Efpc))
 ## browser()
  cont.df <- xx[xx$Efpc == 0,]
  treat.df <- xx[xx$Efpc > 0,]
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
## Fish out Ndx pieces
  use.df <- within(use.df, Ndx <- as.character(Ndx))
  use.df <- within(use.df, Placement <- getbit(Ndx, "\\|", 1))
  use.df <- within(use.df, Rep <- getbit(Ndx, "\\|", 2))

  xx <- use.df %>%
    arrange(Placement, Rep, Efpc) %>%
      select(Placement, Rep, Efpc, dead, Total)

### Then a normal glean-type function
  cutx <- NULL
  xx$Idset <- idset <- with(xx, make.id(Efpc))
##   xx <- within(xx, Temp <- paste0(Temperature, "°C"))
##   xx <- within(xx, Hours <- paste0(Duration, "h"))
  leg.brief <- with(xx, unique(paste(Placement, " R", Rep, sep= "")))
  maint <- "Mortality of codling moth 5* with fruit in ethyl formate after 2h at 15°C"
  xlabels <- c(0, 0)
  xaxtitle <- "Dose (%)"
  with(xx,
       list(id = idset, times = Efpc, total = Total, dead = dead, 
            cutx = cutx, offset = 0, xaxtitle = xaxtitle, maint = maint, 
            legend = leg.brief, xlabels = xlabels, takelog = FALSE))
}
