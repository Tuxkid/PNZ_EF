gleanWithFruitSeptAll_J <- function(choice = 1)
{
### Purpose:- Joins reps
### ----------------------------------------------------------------------
### Modified from:- gleanWithFruitSeptAll
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  2 Oct 2015, 13:21
### ----------------------------------------------------------------------
### Revisions:- 

  require(dplyr)
  xx <- septwithFIXall.df
    xx <- septwithFIXall.df
  xx$Ndx <- substring(as.character(xx$Ndx), 12, 40)
    xx <- within(xx, Ndx <- factor(Ndx)) # otherwise screws up group_by()

  ## which control mortality is greater?
  
  cont.df <- xx[xx$Efpc == 0,]
  treat.df <- xx[xx$Efpc > 0,]
  smallest <- function(x){
    biff <- logical(length(x)) # sometimes only 1 which will be also a min
    if(length(x) > 1){ # x is vector no more than 2 long
      xmin <- min(x, na.rm = TRUE)
      wx <- which(x == xmin)
      wx <- wx[1] # necessary for ties: want only one
      biff[wx] <- TRUE
    }
    biff
  }
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

  use.df <- rbind(as.data.frame(use.cont.df), as.data.frame(use.treat.df)) %>%
    arrange(Ndx)
  use.df <- within(use.df, Ndx <- as.character(Ndx)) # no longer want a factor

  use.df <- within(use.df, SLS <- getbit(Ndx, "\\|", 1))
  use.df <- within(use.df, Fruit <- getbit(Ndx, "\\|", 2))
  use.df <- within(use.df, Temperature <- as.numeric(getbit(Ndx, "\\|", 3)))
  use.df <- within(use.df, Duration <- as.numeric(getbit(Ndx, "\\|", 4)))
  xx <- within(use.df, Rep <- getbit(Ndx, "\\|", 5))
  xx <- xx %>% arrange(SLS, Fruit, Temperature, Duration, Efpc)
  
### Then a normal glean-type function
  cutx <- NULL
  xx$Idset <- idset <- with(xx, make.id(Efpc))
  xx <- within(xx, Temp <- paste0(Temperature, "°C"))
  xx <- within(xx, Hours <- paste0(Duration, "h"))
  leg.brief <- with(xx, unique(paste(SLS, casefold(substring(Fruit, 1, 1), upper = TRUE),
                                     Temp, Hours, sep= "|")))
  maint <- "Mortality of PNZ pests with fruit in ethyl formate after various durations (joined)"
  xlabels <- c(0, 0)
  xaxtitle <- "Dose (%)"
  with(xx,
       list(id = idset, times = Efpc, total = Total, dead = dead, 
            cutx = cutx, offset = 0, xaxtitle = xaxtitle, maint = maint, 
            legend = leg.brief, xlabels = xlabels, takelog = FALSE))
}
