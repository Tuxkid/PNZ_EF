get100mort <- function(xx, max.control = TRUE)
{
### Purpose:- Finds the first consistent 100% mortality point
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- xx: dataframe that can be read by a glean-type function
###             max.control: use only greater of handling and CO2 control?
###                          which probably makes no difference to max
###                          but does rearrange to join different days
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 17 Sep 2015, 09:56
### ----------------------------------------------------------------------
### Revisions:-

  require(dplyr)
  if(max.control){ # which control mortality is greater?
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
    cont.df$Smaller <- unlist(with(cont.df, tapply(Mort, Ndx, smallest)))
    ignore.rows <- with(cont.df, Row[Smaller])
    ## rearrange if more than one lot is done for the same combination
    xx <- xx[!xx$Row %in% ignore.rows, ] %>%
      arrange(SLS, Fruit, Temperature, Duration, Rep, Efpc, HC) %>%
        select(SLS, Fruit, Temperature, Duration, Rep, Efpc, dead, Total)
  } 

  xx <- within(xx, Temp <- paste0(Temperature, "°C"))
  xx <- within(xx, Hours <- paste0(Duration, "h"))
  leg.brief <- with(xx, unique(paste(SLS, substring(Fruit, 1, 1), Temp,
                                     Hours, Rep, sep= "|")))
  xx$Idset <- idset <- with(xx, make.id(Efpc))
  idset.tab <- table(idset)
  out.df <- data.frame(legend = leg.brief, Id = unique(idset))
  out.df$MinConc <- NA  
  for(i in unique(idset)){
    xx.i <- xx %>% filter(Idset == i) %>%
      mutate(Complete = dead == Total)
    xx.i <- xx.i %>%
        mutate(Row = seq(nrow(xx.i)))
    complete.rows <- with(xx.i, Row[Complete], function(x) x)
    if(max(xx.i$Row) %in% complete.rows){ # iff highest is already 100%
      ## find last Complete reading backwards
      if(all(diff(complete.rows) == 1)){
        first.complete <- complete.rows[1]
        min.conc <- xx.i$Efpc[first.complete]
      } else {
      first.complete <- which(diff(rev(complete.rows)) < -1)
      min.conc <- rev(xx.i$Efpc)[first.complete]
    }
##      browser()
      out.df$MinConc[i] <- min.conc[1] # sometimes more than 1
    }
  }
  out.df
}
  

