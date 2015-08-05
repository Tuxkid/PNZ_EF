what.sizes <- function(xx = june15Off.df)
{
### Purpose:- What durations at what temperatures
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  9 Jun 2015, 15:33
### ----------------------------------------------------------------------
### Revisions:- 

##  browser()
  sls <- levels(xx$SLS)
  for(sl in sls){
    dfs <- xx[xx$SLS == sl,]
    cat("\n\n", sl, ":\n =======================\n")
    with(dfs, print(table(Duration, Temperature)))
  }

  }
