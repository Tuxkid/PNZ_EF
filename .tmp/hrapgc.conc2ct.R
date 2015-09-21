conc2ct <- function(xx = septLBAMOffFirst100.df)
{
### Purpose:- modify table of Conc and duration to CTs
###           (less mucking around that how others were done
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- xx: dataframe of first 100% points
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 22 Sep 2015, 11:26
### ----------------------------------------------------------------------
### Revisions:-

  xx <- within(xx, Duration <- getbit(as.character(legend), "\\|", 3))
  xx <- within(xx, Duration <- as.numeric(gsub("h", "", Duration)))
  xx <- within(xx, MinConc <- MinConc * Duration)
  xx
}
