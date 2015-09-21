df2ablist <- function(xx = septLBAMwithFirst100.df)
{
### Purpose:- convert dataframe into part of an ab-type list
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 17 Sep 2015, 16:19
### ----------------------------------------------------------------------
### Revisions:-
  ab <- list()
  ab$legend <- xx$legend
  
  ab$lt <- as.data.frame(matrix(xx$MinConc, ncol = 1))
  names(ab$lt) <- "100"
  ab
}
