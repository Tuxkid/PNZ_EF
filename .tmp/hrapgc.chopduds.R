chopduds <- function(xx = ab.rep)
{
### Purpose:- removes data pertaining to low numbers
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  7 Jun 2013, 11:50
### ----------------------------------------------------------------------
### Revisions:-

  begin <- xx[[1]]
  all.legs <- begin$legend

  duds <- grep(" male", all.legs)
  keep <- list()
  keep$legend <- begin$legend[-duds]
  keep$lt <- begin$lt[-duds,]
  keep
  
}
