chopduds.june <- function(xx = ab.june)
{
### Purpose:- Omit lots of unconvincing trials
### ----------------------------------------------------------------------
### Modified from:- chopduds
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 15 Jul 2014, 12:04
### ----------------------------------------------------------------------
### Revisions:- 


  begin <- xx[[1]]
  all.legs <- begin$legend
browser()
  duds3 <- grep("Male", all.legs)
  duds1 <- grep("alate", all.legs)
  duds2 <- grep("Pupae", all.legs)
  duds <- c(duds1, duds2, duds3)
  keep <- list()
  keep$legend <- begin$legend[-duds]
  keep$lt <- begin$lt[-duds,]
  keep
  
}
