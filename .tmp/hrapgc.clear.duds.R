clear.duds <- function(xx = aug.df)
{
### Purpose:- Removes reps that have no control data
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 28 Aug 2014, 11:17
### ----------------------------------------------------------------------
### Revisions:-

  xx.suss <- xx[is.na(xx$Total), ]
  xx.suss.id <- unique(xx.suss[, c(1, 2, 7,8)])
  browser()
  suss.id <- apply(xx.suss.id, 1, paste, collapse = "|")
  xx.id <- apply(xx[, c(1, 2, 7,8)], 1, paste, collapse = "|")
  xx.id%in%suss.id
  out <- xx[!xx.id%in%suss.id,]
  out
}
