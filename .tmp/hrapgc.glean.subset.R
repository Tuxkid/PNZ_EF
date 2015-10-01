glean.subset <- function(x, k = 1)
{
### Purpose:- Subsets data gathered by a glean function
### ----------------------------------------------------------------------
### Modified from:- x: glean function
###                 k: id/s required
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  1 Oct 2015, 09:27
### ----------------------------------------------------------------------
### Revisions:-

  xx <- x()
  xx.df <- with(xx, data.frame(id, times, dead, total))
  id.tab <- table(xx$id)
  xx.df$legend <- rep(xx$legend, id.tab)
  xx.df[xx.df$id %in% k,]  
}
