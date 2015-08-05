stripC <- function(..., fax = 2,  fat = 0.4)
{
### Purpose:- Draws thin lines under strip labels to make them more
###            obviously referring to the data plotted below and not above
### ----------------------------------------------------------------------
### Modified from:- 
### ----------------------------------------------------------------------
### Arguments:- fax: how many labels are in each strip?
###             fat: how fat are the lines to be?
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 29 Jan 2013, 14:21
### ----------------------------------------------------------------------
### Revisions:-

  strip = function(...) {
    fax <- fax
    fat <- fat
    strip.default(...)
    ## always do at least 1 thin line
    grid.segments(0, 0, 1, 0, gp=gpar(col = "white"))
    grid.segments(0, 0, 1, 0, gp=gpar(lwd = fat))
    if(fax > 1) { # more lines if necessary
      for(i in 2:fax - 1){
        grid.segments(0, 1/fax * i, 1, 1/fax * i, gp=gpar(col = "white"))
        grid.segments(0, 1/fax * i, 1, 1/fax * i, gp=gpar(lwd = fat))
      }
    }
    ## cover white gaps in corners
    grid.segments(0, 0, 0, 1)
    grid.segments(1, 0, 1, 1)
  }
}
