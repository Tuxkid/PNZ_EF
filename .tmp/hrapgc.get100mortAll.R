get100mortAll <- function(xxx = gleanOffFruitSeptLBAM, max.control = TRUE)
{
### Purpose:- Finds the first consistent 100% mortality point, all species
### ----------------------------------------------------------------------
### Modified from:- get100mort
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  2 Oct 2015, 14:43
### ----------------------------------------------------------------------
### Revisions:- 


  require(dplyr)
  yy <- xxx()
  all.legs <- yy$legend
  use.ids <- unique(yy$id)
  mort.df <- data.frame(id = yy$id, dead = yy$dead, total = yy$total, dose = yy$times)
  xx <- mort.df[mort.df$id %in% use.ids,]
  xx$leg <- rep(all.legs, table(xx$id))

  xx$Idset <- idset <- with(xx, make.id(dose)) 
  ##   idset.tab <- table(idset)
  out.df <- data.frame(legend = all.legs, Id = unique(idset))
  out.df$MinConc <- NA  
  for(i in unique(idset)){
    xx.i <- xx %>% filter(Idset == i) %>%
      mutate(Complete = dead == total)
    xx.i <- xx.i %>%
      mutate(Row = seq(nrow(xx.i)))
    complete.rows <- with(xx.i, Row[Complete], function(x) x)
    if(length(complete.rows) > 0){
      if(max(xx.i$Row) %in% complete.rows){ # iff highest is already 100%
        ## find last Complete reading backwards
        if(all(diff(complete.rows) == 1)){
          first.complete <- complete.rows[1]
          min.conc <- xx.i$dose[first.complete]
        } else {
          first.complete <- which(diff(rev(complete.rows)) < -1)
          min.conc <- rev(xx.i$dose)[first.complete]
        }
        out.df$MinConc[out.df$Id == i] <- min.conc[1]
      }
    }
  }
  out.df$MinConc[is.na(out.df$MinConc)] <- NA
  out.df
}
