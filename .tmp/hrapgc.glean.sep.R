glean.sep <- function(choice = 1)
{
### Purpose:- "September" data BHLR
### ----------------------------------------------------------------------
### Modified from:- glean.juneE
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 14 Oct 2014, 16:07
### ----------------------------------------------------------------------
### Revisions:- 


  species <- unique(sep.df$Species)
  species.abbr <- c("LTMB", "WAA", "GHT", "SJS")
  sep.df <- sep.df[sep.df$Efg != "Handling control",]
  sep.df <- within(sep.df, Efppm[is.na(Efppm)] <- 0)
  sep.df <- sep.df[, -7]
  sep.df <- within(sep.df, Live <- Live + Moribund)
##   sjs.df <- sep.df[sep.df$Species == "San Jose Scale",]
##   sjs.df <- df.sort(sjs.df, c("Efppm", "Rep", "Lifestage"))
##   sjs.df <- within(sjs.df, Lifestage[Lifestage == "Crawler"] <- "Crawler/1*")
##   sjs.df <- within(sjs.df, Lifestage[Lifestage == "1*"] <- "Crawler/1*")
##   sjs.df <- within(sjs.df, Lifestage[Lifestage == "2* female"] <- "2*")
##   sjs.df <- within(sjs.df, Lifestage[Lifestage == "2* male"] <- "2*")
##   sjs.df <- within(sjs.df, Lifestage[Lifestage == "3* mature"] <- "3*")
##   sjs.df <- within(sjs.df, Dead <- Dead + Moribund)
##   sjs.df$Species <- "SJS"


##  browser()
  use.df <- df.sort(sep.df, rev(c("Species", "Lifestage", "Rep", "Efppm")))
  with(use.df, table(Lifestage))  
  ## Check if there's any difference between "Controls"
  if(FALSE){
    test.control <- function(dff){
      dff <- dff[dff$Efppm == 0,]
      dff <- within(dff, Efg <- factor(Efg))
      dff <- within(dff, Mort <- round(Dead/Total * 100))
      
      spec.glm <- glm(cbind(Dead, Live) ~ Lifestage + Efg, data = dff,
                      family = quasibinomial)
      browser()
      anova(spec.glm, test = "Chi")
    }
    test.control(sep.df)
    browser()
  }


### Get rid of Rep 8 of Early eggs
  use.df <- use.df[!with(use.df, Lifestage == "Early eggs" & Rep == 8),]
  
### The a normal glean function
  idset <- with(use.df, make.id(Efppm))
  cutx <- NULL
  leg.brief <- with(use.df, unique(paste(Lifestage, Rep, sep= "|")))
  maint <- "Mortality of BHLR in ethyl formate after two hours"
  xlabels <- c(0, 0)
  xaxtitle <- "Dose (%)"
  with(use.df,
       list(id = idset, times = Efppm, total = unlist(Dead) + unlist(Live),
            dead = Dead, 
            cutx = cutx, offset = 0, xaxtitle = xaxtitle, maint = maint, 
            legend = leg.brief, xlabels = xlabels, takelog = FALSE))

  
}
