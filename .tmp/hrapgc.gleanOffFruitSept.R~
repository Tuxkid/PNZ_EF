gleanOffFruitSept <- function(choice = 1)
{
### Purpose:- September lot
### ----------------------------------------------------------------------
### Modified from:- gleanOffFruitJuly
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:-  3 Sep 2015, 15:51
### ----------------------------------------------------------------------
### Revisions:- 


  xx <- sept15Off.df
  xx <- within(xx, EfNom <- as.numeric(Efnom))
##   xx <- xx[!is.na(xx$EfNom),]
   xx <- within(xx, Efnom[is.na(Efnom)] <- 0) # no CO2
   xx <- within(xx, Dead[is.na(Dead)] <- 0) # one empty cell should be zero
  is.egg <- grep("egg", levels(xx$Lifestage), ignore.case = TRUE,value = TRUE)
  xx <- within(xx, IsEgg <- Lifestage%in%is.egg)
  is.scale <- grep("OS", levels(xx$SLS), ignore.case = TRUE,value = TRUE)
  xx <- within(xx, IsScale <- SLS%in%is.scale)
  xx <- xx[!is.na(xx$Total), ] # won't total unless

## Define what is dead
  xx <- within(xx, dead <- Dead)
  xx <- within(xx, dead[IsEgg] <- Unhatched[IsEgg])
  xx <- within(xx, Dead[IsScale] <- Dead[IsScale] + Moribund[IsScale])

  xx <- within(xx, Efpc[is.na(Efpc)] <- EfNom[is.na(Efpc)]/2) # might fix later
  xx <- within(xx, Efpc[EfNom ==  0] <- 0) # no controls sometimes otherwise

require(dplyr)
  
##  browser()

  use.df <- xx %>%
    tbl_df %>% arrange(SLS, Temperature, Duration, Rep, Efpc) 

##   use.df2 <- df.sort(xx, rev(c("Pest", "Lifestage", "Temperature",
##                               "Duration", "Rep", "Efpc")))
##   with(use.df, table(Lifestage))  
  ## Check if there's any difference between "Controls"
  if(FALSE){
    test.control <- function(dff){
      dff <- dff[dff$Efpc == 0,]       
      dff <- within(dff, Efnom <- factor(Efnom))
      dff <- within(dff, Mort <- round(dead/Total * 100))
      sls <- unique(dff$SLS)
      for(sl in sls){
        dfs <- dff[dff$SLS == sl,]
      cat("\n\n", sl, ":\n =======================\n\n")
        spec.glm <- glm(cbind(dead, Total - dead) ~ Efnom, data = dfs,
                        family = quasibinomial)
    ##        browser()
        print(anova(spec.glm, test = "Chi"))
      }
    }
    test.control(use.df)
    browser()
  } # Only TSM egg looks close


### Then a normal glean function
  idset <- with(use.df, make.id(Efpc))
  cutx <- NULL
  
  use.df <- within(use.df, Temp <- paste0(Temperature, "°C"))
  use.df <- within(use.df, Hours <- paste0(Duration, "h"))
##  use.df <- within(use.df, DAT <- paste0(Assessed, "d"))
  leg.brief <- with(use.df, unique(paste(SLS, Temp,
                              Hours, Rep, sep= "|")))
  maint <- "Mortality of PNZ targets off fruit in ethyl formate after various durations"
  xlabels <- c(0, 0)
  xaxtitle <- "Dose (%)"
  with(use.df,
       list(id = idset, times = Efpc, total = Total, dead = dead, 
            cutx = cutx, offset = 0, xaxtitle = xaxtitle, maint = maint, 
            legend = leg.brief, xlabels = xlabels, takelog = FALSE))

  
}
