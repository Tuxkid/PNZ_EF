group.responsesD2hS_ak <- function(xx = sept15Off.df, xtitle = "Dose (%)", main = "", yfun = "cloglog", 
           plimits = c(0.04, 0.9995), xlab.line = 2, x.mgp = 0.1,
           x.range = c(0, 3), hours = 2, store = 5, ab.list = ab.sept15OffAll, 
           ylab.line = 1.6, lab.cex = .9, ax.cex = 0.85, crop = "apple",
           main.cex = 1, post = TRUE, ...)
{
### Purpose:- Does separate plots for kiwifruit and apple pests
### ----------------------------------------------------------------------
### Modified from:- group.responsesD2hS
### ----------------------------------------------------------------------
### Arguments:- crop: apple or kiwifruit pests
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 29 Oct 2015, 11:59
### ----------------------------------------------------------------------
### Revisions:- 2/11/2015 removed the crop information (now we know it works)

  require(RColorBrewer)
  require(dplyr)
  g <- get(yfun)
  y.mgp = ifelse(post, 0.7, 0.6)
  ab.lty <- 2
  yy <-  xx %>% tbl_df %>% filter(Duration == hours)
  sls.all <- sort(unique(as.character(yy$SLS)))## all SLS for this duration
##   AP <- sls.all[c(2, 4, 6:11)] # apple pests
##   KP <- sls.all[c(1, 3, 5:7, 11)] # kiwifruit pests
  AP <- c("CM5", "CM Egg", "LBAM 5", "LBAM ME", "OS", "TSM Egg", "WAA", "WFT") # apple pests
  KP <- c("BHLREgg", "CMBEgg", "dTSM", "LBAM 5", "LBAM ME", "WFT") # kiwifruit pests
  
## Fish out the ones we're using in this plot  (but keep information for all sls
  xx <- xx %>% tbl_df %>% filter(Temperature == store, Duration == hours)
##   sls <- sort(unique(as.character(xx$SLS))) ## SLSs in this plot
  if(crop == "apple")
    sls <- AP
  if(crop == "kiwifruit")
    sls <- KP

  ##  browser()

  temps <- unique(xx$Temperature) # that's store value
  colours <- brewer.pal(8, "Dark2")[c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3)]#[c(1:4, 1:4, 1:3)]
##   c(1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4) #
  ltys <- c(rep(2, 4), rep(1, 4), rep(4, 3))
  names(colours) <- names(ltys) <- sls.all
 ## browser()
  colours <- colours[sls]
  ltys <- ltys[sls]
  ab <- ab.list$concJ # could be made adjustable
  data.fun <- ab$datafun
  data.supply <- get(data.fun)
  glean.list <- data.supply() # use to get range of concentrations
  glean.df <- as.data.frame(glean.list[c("id", "times")])
  glean.id.df <- with(glean.list, data.frame(Id = unique(id), SLS = legend))
##   glean.id.df <- within(glean.id.df, Pest <- getbit(as.character(SLS), "\\|", 1))
##   ## just the pests for this plot
##   glean.id.df <- glean.id.df[glean.id.df$Pest %in% sls,]
  id.use <- grep(paste0("\\|", store, "°C\\|", hours, "h"), glean.id.df$SLS)
 
    dfs <- xx 

    durations <- with(dfs, sort(unique(Duration)))
    for(D in durations){ #  single value
      dfsD <- dfs # saves editing
##
      ## Make a pdf of this one
  if(post){
     ##    browser()
    filename <- paste0("Lines_for", temps, "C_", hours, "hS_", crop, ".pdf")
    pdf(file = filename,
        height = 125/25.4, width = 125/25.4, pointsize = 12)
    par(mai = c(16, 12, 8, 3)/25.4, mgp =c(1.75, 0.7, 0), lwd = 2)
    on.exit(dev.off(), add = TRUE)
  }

      tempsD <- with(dfsD, sort(unique(Temperature)))
### Aim is to draw lines for each of tempsD available for this D
      tempsDC <- paste0(tempsD, "°C")
      ab.bits <- paste(sls, tempsDC, paste0(hours, "h"), sep = "|") # ab list
      ab.bits.pest <- getbit(ab.bits, "\\|", 1)
      ab.D <- which(ab$legend%in% ab.bits) # same as id.use (sometimes)
      leg.bits <- ab$legend[ab.D]
      ############### fudge to get 5deg apple to work
##       if(crop == "apple" & store == 5)
        sls <- getbit(leg.bits, "\\|", 1)# usually redundant
      ############## End of fudge
     interceptsD <- ab$intercept[ab.D]
      slopesD <- ab$slope[ab.D]
      ## Corresponding ranges of concentration
      glean.dfD <- glean.df %>% tbl_df %>% filter(id %in% ab.D)
      concsD <- aggregate(times ~ id, glean.dfD, max, na.rm = TRUE)$times
      concsD0 <- aggregate(times ~ id, glean.dfD, function(z)
                           min(z[z > 0], na.rm = TRUE))$times
      names(interceptsD) <- names(slopesD) <-  names(concsD) <- names(concsD0) <- sls
## enough information to do complot-type plot
### browser()

### Calculate adjustments for the space on the y-axis:
      ylow <- g(plimits[1])
      yhigh <- g(plimits[2])
      eps <- (yhigh - ylow) * 0.01
      ylow <- ylow - eps
      yhigh <- yhigh + eps
      labp <- c(1, 5, 10, 25, 50, 75, 95, 99)
      labp <- labp[labp/100 >= plimits[1] & labp/100 <= plimits[2]]
### 
      blank.plot(x.range, c(ylow, yhigh), ...)
      usr <- par()$usr
      epsx <- 0.0125 * diff(usr[1:2])
      epsy <- 0.0125 * diff(usr[3:4])
      midhigh <- 0.5 * (yhigh + usr[4])
      midlow <- 0.5 * (ylow + usr[3])
      hundred.line <- TRUE
      zero.line <- TRUE
## set up scales and border

### Do tricky things to y-axis:
      y.mgp = ifelse(post, 0.7, 0.6)
      mgp <- c(3, y.mgp, 0)
      axis(2, at = g(labp/100), labels = paste(labp), adj = 1, mgp = mgp, cex.axis = 
           ax.cex, las = 1)
      axis(2, at = c(ylow + 2 * epsy, yhigh - 2 * epsy), labels = F, tck = 0, las = 1)
      for(k in 1:2) {
        y.end <- c(ylow, yhigh)[k]
        axis(2, at = c(y.end, usr[2 + k]), labels = F, tck = 0, las = 1)
      }
      if(zero.line)
        abline(h = ylow, lty = ab.lty)
      if(hundred.line)
        abline(h = yhigh, lty = ab.lty)
      abline(h = g(0.99), lty = 5, col = "grey")
      axis(2, at = midlow, tck = 0, labels = "0", adj = 1, mgp = mgp, cex.axis = 
           ax.cex, las = 1)
      lines(usr[1] + c( - epsx, epsx), c(ylow - 1.5 * epsy, ylow + 1.5 * epsy), 
            xpd = TRUE)
      lines(usr[1] + c( - epsx, epsx), c(ylow + 0.5 * epsy, ylow + 3.5 * epsy), 
            lty = 1, xpd = TRUE)
      axis(2, at = midhigh, tck = 0, labels = "100", adj = 1, mgp = mgp, cex.axis = 
           ax.cex, las = 1)
      lines(usr[1] + c( - epsx, epsx), c(yhigh - 1.5 * epsy, yhigh + 1.5 * epsy),
            xpd = TRUE)
      lines(usr[1] + c( - epsx, epsx), c(yhigh - 3.5 * epsy, yhigh - 0.5 * epsy),
            lty = 1, xpd = TRUE)    #
      
      box(bty = "]")      
       axis(1, cex.axis = ax.cex, mgp = mgp - c(0, x.mgp, 0))     
### Draw X and Y labels
      mtext(side = 1, line = xlab.line, text = xtitle, cex = lab.cex)
      mtext(side = 2, line = ylab.line, text = paste("% Mortality,", yfun, "scale"),
            cex = lab.cex)
      mtext(side = 3, text = paste0(store, "°C"), cex = main.cex, adj = 0)
      mtext(side = 3, text = paste0(D, "h duration"), adj = 1, cex = lab.cex)
##       mtext(side = 3, text = paste0(crop, " pests"), adj = .5, cex = lab.cex)
## removed after we got the function working.
      
### Draw each mortality line and a legend
      draw.leg <- logical(length(concsD)) # draw legend iff this is TRUE
      names(draw.leg) <- names(concsD)
      for(i in names(concsD)){
       success <- try(clipline(c(concsD0[i], concsD[i]), c(-3, 2), interceptsD[i], slopesD[i],
                     col = colours[i], lty = ltys[i], lwd = 3))
      ##       
        if(inherits(success, "try-error"))
          cat(i, "doesn't have any data\n") else
        draw.leg[i] <- TRUE
      }
      draw.leg <- draw.leg[draw.leg]
      leg.labs <- names(draw.leg)
      leg.labs <- gsub("LRE", "LR E", leg.labs)
      leg.labs <- gsub("MBE", "MB E", leg.labs)
      leg.labs <- gsub("CM5", "CM 5", leg.labs)
      leg.labs <- gsub("5", "5*", leg.labs)
      leg.labs <- gsub("ME", "Egg", leg.labs)
      leg.labs <- leg.labs[draw.leg]
      bar.legs(.72, .44, labs =leg.labs, cols = colours[names(concsD)][draw.leg], pchs = NULL,
               leg.cex = ax.cex, ygap = 0.038, 
               ltys = ltys[names(concsD)][draw.leg], line.leng = .1, line.lwd = 3)
    }  
}
