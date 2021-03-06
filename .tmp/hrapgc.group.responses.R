group.responses <-
  function(xx = june15Off.df, xtitle = "Dose (%)", main = "", yfun = "cloglog", 
           plimits = c(0.04, 0.9995), xlab.line = 2, x.mgp = 0.1,
           x.range = c(0, 3), ab.list = ab.june15Off,
           rha = "Duration", ylab.line = 1.6, lab.cex = .9, ax.cex = 0.85,
           main.cex = 1, post = TRUE, ...)
{
### Purpose:- Plot groups of mortality responses on one graph
### ----------------------------------------------------------------------
### Modified from:- complot mostly
### ----------------------------------------------------------------------
### Arguments:- rha: arranged by Duration or Temperature
### 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 10 Jun 2015, 09:59
### ----------------------------------------------------------------------
### Revisions:- 15th July 2015 add in ab.list arg.

  if(post){
    pdf(file = ppaste("Multimort_by", rha, ".pdf"),
        height = 195/25.4, width = 255/25.4, pointsize = 12)
    par(mai = c(10, 12, 8, 3)/25.4,
        mgp =c(1.75, 0.7, 0),
        mfrow = c(3, 4))
    on.exit(dev.off(), add = TRUE)
  }
  require(RColorBrewer)
  require(dplyr)
  g <- get(yfun)
  y.mgp = ifelse(post, 0.7, 0.6)
  ab.lty <- 2
  sls <- levels(xx$SLS)
  temps <- unique(xx$Temperature)
  colours <- brewer.pal(8, "Dark2")[seq(temps)]
  names(temps) <- names(colours) <- paste0(temps, "°C")
  
  ab <- ab.list[[2]] # could be made adjustable
  data.fun <- ab$datafun
  data.supply <- get(data.fun)
  glean.list <- data.supply() # use to get range of concentrations
  glean.df <- as.data.frame(glean.list[c("id", "times")])
  for(sl in sls){
    dfs <- xx[xx$SLS == sl,]
##      browser()
##    cat("\n\n", sl, ":\n =======================\n")
    with(dfs, print(table(Duration, Temperature)))
    durations <- with(dfs, sort(unique(Duration)))
    for(D in durations){
      dfsD <- dfs[dfs$Duration == D,]
      tempsD <- with(dfsD, sort(unique(Temperature)))
### Aim is to draw lines for each of tempsD available for this D
      tempsDC <- paste0(tempsD, "°C")
      ab.bits <- paste(sl, tempsDC, paste0(D, "h"), sep = "|") # ab list
      ab.D <- which(ab$legend%in% ab.bits)
      interceptsD <- ab$intercept[ab.D]
      slopesD <- ab$slope[ab.D]
      ## Corresponding ranges of concentration
      glean.dfD <- glean.df %>% tbl_df %>% filter(id %in% ab.D)
      concsD <- try(aggregate(times ~ id, glean.dfD, max, na.rm = TRUE)$times)
      if(inherits(concsD, "try-error"))
browser()
        
         concsD0 <- aggregate(times ~ id, glean.dfD, function(z)
                           min(z[z > 0], na.rm = TRUE))$times
      names(interceptsD) <- names(slopesD) <-  names(concsD) <-
        names(concsD0) <- tempsDC
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
      x.range <- c(0, 3)
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
      mtext(side = 3, text = sl, cex = main.cex, adj = 0)
      mtext(side = 3, text = paste0(D, "h duration"), adj = 1, cex = lab.cex)
      
### Draw each mortality line and a legend
      for(i in names(concsD))
        try(clipline(c(concsD0[i], concsD[i]), c(-3, 2), interceptsD[i], slopesD[i],
                     col = colours[i], lwd = 1.5))
      bar.legs(.7, .3, labs = names(concsD), cols = colours[names(concsD)], pchs = NULL,
               leg.cex = ax.cex,
               ltys = rep(1, length(concsD)), line.leng = .1, line.lwd = 1.5)
    }
  }
}
