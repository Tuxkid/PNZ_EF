simplot <-
structure(function(x, resp, tot, fun, cm.strategy = "adjust.later", cm = 0, cm.code = 
           NULL, cm.allcodes = NULL, xtit = "Time in Min", main = "", mkh = 0.025, 
           xfun = function(x) x, takelog = F, line = F, ab = c(NA, NA),
           clip = NULL, xlimits = c(0, 0), 
           xlabels = c(0, 0), offset = 0, title.line = 1, plimits = c(0.0025, 0.9995),
           ytit = NULL, new = F, lty = 1, plot.char = c(0, 1, 2, 5, 6:16),
           id = NULL, points.lwd = 1)
{
  if(missing(mkh))
    mkh <- par()$cin[2]/3
  if(new)
    par(new = T)
  g <- link.function(fun)
  m <- length(x)
  if(is.null(id))
    id <- 1
  if(length(id) == 1)
    id <- rep(id, length(x))
  if(takelog)
    xfun <- log
  if(all(xfun(exp(1:8)) == (1:8))) takelog <- T    
### *****************************************************************
  if(is.null(cm.code))
    cm.code <- 0
  if(is.null(cm.allcodes))
    cm.allcodes <- cm.code
  other.rows <- match(x, cm.allcodes, nomatch = 0)
  cm.here.codes <- unique(cm.allcodes[other.rows])
  cmrows <- as.logical(other.rows)
  if(cm.strategy == "adjust.later" & !takelog)
    here.trt <- !cmrows | x == "0"
  else here.trt <- !cmrows
  x.trt <- as.numeric(x[here.trt])
  resp.trt <- resp[here.trt]
  tot.trt <- tot[here.trt]
  id.trt <- id[here.trt]
  if(missing(xlimits))
    xlimits <- range(x.trt)
  if(takelog) {
    if(any(x.trt + offset < 0)) {
      cat(
          "*** Warning: The following x-values have been omitted because\n x"
          )
      if(offset != 0)
        cat("+", offset)
      cat(" < 0:", fill = T)
      cat(format(x.trt[x.trt + offset < 0]), fill = T)
    }
    if(any(x.trt + offset <= 0)) {
      x.logable <- x.trt + offset > 0
      resp.trt <- resp.trt[x.logable]
      tot.trt <- tot.trt[x.logable]
      x.trt <- x.trt[x.logable]
      id.trt <- id.trt[x.logable]
    }
    if(xlimits[1] + offset <= 0)
      xlimits[1] <- min(x.trt[x.trt + offset > 0], na.rm = T)
  }
  if(diff(range(xlabels)) == 0)
    xlabels <- pretty(xlimits)
  if(takelog)
    xlabels <- xlabels[xlabels > 0]
  cat(xlabels, "  ")    # xlabels specifies numeric labels
  p <- resp.trt/tot.trt
  prange <- range(p)
  if(prange[1] < 0 | prange[2] > 1) {
    cat("Error: Smallest p is", prange[1], fill = T)
    cat("     Largest p is", prange[2], fill = T)
    break
  }
  cm0 <- switch(cm.strategy,
                adjust.later = 0,
                abbott = cm)
  here <- p > cm0 & p < 1
  p.adj <- (p - cm0)/(1 - cm0)
  ylow <- g(plimits[1])
  yhigh <- g(plimits[2])
  eps <- (yhigh - ylow) * 0.01
  ylow <- ylow - eps
  yhigh <- yhigh + eps
  labp <- c(1, 5, 25, 50, 75, 95, 99, 99.9)
  labp <- labp[labp/100 > plimits[1] & labp/100 < plimits[2]]
  aty <- g(labp/100)
  dist <- yhigh - ylow
  if(is.null(ytit)) {
    fun.text <- fun
    ytit <- paste("% Mortality,", fun.text, "scale")
  }
  par(mkh = mkh)
  u.id <- unique(id.trt)
  xlim.here <- xfun(xlimits + offset)
  if(takelog)
    xlim.here <- xlim.here - c(0.115 * diff(xlim.here), 0)
  x.cpos <- xlim.here[1]
  if(sum(here) > 0) {
    plot(xfun(x.trt + offset)[here], g(p.adj[here]), xlim = xlim.here, ylim
         = c(ylow, yhigh), xlab = "", ylab = ytit, axes = F, type = "n", cex.lab = 1.2)
  }
  else plot(xfun(xlimits + offset), c(ylow, yhigh), xlim = xlim.here, xlab = 
            "", ylab = ytit, axes = F, type = "n")
  midhigh <- 0.5 * (yhigh + par()$usr[4])
  midlow <- 0.5 * (ylow + par()$usr[3])
  chw <- par()$cxy[1]
  chh <- par()$cxy[2]
  j <- 0
  for(i in u.id) {
    j <- j + 1
    here.i <- here & i == id.trt
    g.i <- g(p.adj[here.i])
    if(any(here.i) & any(g.i < ylow | g.i > yhigh)) {
      cat("\n*** One or more points is outside limits set by plimits.***\n"
          )
      cat("\n*** plimits =", format(round(plimits, 4)), "***", "\n")
      cat("\n*** Points are:\n")
      if(any(g.i < ylow)) {
        cat(format(round(p.adj[g.i < ylow], 4)), "\n")
        g.i[g.i < ylow] <- ylow
      }
      if(any(g.i > yhigh)) {
        cat(format(round(p.adj[g.i > yhigh], 4)), "\n")
        g.i[g.i > yhigh] <- yhigh
      }
    }
    if(any(here.i))
      points(xfun(x.trt + offset)[here.i], g(p.adj[here.i]), pch = 
             plot.char[j], mkh = mkh)
    x100 <- x.trt[p == 1 & i == id.trt]
    x0 <- x.trt[p == 0 & i == id.trt]
    x0.100 <- c(x0, x100)
    y0.100 <- c(rep(midlow, length(x0)), rep(midhigh, length(x100)))
    if(length(x0.100) > 0) {
      points(xfun(x0.100 + offset), y0.100, pch = ".", mkh = mkh)
      points(xfun(x0.100 + offset), y0.100, pch = plot.char[j], mkh = mkh)
    }
  }
  if((length(cm.here.codes) > 0) & cm.strategy == "adjust.later") {
    u.cm <- unique(cm.here.codes)
    for(cm.i in u.cm) {
###       here.cm <- x == cm.i ## Try character 8/9/04
      here.cm <- as.character(cm.i)  == x
      p.cm <- resp[here.cm]/tot[here.cm]
      here.cm <- here.cm[p.cm > 0]
      p.cm <- p.cm[p.cm >= 0]## add in equality
                                        #browser()
      if(sum(here) > 0)
        text(rep(x.cpos, length(p.cm)), g(p.cm), rep(cm.i, length(p.cm)))
    }
  }
  if(line) {
    b <- ab[2]
    a <- ab[1]
    if(is.na(b))
      b <- -999
    if(is.null(clip))
      clip <- xlimits
    if(b > 0)
      clipline(xfun(clip + offset), c(ylow, yhigh - chh/3), a, b)
  }
  xmax <- max(x.trt)
  epsx <- 0.0125 * diff(par()$usr[1:2])
  epsy <- 0.0125 * diff(par()$usr[3:4])
  if(takelog) {
    x.cut1 <- x.cpos + epsx
    x.cut2 <- x.cpos + 2.5 * epsx
    here.pos <- xfun(xlabels + offset) > x.cut2 & xlabels <= xmax
  }
  else here.pos <- xlabels <= xmax
  xpos <- xlabels[here.pos]
  if(length(xpos) <= 2)
    xpos <- xlabels[xlabels <= xmax]
###   if(FALSE) {
###       if(names(dev.cur()) == "postscript" & nchar(main) > 0)
###         mixed.mtext(side = 3, line = title.line, texts = main, cex = par()$cex * 
###                     1.2, adj = 0.5)
###       else
###       }
  mtext(side = 3, line = title.line, text = main, cex = par()$cex * 1.1)
  title(xlab = xtit, cex.lab = 1.2)
  axis(1, at = xfun(xpos + offset), labels = F, cex.axis = .8)
  xusr1 <- par()$usr[1]
  axis(1, at = xfun(xpos + offset), labels = paste(xpos), mgp = c(2, 0.5, 0), cex.axis = .8)
  if(takelog) {
    axis(1, at = c(par()$usr[1], x.cut1), labels = F, tck = 0, mgp = c(2, 
                                                                 0.5, 0))
    axis(1, at = c(x.cut2, par()$usr[2]), labels = F, tck = 0, mgp = c(2, 
                                                                 0.5, 0))
    lines(x.cut1 + c( - epsx, epsx), par()$usr[3] + 2 * c( - epsy, epsy), 
          xpd = T)
    lines(x.cut2 + c( - epsx, epsx), par()$usr[3] + 2 * c( - epsy, epsy), 
          xpd = T)
  }
  else axis(1, at = par()$usr[1:2], labels = F, tck = 0, mgp = c(2, 0.5, 0), cex.axis = .8)
  pr <- c(1, 5, 20, 50, 80, 99)
  extreme.pr <- c(0, 100)
  npr <- length(pr)
  if(all(g(pr/100) == pr/100))
    identity <- T
  else identity <- F
  if(identity)
    pr <- c(0, 25, 50, 75, 100)
  else pr <- pr[g(pr/100) > ylow & g(pr/100) < yhigh]
  axis(2, at = g(pr/100), labels = paste(pr), cex.axis = .8)
  if(identity) {
    midhigh <- g(1)
    axis(2, at = (0:20)/20, labels = F)
  }
  if(!identity) {
    axis(2, at = c(ylow + 2 * epsy, yhigh - 2 * epsy), labels = F, tck = 0)
    for(k in 1:2) {
      y.end <- c(ylow, yhigh)[k]
      axis(2, at = c(y.end, par()$usr[2 + k]), labels = F, tck = 0)
    }
    kk <- (1:2)[c(length(x0) > 0, length(x100) > 0)]
    for(k in kk) {
      y.mid <- c(midlow, midhigh)[k]
      axis(2, at = y.mid, tck = 0, labels = paste(extreme.pr[k]), cex.axis = .8)
      y.end <- c(ylow, yhigh)[k]
      k <- k + 1
      abline(h = y.end, lty = 3)
      lines(xusr1 + c( - epsx, epsx), c(y.end - 1.5 * epsy, y.end + 1.5 * 
                                        epsy), xpd = T)
      ## 1/5/06 fixed up long running error with this: ifelse(k == 2, 4, 0) * epsy +
      lines(xusr1 + c( - epsx, epsx), ifelse(k == 2, 4, 0) * epsy +
            c(y.end - 3.5 * epsy, y.end - 0.5 * epsy), lty = lty, xpd = T)
      
    }
  }
  else axis(2, at = par()$usr[3:4], labels = F, tck = 0)
  box(bty = "7")
}
, comment = "12/06/2008")
