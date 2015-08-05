flyplot <-
function(mn = 1:12, ri = 3, cj = 4, line = T, cutoff.mono = NULL, data = NULL, 
           datafun = NULL, choice = 1, plimits = c(0.1, 0.9995), link = "cloglog", 
           range.strategy = "page", fit.trend = "line",
           pc = c(line = 50, line = 99), span.mono = 1,
           n.strategy = "mn", xlabels = NULL,
           xfun = function(x) x, takelog = F, before = NA, new.page = T,
           mkh = 0.025, cex = NULL, maint.line = 1, title.line = 0.025,
           title.space = 0, xtitle = NULL, ytitle = NULL, ext = NULL,
           lt.ld = "LT", lt.rnd = 1, datestamp = TRUE,
           byrow = TRUE, mtext.line = 2, out.cex = 1.3)
{
### Title for data subset j is maint
### Prints graphs 4 to a page, starting with graph for data subset j=i
### dset holds all the data
### values of idset (1, 2, ...) identify subsets of dset
### range.strategy may be "individual", or "page", or "all"
### fit.trend may be line, or mono(tone), or mono.line
### mono.line gives monotone fit, then line
### lt.rnd: how much rounding of LTs 
### Set pc to decide which LTs will appear on the margin of the plot.
  ##
  options(width = 200)
  on.exit(options(width = 80))
  trend.4 <- substring(fit.trend, 1, 4)
  do.abers <- trend.4 == "mono" | trend.4 == "loes"
  oldpar <- par() 
  on.exit(par(oldpar), T)    #
### Set up different par parameters for postscript and motif devices.
  
  if(names(dev.cur()) == "pdf"| names(dev.cur()) == "postscript") {
    omi <- c(10, 12 + title.space/3, 12 + title.space, 12 + title.space/3)/25.4
    mgp <- c(1.75, 0.7, 0)
    mai <- c(9, 10, 5, 1)/25.4
  }
  else {
    omi <- c(5, 5 + title.space/3, 7 + title.space, 10 + title.space/3)/25.4
    mgp <- c(2.25, 1, 0)
    mai <- c(15, 20, 10, 0)/25.4
  }
  
  if(new.page & byrow)
    par(mai = mai, mgp = mgp, mfrow = c(ri, cj), omi = omi)
  else if(new.page & !byrow)
    par(mai = mai, mgp = mgp, mfcol = c(ri, cj), omi = omi)
  if(is.null(cex) & (ri * cj == 6))
    cex <- 0.65
  if(!is.null(cex))
    par(cex = cex)
  final.choice <- choice[length(choice)]
  if(!is.null(data)) {
    ab <- data[[final.choice]]
    leg.ab <- ab$leg.brief
    if(is.null(leg.ab))
      leg.ab <- ab$legend
    choice.call <- ab$choice
    if(missing(takelog) & missing(xfun)) {
      takelog <- ab$takelog
      if(takelog)
        xfun <- log
    }
  }
  else {
    ab <- NULL
    leg.ab <- NULL
    choice.call <- choice
  }
  if(missing(datafun) & !is.null(ab))
    datafun <- get(ab$datafun)
  if(!is.null(ext))
    u <- datafun(choice.call, ext = ext)
  else u <- datafun(choice.call)
  if(is.null(leg.ab))
    leg.ab <- u$leg.brief
  if(!is.null(pc)) {
    lt.txts <- paste(pc)
    lt.prefix <- paste(lt.ld, pc, "=", sep = "")
    lt.strategy <- names(pc)
  }
  else {
    lt.strategy <- NULL
    lt.txts <- NULL
  }
  if(missing(ext))
    ext <- NULL
  if(!is.null(ab) & !is.null(lt.strategy)) {
    first.one <- switch(lt.strategy[1],
                        loess = ,
                        monotone = ,
                        mono = "lt.monotone",
                        line = "lt")
    ltmat.leg <- matrix(, dim(ab[[first.one]])[1], length(pc))
    
    for(k in seq(along = lt.strategy)) {
      which.lt <- switch(lt.strategy[k],
                         loess = ,
                         monotone = ,
                         mono = "lt.monotone",
                         line = "lt")
      ltmat.leg[, k] <- ab[[which.lt]][, lt.txts[k]]
    }
    dp <- max(c(3 - ceiling(log(quantile(ab[[first.one]], probs = 0.75, 
                                         na.rm = T))/log(10)), 0))
  }
  else ltmat.leg <- NULL
  if(!is.null(ltmat.leg)) {
    if(is.null(leg.ab))
      leg.ab <- rep("", dim(ltmat.leg)[1])
    ##    browser()
    leg.lt <- apply(ltmat.leg, 1, function(x, z, u, dp)
                    paste(z, round(x, dp), collapse = "; ", sep = ""),
                    z = lt.prefix, dp = dp)
    
    legend <- paste(leg.ab, " [", leg.lt, "]", sep = "")
  }
  else if(!is.null(leg.ab))
    legend <- leg.ab
  else if(length(lt.txts) > 0)
    cat(paste(paste("LT", lt.txts, sep = ""), collapse = ""), 
        "values are not available, perhaps because pc was not given correctly\n"
        )
  cutx <- u$cutx
  offset <- u$offset
  cm <- NULL
  if(!is.null(ab)) {
    cm.code <- ab$cm.code
    cm <- ab$cm
    cm.strategy <- ab$cm.strategy
    cmtot <- u$cmtot
  }
  else {
    cm.code <- u$cm.code
    cm <- u$cm
    cmtot <- u$cmtot
    cm.strategy <- u$cm.strategy
  }
  if(is.null(cm.strategy))
    cm.strategy <- "adjust.later"
  if(is.null(cm.code))
    cm.code <- 0
  cm.allcodes <- u$cm.allcodes
  if(is.null(cm.allcodes))
    cm.allcodes <- cm.code
  times.all <- as.character(u$time)
  tot.all <- u$tot
  other.rows <- as.logical(match(times.all, cm.allcodes, nomatch = 0))
  keep <- rep(T, length(other.rows))
  if(any(is.na(as.numeric(times.all[!other.rows])))) {
    cat("\n*** NA(s) appear among non-control times ***\n")
    numit <- times.all[!other.rows]
    look <- is.na(as.numeric(numit))
    print(data.frame(times = numit[look], totals = tot.all[!other.rows][look
                                            ], dead = u$dead[!other.rows][look]))
    keep[!other.rows][look] <- F
  }
  if(any(is.na(tot.all) | tot.all == 0)) {
    look <- is.na(tot.all) | tot.all == 0
    cat("\nNAs or zeros appear among totals\n")
    print(data.frame(times = times.all[look], total = tot.all[look], dead = 
                     u$dead[look]))
    keep[look] <- F
  }
  time.txt <- times.all[keep]
  times <- array( - Inf, length(time.txt))
  use.trt <- !other.rows[keep] | time.txt == "0"
  times[use.trt] <- as.numeric(time.txt[use.trt])
  xmin <- min(times[use.trt])
  idset <- u$id[keep]
  tot <- u$total[keep]
  dead <- u$dead[keep]
  if(missing(xtitle))
    xaxtitle <- u$xaxtitle
  else xaxtitle <- xtitle
  treat <- u$treat
  maint <- u$maint
  if(is.null(xlabels))
    xxlabels <- u$xlabels
  else xxlabels <- xlabels
  nu <- unique(idset)
  nn <- match(nu, idset)    # bigstage <- dset[, stagecol]
  n <- ri * cj
  if(max(mn) > length(nu)) {
    maxj <- length(nu)
    cat("You have specified plots up to plot no.", max(mn), 
        "\nwhereas data is available for", maxj, "plots only.\n")
    mn <- mn[mn <= maxj]
  }
  graphnum <- mn
  maxj <- max(mn)
  if(range.strategy != "individual")
    common.range <- switch(range.strategy,
                           page = mn,
                           all = 1:length(nu))
  todo <- length(nu) - maxj
  if(todo > 0)
    cat("Datasets", maxj + 1, "to", length(nu), "will remain after this.", 
        fill = T)
  xmin <- min(times)
  if(takelog)
    xmin <- min(times[times + offset > 0])
  xrange <- switch(cm.strategy,
                   adjust.later = range(times),
                   abbott = range(times))
  ij <- 0
  if(range.strategy != "individual") {
    xrm <- matrix(, 2, length(common.range))
    for(k in common.range) {
      ij <- ij + 1
      timtim <- times[idset == nu[k]]
      xrm[, ij] <- switch(cm.strategy,
                          adjust.later = range(timtim),
                          abbott = range(timtim))
    }
    xrange <- range(xrm)
  }
  if(!missing(before))
    xrange[2] <- before
  ij <- 0
  cmk <- NULL
  for(k in graphnum) {
    cat(paste("attempting Graph No", k, "\n"))
    leg.k <- legend[k]
    ij <- ij + 1
    j <- nu[k]
    take <- idset == j
    timesj <- times[take]
    deadj <- dead[take]
    totj <- tot[take]
    time.txj <- time.txt[take]
    cmrows <- time.txj == cm.code[1]
    second.cm <- F
    if(sum(cmrows) == 0 & length(cm.code) > 1) {
      cmrows <- time.txj == cm.code[2]
      second.cm <- T
    }
    else second.cm <- F
    if(!is.null(cmtot))
      numcm <- cmtot[k]
    else numcm <- NULL
    if(is.null(numcm)) numcm <- 0    # numcm may be reset below
    if(is.null(cm)) {
      if(sum(cmrows) > 0) {
        if(any(timesj[cmrows] !=  - Inf & timesj[cmrows] != 0))
          cat("\n*** Warning: Fault in specification of cm rows ***", 
              "\n")
        dead0 <- sum(deadj[cmrows])
        tot0 <- sum(totj[cmrows])
        cmk <- dead0/tot0
        numcm <- tot0
        n.cm <- sum(cmrows)
        cmspeak <- paste("   cm(obs) =", format(round(cm, 3)), "(from", 
                         n.cm, paste("point", switch((n.cm > 1) + 1,
                                                     "",
                                                     "s",
                                                     ), ")", sep = ""))
        if(second.cm)
          cmspeak <- paste(cmspeak, "  cm code (2nd choice) =", cm.code[2
                                                                        ])
      }
      else cmspeak <- "No information is available on cm"
    }
    else {
                                        # !is.null(cm)
      cmk <- cm[k]
      if(cm.strategy == "abbott") {
        cat("Take cm =", format(round(cmk, 3)), "\n")
        take <- take & !other.rows
      }
    }
    if(do.abers) {
      use.mono <- use.trt[take]
      if(!is.null(cutoff.mono))
        use.mono[timesj[use.mono] < cutoff.mono] <- F
      time.here <- timesj[use.mono]
      ord.x <- order(time.here)
      subs.here <- (1:length(use.mono))[use.mono][ord.x]
      dead.mono <- deadj[subs.here]
      tot.mono <- totj[subs.here]
      p.mono <- (dead.mono/tot.mono)
      time.mono <- time.here[ord.x]
      other.take <- other.rows[take]
      x.cm <- time.txt[take][other.take]
      dead.cm <- deadj[other.take]
      tot.cm <- totj[other.take]
      if(length(time.mono) < 2)
        do.abers <- F
    }
    if(do.abers) {
      if(link == "loglog")
        link <- "cloglog"
      lt.mono <-
        pool.adj(time.mono, dead.mono, tot.mono, phat = NULL, cm = cmk,
                 cm.strategy = cm.strategy, cm.allcodes = cm.allcodes, 
                 plimits = plimits, xtit = xaxtitle, plotit = fit.trend,
                 link = link, xfun = xfun, legend = leg.k, x.cm = x.cm,
                 dead.cm = dead.cm, tot.cm = tot.cm, span = span.mono)
    }
    if(range.strategy == "individual")
      xrange <- switch(cm.strategy,
                       adjust.later = ,
                       abbott = range(times[take]))
    if(diff(range(xxlabels)) > 0)
      xlabels <- xxlabels
    else xlabels <- pretty(range(times[take]))
    if(line) {
      ltab <- ab
      b <- ltab[["slope"]][k]
      a <- ltab[["intercept"]][k]
    }
    else {
      a <- -999
      b <- -999
    }
    startx <- max(xmin, cutx[k])
    jk <- switch(n.strategy,
                 id = j,
                 mn = k)
    if(!do.abers) {
### Try an expression in title
      exp.main <- NULL
      for(m in seq(ncol(ltmat.leg))){
        lt.m <- lt.txts[m]
        ltmat.leg.m <- round(ltmat.leg[k, m], lt.rnd)
        main.m <- substitute(lt.ld[lt.m] == ltmat.leg.m,
                             list(lt.ld = lt.ld, lt.m = lt.m,
                                  ltmat.leg.m = ltmat.leg.m))
        if(!is.null(exp.main))
          exp.main <- substitute(paste(exp.main, "; ", main.m),
                                 list(exp.main = exp.main, main.m = main.m))
        else exp.main <- substitute(main.m, list(main.m = main.m))
      }

      ## Now, getting plot No and ID in same string:
      leg.ab.jk <- leg.ab[jk]
      plot.id <- ppaste(jk, ": ", leg.ab.jk)
      ## join in the LT info to use as main in this plot
      exp.main.jk <- substitute(paste(plot.id, ' [', exp.main, ']'),
                                list(exp.main = exp.main,
                                     plot.id = plot.id))      
      simplot(time.txt[take], deadj, totj, link, cm = cmk,
              cm.strategy = cm.strategy, xtit = xaxtitle,
              ytit = ytitle, main = exp.main.jk, xfun = xfun,
              takelog = takelog, line = line,
              ab = c(a, b), clip = c(startx, max(timesj)), xlimits = xrange, 
              xlabels = xlabels, offset = offset, plimits = plimits, mkh = mkh, 
              title.line = title.line, id = NULL)
    
    
###     if(names(dev.cur()) == "postscript" & nchar(maint) > 0)
###       mixed.mtext(texts = maint, side = 3, outer = T, cex = 1.25, line = 
###                   maint.line, adj = 0.5)
###     else
    
    }
    
    mtext(maint, 3, outer = T, cex = out.cex, font = 2, line = maint.line)
  }
  if(cm.strategy == "abbott")
    mtext("Data is plotted following Abott's adjustment", 3, line = 
          maint.line - 1, outer = T, cex = 0.5)
  lt.leg <- paste(paste("lt", pc, ":", names(pc), sep = ""), collapse = "; ")
  par(cex = 0.4)
  
  if(datestamp) {
    r.txt <- paste(lt.leg, "   ", system("date +%d/%m/%y'  '%H:%M", TRUE))
    l.txt <- system("pwd", TRUE) ## get call to work sometime, deparse(match.call())
    mtext(r.txt, side = 1, adj = 1, outer = T, line = mtext.line, cex = .6)
    mtext(l.txt, side = 1, adj = 0, outer = T, line = mtext.line, cex = .6)
  }
}
