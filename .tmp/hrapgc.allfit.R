allfit <- function(choice = 1, mn = NULL, xfun = function(x)
           x, xfun.inv = NULL, datafun = glean.ramp, link = "cloglog",
           interval = T, pc = c(50, 95, 99), pc.monotone = NULL,
           plot.monotone = NULL, cutoff.mono = 0, span.mono = 1,
           pc.spline = NULL, plot.spline = F, 
           p.min = NULL, printdat = F, save.resid = F,
           cm.strategy = "adjust.later", 
           ext = "", cm.code = NULL, print.plot = F, full.robust = F, ...)
{
  ## Purpose: 
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments: 
  ## ----------------------------------------------------------------------
  ## Author: John Maindonald, Creation date:  7 Sep 1994 (about)
  ## Changed May 1999 to make pc = 95 part of the default
  ## Added id.total 14/7/2014
  ## Added check for negative slopes 16/7/2014

  xtra <- list(...)
  new.page <- T
  if(!is.null(names(xtra))) {
    h <- xtra$h
    ri <- xtra$ri
    cj <- xtra$cj
    if(any(is.na(match(names(xtra), c("h", "ri", "cj")))))
      cat("\n*** There are one or more illegal parameters to allfit()\n", 
          "from among the names:", names(xtra), "***\n")
  }
  else {
    ri <- 3
    cj <- 4
    h <- NULL
  }
  if(is.character(choice))
    cat(choice)
  if((!is.null(plot.monotone)) | plot.spline) {
    oldpar <- par(mfrow = c(ri, cj), oma = c(0, 1, 5, 0))
    on.exit(par(oldpar))
  }
  cat("   ", date(), fill = T)
  cat("** Link =", link, "**  x-transformation =", as.character(substitute(
                                                                           xfun)), "**", fill = T)
  if(!is.null(h))
    u <- datafun(choice = choice, h = h)
  else u <- datafun(choice = choice)
  if(is.null(cm.code))
    cm.code <- u$cm.code
  if(is.null(cm.code))
    cm.code <- 0
  cm.allcodes <- u$cm.allcodes
  temp <- u$temp
  stage <- u$stage
  if(all(xfun(exp(1:5)) == (1:5)))
    takelog <- T
  else takelog <- F
  idset <- u$id
  uset <- u$trial.id.order
  if(is.null(uset))
    uset <- unique(idset)
  if(is.null(mn))
    mn <- 1:length(uset)
  if(max(mn) > length(uset)) {
    maxj <- length(uset)
    cat("You have specified a final dataset no.", max(mn), "\nwhereas", maxj,
        "datasets only are available\n")
    keep <- mn <= maxj
    if(sum(keep) > 0)
      mn <- mn[keep]
    else stop(paste("Dataset(s)", mn, "not found."))
  }
  alltimes <- u$times
  alltot <- u$total
  alldead <- u$dead
  cm <- u$cm
  cmtot <- u$cmtot
  cutx <- u$cutx
  offset <- u$offset
  xaxtitle <- u$xaxtitle
  treat <- u$treat
  maint <- u$maint
  add.main <- u$add.main
  legend <- u$legend
  leg.brief <- u$leg.brief
  xxlabels <- u$xlabels
  here <- !is.na(alltimes) & !is.na(alldead) & !is.na(alltot)
  print(maint)
  if(!is.null(pc)) {
    ltmat <- matrix(, nrow = length(mn), ncol = length(pc))
    semat <- matrix(, nrow = length(mn), ncol = length(pc))
    dimnames(ltmat) <- list(NULL, paste(pc))
  }
  else {
    ltmat <- NULL
    semat <- NULL
  }
  if(!is.null(pc.spline)) {
    ltmat.spline <- matrix(, nrow = length(mn), ncol = length(pc.spline))
    dimnames(ltmat.spline) <- list(NULL, paste(pc.spline))
  }
  else ltmat.spline <- NULL
  if(!is.null(pc.monotone)) {
    ltmat.monotone <- matrix(, nrow = length(mn), ncol = length(pc.monotone)
                             )
    dimnames(ltmat.monotone) <- list(NULL, paste(pc.monotone))
  }
  else ltmat.monotone <- NULL
  dimnames(ltmat) <- list(NULL, paste(pc))
  dimnames(semat) <- list(NULL, paste(pc))
  b0 <- array(, length(mn))
  b1 <- array(, length(mn))
  cm.est <- array(, length(mn))
  dev <- array(, length(mn))
  df <- array(, length(mn))
  dev.robust <- array(, length(mn))
  df.robust <- array(, length(mn))
  times.list <- vector("list", length(mn))
  total.list <- vector("list", length(mn))
  resid.list <- vector("list", length(mn))
  resid.dev.list <- vector("list", length(mn))
  leg.allfit <- array(, length(mn))
  i <- 0
  for(j in mn) {
    i <- i + 1
    idj <- uset[j]
    here.j <- idset == idj & here
    leg <- legend[j]
    leg.allfit[i] <- leg
    dead <- alldead[here.j]
    tot <- alltot[here.j]
    mins <- alltimes[here.j]
    if(!is.null(cutx))
      cutoff <- cutx[j]
    else cutoff <- NULL
    if(!is.null(cm))
      cmj <- cm[j]
    else cmj <- NULL
    uu <- fitconf(link = link, mins, dead, tot, cutoff, offset, j, leg = leg,
                  interval = interval, pc = pc, pc.spline = pc.spline, pc.monotone = 
                  pc.monotone, plot.spline = plot.spline, plot.monotone = plot.monotone,
                  cutoff.mono = cutoff.mono, span.mono = span.mono, xfun = xfun, cm = 
                  cmj, numcm = cmtot[j], p.min = p.min, printdat = F, save.resid = 
                  save.resid, cm.strategy = cm.strategy, cm.code = cm.code, cm.allcodes
                  = cm.allcodes, xfun.inv = xfun.inv, full.robust = full.robust)
    if((!is.null(plot.monotone)) && prod(par()$mfg[1:2]) == 1) {
      new.page <- T
      mtext(paste(maint, add.main), 3, line = 2.75, outer = T, cex = 1)
      mtext("Curves & LT's are from smooths following monotone regression",
            3, line = -0.75, outer = T, cex = 0.80000000000000004)
    }
    if((!is.null(plot.monotone)) && prod(par()$mfg[1:2]) == prod(par()$mfg[3:
                                          4]) && print.plot && new.page) {
      dev.print(device = pscript, horiz = T)
      new.page <- F
    }
    if(printdat) {
      print(legend[j])
      print(cbind(mins, dead, tot))
    }
    ltmat[i,  ] <- uu$lt
    if(!is.null(ltmat.spline))
      ltmat.spline[i,  ] <- uu$lt.spline
    if(!is.null(ltmat.monotone))
      ltmat.monotone[i,  ] <- uu$lt.monotone
    semat[i,  ] <- uu$se
    b0[i] <- uu$b0
    b1[i] <- uu$b1
    dev[i] <- uu$dev
    df[i] <- uu$df
    cm.est[i] <- uu$cm
    dev.robust[i] <- uu$dev.robust
    df.robust[i] <- uu$df.robust
    resid.list[[i]] <- uu$resid
    resid.dev.list[[i]] <- uu$resid.dev
    times.list[[i]] <- uu$times
    total.list[[i]] <- uu$total
  }
  if((!is.null(plot.monotone)) && print.plot && new.page)
    dev.print(device = pscript, horiz = T)    #  browser()
  ## Find totals for each idset
  id.total <- tapply(alltot, idset, sum)
  if(any(b1 < 0)){## check for negative slopes 16/7/2014
    b1[b1 < 0] <- NA
    ltmat[is.na(b1), ] <- NA ## negative slopes to give no lts
  }
  invisible(list(lt50 = NULL, lt99 = NULL, se50 = NULL, se99 = NULL, 
                 intercept = b0, slope = b1, lt = ltmat, se = semat, lt.spline = 
                 ltmat.spline, lt.monotone = ltmat.monotone, span.mono = span.mono, cutx
                 = cutx, dev = dev, df = df, cm.strategy = cm.strategy, cm = cm.est, 
                 cm.code = cm.code, dev.robust = dev.robust, df.robust = df.robust, 
                 takelog = takelog, total = total.list, resid = resid.list, resid.dev = 
                 resid.dev.list, times = times.list, id.total = id.total,
                 datafun = as.character(substitute(datafun)), choice = choice,
                 temp = temp, stage = stage, add.main = 
                 add.main, legend = leg.allfit, leg.brief = leg.brief))

}
