mean.lt <- function(start.list = ab.soluble, choose = 1, lt = 99, intv = 95, new.order = F,
           leg.beg = NULL, leg.end = 4, insect = "interesting critters", rm.deg = TRUE,
         fit = NULL, sub = NULL, rnd = 1, lt.ld = "LT", xlout = NULL, df.out = FALSE,
           two.tables = F, borders = F, omit = NULL)
{
  ## Requires LT data in an ab. type list with sublist names such as "total"
  ## referring to LTs at those locations
  ##
  ## LT50s calculated by mono fit: LT99s calculated by line fit
  ## Has to group species to take mean, and then restore to the original
  ##  order of species (otherwise, it's very simple)
  ## The name "speciesindPed" really refers to location and life stage in 
  ##   some cases: it is what appears on the individual mortality plots
  ##
  ## leg.end will usually be used to remove "Rep_" from hames. If there are more than 
  ##  9 reps in the legend name, it won't work.
  ##indPed
  ## leg.beg is the number of characters to omit from the legend text in determining what
  ##    groups similar trials 
  ## leg.beg by default is the postion of the first blank in the legend name: That is 
  ##   to allow the practice of having trial numbers in that legend: if there's no space
  ##   in the legend, it won't work.
  ## rnd: amount of rounding. (might make this one the "official" Gems version.
  ## rm.deg: remove degree symbol?
  ## xlout: name of Excel file for output
  ## df.out: Do we want a dataframe returned?
##### August 2013: added lt.ld and a rounding
##### February 2015: added xlout
  
  ablist <- as.character(substitute(start.list))
  leng.name <- nchar(ablist)
  interval <- intv/100
  if(is.character(choose)) {
    choose.i <- (1:length(names(start.list)))[names(start.list) == 
                                              as.character(choose)]
  }
  else (choose.i <- choose)
  ab <- start.list[[choose.i]]
#### If not all values are being used, what subset?
  if(is.null(sub))
    sub <- seq(ab$legend)
  legs <- as.character(ab$legend)[sub]
  if(rm.deg)
    legs <- gsub("°", "", legs)
### Seems to be necessary for correct class
  if(is.null(leg.beg)) {
### Find the first space in the legend names:
    leg.beg <- NULL
    for(k in 1:length(legs)) {
      leg.vec <- paste(substring(legs[k], 1:nchar(legs[k]),
                                 1:nchar(legs[k])), sep = "")
      leg.beg[k] <- match(" ", leg.vec)
    }
  }
  spec <- substring(legs, leg.beg + 1, nchar(legs) - leg.end)
  include <- rep(TRUE, length = (length(spec)))    #
### Are there any we wish to exclude? (22/1/2001)
  if(!is.null(omit)){ # add in exclusion by index 17/11/2014
    if(is.character(omit))
      include[grep(omit, spec)] <- FALSE
    if(is.numeric(omit))
        include[omit] <- FALSE
  } 
  spec <- spec[include]
  uniq.spec <- unique(spec)
  sortspec <- sort(spec)
  if(is.null(fit))
    fit <- ifelse(lt == 50, "monotone", "line")
  if(fit == "line")
    lt.vector <- ab$lt[, paste(lt)][sub]
  else lt.vector <- ab$lt.monotone[, paste(lt)][sub]
  type <- paste(insect, "  [", choose, "] (", fit, " fit)", sep = "")
  lt.vector[lt.vector < 0 | lt.vector == Inf] <- NA    # (22/1/2001)
  lt.vector <- lt.vector[include]    # (22/1/2001)
  lts <- round(as.numeric(lt.vector), 5)
  lt.1 <- matrix(round(lts, rnd), ncol = 1, dimnames = list(spec, NULL))
  lt.2 <- NULL    #
### Sort to arrange species together
  for(i in uniq.spec) {
    lt.2 <- c(lt.2, lt.1[dimnames(lt.1)[[1]] == i,  ])
  }
### Change vector into single column matrix
  lt.3 <- matrix(lt.2, nc = 1, dimnames = list(names(lt.2), NULL))    #
### Have vector of grouped lts. Continue finding means, etc.
  av.log.lt <- tapply(log(lts), spec, mean, na.rm = T)
  my.var <- function(x) var(x, na.rm = T)
  var.log.lt.i <- tapply(log(lts), spec, my.var)    #
### Replace any missing variances with zero 
  var.log.lt <- ifelse(is.na(var.log.lt.i), 0, var.log.lt.i)
  sem.log.lt <- tapply(log(lts), spec, sem, na.rm = T)
  reps <- tapply(lts, spec, function(x)
                 length(x[!is.na(x)]))
  deg.pool <- sum(reps[reps > 0] - 1)
  ind.var.free <- var.log.lt * (reps - 1)
  pool.var.free <- sum(ind.var.free)/deg.pool
  delta <- qt(1 - (1 - interval)/2, deg.pool) * sqrt(pool.var.free/reps)
  log.up <- av.log.lt + delta
  log.low <- av.log.lt - delta
  lt.mean <- round(exp(av.log.lt), rnd)
  upper <- round(exp(log.up), rnd)
  lower <- round(exp(log.low), rnd)
  sem <- round(sem.log.lt * lt.mean, rnd + 2)    #
### Make matrix of means, etc
  y <- cbind(reps, lt.mean, lower, upper, sem)
  x <- y[uniq.spec,  ]    #
### Put individual LTs into matrix
  rn <- dimnames(lt.3)[[1]]
  urn <- unique(rn)
  cn <- max(table(rn))
  mm <- matrix(rep("---", length(urn) * cn), nc = cn,
               dimnames = list(urn, paste("Rep", 1:cn, sep = "")))
  for(i in urn) {
### Not all rows have same number of reps
    kk <- lt.2[names(lt.2) == i]
    for(k in 1:length(kk))
      mm[i, k] <- kk[k]
  }
### If original order of plots is not wanted, sort into new order
  if(new.order) {
    mm <- mm[order(dimnames(mm)[[1]]),  ]
    x <- x[order(dimnames(x)[[1]]),  ]
  }
### Output to screen
  if(two.tables) {
    cat(c(paste("Separate LT", lt, "s for ", type, ":", sep = ""), "\n\n"))
    tab.mat(mm)
    cat("\n\n")
    cat(c(paste("Mean LT", lt, "s and ", intv, "% c.i.:", sep = ""), "\n\n")
        )
    tab.mat(x)
    cat("\n\n")
  }
  else {
    one.table <- cbind(mm, x)
    one.df <- unfactor(as.data.frame(one.table))
    if(df.out) # return the dataframe then quit
      return(one.df)
    if(!is.null(xlout)){ # use WriteXLS
      require(WriteXLS)
      sheet.lab <- paste0(lt.ld, lt, "s CI for ", insect)
      names(one.df)[names(one.df) == "lt.mean"] <- paste(lt.ld, "mean")
      WriteXLS("one.df", xlout, sheet.lab, row.names = TRUE,
               BoldHeaderRow = TRUE)} else
    {
      cat(c(paste("Separate ", lt.ld, lt, "s with mean and ", intv,
                  "% c.i. for ", type, ":", sep = ""), "\n\n"))
      if(borders)
        print.char.matrix(unfactor(as.data.frame(one.table)), col.names = TRUE,
                          hsep = " | ", csep = "-+-") else tab.mat(one.table)
    }
  }
}

