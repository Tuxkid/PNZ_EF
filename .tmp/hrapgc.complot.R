complot <-
structure(function(data, xtitle = "Time (Minutes)", main = "", yfun = "cloglog", logx = F,
    plimits = c(0.04, 0.9995), xtix = NULL, ytit = NULL, labp
     = NULL, lty = 1, colours = F, dot.marks = T, xlab.line = 2, x.mgp = 0.2, 
    y.mgp = ifelse(post, 0.5, 0.6), ylab.line = 1.6, pchs = c(0, 1, 2, 5, 6:16),
    id = NULL, ab.lty = 2, lab.cex = 1, ax.cex = 0.8, main.cex = 1.1, point.cex
     = 0.9, mark.control = F, ...)
{
# Modified (very) version of simplot
# 
# data is a dataframe of the sets to be plotted on one plot.
#  with names of columns: Time, Dead, Total, Id 
#
# xfun is the function to be applied to the x axis data before plotting it.
#   It ia a list which will be extended to the same length as the number of sets
#   of points to be plotted (unless already given as that length). 
#
# y.mgp is the second value in an mgp parameter
# x.mgp is the amount by which the corresponding x axis parameter is decreased
#  
#
# dot.marks: Mark 100% and 0% points with dots?
    post <- names(dev.cur()) == "postscript"
    par(...)
    if(logx)
       f <- log
    else f <- function(x)
       x
    if(logx)
       stop(
          "\ncomplot function needs fixing to do log transformation on x-axis")
    u.id <- unique(data$Id)
    g <- link.function(yfun)

# Identify Control and Treatment data:
    if(logx)
       data <- data[data$Time > 0,  ]
    data$Mort <- data$Dead/data$Total
    if(is.null(xtix))
       x.range <- range(data$Time)
    else x.range <- range(xtix)
    p.range <- range(data$Mort)    #
# Calculate adjustments for the space on the y-axis:
    ylow <- g(plimits[1])
    yhigh <- g(plimits[2])
    eps <- (yhigh - ylow) * 0.01
    ylow <- ylow - eps
    yhigh <- yhigh + eps
    if(is.null(labp))
       labp <- c(1, 5, 10, 25, 50, 75, 95, 99)
    labp <- labp[labp/100 >= plimits[1] & labp/100 <= plimits[2]]    #
# Set up blank plot: (uses blank.plot() in /home/hrapgc/Gems/.Data/ )
    blank.plot(f(x.range), c(ylow, yhigh), ...)
   usr <- par()$usr
    epsx <- 0.0125 * diff(usr[1:2])
    epsy <- 0.0125 * diff(usr[3:4])
    midhigh <- 0.5 * (yhigh + usr[4])
    midlow <- 0.5 * (ylow + usr[3])
    hundred.line <- F
    zero.line <- F
    colour <- is.character(colours)
    for(i in seq(u.id)) {
       df.i <- data[data$Id == u.id[i],  ]
       attach(df.i)    #
# Plot "normal" points:

       time <- Time[Mort > plimits[1] & Mort < plimits[2]]
       mort <- Mort[Mort > plimits[1] & Mort < plimits[2]]
       points(f(time), g(mort), pch = pchs[i], col = ifelse(colour, colours[i],
          1), cex = point.cex)    #
# Plot "high" points (i.e. ones near 100%)
       high.mort <- Mort[Mort > plimits[2]]
       high.time <- Time[Mort > plimits[2]]
       points(f(high.time), rep(midhigh, length(high.time)), pch = pchs[i], cex
           = point.cex, col = ifelse(colour, colours[i], 1))
       if(any(Mort == 1)) {
          hundred.line <- T
          full.mort <- Mort[Mort == 1]
          full.time <- Time[Mort == 1]
          if(dot.marks)
             points(f(full.time), rep(midhigh, length(full.time)), pch = ".", 
                cex = point.cex, col = ifelse(colour, colours[i], 1))
       }
# Plot "low" points (i.e. ones near 0%)
       low.mort <- Mort[Mort < plimits[1]]
       low.time <- Time[Mort < plimits[1]]
       points(f(low.time), rep(midlow, length(low.time)), pch = pchs[i], cex = 
          point.cex, col = ifelse(colour, colours[i], 1))
       if(any(Mort == 0)) {
          zero.line <- T
          zero.mort <- Mort[Mort == 0]
          zero.time <- Time[Mort == 0]
          if(logx) {
             zero.time <- zero.time[zero.time > 0]
             zero.mort <- zero.mort[zero.time > 0]
          }
          if(dot.marks)
             points(f(zero.time), rep(midlow, length(zero.time)), pch = ".", 
                cex = point.cex)
       }
       cont.mort <- Mort[Time == 0]
       if(mark.control) {
# Mark control mortalities:
         if(any(cont.mort < plimits[1])) text(0, midlow, "C")
         else text(0, g(cont.mort), "C", cex = point.cex,
                   col = ifelse(colour, colours[i], 1))
       }
       detach("df.i")
    }
# Do tricky things to y-axis:
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
    axis(2, at = midlow, tck = 0, labels = "0", adj = 1, mgp = mgp, cex.axis = 
       ax.cex, las = 1)
    lines(usr[1] + c( - epsx, epsx), c(ylow - 1.5 * epsy, ylow + 1.5 * epsy), 
       xpd = T)
    lines(usr[1] + c( - epsx, epsx), c(ylow + 0.5 * epsy, ylow + 3.5 * epsy), 
       lty = lty, xpd = T)
    axis(2, at = midhigh, tck = 0, labels = "100", adj = 1, mgp = mgp, cex.axis = 
       ax.cex, las = 1)
    lines(usr[1] + c( - epsx, epsx), c(yhigh - 1.5 * epsy, yhigh + 1.5 * epsy),
       xpd = T)
    lines(usr[1] + c( - epsx, epsx), c(yhigh - 3.5 * epsy, yhigh - 0.5 * epsy),
       lty = lty, xpd = T)    #
# Make x-axis and labels
    if(is.null(xtix))
       xlab.pos <- pretty(range(data$Time))
    else xlab.pos <- xtix
    if(logx)
       xlab.pos <- xlab.pos[xlab.pos > 0]
    axis(1, at = f(xlab.pos), labels = paste(xlab.pos), cex.axis = ax.cex, mgp = mgp -
       c(0, x.mgp, 0))
    cat(xlab.pos, "  \n")    # xlab.pos specifies numeric labels
# Add break in x-axis if necessary: fill in left space
    
    if(logx) {
       x.cut1 <- diff(usr[1:2]) * 0.03 + usr[1]
       x.cut2 <- diff(usr[1:2]) * 0.5 + usr[1]
       axis(1, at = c(usr[1], x.cut1), labels = F, tck = 0)
       axis(1, at = c(x.cut2, usr[2]), labels = F, tck = 0)
       lines(x.cut1 + c( - epsx, epsx), usr[3] + 2 * c( - epsy, epsy), xpd = T)
       lines(x.cut2 + c( - epsx, epsx), usr[3] + 2 * c( - epsy, epsy), xpd = T)
    }
    else lines(usr[1:2], usr[c(3, 3)])
    mtext(side = 1, line = xlab.line, text = xtitle, cex = lab.cex)
    mtext(side = 2, line = ylab.line, text = ifelse(is.null(ytit), paste(
       "% Mortality,", yfun, "scale"), ytit), cex = lab.cex)
    mtext(side = 3, text = main, cex = main.cex)
    box(bty = "7")
}
, comment = "13/09/2002")
