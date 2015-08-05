glean.blank <-
function (choice = 1) 
{
    blank.df <- df.sort(blank.df, rev(c("Stage", "Position", 
        "Rep")))
    attach(blank.df)
    on.exit(detach("blank.df"))
    idset <- make.id(Time)
    cutx <- NULL
    leg.brief <- unique(paste(Stage, Position, " Rep", Rep, sep = ""))
    maint <- paste(degree(40), "heat treatment of some bug or other")
    xlabels <- c(0, 0)
    xaxtitle <- "Time (minutes)"
    list(id = idset, times = Time, total = Dead + Live, dead = Dead, 
        cutx = cutx, offset = 0, xaxtitle = xaxtitle, maint = maint, 
        legend = leg.brief, xlabels = xlabels, takelog = F)
}
