ct.checkM <- function(xx = ab.june15Off,  bit = 2, post = TRUE)
{
### Purpose:- Trellis plot of 99s and 95s
### ----------------------------------------------------------------------
### Modified from:- ct.check
### ----------------------------------------------------------------------
### Arguments:- 
### ----------------------------------------------------------------------
### Author:-   Patrick Connolly, Date:- 18 Jun 2015, 12:03
### ----------------------------------------------------------------------
### Revisions:- 

  require(reshape2)
  require(dplyr)
  if(post){
    require(RColorBrewer)
    require(grid)
    filename <- ppaste("LBAM_CT_coeff.pdf")
    trellis.device("pdf", file = filename, #pointsize = 17,
                   height = 180/25.4, width = 280/25.4)
    trellis.par.set(kullas(ad.tex.cex = .9))
    remove.shading()
    on.exit(dev.off())
    
  }
  super.line <- trellis.par.get()$superpose.line
  super.line <- within(super.line, lwd <- lwd * 2)
  trellis.par.set(superpose.line = super.line)
  ab <- xx[[bit]]
  legs <- ab$legend
  use <- grep("LBAM", legs)
##   use <- grep("2h", legs[use])
  lcs <- ab$lt
  rownames(lcs) <- legs
  use.lcs <- lcs[use,]
  use.df <- as.data.frame(use.lcs)
  row.ids <- getbit(rownames(use.df), " ", 2) 
  use.df <- within(use.df, Stage <- getbit(row.ids,  "\\|", 1))
  use.df <- within(use.df, temp <- getbit(row.ids,  "\\|", 2))
  use.df <- within(use.df, duration <- getbit(row.ids,  "\\|", 3))
  use.df <- within(use.df, Temp <- as.numeric(gsub("°C", "", temp)))
  use.df <- within(use.df, Duration <- as.numeric(gsub("h", "", duration)))
  if(bit == 1){ # 3 replicates available
      use.df <- within(use.df, rep <- getbit(row.ids,  "\\|", 4))
      use.df <- within(use.df, Rep <- as.numeric(rep))
    }
  use.temps <- c(5, 15)
  use.df <- use.df %>% filter(Temp %in% use.temps)
  
  egg.df <- use.df %>% filter(Stage == "ME")
  fifth.df <- use.df %>% filter(Stage == "5*")

  egg15.df <- egg.df %>% filter(Temp == 15)
  fifth5.df <- fifth.df %>% filter(Temp == 5)
  fifth15.df <- fifth.df %>% filter(Temp == 15)
### Three lots to check out
  dfs <- c("egg15.df", "fifth15.df", "fifth5.df")[c(3, 2, 1)]
  exps <- seq(.55, 1.55, length = 100)
plot.df <- NULL
  for(d in dfs){
    out.df <- NULL
    d.df <- get(d)
    for(m in exps){
      ## First the LC99s
      d.df <- within(d.df, CT99 <- Duration * `99`^m)
      d.df <- within(d.df, CT95 <- Duration * `95`^m)
      out.d <- d.df %>% select(duration, Duration, CT99, CT95)
      out.d$m <- m
      out.df <- rbind(out.df, out.d)
    }
##
    out.df$SLT <- with(d.df, paste("LBAM", Stage, "at", temp)[1])
    out2 <- melt(out.df, c("duration", "Duration", "m", "SLT"))
    names(out2)[5:6] <- c("LC", "CT")
##     out2 <- within(out2, LC <- gsub("[:alpha:]", "", LC))
    out2 <- within(out2, LC <- paste0(gsub("[A-Za-z]", "", LC), "%"))
    plot.df <- rbind(plot.df, out2)
    plot.df <- within(plot.df, SLT <- ipsofactor(SLT))
    plot.df <- within(plot.df, duration <- factor(duration))
    dur.levs <- levels(plot.df$duration) 
                 ## c("CT99", "CT95"))
  }
##    browser()



  print(xyplot(CT ~ m | SLT + LC, data = plot.df, groups = duration, type = "l",
                                        # main = with(d.df, paste(Stage, temp)[1]),
               strip =  stripC(fax = 2, fat = 0.2), 
               xlab = "Exponent parameter (m)", ylab = expression(C^m * T),
               key = list(space = "right", lines = Rows(super.line, seq(dur.levs)),
                 text = list(dur.levs),cex = .8, border = "grey80", padding.text = 1.5,
                 title = "Duration"))
        )
}
