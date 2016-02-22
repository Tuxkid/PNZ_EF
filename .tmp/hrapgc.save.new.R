save.new <-
function(neuobj, gem.pos = NULL, put.also = TRUE)
{
  ## Purpose: For saving a new function to the Gems database
  ## ----------------------------------------------------------------------
  ## Modified from: 
  ## ----------------------------------------------------------------------
  ## Arguments:
  ##           neuobj: new (usually function) to be saved to Gems
  ##           gem.pos: where on search list to save (usually Gems)
  ##           put.also: put the code into the Gems/.tmp/ directory?
  ## ----------------------------------------------------------------------
  ## Author: Patrick Connolly, Creation date: 27 Apr 2001, 11:11
  ## ----------------------------------------------------------------------
  ## Revisions:- 30/01/2013 added put.also
### 
  if(is.null(gem.pos)) gem.pos <- grep("Gems", search())
  fun.name <- deparse(substitute(neuobj))
  assign(fun.name, neuobj, pos = gem.pos)
  save(list = ls(all = TRUE, pos = gem.pos),
       file = substring(search()[gem.pos], 6))
  if(put.also) {# Get code from Put function
    filedate <- comment(get(fun.name))
    location <- system("pwd", TRUE)
    source.dir <- substring(search()[gem.pos], 6) # remove "file:"
    use.source.dir <- gsub("RData", "tmp/hrapgc.", source.dir) # remove .RData
    file.name <- ppaste(use.source.dir, fun.name, ".R")
    system(ppaste("cp ", location, "/.tmp/hrapgc.", fun.name, ".R ", file.name))
  }
}
