# Title: Philviz Functions

# Creator: Nick Bloom

# Date: May 17, 2015

# Purpose: Functions to be used in philviz.R
# --------------------------------------------------------------------------

# This function fills in missing columns to make a data frame that includes soloist info

fill_missing <- function(x){
    dfnames <- c("soloistname", "soloistinstrument", "soloistroles", "composername", "worktitle", "conductorname", "orch", "season", "eventtype", "location", "venue", "date", "time")
    mnames <- dfnames[!dfnames %in% names(x)]
    for(i in mnames){
      x[[i]]  <- NA
    }
    x[,dfnames]
}

# This funcions fills in missing columns to make a data frame that does NOT include soloist info

fill_missing_nosolo <- function(x){
    dfnames <- c("progid", "composername", "worktitle", "conductorname", "orch", "season", "eventtype", "location", "venue", "date", "time")
    mnames <- dfnames[!dfnames %in% names(x)]
    for(i in mnames){
      x[[i]]  <- NA
    }
    x[,dfnames]
}

# This function extracts information from the parsed, runs it through the fill_missing cleaner, and outputs a complete data frame with soloist information

nyp_lister <- function(t){
  orch <- t$orchestra
  season <- t$season
  locinfo <- as.data.frame(t$concertinfo)
  worksin <- t$worksinfo
  infdf <- data.frame(orch, season, locinfo)
  oodf <- data.frame()
  for(i in 1:length(worksin)){
    wii <- worksin[[i]]
    if('soloists' %in% names(wii)){
      solodf <- wii$soloists %>% t(.) %>% data.frame(.)
      wii[['soloists']] <- NULL
      othdf <- as.data.frame(wii)
      odf <- cbind(solodf,othdf, infdf) %>% data.frame(.)
      odf1 <- fill_missing(odf)
      oodf <- rbind(oodf,odf1)
    }
    }
  oodf
}

# This function extracts information from the parsed, runs it through the fill_missing cleaner, and outputs a complete data frame WITHOUT soloist information

nyp_lister_nosolo <- function(t){
  progid <- t$programid
  orch <- t$orchestra
  season <- t$season
  locinfo <- as.data.frame(t$concertinfo)
  worksin <- t$worksinfo
  infdf <- data.frame(progid, orch, season, locinfo)
  oodf <- data.frame()
  for(i in 1:length(worksin)){
  wii <- worksin[[i]]
  if('soloists' %in% names(wii)){
      wii[['soloists']] <- NULL
  }
  othdf <- as.data.frame(wii)
  odf <- cbind(othdf, infdf) %>% data.frame(.)
  odf1 <- fill_missing_nosolo(odf)
  oodf <- rbind(oodf,odf1)

}
oodf
}


# Function to change the color of Anybar. Requires Anybar (https://github.com/tonsky/AnyBar), and OS X. You're running OS X, right?

anybar <- function(color, port = 1738) 
{
    com <- sprintf("-n '%s' | nc -4u -w0 localhost %s;", color, 
        port)
    system2("/bin/echo", args = com)
}

# Used to clean up names of composers

namefix <- function(x, patt){
  splits <- str_split(x, patt) %>% 
    unlist(.) %>% 
    str_trim(.)
  if(length(splits)>1){
    newn <- paste(rev(splits), collapse=' ') %>% 
      str_replace_all(., perl('[ ]{2,100}'), ' ')
  } else{
    newn <- str_replace_all(x, patt, '') %>% 
      str_trim(.)
    newn <- str_replace_all(newn, perl('\\[.+?\\]'), '')
  }
  newn <- str_replace_all(newn, perl('\\[.+?\\]'), '') %>% 
    str_trim(.)
  newn
}

# This leverages some base R functions to generate all of the network ties between composers as reciprocal

networkize <- function(x){
  cdf <- expand.grid(nypnet_mer$cleaname[nypnet_mer$progid == x], nypnet_mer$cleaname[nypnet_mer$progid == x])
}