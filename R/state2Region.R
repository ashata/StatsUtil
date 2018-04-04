#' @import dplyr
#' @import tidyverse
#' @importFrom openintro state2abbr

state.abb
state.name
state.region
states <- cbind.data.frame(Abbr = state.abb, Name = state.name, Region = state.region)

#' Convert state name or abbr to region, cleans mixed format states
#'
#' @param states vector with state name or abbr
#' @keywords state names or abbreviation to Region
#' @export
#' groupStatesToRegions
groupStatesToRegions <- function(states, custom){
  states <- state2abbrV2(states)
  if(custom){
    states <- lapply(states, toCustomRegions)
  }
  else{
    states <- lapply(states, toRegions)
  }
  return(unlist(states, use.names=FALSE))
}

#library(tidyverse)
#library(dplyr)

toRegions <- function(x){
  if(is.element(x, states$Abbr)){
    filteredState <- states %>% filter(Abbr == x)
    return(as.character(filteredState[1,3]))
  }
  else{
    return(x)
  }
}

#' Convert state names to abbr, cleans mixed format states
#'
#' @param statesList vector with state name or abbr
#' @keywords state names and abbreviation to abbreviation
#' @export
#' state2abbrV2
state2abbrV2 <- function(statesList){
  #Convert full names to abbr if needed
  normalize<- function(x){
    #if already abbr, return abbr
    if(nchar(x) == 2 && is.element(toupper(x), states$Abbr)){
      return(toupper(x))
    }
    else {
      #if full name, convert and return abbr
      return(toupper(state2abbr(x)))
    }
  }
  statesList <- lapply(statesList, normalize)
  return(statesList)
}

northeast.region <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA")

south.region <- c("DE", "FL", "GA", "MD", "NC", "SC", "VA", "DC", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX")

West.region <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA")
midwest.region <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
regions.list <- c(northeast.region, south.region, West.region, midwest.region)

toCustomRegions <- function(x){
  if(is.element(x, northeast.region)){
    return("North East")
  }
  else if(is.element(x, south.region)){
    return("South")
  }
  else if(is.element(x, West.region)){
    return("West")
  }
  else if(is.element(x, midwest.region)){
    return("Midwest")
  }
  else{
    return(x)
  }
}
