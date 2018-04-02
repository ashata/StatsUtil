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
#' @param naLbl if present missing/incorrect values will be assigned this label
#' @keywords state names or abbreviation to Region
#' @export
#' groupStatesToRegions
groupStatesToRegions<- function(states, naLbl){
  states <- state2abbrV2(states)
  regions <- lapply(states, toRegions(naLbl))
  return(regions)
}

#library(tidyverse)
#library(dplyr)

toRegions <- function(x, naLbl){
  region <- naLbl
  if(is.element(x, states$Abbr)){
    filteredState <- states %>% filter(Abbr == x)
    region <- filteredState$Region
  }
  if(is.na(region)){
    region <- x
  }
  return(region)
}

#' Convert state names to abbr, cleans mixed format states
#'
#' @param statesList vector with state name or abbr
#' @keywords state names and abbreviation to abbreviation
#' @export
#' state2abbrV2
state2abbrV2<- function(statesList){
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