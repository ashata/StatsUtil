#Create your own R package: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
#Create package directory
#library(devtools)
#library(roxygen2)
#setwd("./StatsUtil")
#create("StatsUtil")
#load_all()
#process package documentation etc
#document()
#github
#install_github('ashata/StatsUtil', host="github.com")
#Library class for BIA6301 so I don't need to write these functions in each file
#' @import e1071

# Convert features of a dataframe to factors
#'
#' @description This function allows you to convert all features of a dataframe to factors.
#' @param ds dataframe with numeric, non-categorical data.
#' @keywords dataframe to factors
#' @export
#' @title dfToFactors
dfToFactors<- function(ds){
  col_names <- names(ds)
  ds[,col_names]<- lapply(ds[,col_names], factor)
  ds<- as.data.frame(ds)
  return(ds)
}

# Convert a subset of features (names(df)) in a dataframe to factors
#'
#' @description This function allows you to convert a select few features in a dataframe to factors.
#' @param df dataframe with numeric, non-categorical data.
#' @param col_names1 feature names to convert into factors
#' @keywords vector to factors
#' @export
#' @title listToFactors
listToFactors<- function(df, col_names1){
  return(lapply(df[,col_names1], factor))
}

#' Normalize a column
#'
#' Min-Max normalization for a column.
#' @param x feature or column of a data frame
#' @keywords minmax, norm
#' @export
#' normalize
normalize<- function(x){return((x-min(x))/(max(x)-min(x)))}

#' Normalize all or selected columns of a dataframe
#'
#' Min-Max normalization for dataframe
#' @param df data frame
#' @param  columnSelector indexes or range of columns to normalize
#' @keywords minmax, norm
#' @export
#' normalizeDF
normalizeDF<- function(df, columnSelector){
  columns<-df[columnSelector]
  normalize<- function(x){return((x-min(x))/(max(x)-min(x)))}
  dummies.df<-as.data.frame(lapply(columns, normalize))
  return(dummies.df)
}

#' Count total no. of missing values in a dataframe
#'
#' sum is na for dataframe
#' @param df data frame
#' @keywords sum na,missing value
#' @export
#' countMissing
countMissing<- function(df){
  return(sapply(df, function(x) sum(is.na(x))))
}
