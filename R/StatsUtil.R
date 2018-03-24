#Create your own R package: https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
#Create package directory
#library(devtools)
#library(roxygen2)
#create("StatsUtil")
#process package documentation etc
#document()
#github
#install_github('ashata/StatsUtil', host="github.com")
#Library class for BIA6301 so I don't need to write these functions in each file
library(e1071)

#'Convert features of a dataframe to factors
#'
#' This function allows you to convert all features of a dataframe to factors.
#' @param df dataframe with numeric, non-categorical data.
#' @keywords factors
#' @export
#' @examples
#' dfToFactors()
dfToFactors<- function(df){
  col_names <- names(df)
  df[,col_names]<- lapply(df[,col_names], factor)
  return df
}

#'Convert a subset of features (names(df)) in a dataframe to factors
#'
#' This function allows you to convert a select few features in a dataframe to factors.
#' @param df dataframe with numeric, non-categorical data.
#' @param col_names feature names to convert into factors
#' @keywords factors
#' @export
#' @examples
#' dfToFactors()
dfToFactors<- function(df, col_names){
  return(lapply(df[,col_names], factor))
}

#'Normalize a column
#'
#' Min-Max normalization for a column.
#' @param x feature or column of a data frame
#' @keywords minmax, norm
#' @export
#' @examples
#' normalize()
normalize<- function(x){return((x-min(x))/(max(x)-min(x)))}

#'Normalize all or selected columns of a dataframe
#'
#' Min-Max normalization for dataframe
#' @param df data frame
#' @param  columnSelector indexes or range of columns to normalize
#' @keywords minmax, norm
#' @export
#' @examples
#' normalizeDF()
normalizeDF<- function(df, columnSelector){
  columns<-df[columnSelector]
  normalize<- function(x){return((x-min(x))/(max(x)-min(x)))}
  dummies.df<-as.data.frame(lapply(columns, normalize))
  return(dummies.df)
}

#'Count total no. of missing values in a dataframe
#'
#' sum is na for dataframe
#' @param df data frame
#' @keywords sum na,missing value
#' @export
#' @examples
#' missingCount()
missingCount<- function(df){
  return(sapply(df, function(x) sum(is.na(x))))
}
