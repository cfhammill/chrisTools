#' Fill a database table
#'
#' Builds a SQL query to inset a list of values into a database table
#' @param table name of the database table
#' @param columns the names of the columns to be inserted into
#' @param a matrix of values to be inserted
#' @keywords database fill function
#' @export
#' @examples
#' fillTable()
#' 
fillTable <- function(table, columns, valMat){
  columns <- paste(columns, collapse=", ")
  
  query <- apply(valMat, 1, function(values){
    values <- paste(values, collapse = ", ")
    
    paste(
      "insert  into ", table, " (", columns, ") " , "values (", values, ");" 
    )
  })
  
  paste(query, collapse= " ")
}

#' Creates a database table
#'
#' Builds a SQL query to inset a list of values into a database table
#' @param table name of the database table
#' @param colNames the names of the columns in the table to be created
#' @param colType defining characteristics of each column
#' @keywords database create function
#' @export
#' @examples
#' createTable()
#' 
createTable <- function(table, colNames, colType){
  m <- cbind(colNames, colType)
  
  colT <- paste(apply(m, 1, paste, collapse = " "), collapse=", ")
  
  paste("create table ", table, " (", colT, ");")
}



#' Reads in and writes to a connection simulation result file
#'
#' Builds a SQL query to read in one year's worth of results
#' @param connection to a database
#' @param table name of the database table
#' @param file file to read results from
#' @param ... additional parameters to read table
#' @keywords database read results
#' @export
#' @examples
#' readInfileResults()
#'
readInfileResults <- function(con, table, file, sep = ",", header=F, ...){
  df <- read.table(file, header, sep, ...)
  names(df) <- c("class", "env", "age", "x", "y", "cA","cP","sA","sP")
  dbWriteTable(con, table, df)
}

#'
#' Builds a SQL query to read in one experiments collated results
#' @param connection to a database
#' @param directory containing collatedResults
#' @keywords database read results
#' @export
#' @examples
#' readCollatedResults()
#'
readCollatedResults <- function(con, directory){
  if(!grepl("/$", directory)) directory <- paste(directory, "/", sep="")
  
  files <- list.files(directory, "*collatedResults.csv")   
  #dirSplit <- strsplit(directory, "_")
  
  #prIndex <- regmatches(dirSplit[[1]][1], regexpr(".*/Pr", dirSplit[[1]][1]), invert=TRUE)[[1]][2]
  #rIndex <- substr(dirSplit[[1]][2], 2, nchar(dirSplit[[1]][2]))
  #aIndex <- substr(dirSplit[[1]][3], 2, nchar(dirSplit[[1]][3])) 
  #phenIndex <- substr(dirSplit[[1]][4], 5, nchar(dirSplit[[1]][4]))
  
 lapply(files, function(file){
   fullPath <- paste(directory, file, sep="")
   fileName <- sub("collatedResults\\.csv", "", file)
   print(fileName)
   readInfileResults(con, fileName, fullPath)
 })
 
 invisible(NULL)
}

#'
#' Vectorized table removal for purging databases
#' @param connection to a database
#' @param tables character vector of table names to be removed
#' @keywords database remove table purge
#' @export
#' @examples
#' dbRemoveTables()
#'
dbRemoveTables <- function(con, tables){
  sapply(tables, function(table){dbRemoveTable(con, table)})
  invisible(NULL)
}
