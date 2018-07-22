
#' Imports all chosen files
#' @param folder Local folder with data
#' @param files Vector with all files to import without extension (e.g. no .csv)
#' @usage If importing several data tables with same structure (e.g. several months
#' of the same data), could easily join aferwards with \code{rbindlist}
#' @return List contaning imported files
import_as_list <- function(folder, files){
  
  # initialize empty list to keep all files separated
  all_files <- vector("list", length(files))
  
  i <- 1 # index for files
  
  for(file in files){ # all files
    
    path <- paste0(folder, file, ".csv")
    print(path)
    all_files[[i]] <- fread(path) # import file in corresponding list position
    
    i <- i + 1 # next index
  }
  
  return(all_files)
  
} # end of import_as_list


#' Converts "YYYYMM" string into date format. Assumes all strings have same format
convert_to_yearmon <- function(s){
  
  years <- substr(s, 1, 4)
  months <- substr(s, 5, 6)
  date <- paste0(years, "_",months)
  date <- as.yearmon(date, "%Y_%m")
  
  return(date)
} # end of convert_to_yearmon


#' Transforms the list with all months of data, deleting columns, converting date
#' into proper format and renaming variables
#' @param dt_list A list with several data.tables, all in the same format
#' @param cols_to_del Vector with column names to delete
#' @usage Changes all data.tables in the list dinamycally
clean_dt_in_list <- function(dt_list, cols_to_del){
  for(i in 1:length(dt_list)){
    
    all_months[[i]][, (cols_to_del) := NULL]
    all_months[[i]][, PERIODO := convert_to_yearmon(PERIODO)]
    old_cols <- names(all_months[[i]])
    setnames(all_months[[i]], old_cols, format_columns(old_cols))
    
  } # for loop
  
} # end of clean_dt_in_list

#' Used to format column names quickly
#' @param old_cols Vector of strings to modify
#' @return Vector of formatted strings 
format_columns <- function(old_cols){
  
  new_cols <- sapply(old_cols, tolower) # to lower case
  new_cols <- gsub(" ", "_", new_cols) # change spaces with underscores
  
  return(new_cols)
  
}





