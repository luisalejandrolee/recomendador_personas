
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
    
    all_months[[i]][, num_id := as.character(num_id)] # convert to character
    all_months[[i]][, (cols_to_del) := NULL] # delete columns
    all_months[[i]][, PERIODO := convert_to_yearmon(PERIODO)] #convert to date
    
    # format column names (using custom function)
    old_cols <- names(all_months[[i]]) 
    setnames(all_months[[i]], old_cols, format_columns(old_cols))
    
  } # for loop
  
} # end of clean_dt_in_list

#' Used to format column names quickly (to lowercase and replace spaces with
#' underscore)
#' @param old_cols Vector of strings to modify
#' @return Vector of formatted strings

format_columns <- function(old_cols){
  
  new_cols <- sapply(old_cols, tolower) # to lower case
  new_cols <- gsub(" ", "_", new_cols) # change spaces with underscores
  
  return(new_cols)
  
} # end of format_columns


#' Gets the data.tables in a list and samples them. This is used to work with
#' samples when there's a lot of data (so that one can merge after sampling). It
#' chooses randomly a number of clients from the first month, and chooses only 
#' those clients for all following months (each data.table in the list)
#' @param dt_list List with data from all months
#' @param sample_size Number of rows (clients, assuming they are unique) to
#' sample from the first month (first position in the list), if \code{frac == FALSE}.
#' If \code{frac == TRUE}, must be between zero and one
#' @param frac Whether sampling is by fraction or by number of rows
#' @value List of the same lenght of \code{dt_list}, each element containing a 
#' sample of the data in the original

get_samples_dts <- function(dt_list, sample_size, frac = FALSE){
  
  # Sampling is done based on the first month
  first_dt <- dt_list[[1]]
  
  # Get a sample of clients
  if(frac == TRUE){
    chosen_clients <- sample_frac(first_dt, sample_size)[, .(num_id)]
  } else{
    chosen_clients <- sample_n(first_dt, sample_size)[, .(num_id)]
  }
  
  # Will hold the final data
  sampled_dts <- vector("list", length(files)) # empty list
  
  i <- 1 # index
  
  for(i in 1:length(dt_list)){ # all datasets in the list
    
    temp_dt <- all_months[[i]] # current month (position in list)
    sampled_dts[[i]] <- merge(temp_dt, chosen_clients) # get sample
    
    i <- i + 1 # index
    
  } # for loop
  return(sampled_dts)
  
} # end of sample_data


