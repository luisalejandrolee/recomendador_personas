
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


#' Wrapper for \code{convert_to_date}, to apply it and convert to date format the
#' time identifier in all data.tables in \code{dt_list}
#' @param dt_list A list with several data.tables, all in the same format
#' @param time_var String. A column to convert to date format
convert_to_date_all_list <- function(dt_list, time_var){
  
  for(i in 1:length(dt_list)){ # all data.tables in the list
    # applies custom function to each time_var column in the data.table
    dt_list[[i]][, (time_var) := convert_to_date(get(time_var))]
  } # for loop
  
} # end of convert_to_date_all_list



#' Converts "YYYYMM" string into date format. Assumes all strings have same format:
#' a numeric variable as YYYYMM.

convert_to_date <- function(s){

  years <- substr(s, 1, 4)
  months <- substr(s, 5, 6)

  # converting to date has a trick: yearmon understands it is a date with year and
  # month. However, many other functions (e.g. ggplot) are used to the traditional
  # format including a day. For this, the as.Date() transforms the
  # yearmon object into a date, including the last day of the month. This is perfect
  # for this data, since the cut for each month to obtein the date is at the end
  # of the month
  #my_date <- as.yearmon(my_date, "%Y_%m")
  #my_date <- as.Date(my_date, frac = 1, origin = "1960-10-01")
  
  my_date <- paste0(years, "_", months) %>% as.yearmon("%Y_%m") %>%
    as.Date(frac = 1, origin = "1960-10-01")
  
  return(my_date)
} # end of convert_to_yearmon


#' Transforms the list with all months of data, deleting columns, converting date
#' into proper format and renaming variables
#' @param dt_list A list with several data.tables, all in the same format
#' @param cols_to_del Vector with column names to delete
#' @usage Changes all data.tables in the list dinamycally

clean_dt_in_list <- function(dt_list, cols_to_del){
  for(i in 1:length(dt_list)){
    
    dt_list[[i]][, num_id := as.character(num_id)] # convert to character
    dt_list[[i]][, (cols_to_del) := NULL] # delete columns
    #dt_list[[i]][, PERIODO := convert_to_date(PERIODO)] #convert to date
    
    # format column names (using custom function)
    old_cols <- names(dt_list[[i]]) 
    setnames(dt_list[[i]], old_cols, format_columns(old_cols))
    
    # replace NA's with zeros
    utilucho::input_nas(dt_list[[i]], names(dt_list[[i]]), "zero")
    
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
#' chooses randomly a number of clients from the last month, and chooses only 
#' those clients for all previous months (each data.table in the list)
#' @param dt_list List with data from all months
#' @param sample_size Number of rows (clients, assuming they are unique) to
#' sample from the last month (last position in the list), if \code{frac == FALSE}.
#' If \code{frac == TRUE}, must be between zero and one
#' @param frac Whether sampling is by fraction or by number of rows
#' @value List of the same lenght of \code{dt_list}, each element containing a 
#' sample of the data in the original

get_samples_dts <- function(dt_list, sample_size, frac = FALSE){
  
  # Sampling is done based on the first month
  last_month <- length(dt_list)
  first_dt <- dt_list[[last_month]]
  
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


#' In a data.table containing ids and time variables, returns a data.table in which
#' every id has associated a row with all time periods present in the whole data.
#' 
#' @usage This is used so that when lagging a variable the lag is for calendar time. For
#' example, when including a lag of 1, it is related to the last calendar point in time,
#' instead of the last point in time in which that id reported something. In points in
#' time where the client did not report aanything (e.g. was not in the data), it will
#' appear with NA for all columns besides \code{time_var} and \code{id_var}
#' @param dt data.table containing a time variable and an id variable
#' @param time_var String. Column in \code{dt} representing time
#' @param id_var String. Column in \code{dt} representing a an observation (e.g. a client)
#' 
complete_id_time <- function(dt, time_var, id_var){
  
  # get unique dates and ids as vectors
  dates <- unique(dt[, ..time_var])[[1]] 
  ids <- unique(dt[, ..id_var])[[1]]
  
  # Get all possible combinations of id and time
  # TODO: CJ() function (cross join from data.table) might be more efficient
  all_rows <- expand.grid(dates, ids) %>% as.data.table()
  
  # Rename for easy merging
  setnames(all_rows, names(all_rows), c(time_var, id_var))
  
  # data.table with all ids and times. If a cliente does not report anything
  # in a given point in time, it shows NA
  final_dt <- merge(all_rows, dt, all.x = T)
  
  return(final_dt)
  
} # end of complete_id_time




#' Creates all chosen lags (concatenated directly to \code{dt}) for all chosen
#' variables (products).
#' @param return \code{dt} with added columns for lagged variables
#' @param dt data.table with \code{num_id} as id variable, and \code{periodo} as
#' time variable
#' @param vars_to_lag Vector (strings) with column names to lag
#' @param lags Numeric vector with lags to include for all \code{vars_to_lag}
create_lagged_vars <- function(dt, vars_to_lag, lags){
  
  # Very important to order dt. Otherwise, can't concatenate later with the lagged dt
  # TODO: if want to use for other projects, must make id and time variable
  # parameters of the function (not directly chosen as is now)
  dt <- dt[order(num_id, periodo)]
  
  # Get a separate data.table with all lags for all chosen variables
  lagged_dt <- dt[, shift(.SD, lags, give.names = TRUE), by = num_id,
                  .SDcols = vars_to_lag]
  
  lagged_dt[, 1] <- NULL # to avoid repeating 'num_id' column, drop it
  final_dt <- cbind(dt, lagged_dt) # possible due to the previous ordering of dt
  
  
  return(final_dt)
  
}

#' Adds dinamycally in \code{dt}, for each variable in \code{cols}, three new 
#' variables: one with the difference compared to one period lag, and two dummies,
#' one if a new product was aqcuired and other if a product was lost
#' @param dt data.table with lags already included for \code{cols}
#' @param cols Column names for which to include difference with lag 1, and dummies
#' for new tenency and losses in tenency
#' @value Return \code{dt} with added columns for difference and tenency

create_acquisition_and_loss <- function(dt, cols){
  
  for (var in cols) { # all products
    
    lag_diff <- paste0(var, "_diff") # name for tenency difference (variable to create)
    lagged_var <- paste0(var, "_lag_1") # assumes name structure of lagged variables
    
    # creates difference compared to last period
    dt[, (lag_diff) := get(var) - get(lagged_var)]
    
    new_var <- paste0(var, "_new") # name for tenency dummy when a new product is acquired
    loss_var <- paste0(var, "_loss") # name for tenency dummy when a product is cancelled
    
    # create dummy if product is acquired
    dt[, (new_var) := ifelse(get(lag_diff) > 0, 1, 0)]
    # create dummy if product is cancelled
    dt[, (loss_var) := ifelse(get(lag_diff) < 0, 1, 0)]
    
  } # for loop
} # end of create_acquisition_and_loss






