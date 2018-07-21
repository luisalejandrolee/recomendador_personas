
#' Imports all chosen files
#' @param folder Local folder with data
#' @param files Vector with all files to import without extension (e.g. no .csv)
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
  
}
