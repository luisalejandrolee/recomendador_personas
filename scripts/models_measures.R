calculate_lift_table <- function(depvar, predcol, groups=10) {
  
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  
  gaintable <-  helper %>% group_by(bucket)  %>%
    
    summarise_at(vars(depvar),
                 funs(total = n(), totalresp = sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp / sum(totalresp) * 100,
           Cumlift = Gain / (bucket * (100 / groups)))
  
  return(gaintable)
}
