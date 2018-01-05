
library(purrr)
library(dplyr)
library(lazyeval)

#Centering with NSE

center_ = function(data, ...) {

  varlist = c(...)
  
  if (!missing(varlist) && length(varlist)!=0) {
  
    neg = as.character(varlist) %>% stringr::str_sub(2) %>% substr(1, 1)
    if (neg[1] == "-") {
      #center all numeric variables except those listed
      remove = as.character(varlist) %>% stringr::str_sub(3)
      data = data[, !names(data) %in% remove] 
      
      cvar = map_lgl(data, is.numeric)
      for (x in names(data[cvar])){
        varname = paste0(x, "_cen")
        data[[varname]] = data[[x]] - mean(data[[x]], na.rm = TRUE)
      }
      
      data
      
  } else {
      #center specific variables)
      for (i in seq_along(varlist)) {
        x = stringr::str_sub(as.character(varlist[i]), 2)
        varname = paste0(x, "_cen")
        data[[varname]] = data[[x]] - mean(data[[x]], na.rm = TRUE)
      }
      
      data
      
    }
  
  } else {
      #center all numeric variables
      cvar = map_lgl(data, is.numeric)
      for (x in names(data[cvar])){
        varname = paste0(x, "_cen")
        data[[varname]] = data[[x]] - mean(data[[x]], na.rm = TRUE)
      }
      
      data
    
    }
}


center = function(data, ...){
  center_(data, dots_capture(...))
}


center(afeds_qol2, age, v25_sm, v25cv, v25pv) %>% View
center(afeds_qol2, age, v25_sm, v25cv, v25pv) 
center_(afeds_qol2, ~age, ~v25_sm, ~v25cv, ~v25pv) 

center(afeds_qol2) %>% View
center(afeds_qol2)
center_(afeds_qol2)

center(afeds_qol2, -v25_sm2, -v25_sm, -v25pv, -v25pv) %>% View
center(afeds_qol2, -v25_sm2, -v25_sm, -v25pv, -v25pv)
center_(afeds_qol2, ~-v25_sm2, ~-v25_sm, ~-v25pv, ~-v25pv)

