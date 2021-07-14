## ADDITIONAL FUNCTIONS TO HELP WITH THE SETUP SCRIPTS

## Function to insert stock/species name into g3 parameter
g3_stock_param <- function(sname, param, lower=NULL, upper=NULL, xby=NULL, sep0="."){
  
  nullval <- is.null(lower) + is.null(upper)
  
  ## No bounds 
  if (nullval > 0){
    if (nullval == 1) warning("One bound is NULL & the other is not NULL, assuming unbounded")
    tmp <- gadget3:::f_substitute(~g3_param(x), list(x=paste(sname, param, sep=sep0)))
  }
  ## Bounds
  else{
    lb <- as.numeric(min(lower))
    ub <- as.numeric(max(upper))
    tmp <- gadget3:::f_substitute(~bounded(g3_param(x), lb, ub), 
                                  list(x=paste(sname, param, sep=sep0), lb=lb, ub=ub))
  }
  if (!is.null(xby)){
    tmp <- gadget3:::f_substitute(~y * x, list(y=xby, x=tmp))
  }
  return(tmp)
}

## Function to insert stock/species name into g3 param table (just by age)
g3_stock_param_table <- function(sname, param, lowage, highage, lower=NULL, upper=NULL, sep0="."){
  
  nullval <- is.null(lower) + is.null(upper)
  
  ## No bounds
  if (nullval > 0){
    if (nullval == 1) warning("One bound is NULL & the other is not NULL, assuming unbounded")
    tmp <- gadget3:::f_substitute(~g3_param_table(nm, data.frame(age=seq(la, ha))),
                                  list(nm=paste(sname, param, sep=sep0), la=lowage, ha=highage))
  }
  ## Bounds
  else{
    lb <- as.numeric(min(lower))
    ub <- as.numeric(max(upper))
    tmp <- gadget3:::f_substitute(~bounded(g3_param_table(nm, data.frame(age=seq(la, ha))), lb, ub),
                                  list(nm=paste(sname, param, sep=sep0), 
                                       la=lowage, ha=highage,
                                       lb=lb, ub=ub))
  }
  return(tmp)
}