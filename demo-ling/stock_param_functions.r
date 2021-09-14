## ADDITIONAL FUNCTIONS TO HELP WITH THE SETUP SCRIPTS

## Extract species string from stock object
stock_species <- function(stock){
  return(g3_stock_name(stock,'species'))
}

g3_stock_name <- function(stock,id='full'){
  stopifnot(gadget3:::g3_is_stock(stock))
  if(id[1] == 'full'){
    return(stock$name)
  } else if(setequal(id,intersect(id, names(stock$name_parts)))){
    return(paste(stock$name_parts[id],collapse = '_'))
  } else {
    stop(sprintf('id should be %s, supplied %s', 
                 paste(names(stock$name_parts),collapse = ', '),
                 id))
  }
}

## Create stock/species-specific parameter
g3_stock_param <- function(stock, param_name, id = 'full'){
  stock_param <- paste(g3_stock_name(stock,id), param_name, sep = ".")
  out <- gadget3:::f_substitute(~g3_param(x), list(x=stock_param))
  return(out)
}



## Adds bounds to a g3_param
bounded_param <- function(stock, param_name, bounds,id='full'){
  
  ## Check names
  # TO-DO: rather than an error, create an unbounded parameter param_name is not found in bounds?
  if (!param_name %in% names(bounds)){
    stop("'param_name' was not found in names(bounds)")
  }
  
  ## Unbounded parameter
  param0 <- g3_stock_param(stock, param_name,id)
  
  ## Bounds
  #which(sapply(names(fleet_bounds), FUN=grepl, param_name))
  vars <- bounds[[param_name, exact = FALSE]]
  ## Check
  num_bounds <- is.numeric(vars$lower) + is.numeric(vars$upper)
  
  ## Error if only one bound is specified
  if (num_bounds == 1){
    stop("For bounded tables, the upper and lower arguments should both be numeric.")
  }
  
  ## Unbounded
  if (num_bounds == 0){
    return(param0)
  }
  
  if (length(unlist(vars)) > 2) stop("The 'upper' and 'lower' arguments should have a length of 1")
  if (vars$lower > vars$upper)  stop("The 'lower' argument should be less than the 'upper' argument")
  
  ## Add bounds to parameter reference
  out <- gadget3:::f_substitute(~bounded(x, lower, upper), c(x = param0, vars))
  return(out)
}

## Creates stock-specific reference to unbounded table
g3_stock_table <- function(stock, param_name,id = 'full'){
  
  stock_param <- paste(g3_stock_name(stock,id=id),param_name,sep = '.')
    
    #g3_stock_param(stock,id = id, param_name = param_name)
  vars <- list(stock_param = stock_param,
               minage = gadget3:::stock_definition(stock, 'minage'),
               maxage = gadget3:::stock_definition(stock, 'maxage'))
  out <- gadget3:::f_substitute(~g3_param_table(stock_param, 
                                                data.frame(age = seq(minage, maxage))), vars)
  return(out)
}

## Function to insert stock/species name into g3 param table (just by age)
bounded_table <- function(stock, param_name, bounds,id='full'){
  
  ## Check names
  if (!param_name %in% names(bounds)){
    stop("'param_name' was not found in names(bounds)")
  }
  
  ## Unbound table
  stock_tab <- g3_stock_table(stock, param_name, id = id)
  
  ## Bounds
  vars <- bounds[[param_name, exact = FALSE]]
  ## If specified, bounds should be numeric
  num_bounds <- is.numeric(vars$lower) + is.numeric(vars$upper)
  
  ## Error if only one bound is specified
  if (num_bounds == 1){
    stop("For bounded tables, the upper and lower arguments should both be numeric.")
  }
  
  ## Unbounded
  if (num_bounds == 0){
    return(stock_tab)
  }
  
  if (length(unlist(vars)) > 2) stop("The 'upper' and 'lower' arguments should have a length of 1")
  if (vars$lower > vars$upper)  stop("The 'lower' argument should be less than the 'upper' argument")
  
  out <- gadget3:::f_substitute(~bounded(x, lower, upper), c(x = stock_tab, vars))
  
  return(out)
}


# bounded_param <- function (val,...){
#   
#   substitute(
#     bounded(g3_param(x, val), g3_param(x_lower, optimise = FALSE),
#             g3_param(x_upper, optimise = FALSE),
#             list(
#               val = val, 
#               x = paste(..., sep = "."),
#               x_lower = paste(..., "lbound", sep = "."),
#               x_upper = paste(..., "ubound", sep = ".") )))
#   
#   
# } 
