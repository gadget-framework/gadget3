## -----------------------------------------------------------------------------
## Set up report actions:

# Disaggregated report storage for *_imm stock (we store with same age/step/length as ling itself)
imm_report <- g3s_clone(stock_list$imm_stock, 'imm_report') %>%
  g3s_time(year = local(year_range), step = 1:4)

# Disaggregated report storage for *_mat (we store with same age/step/length as ling itself)
mat_report <- g3s_clone(stock_list$mat_stock, 'mat_report') %>%
  g3s_time(year = local(year_range), step = 1:4)


report_actions <- list(
  # Report numbers
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__num"))))),
  g3a_report_stock(mat_report, stock_list$mat_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(mat_stock, "__num"))))),
  
  # Report mean weight
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__wgt"))))),
  g3a_report_stock(mat_report, stock_list$mat_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(mat_stock, "__wgt"))))),
  
  # Report biomass caught by survey
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__igfs"))))),
  g3a_report_stock(mat_report, stock_list$mat_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(mat_stock, "__igfs"))))),
  
  # Report biomass caught by commercial, longline
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__lln"))))),
  g3a_report_stock(mat_report, stock_list$mat_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(mat_stock, "__lln"))))),
  
  # Bottom trawl
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__bmt"))))),
  g3a_report_stock(mat_report, stock_list$mat_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(mat_stock, "__bmt"))))),
  
  # Gillnet
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__gil"))))),
  g3a_report_stock(mat_report, stock_list$mat_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(mat_stock, "__gil"))))),
  
  # Foreign 
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__foreign"))))),
  g3a_report_stock(mat_report, stock_list$mat_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(mat_stock, "__foreign"))))),
  
  # Recruitment numbers and weight
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__renewalnum"))))),
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__renewalwgt"))))),
  
  # Report suitability (survey)
  g3a_report_stock(imm_report, stock_list$imm_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(imm_stock, "__suit_igfs"))))),
  g3a_report_stock(mat_report, stock_list$mat_stock, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0(mat_stock, "__suit_igfs"))))))
  