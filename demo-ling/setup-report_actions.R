## -----------------------------------------------------------------------------
## Set up report actions:

# Disaggregated report storage for *_imm stock (we store with same age/step/length as ling itself)
imm_report <- g3s_clone(ling_imm, 'imm_report') %>%
  g3s_time(year = local(year_range), step = 1:4)

# Disaggregated report storage for *_mat (we store with same age/step/length as ling itself)
mat_report <- g3s_clone(ling_mat, 'mat_report') %>%
  g3s_time(year = local(year_range), step = 1:4)


report_actions <- list(
  
  # Report numbers
  g3a_report_stock(imm_report, ling_imm, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__num"))))),
  g3a_report_stock(mat_report, ling_mat, 
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_mat", "__num"))))),
  
  # Report mean weight
  g3a_report_stock(imm_report, ling_imm,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__wgt"))))),
  g3a_report_stock(mat_report, ling_mat,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_mat", "__wgt"))))),

  # Report biomass caught by survey
  g3a_report_stock(imm_report, ling_imm,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__igfs"))))),
  g3a_report_stock(mat_report, ling_mat,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_mat", "__igfs"))))),

  # Report biomass caught by commercial, longline
  g3a_report_stock(imm_report, ling_imm,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__lln"))))),
  g3a_report_stock(mat_report, ling_mat,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_mat", "__lln"))))),

  # Bottom trawl
  g3a_report_stock(imm_report, ling_imm,
                  gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__bmt"))))),
  g3a_report_stock(mat_report, ling_mat,
                  gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_mat", "__bmt"))))),

  # Gillnet
  g3a_report_stock(imm_report, ling_imm,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__gil"))))),
  g3a_report_stock(mat_report, ling_mat,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_mat", "__gil"))))),

  # Foreign
  g3a_report_stock(imm_report, ling_imm,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__foreign"))))),
  g3a_report_stock(mat_report, ling_mat,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_mat", "__foreign"))))),

  # Recruitment numbers and weight
  g3a_report_stock(imm_report, ling_imm,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__renewalnum"))))),
  g3a_report_stock(imm_report, ling_imm,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__renewalwgt"))))),

  # Report suitability (survey)
  g3a_report_stock(imm_report, ling_imm,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_imm", "__suit_igfs"))))),
  g3a_report_stock(mat_report, ling_mat,
                   gadget3:::f_substitute(~stock_ss(x), list(x=as.symbol(paste0("ling_mat", "__suit_igfs"))))))
