
# Turn fit parameters back into list, which can be used in table or R function
# param_list <- relist(fit$par, unclass(tmb_param$value))
# for(i in seq_along(param_list)){
#   if(is.na(param_list[[i]])){
#     param_list[[i]] <-  tmb_param$value[[i]]
#   }
# }
# Gather results

param_list <- ling_param
gadget3::g3_tmb_relist(tmb_param,fit.opt$par) -> tmp
for(nn in names(tmp)){param_list[[nn]] <- tmp[[nn]]}
# par <- fit.opt$par
#
# names(par) <- gsub('\\_\\_','\\.', names(par))
#
# for(var in names(par)){
#   param_list[[var]] <- par[[var]]
# }
## change names (do we need to do this?)

ling_model <- g3_to_r(c(
  ling_mat_actions,
  ling_imm_actions,
  fleet_actions,
  ling_likelihood_actions,
  report_actions,
  time_actions))

res <- ling_model(param_list)
res[[1]]

out <- attributes(res)
names(out)

#out.tmb <- ling_model_tmb$report(fit.opt2$par)

# si_dat <-
#   out[grepl('cdist_si_igfs_si.+obs',names(out))] %>%
#   map(as.data.frame.table,responseName = 'obs') %>%
#   map(mutate,year=1985:(1984+n())) %>%
#   bind_rows(.id='data') %>%
#   mutate(data = gsub('cdist_(.+)_obs__num','\\1',data)) %>%
#   left_join(out[grepl('cdist_si_igfs_si.+model',names(out))] %>%
#               map(as.data.frame.table,responseName = 'model') %>%
#               map(mutate,year=1985:(1984+n())) %>%
#               bind_rows(.id='data') %>%
#               mutate(data = gsub('cdist_(.+)_model__num','\\1',data)) ) %>%
#   as_tibble()
# ggplot(aes(year,Freq)) + geom_point() + facet_wrap(~data)

dat <-
  out[grep('^cdist',names(out))] %>%
  map(as.data.frame.table, stringsAsFactors = FALSE) %>%
  map(as_tibble) %>%
  bind_rows(.id = 'comp') %>%
  mutate(type = gsub('.+(model|obs)','\\1',comp),
         comp = gsub('(.+)model|obs','\\1',comp),
         comp = gsub('\\_\\_[a-z]+','',comp),
         length = gsub('len','',length) %>% as.numeric(),
         area = as.numeric(area),
         age = gsub('age','',age) %>% as.numeric(),
         year = round(as.numeric(time)/1000),
         step = as.numeric(time) - year*1e3) %>%
  separate(type,c('origin','representation')) %>%
  filter(representation == 'num')

dat %>%
  filter(grepl('si',comp)) %>%
  spread(origin,Freq) %>%
  group_by(comp) %>%
  mutate(pred = model*exp(mean(log(obs)-log(model)))) %>%
  ggplot(aes(year,obs)) + geom_point() +
  geom_line(aes(y=pred)) +
  #ggplot(aes(model,obs)) + geom_point() +
  facet_wrap(~comp, scale = 'free')
#ggplot(aes(round(as.numeric(as.character(time))/1000), Freq, lty = origin)) + geom_line() +
#facet_wrap(~comp)




dat %>%
  filter(comp == 'cdist_ldist_gil_') %>%
  group_by(year,step,origin) %>%
  mutate(Freq = Freq/sum(Freq,na.rm = TRUE)) %>%
  spread(origin,Freq) %>%
  ggplot(aes(length,obs)) + geom_point() +
  geom_line(aes(y=model)) +
  facet_wrap(~year + step, scale = 'free')

dat %>%
  filter(comp == 'cdist_ldist_igfs_') %>%
  group_by(year,step,origin) %>%
  mutate(Freq = Freq/sum(Freq,na.rm = TRUE)) %>%
  spread(origin,Freq) %>%
  ggplot(aes(length,obs)) + geom_point() +
  geom_line(aes(y=model)) +
  facet_wrap(~year + step, scale = 'free')

dat %>%
  filter(comp == 'cdist_aldist_igfs_') %>%
  group_by(year,step,origin) %>%
  mutate(Freq = Freq/sum(Freq,na.rm = TRUE)) %>%
  group_by(year,step,age,origin) %>%
  summarise(Freq = sum(Freq)) %>%
  spread(origin,Freq) %>%
  ggplot(aes(age,obs)) + geom_point() +
  geom_line(aes(y=model)) +
  facet_wrap(~year + step, scale = 'free')


dat %>%
  filter(comp == 'cdist_ldist_bmt_') %>%
  group_by(year,step,origin) %>%
  mutate(Freq = Freq/sum(Freq,na.rm = TRUE)) %>%
  spread(origin,Freq) %>%
  ggplot(aes(length,obs)) + geom_point() +
  geom_point(aes(y=model),col='red') +
  facet_wrap(~year + step, scale = 'free')
dat %>%
  filter(comp == 'cdist_aldist_bmt_') %>%
  group_by(year,step,origin) %>%
  mutate(Freq = Freq/sum(Freq,na.rm = TRUE)) %>%
  group_by(year,step,age,origin) %>%
  summarise(Freq = sum(Freq)) %>%
  spread(origin,Freq) %>%
  ggplot(aes(age,obs)) + geom_point() +
  geom_line(aes(y=model)) +
  facet_wrap(~year + step, scale = 'free')


out[grep('nll',names(out))] %>%
  map(~tibble(time = names(.), lik_score = as.numeric(.))) %>%
  bind_rows(.id = 'lik_comp') %>%
  mutate(type = gsub('.+(wgt|num|weight)','\\1',lik_comp),
         lik_comp = gsub('(.+)wgt|num|weight','\\1',lik_comp),
         lik_comp = gsub('\\_\\_[a-z]+','',lik_comp)) %>%
  spread(type, lik_score) %>%
  separate(time, c('year','step'), convert = TRUE) %>%
  filter(weight > 0) %>%
  ggplot(aes(year + (step - 1)/4,weight*pmax(num,wgt,na.rm = TRUE))) + geom_point() +
  facet_wrap(~lik_comp, scales = 'free_y')


out[grep('report',names(out))] %>%
  map(as.data.frame.table, stringsAsFactors = FALSE) %>%
  map(as_tibble) %>%
  bind_rows(.id = 'data') %>%
  mutate(length = gsub('len','',length) %>% as.numeric(),
         area = as.numeric(area),
         age = gsub('age','',age) %>% as.numeric(),
         year = round(as.numeric(time)/1000),
         step = as.numeric(time) - year*1e3) %>%
  separate(data,c('stock','fleet'), sep = '_report__')


out$mat_report__num %>%
  as.data.frame.table(stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  mutate(length = gsub('len','',length) %>% as.numeric(),
         age = gsub('age','',age) %>% as.numeric(),
         year = round(as.numeric(time)/1000),
         step = as.numeric(time) - year*1e3) %>%
  group_by(year,step,length) %>%
  summarise(n = sum(Freq)) %>%
  ggplot(aes(length,n))+ geom_line() + facet_wrap(~year + (step-1)/4)


out[grep('report__num',names(out))] %>%
  map( as.data.frame.table, stringsAsFactors = FALSE) %>%
  map(as_tibble) %>%
  bind_rows(.id = 'stock') %>%
  mutate(length = gsub('len','',length) %>% as.numeric(),
         age = gsub('age','',age) %>% as.numeric(),
         year = round(as.numeric(time)/1000),
         step = as.numeric(time) - year*1e3,
         yc = year-age) %>%
  group_by(year,stock,yc,step,age,length) %>%
     summarise(n = sum(Freq)) %>%
  filter(year == 2000,step == 1) %>%
     ggplot(aes(length,n,col = interaction(stock,as.factor(age))))+ geom_line() + facet_wrap(~year + (step-1)/4)



out[grep('report__num',names(out))] %>%
  map( as.data.frame.table, stringsAsFactors = FALSE) %>%
  map(as_tibble) %>%
  bind_rows(.id = 'stock') %>%
  mutate(length = gsub('len','',length) %>% as.numeric(),
         age = gsub('age','',age) %>% as.numeric(),
         year = round(as.numeric(time)/1000),
         step = as.numeric(time) - year*1e3,
         yc = year-age) %>%
  group_by(year,yc,step,age,length) %>%
  summarise(n = sum(Freq)) %>%
  filter(year %in% 1990:1994,step == 1) %>%
  ggplot(aes(length,n,col = as.factor(age)))+ geom_line() + facet_wrap(~year + (step-1)/4,scales = 'free_y')


