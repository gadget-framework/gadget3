## -----------------------------------------------------------------------------
## Collect catches by fleet:
## -----------------------------------------------------------------------------

## Surveys
igfs_landings <- 
  structure(data.frame(year=defaults$year,step=2,area=1,total_weight=1),
            area_group = mfdb_group(`1` = 1))

## Commercial
lln_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('HLN','LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

bmt_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

gil_landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear='GIL',
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

foreign_landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            sampling_type = 'FLND',
                            data_source = c('lods.foreign.landings','statlant.foreign.landings'),
                            species = defaults$species),
                            defaults))

if (TRUE){
  save(lln_landings, file="demo-ling/data/lln_landings.Rdata")
  save(bmt_landings, file="demo-ling/data/bmt_landings.Rdata")
  save(gil_landings, file="demo-ling/data/gil_landings.Rdata")
  save(foreign_landings, file="demo-ling/data/foreign_landings.Rdata")
  save(igfs_landings, file="demo-ling/data/igfs_landings.Rdata")
}
