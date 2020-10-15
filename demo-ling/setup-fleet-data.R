
## Collect catches by fleet:
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
