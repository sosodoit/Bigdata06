arima.so2 <- list()
for ( i in 1:39 ){
  load(paste0('/arimadata/arima_so2_sgg', i, '.RData'))
  list[[i]] <- mod_lst[[1]]$arma
}