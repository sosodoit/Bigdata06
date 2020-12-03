# setNames(fit$arma, c("p", "q", "P", "Q", "m", "d", "D"))
# p q P Q m d D 
# 1 1 0 0 1 1 0 
# arima(p,d,q)(P,D,Q)m

arima.so2 <- list()
for ( i in 1:34 ){
  load(paste0('./arima/arima_SO2_sgg', i, '.RData'))
  arima.so2[[i]] <- mod_lst[[1]]$arma
}
