# setNames(fit$arma, c("p", "q", "P", "Q", "m", "d", "D"))
# p q P Q m d D 
# 1 1 0 0 1 1 0 
# arima(p,d,q)(P,D,Q)m

# 다시 저장용 arima2
for ( i in 1:39 ){
  load(paste0('./arima/arima_SO2_sgg', i, '.RData'))
  saveRDS(mod_lst, file=paste0("./arima2/arima2_SO2_sgg",i,".rds"))
  rm(list=ls())
}

# 불러오기
#mod_lst <- readRDS("./arima2/arima2_SO2_sgg5.rds")

arima.so2 <- list()
for ( i in 1:39 ){
  #load(paste0('./arima/arima_SO2_sgg', i, '.RData'))
  mod_lst <- readRDS(paste0("./arima2/arima2_SO2_sgg", i, ".rds"))
  arima.so2[[i]] <- mod_lst[[1]]
}
