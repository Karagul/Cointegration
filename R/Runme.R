# From https://statcompute.wordpress.com/2018/07/29/co-integration-and-pairs-trading/

pkgs <- list("quantmod", "doParallel", "foreach", "urca")
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)

jtest <- function(t1, t2) {
  start <- sd
  getSymbols(t1, from = start)
  getSymbols(t2, from = start)
  j <- summary(ca.jo(cbind(get(t1)[, 6], get(t2)[, 6])))
  r <- data.frame(stock1 = t1, stock2 = t2, stat = j@teststat[2])
  r[, c("pct10", "pct5", "pct1")] <- j@cval[2, ]
  return(r)
}

pair <- function(lst) {
  d2 <- data.frame(t(combn(lst, 2)))
  #stat <- foreach(i = 1:nrow(d2), .combine = rbind) %dopar% jtest(as.character(d2[i, 1]), as.character(d2[i, 2]))
  stat <- foreach(i = 1:nrow(d2), .combine = rbind) %do% jtest(as.character(d2[i, 1]), as.character(d2[i, 2]))
  stat <- stat[order(-stat$stat), ]
  stat$coint <- ""
  stat$coint[stat$stat > stat$pct10] <- "*"
  stat$coint[stat$stat > stat$pct5] <- "**"
  stat$coint[stat$stat > stat$pct1] <- "***"
  rownames(stat) <- NULL
  return(stat)
}

sd <- "2010-01-01"
tickers <- c("FITB", "BBT", "MTB", "STI", "PNC", "HBAN", "CMA", "USB", "KEY", "JPM", "C", "BAC", "WFC")
pair(tickers)