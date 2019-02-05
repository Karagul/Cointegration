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



# From https://statcompute.wordpress.com/2019/01/05/co-integration-and-mean-reverting-portfolio/
  
#First of all, we downloaded series of three stock prices from finance.yahoo.com.
### GET DATA FROM YAHOO FINANCE

quantmod::getSymbols("FITB", from = "2010-01-01")
FITB <- get("FITB")[, 6]
quantmod::getSymbols("MTB", from = "2010-01-01")
MTB <- get("MTB")[, 6]
quantmod::getSymbols("BAC", from = "2010-01-01")
BAC <- get("BAC")[, 6]

# We can utilize the Pu statistic in the Phillips-Ouliaris test to identify the co-integration among three stocks

k <- trunc(4 + (length(FITB) / 100) ^ 0.25)
po.test <- urca::ca.po(cbind(FITB, MTB, BAC), demean = "constant", lag = "short", type = "Pu")
#Value of test-statistic is: 62.7037
#Critical values of Pu are:
#                  10pct    5pct    1pct
#critical values 33.6955 40.5252 53.8731

po.test@testreg

# Adjust as per the data fit
#ts1 <- FITB + 1.097465 - 0.152637 * MTB - 0.140457 * BAC
ts1 <- FITB - coef(po.test@testreg)[1,1] - coef(po.test@testreg)[2,1] * MTB - coef(po.test@testreg)[3,1] * BAC

# Check if linear combination is stationary
# First use an Augmented Dickey-Fuller Unit Root Test
tseries::adf.test(ts1, k = k)
plot(ts1)

# Now a Johansen test
js.test <- urca::ca.jo(cbind(FITB, MTB, BAC), type = "trace", K = k, spec = "longrun", ecdet = "const")
#          test 10pct  5pct  1pct
#r <= 2 |  3.26  7.52  9.24 12.97
#r <= 1 | 19.72 17.85 19.96 24.60
#r = 0  | 45.88 32.00 34.91 41.07

js.test@V

# Another stationary linear combination can be made use the eigenvectors provided by the Johansen test
# ts2 <- FITB + 0.6216917 - 0.1398349 * MTB - 0.1916826 * BAC

ts2 <- FITB + js.test@V[4,1] + js.test@V[2,1] * MTB + js.test@V[3,1] * BAC
tseries::adf.test(ts2, k = k)
plot(ts1)
  