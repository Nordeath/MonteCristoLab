library(pacman)
p_load(quantmod, xts, zoo, TTR, PerformanceAnalytics, ggplot2, rugarch)
tqdata <- getSymbols('TQQQ', from = "2023-05-01", to = '2026-07-10', src = 'yahoo')

head(TQQQ [, 1:5], 5)
tail(TQQQ [, 1:5], 5)

daily_ret <- dailyReturn(Cl(TQQQ), type = 'log')
daily_ret <- na.omit(diff(log(Cl(TQQQ))))
daily_ret <- data.frame(index(daily_ret), daily_ret)
colnames(daily_ret) <- c("date", "return")
rownames(daily_ret) <- 1:nrow(daily_ret)

p1 <- ggplot(daily_ret, aes(x=date, y=return))
p1 + geom_line(colour="#471585") + labs(title="3x Leverage NASDAQ Return", x="Date", y="Return")

p2 <- ggplot(daily_ret)
p2 + geom_histogram(aes(x=return, y=..density..), binwidth = 0.005, color="#071585", fill="pink", size=1) +
  stat_function(fun = dnorm, args = list(mean = mean(daily_ret$return, na.rm = T), sd = sd(daily_ret$return, na.rm = T)), size=1)

daily_ret_xts <- xts(daily_ret[,-1], order.by=daily_ret[,1])
realizedvol <- rollapply(daily_ret_xts, width = 20, FUN=sd.annualized)
vol <- data.frame(index(realizedvol), realizedvol)
colnames(vol) <- c("date", "volatility")

p3 <- ggplot(vol, aes(x=date, y=volatility))
p3 + geom_line(color="#037801") + labs(title="Monthly Volatility for TQQQ Hedge", x="Date", y="Volatility")

ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "sstd" # CHANGE FROM 'norm' TO SKEW STUDENT'S T
)

garch_spec <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,1), include.mean = TRUE), distribution.model = "sstd")
                                                                                                 

#fit_garch <- ugarchfit(spec = garch_spec, data = vol[-c(1:19),2])
fit_garch <- ugarchfit(
  spec = garch_spec,
  data = na.omit(daily_ret_xts))
fit_garch

  #I kept this as a diagnostic test to insure log-values remain around +/- .03
  head(daily_ret_xts)
  summary(daily_ret_xts)
  head(na.omit(daily_ret_xts))
  str(daily_ret_xts)
  
  # Generate the forecast for 1 day ahead
  garch_forecast = ugarchforecast(fit_garch, n.ahead = 1)
  
  # Print the expected volatility (sigma) for tomorrow (Decimal value indicates a markup or drawdown of that magnitude)
  print(sigma(garch_forecast))
  
  # Print the expected return (series) for tomorrow
  print(fitted(garch_forecast))
