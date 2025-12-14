{
library(quantmod)
library(lubridate)
library(psych)
library(yfR)
ticker = c("PG", "KO", "PEP", "CL", "KMB", "MCD", "WMT", "ADP", "EMR", "ITW")

Market_Return = yf_get(
  "^GSPC",
  first_date = Sys.Date() - 3650,
  last_date = Sys.Date(),
  thresh_bad_data = 0.75,
  bench_ticker = "^GSPC",
  type_return = "arit",
  freq_data = "daily",
  how_to_aggregate = "last",
  do_complete_data = FALSE,
  do_cache = TRUE,
  cache_folder = yf_cachefolder_get(),
  do_parallel = FALSE,
  be_quiet = FALSE
)

R_f = yf_live_prices("^TNX")

Decision_Matrix <- data.frame(
  P_E = numeric(),
  P_R = numeric(),
  Decision = character(),
  stringsAsFactors = FALSE
)
colnames(Decision_Matrix) = c("Expected Price", "Actual Price", "Valuation")

for (k in 1:length(ticker)) {

Div_n = getDividends(ticker[k], from = "1970-01-01",
             to = Sys.Date(), 
             src = "yahoo", 
             auto.assign = FALSE, 
             auto.update = TRUE, 
             verbose = FALSE,
             split.adjust = TRUE,
             curl.options = list())
yearly_Div = apply.yearly(Div_n, sum)
#AI Block
if (nrow(yearly_Div) < 15) next
colnames(yearly_Div) = c("ticker")
dividend.geometric.mean = numeric(0)
for (f in (length(yearly_Div)-15):length(yearly_Div)){
  
  Difference = ((as.numeric(yearly_Div$ticker[f]) - as.numeric(yearly_Div$ticker[(f-1)])) / as.numeric(yearly_Div$ticker[(f-1)]) + 1)
  dividend.geometric.mean = c(dividend.geometric.mean, Difference)
}
g = (geometric.mean(dividend.geometric.mean) - 1)

ticker_price = yf_get(
  ticker[k],
  first_date = Sys.Date() - 3650,
  last_date = Sys.Date(),
  thresh_bad_data = 0.75,
  bench_ticker = "^GSPC",
  type_return = "arit",
  freq_data = "daily",
  how_to_aggregate = "last",
  do_complete_data = FALSE,
  do_cache = TRUE,
  cache_folder = yf_cachefolder_get(),
  do_parallel = FALSE,
  be_quiet = FALSE
)





beta = (cov(ticker_price$ret_adjusted_prices[2:nrow(ticker_price)], Market_Return$ret_adjusted_prices[2:nrow(Market_Return)]) / var(Market_Return$ret_adjusted_prices[2:nrow(Market_Return)]))

M_r = mean(na.omit(Market_Return$ret_adjusted_prices)) * 252
R = R_f$last_price / 1000 + beta * M_r
P_E = (((yearly_Div[length(yearly_Div)])*(1+g)) / (R - g))
P_R = yf_live_prices(ticker[k])

#AI Block
if (R <= g) next("Gordon model invalid: R must be greater than g")

if (P_E > P_R$last_price) {
  print("This stock is undervalued, you should buy it!")
  #AI BLOCK
  Decision_Matrix <- rbind(
    Decision_Matrix,
    data.frame(
      `Expected Price` = as.numeric(P_E),
      `Actual Price` = as.numeric(P_R$last_price),
      Valuation = "undervalued"
    )
  
  )
  
} else if (P_E < P_R$last_price) {
  print("This stock is overvalued, don't buy it!")
  #AI BLOCK
  Decision_Matrix <- rbind(
    Decision_Matrix,
    data.frame(
      `Expected Price` = as.numeric(P_E),
      `Actual Price` = as.numeric(P_R$last_price),
      Valuation = "overvalued"
    )
  )
  
} else {
  print("This stock is just perfect, go with the flow")
  #AI BLOCK
  Decision_Matrix <- rbind(
    Decision_Matrix,
    data.frame(
      `Expected Price` = as.numeric(P_E),
      `Actual Price` = as.numeric(P_R$last_price),
      Valuation = "fair"
    )
  )
  
}

}
}
