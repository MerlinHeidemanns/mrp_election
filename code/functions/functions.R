# functions
# extract quantiles and medians
extract_quantiles_mean <- function(draws, dimensions, t_scale){
  t <- 1:t_scale
  median <- apply(draws, MARGIN = dimensions, median)
  q25   <- apply(draws, MARGIN = dimensions, function(x) quantile(x, c(0.25)))
  q75   <- apply(draws, MARGIN = dimensions, function(x) quantile(x, c(0.75)))
  q10   <- apply(draws, MARGIN = dimensions, function(x) quantile(x, c(0.10)))
  q90   <- apply(draws, MARGIN = dimensions, function(x) quantile(x, c(0.90)))
  return_df <- data.frame(
    t = t,
    median = median,
    q25 = q25,
    q75 = q75,
    q10 = q10,
    q90 = q90
  )
  return(return_df)
}