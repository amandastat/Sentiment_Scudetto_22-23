library(rtweet)

token <- create_token(
  app = "",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "")
rt <- search_tweets(
  "Scudetto", n = 18000, include_rts = FALSE, token = token
)
ts_plot(
  rt,
  title = "Numero di tweet giornalieri",
  xlab= "frequenza",
  ylab= "Data",
  subtitle = "The classic Box & Jenkins airline data"
)
