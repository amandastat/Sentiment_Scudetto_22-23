library(rtweet)

token <- create_token(
  app = "tesi_di_laurea",
  consumer_key = "K0Ry8umWeuoEYfdlljIJpiiCk",
  consumer_secret = "MNhf3RqxOqnpxQionU6bYrYpDa8mMstQGzXePDcO3DTwESqVEq",
  access_token = "1246618694941396997-JbLewQkAtVIqIwYE8SnsxVWPTxM0KV",
  access_secret = "Cr7YKjF38KDzhGK9FRLcZmw3KWDvRSbgGks8rGUGlATfQ")
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
