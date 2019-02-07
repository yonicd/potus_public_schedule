#remotes::install_github('yonicd/rtweet')
library(rtweet)

tw <- rtweet::get_timeline('realDonaldTrump',1000)

tw <- tw%>%
  dplyr::mutate(
    created_at = format(created_at,tz = 'EST',usetz = TRUE),
    d = as.Date(created_at)
  )%>%
  dplyr::filter(dplyr::between(d,as.Date('2018-11-07'),as.Date('2019-02-01')))

tw_embed <- purrr::map_chr(tw$status_id,rtweet::tweet_embed,screen_name = tw$screen_name[1])

twn <- tw%>%
  dplyr::select(date=d,created_at,text,source)%>%
  dplyr::mutate(embed = tw_embed)%>%
  dplyr::arrange(created_at)%>%
  tidyr::nest(-date,.key='tweet')

saveRDS(twn,file = 'tweets.Rds',compress = TRUE)
