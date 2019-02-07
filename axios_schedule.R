# !diagnostics off

library(docxtractr)
library(lubridate)
library(timevis)
library(slickR)
library(magrittr)

# Functions ----

pad_day <- function(x){
  
  start <- head(x,1)
  end <- tail(x,1)
  
  start$TIME <- '00:00:00'
  start$DATE_TIME <- as.POSIXct(paste(as.Date(start$DATE_TIME),start$TIME))
  start$TASK <- 'PRE DAY'
  start[1,c(3:9)] <- NA
  
  end$TIME <- '23:59:59'
  end$DATE_TIME <- as.POSIXct(paste(as.Date(start$DATE_TIME),end$TIME))
  end$TASK <- 'POST DAY'
  end[1,c(3:9)] <- NA
  
  ret <- dplyr::bind_rows(start,x,end)
  
  ret <- ret%>%
    dplyr::mutate(
      TIME_END = dplyr::lead(TIME,1)
    )%>%
    head(.,-1)
  
  
  ret$DATE_TIME_END <- as.POSIXct(paste(as.Date(ret$DATE_TIME),ret$TIME_END))
  
  ret
}

time_find <- function(x,y){
  
  xx <- as.POSIXct(x$created_at)
  lb <- y$DATE_TIME
  ub <- y$DATE_TIME_END
  
  
  TASK <- purrr::map_chr(xx, function(x){
    ret <- y$TASK[which(x%within%lubridate::interval(lb,ub))] 
    if(length(ret)>1){
      ret <- tail(ret,1)
    }
    
    ret
  })
  
  x$TASK <- TASK
  x%>%tidyr::nest(-TASK,.key='tweets')
}



final <- tbls_days_trim%>%
  tidyr::unnest(NOTES)%>%
  dplyr::filter(!grepl('EST',V4)&TIME!='RON')%>%
  dplyr::distinct()

final <- final%>%dplyr::group_by(DATE,TIME,TASK,FIELD)%>%dplyr::mutate(n=1:dplyr::n())

final_rep <- final%>%dplyr::filter(n>1)%>%dplyr::select(DATE,TIME,TASK,FIELD)%>%dplyr::distinct()%>%dplyr::mutate(flag = TRUE)

final <- final%>%dplyr::left_join(final_rep)%>%dplyr::mutate(flag = ifelse(is.na(flag),FALSE,flag))%>%dplyr::ungroup()

final_idx <- final%>%dplyr::mutate(n=1:dplyr::n())%>%dplyr::filter(flag)

bad_idx <- final_idx%>%dplyr::pull(n)

final$FIELD[bad_idx[c(3,5,7,12)]] <- 'Project_Officer'
final$TASK[bad_idx[c(9,11,14,16,18,20,22)]] <- paste(final$TASK[bad_idx[c(9,11,14,16,18,20,22)]],final$V4[bad_idx[c(9,11,14,16,18,20,22)]])
final$UPDATE_TASK <- FALSE
final$UPDATE_TASK[bad_idx[c(9,11,14,16,18,20,22)]] <- TRUE

dt_fix <- final[final$UPDATE_TASK,c('DATE','TIME')]

for(i in 1:nrow(dt_fix)){
  new_task <- final$TASK[(final$DATE==dt_fix$DATE[i])&(final$TIME==dt_fix$TIME[i])&(final$UPDATE_TASK)]
  final$TASK[(final$DATE==dt_fix$DATE[i])&(final$TIME==dt_fix$TIME[i])] <- new_task
}

final <- final%>%dplyr::slice(-bad_idx[c(2,9,11,14,16,18,20,22,25)])

final_wide <- final%>%
  dplyr::select(-c(n,flag,UPDATE_TASK))%>%
  tidyr::spread(FIELD,V4)

press_idx <- which(is.na(final_wide$Location)&!is.na(final_wide$Press))

final_wide$Location[press_idx] <- final_wide$Press[press_idx]
final_wide$Press[press_idx] <- NA
final_wide$Project_Officer[31] <- final_wide$Location[31]
final_wide$Location[31] <- final_wide$Press[31]
final_wide$Press[31] <- NA

final_wide$Project_Officer[503] <- final_wide$Program_Officer[503]
final_wide$Program_Officer[503] <- NA

final_wide$Program_Officer <- NULL

final_wide$Location[c(30,290)] <- final_wide$RON[c(30,290)]
final_wide$RON <- NULL


final_wide$Project_Officer[207] <- final_wide$Project_Office[207]
final_wide$Project_Office <- NULL

final_wide$Press[202] <- final_wide$Location[202]
final_wide$Location[202] <- 'Tupelo, MS'
final_wide <- final_wide[-204,]

final_wide$TASK[483] <- paste(final_wide$TASK[482],final_wide$TASK[483])
final_wide$Duration[483] <- final_wide$Duration[482]
final_wide <- final_wide[-482,]

final_wide$Note[29] <- 'The First Lady will attend'
final_wide <- final_wide[-30,]

final_wide$TASK[536] <- paste(final_wide$TASK[537],final_wide$TASK[536])
final_wide$Duration[536] <- final_wide$Duration[537]
final_wide <- final_wide[-537,]

final_wide$TASK[grepl('NA OF THE REPUBLIC OF INDIA',final_wide$TASK)] <- 'TRILATERAL MEETING WITH THE PRIME MINISTER OF JAPAN AND THE PRIME MINISTER OF THE REPUBLIC OF INDIA'
final_wide$TASK[grepl('NA AUSTRALIA',final_wide$TASK)] <-'BILATERAL PULL-ASIDE WITH THE PRIME MINISTER OF THE COMMONWEALTH OF AUSTRALIA'
final_wide$TASK[grepl('NA LEADERS',final_wide$TASK)] <- 'IRST STEP ACT ROUNDTABLE WITH GOVERNOR PHIL BRYANT AND LAW ENFORCEMENT LEADERS'


final_type <- final_wide%>%
  dplyr::mutate(TYPE = dplyr::case_when(
    grepl('DEPART|ARRIVE',TASK) ~ 'TRAVEL',
    grepl('EXECUTIVE',TASK) ~ 'EXECUTIVE',
    grepl('LUNCH|DINNER',TASK) ~ 'FOOD',
    grepl('RALLY',TASK) ~ 'RALLY',
    grepl('MEDIA|PHOTO OPPORTUNITY|ANNOUNCEMENT|VISIT|CEREMONY|PHOTO|CEREMONY|CHRISTMAS|CLEMSON|SIGNING|INTERVIEW|VIDEO RECORDING|CONVENTION',TASK) ~ 'MEDIA',
    grepl('CEREMONIAL|FUNERAL|PERFORMANCE|INVESTITURE|MEET AND GREET|RECEPTION|OPENING REMARKS',TASK) ~ 'CEREMONY',
    grepl('BRIEFING|DISCUSSION|POLICY|BRIEF|ROUNDTABLE|SPEECH PREP|PHONE CALL|HUMAN TRAFFICKING|PULL-ASIDE|MEETING|CONFERENCE|REMARKS',TASK) ~ 'WORK',
    TRUE ~ 'OTHER')
    )

final_type$TIME_SHIFT <- 0
tz_idx <- which(!grepl('(AM|PM)$',final_type$TIME))
final_type$TIME_SHIFT[grepl('CST$',final_type$TIME)] <- -1
final_type$TIME_SHIFT[c(23:27,142:147,201:213,235:246)] <- -1
final_type$TIME_SHIFT[final_type$DATE=='Friday, November 30, 2018'] <- 2
final_type$TIME <- gsub(' (CST|EST|E ST|Local)','',final_type$TIME)
final_type$TIME <- gsub('TBD','8:00 AM',final_type$TIME)

final_type$TIME_MISS <- sapply(gregexpr('\\s', final_type$TIME),length)
final_type$TIME[final_type$TIME_MISS>1] <- sub('\\s',':',final_type$TIME[final_type$TIME_MISS>1])
final_type$DATE <- as.Date(strptime(final_type$DATE,'%A, %B %d, %Y'))
final_type$TIME <- strptime(final_type$TIME,'%I:%M %p') + (final_type$TIME_SHIFT*60*60)
final_type$TIME <- format(final_type$TIME,format = "%H:%M:%S")

final_type$TIME_MISS <- NULL

final_type$DATE_TIME <- as.POSIXct(paste(final_type$DATE,final_type$TIME))

schedule <- final_type%>%
  dplyr::rename(date=DATE)%>%
  dplyr::arrange(DATE_TIME)%>%
  tidyr::nest(-date,.key='schedule')%>%
  dplyr::mutate(schedule = purrr::map(schedule,pad_day))



#source('get_tweets.R')
twn <- readRDS(file = 'tweets.Rds')

final_tweet <- schedule%>%
  dplyr::left_join(twn,by='date')%>%
  dplyr::mutate(n_tweet = purrr::map_dbl(tweet,nrow))



final_tweet <- final_tweet%>%
  dplyr::mutate(
    intersect  = purrr::map2(tweet,schedule,time_find),
    merge = purrr::map2(schedule,intersect,.f=function(x,y) x%>%dplyr::left_join(y,by='TASK'))
  )  

ret <- final_tweet%>%dplyr::select(date,merge)%>%tidyr::unnest(merge)

axios <- ret%>%
  dplyr::filter(purrr::map_lgl(tweets,.f=function(x) !is.null(x)))%>%
  tidyr::unnest(tweets)%>%
  dplyr::mutate(
    TYPE = ifelse(is.na(TYPE),'PREDAY',TYPE)
  )

#saveRDS(ret_df,file = 'axios.Rds',compress = TRUE)

#axios <- readRDS('axios.Rds')