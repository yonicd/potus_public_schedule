library(slickR)
library(timevis)
library(treemap)
library(d3treeR)
library(shiny)
library(dplyr)

axios <- readRDS('axios.Rds')

test_background <- axios%>%
  dplyr::mutate(start = DATE_TIME,
                end = DATE_TIME_END,
                style = NA,
                type = 'background',
                dat_type = 'schedule')%>%
  dplyr::select(start,end,type,dat_type,style,TYPE)

test_point <- axios%>%
  dplyr::mutate(start = as.POSIXct(created_at),
                end = as.POSIXct(created_at),
                style = dplyr::case_when(
                  source=="Twitter for iPhone" ~ "background-color: red;",
                  source=="Twitter Media Studio" ~ "background-color: blue;",
                  source=="Twitter for iPad" ~ "background-color: green;"
                  ),
                type = 'background',
                dat_type = 'tweet')%>%
  dplyr::select(start,end,type,dat_type,style,TYPE)

group_data <- axios%>%
  dplyr::select(TYPE)%>%
  dplyr::distinct()%>%
  dplyr::mutate(group=1:dplyr::n())

test <- dplyr::bind_rows(test_background,test_point)%>%dplyr::mutate(id = 1:dplyr::n())

test <- test%>%dplyr::left_join(group_data,by='TYPE')

group_data <- group_data%>%dplyr::rename(content = TYPE,id=group)

ui <- shiny::fluidPage(
    shiny::sidebarLayout(
    shiny::sidebarPanel(
      timevis::timevisOutput("appts"),
      d3treeR::d3tree3Output('plot')
    ),
    shiny::mainPanel(
      shiny::sliderInput(
        inputId = 'n',
        label = 'Tweets to load',
        min = 1,
        max = 5,
        value = 5),
      slickR::slickROutput('slick',width = '95%')      
    )
)
)

server <- function(input, output,session) {
  
  output$appts <- timevis::renderTimevis({
    timevis::timevis(data = test,groups = group_data)
  })
  
  output$window <- renderText({
    w <- input$appts_window
    w1 <- strptime(w[1],'%Y-%m-%dT%H:%M:%S',tz = 'UTC') - 6*60*60
    w2 <- strptime(w[2],'%Y-%m-%dT%H:%M:%S',tz = 'UTC') - 6*60*60
    paste(as.character(w1), "to", as.character(w2))
  })
  
  dat <- eventReactive(c(input$appts_window),{
    
    w    <- input$appts_window
    ret  <- axios
    
    if ( !is.null(w) ) {
      
      w1 <- strptime(w[1],'%Y-%m-%dT%H:%M:%S',tz = 'UTC') - 6*60*60
      w2 <- strptime(w[2],'%Y-%m-%dT%H:%M:%S',tz = 'UTC') - 6*60*60
      
      ret <- ret%>%
        dplyr::filter(created_at>=w1 & created_at<=w2)
    }

    ret
    
  })
  
  dat_d <- dat%>% 
    shiny::throttle(1000)
  
  observeEvent(c(input$appts_window,input$plot_click$name),{
    
    d <- dat_d()
    
    type <- input$plot_click$name
    
    if ( ! is.null(type) ) {
      
      if(input$plot_click$name != "Private Schedule"){
        
        d <- d%>%
          dplyr::filter(TYPE %in% type)
        
      }

    }
    
    nd <- nrow(d)
    now_n <- input$n

    shiny::updateSliderInput(
      session = session,
      inputId = 'n',
      max = nd,
      value = pmin(nd,now_n))
      
    
  })
  
  observeEvent(c(input$appts_window,input$plot_click$name,input$n),{
    output$slick <- slickR::renderSlickR({
      
      d <- dat_d()

      type <- input$plot_click$name
      
      if ( ! is.null(type) ) {
        
        if(input$plot_click$name != "Private Schedule"){
          
          d <- d%>%
            dplyr::filter(TYPE %in% type)
          
        }
        
      }
      
      if( nrow(d) > 0 ){
      
        thisdat <- d%>%
          utils::head(input$n)%>%
          dplyr::mutate(
            slickdat  = sprintf('<p> %s: %s %s </p>', TYPE, TASK,embed)
          )%>%
          dplyr::pull(slickdat)
        
        slickR::slickR(
          thisdat,
          slideType = 'iframe',
          slickOpts = list(
            initialSlide = 0,
            slidesToShow = pmin(length(thisdat),3),
            slidesToScroll = pmin(length(thisdat),3),
            focusOnSelect = TRUE,
            dots = TRUE
          ),
          width = '95%',
          height=350)
          
      }
      
    })
  })

  observeEvent(input$appts_window,{
    output$plot <- d3treeR::renderD3tree3({
      
      d <- dat_d()
      
      if(nrow(d)>0){
        freq <- d%>%
          dplyr::count(TYPE,TASK)%>%
          dplyr::group_by(TYPE)%>%
          dplyr::mutate(
            p = n/sum(n),
            TASK_WRAP = purrr::map_chr(TASK,.f=function(x){
              paste0(strwrap(x,width = 20),collapse = '\n')
            }))
        
        d3treeR::d3tree3(
          treemap::treemap(freq,
                           index=c("TYPE","TASK_WRAP"),
                           vSize="n",
                           vColor = "TYPE.p",
                           palette=viridis::plasma(10),
                           type="index"
          ),rootname = 'Private Schedule',celltext='name') 
      }
    })
  })
  
}
shinyApp(ui, server,options = list(launch.browser = TRUE))


# shiny::observeEvent(input$plot_click,{
#   output$appts <- timevis::renderTimevis({
#     
#     thistest <- test
#     
#     if(!is.null(input$plot_click$name)){
#       
#       thistest <- thistest%>%
#         dplyr::filter(TYPE%in%input$plot_click$name)
#       
#     }
#     
#     timevis::timevis(data = thistest,groups = group_data)
#   })
# })