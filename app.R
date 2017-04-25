#automatically creates inputs and filters table for any data set


library(shiny)
library(dplyr)
library(lazyeval)

ui =fluidPage(pageWithSidebar(
  headerPanel("Load Data with Filters"),
  sidebarPanel(width = 3,
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain'))
    ,uiOutput('all_filters')
  ),
  mainPanel(width = 9,
    tableOutput('output_table')
  )
))
server = function(input, output, session) {
  
  options(shiny.maxRequestSize=100*1024^2) #changes file size limit to 100MB
  
  #This function is repsonsible for loading in the selected file
  filedata <- eventReactive(input$datafile,{
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  #auto generate inputs in UI
  #df <- iris
observeEvent(input$datafile,{
  df <- filedata()
  output$all_filters <- renderUI(
    lapply(colnames(df), function(i) {
      tags$div(
         if(class(df[,i]) %in% c("numeric","double","integer") ){
           sliderInput(inputId = paste0('filter_',i), label = paste0(i,' Filter'), min=min(df[,i]), max=max(df[,i]), value=c(min(df[,i]),max(df[,i])) )
         }else{
            selectInput(inputId = paste0('filter_',i), label = paste0(i,' Filter'), choices=unique(df[,i]), multiple = T )
         }
      )
    })
  )
  
  
  #filter table based on all auto generated inputs
  filter_table <- reactive({
    k0 <- filedata()
    for(i in 1:length(colnames(k0)) ){
      if( class(df[,colnames(df)[i]]) %in% c("numeric","double","integer") ){
        assign(paste0('k',i),
               get(paste0('k',(i-1))) %>% filter_(interp(~ active_col >= input[[paste0('filter_',colnames(k0)[i])]][1], active_col = as.name( colnames(k0)[i] ))
                                                  ,interp(~ active_col <= input[[paste0('filter_',colnames(k0)[i])]][2], active_col = as.name( colnames(k0)[i] )))
        )
      }else{
        if(length(input[[paste0('filter_',colnames(df)[i])]]) > 0){
          assign(paste0('k',i),
                 get(paste0('k',(i-1))) %>% filter_(interp(~ active_col %in% input[[paste0('filter_',colnames(k0)[i])]], active_col = as.name( colnames(k0)[i] )) )
          )
        }else{
          assign(paste0('k',i), get(paste0('k',(i-1))) )
        }
      }
    }
    return( get(paste0('k',length(colnames(k0)))) )
  })
  
  output$output_table <- renderTable({
    filter_table()
  })
  
})
}
runApp(list(ui = ui, server = server))
