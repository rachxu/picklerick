library(shiny)
library(jsonlite)
library(stringr)
library(dplyr)
library(httr)
library(xtable)
library(DT)

# scrap all characters
link = rep(NA,30); data = list()
for (i in 30) {
  data[[i]] = matrix(NA, nrow=20,ncol=12)}

for (i in 1:30){
  link[i] = str_glue("https://rickandmortyapi.com/api/character/?page={p}",p=i)
  data[[i]] = fromJSON(link[i])$results[,-c(7:10)]}  # remove column of data frames

all_char = do.call(rbind,data)   

# scrap all locations
link1 = rep(NA,4); data1 = list()
for (i in 4) {
  data1[[i]] = matrix(NA,nrow=20,ncol=7)}

for (i in 1:4) {
  link1[i] = str_glue("https://rickandmortyapi.com/api/location?page={p}",p=i)
  data1[[i]] = fromJSON(link1[i])$results[,-c(5:7)]}  # remove column of data frames

all_loc = do.call(rbind,data1)

# scrap all episodes
ep1 = fromJSON("https://rickandmortyapi.com/api/episode/")$results
ep2 = fromJSON("https://rickandmortyapi.com/api/episode/?page=2")$results

all_ep = rbind(ep1, ep2)

ui <- fluidPage(title = "Rick and Morty",
                
                tabsetPanel(
                  tabPanel(title = "Character", 
                           fluidRow(
                             column(8, 
                                    h1("Search a character by name"),
                                    selectInput("nom", "Name",c("", all_char$name)),
                                    
                                    actionButton("search", "Search")),
                             column(8,
                                    tableOutput("byNam"),
                                    DT::dataTableOutput("epis"))),
                           br(),
                           
                           column(6, 
                                  h1("Find character(s) by filtering"),
                                  wellPanel(
                                    selectInput("sta", "Status",c("","alive", "dead", "unknown")),
                                    selectInput("spec", "Species",c("", unique(c(all_char$species)))),
                                    selectInput("gen", "Gender",c("","female","male","genderless","unknown"))),
                                  actionButton("fil", "Filter")
                           ),
                           DT::dataTableOutput("byFilt")
                  ),
                  
                  
                  tabPanel(title = "Location",
                           selectInput("loc", "Find a location", c("", all_loc$name)),
                           actionButton("go", "Go!"),
                           tableOutput("locat")),
                  
                  tabPanel(title = "Episode", 
                           selectInput("s", "Season (leave episode empty to obtain the entire season)", c("", 1:4)),
                           selectInput("e", "Episode", c("", 1:11)),
                           actionButton("ep", "Go"),
                           tableOutput("eps"))
                ),
                
                # suppress warning when first launching the app (due to empty urls)
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }")
)

server <- function(input, output, session) {
  
  #### find character by name ####
  rv = reactiveValues(data=NULL)
  
  observeEvent(input$search, {
    req(input$nom !="", cancelOutput = TRUE)
    rv$data = all_char[which(all_char$name == input$nom),]
    rv$eps = fromJSON(rv$data[1,7])$episode
    rv$names = matrix(NA,length(rv$eps))
    for (i in 1:length(rv$eps)){
      rv$names[i] = fromJSON(rv$eps[i])$name
    }
  })
  
  output$byNam = renderTable({
    # display one row for one character
    rv$data[1,-c(7:8)]
  }, caption = "Basic information:",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$epis = DT::renderDataTable(
    # display all episode that a character appeared in
    data.frame(rv$names) %>% rename(Episode_Name = rv.names), 
    caption = str_glue("Episodes that ", input$nom ," appeared on:"),
    options = list(pageLength = 5))
  
  
  #### filtering character ####
  rv2 <- reactiveValues(data=NULL)
  
  observeEvent(input$fil, {
    rv2$data = str_glue("https://rickandmortyapi.com/api/character/?status={st}&species={sp}&gender={g}",
                        st=input$sta, sp=input$spec, g=input$gen)
    # get status code of given url so the app does not crash
    rv2$x = GET(rv2$data)$status_code
  })
  
  output$byFilt = DT::renderDataTable(
    if (rv2$x == "400" | rv2$x == "404")
      data.frame(Results = "No character matches the condition")
    else
      # Get the characters that satisfy the conditions given
      fromJSON(rv2$data)$results[,-c(7:12)],  # columns of data frames removed
     caption = "Character(s) that match the condition(s):",
     options = list(pageLength = 5))
  
  #### location ####
  rv3 <- reactiveValues(data=NULL)
  
  observeEvent(input$go, {
    rv3$data = all_loc[which(all_loc$name == input$loc),]
  })
  
  output$locat = renderTable({
    data.frame(rv3$data)
  }, caption = "Basic information:",
     caption.placement = getOption("xtable.caption.placement", "top"), 
     caption.width = getOption("xtable.caption.width", NULL)
  )
  
  #### episode search ####
  rv4 <- reactiveValues(data=NULL)
  
  observeEvent(input$ep, {
    req(input$s != "")
    if (input$e == "")
      rv4$data = str_glue("S", 0, input$s)
    else if (as.numeric(input$e)>9)
      rv4$data = str_glue("S", 0, input$s, "E", input$e)
    else
      rv4$data = str_glue("S", 0, input$s, "E", 0, input$e)
    rv4$tbl = all_ep %>% filter(grepl(rv4$data, episode))
  })
  
  output$eps = renderTable({
    if (nrow(rv4$tbl) == 0)
      data.frame(Results = "Episode does not exist")
    else
      rv4$tbl[,-c(5:7)]
  }, caption = "Episode information:",
     caption.placement = getOption("xtable.caption.placement", "top"), 
     caption.width = getOption("xtable.caption.width", NULL)
  )
}

shinyApp(ui, server)
