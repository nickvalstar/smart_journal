library(shiny)
#library(datasets)
library(wordcloud)
library(ggplot2)
library(RMySQL)
options(shiny.deprecation.messages=FALSE)

source("connect_info_hidden.R")

#function used in creating wordcloud
aggr <- function(x){
  x <- as.data.frame(x,stringsAsFactors=F)
  x <- cbind(x,rep(1,nrow(x)))
  colnames(x) <- c("naam","aantal")
  if(nrow(x)==0){
    x <- data.frame(matrix(data = NA,ncol=2,nrow=1))
    colnames(x) <- c("naam","aantal")
    x$naam = "Empty"
    x$aantal = 1
    return(x)
  }
  x <- aggregate(aantal ~ naam,data=x,FUN = sum)
  x <- x[with(x, order(-aantal)), ]
  x$naam <- as.character(x$naam)
  return(x)
}

shinyServer(function(input, output,session) {
  values <- reactiveValues() #eenmalig aanmaken van values.
  
  ############################################ PAGE 1: input log
  
  #############KLAARZETTEN KEUZES
  
  #What: get the unique input names
  db <- connect() #connect met database
  query <- "select * from input_info order by input_group,input_name"
  input_info <- dbGetQuery(db, query)
  dbDisconnect(db)
  updateSelectInput(session, 'what', choices = input_info$input_name)
  
  #How much: Quantity button
  db <- connect() #connect met database
  query <- "select * from input_quantity order by input_quantity"
  input_quantity <- dbGetQuery(db, query)
  dbDisconnect(db)
  updateRadioButtons(session, 'quantity',
                     choiceNames = input_quantity$input_quantity_name,
                     choiceValues = input_quantity$input_quantity)
  
  ########### ADD BUTTON
  
  #add button check
  observeEvent(input$addbutton, {
    output$outputtext_food <- renderText({ 
      paste("You have added",isolate(input$what))
    })
  })
  
  #add button schrijf naar mysql
  observeEvent(input$addbutton, {
    #clean wat er wordt toegevoegd:
    input_date <-  isolate(as.character(input$when)) #yyyy-mm-dd 
    input_id <-  isolate(input_info[input_info$input_name==as.character(input$what),"input_id"])
    input_quantity <-  isolate(input$quantity)
    
    #write to database
    db <- connect()
    query <- sprintf(
      "INSERT INTO input_log (input_date,input_id,input_quantity) VALUES ('%s')",
      paste(c(input_date,input_id, input_quantity), collapse = "', '")
    )
    tryCatch(
      dbGetQuery(db, query),
      error=function(cond) {
        errormessage <- paste(cond,query)
        print(errormessage) #print on screen liever
      }
    )
    dbDisconnect(db)
  })
  
  ######### LOG TABLE
  
  #eenmalig de tabel data klaarzetten
  db <- connect() #connect met database
  query <- "select * from output_table_vw order by Date"
  values$output_table <- dbGetQuery(db, query)
  dbDisconnect(db)
  
  #updaten tabel data als iets wordt toegevoegd
  observeEvent(input$addbutton, {
    db <- connect() #connect met database
    query <- "select * from output_table_vw order by Date"
    values$output_table <- dbGetQuery(db, query)
    dbDisconnect(db)
  })
  
  #tabel
  output$table <- renderDataTable(values$output_table,
                                  options = list(
                                    pageLength = 50,
                                    initComplete = I("function(settings, json) {;}")
                                  )
  )
  
  
  ################# WORDCLOUD
  wordcloud_rep <- repeatable(wordcloud)
  output$wordcloudje <- renderPlot({
    input$addbutton
    v <- aggr(isolate(as.character(values$output_table$What)))
    wordcloud_rep(v$naam, v$aantal, scale=c(4,0.5),
                  min.freq = 1, max.words=50,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  ############################################ PAGE 2: complaint log
  #############KLAARZETTEN KEUZES
  
  #What: get the unique complaint names
  db <- connect() #connect met database
  query <- "select * from complaint_info order by complaint_group, complaint_name"
  complaint_info <- dbGetQuery(db, query)
  dbDisconnect(db)
  updateSelectInput(session, 'what_complaint', choices = complaint_info$complaint_name)
  
  #How much: Quantity button
  db <- connect() #connect met database
  query <- "select * from complaint_quantity order by complaint_quantity"
  complaint_quantity <- dbGetQuery(db, query)
  dbDisconnect(db)
  updateRadioButtons(session, 'quantity_complaint',
                     choiceNames = complaint_quantity$complaint_quantity_name,
                     choiceValues = complaint_quantity$complaint_quantity)
  
  ########### ADD BUTTON
  
  #add button check
  observeEvent(input$addbutton_complaint, {
    output$outputtext_complaint <- renderText({ 
      paste("You have added",isolate(input$what_complaint))
    })
  })
  
  #add button schrijf naar mysql
  observeEvent(input$addbutton_complaint, {
    #clean wat er wordt toegevoegd:
    complaint_date <-  isolate(as.character(input$when_complaint)) #yyyy-mm-dd 
    complaint_id <-  isolate(complaint_info[complaint_info$complaint_name==as.character(input$what_complaint),"complaint_id"])
    complaint_quantity <-  isolate(input$quantity_complaint)
    
    #write to database
    db <- connect()
    query <- sprintf(
      "INSERT INTO complaint_log (complaint_date,complaint_id,complaint_quantity) VALUES ('%s')",
      paste(c(complaint_date,complaint_id, complaint_quantity), collapse = "', '")
    )
    tryCatch(
      dbGetQuery(db, query),
      error=function(cond) {
        errormessage <- paste(cond,query)
        browser()
        print(errormessage) #print on screen liever
      }
    )
    dbDisconnect(db)
  })
  
  ######### LOG TABLE
  
  #eenmalig de tabel data klaarzetten
  db <- connect() #connect met database
  query <- "select * from output_table_complaint_vw order by Date"
  values$output_table_complaint <- dbGetQuery(db, query)
  dbDisconnect(db)
  
  #updaten tabel data als iets wordt toegevoegd
  observeEvent(input$addbutton_complaint, {
    db <- connect() #connect met database
    query <- "select * from output_table_complaint_vw order by Date"
    values$output_table_complaint <- dbGetQuery(db, query)
    dbDisconnect(db)
  })
  
  #tabel
  output$table_complaint <- renderDataTable(values$output_table_complaint,
                                            options = list(
                                              pageLength = 50,
                                              initComplete = I("function(settings, json) {;}")
                                            )
  )
  
  
  ################# WORDCLOUD
  wordcloud_rep <- repeatable(wordcloud)
  output$wordcloudje_complaint <- renderPlot({
    input$addbutton_complaint
    v <- aggr(isolate(as.character(values$output_table_complaint$What)))
    wordcloud_rep(v$naam, v$aantal, scale=c(4,0.5),
                  min.freq = 1, max.words=50,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  ############################################ PAGE 3: Graphs
  
  updateCheckboxGroupInput(session, "graph_checkbox",
                           choiceNames = input_info$input_name,
                           choiceValues = input_info$input_id,
                           selected = input_info$input_id
  )
  
  updateCheckboxGroupInput(session, "graph_checkbox_complaint",
                           choiceNames = complaint_info$complaint_name,
                           choiceValues = complaint_info$complaint_id,
                           selected = complaint_info$complaint_id
  )
    
  #output$myChartGrid <- reactivePlot(function() {
    output$myChartGrid <- renderPlot({
      print("show graph")
      input$show_graph #maakt em afhankelijk van de show graph button
      
      db <- connect() #connect met database
      query <- paste0("select * from graph_final where 1=1",
      " and date >= '", isolate(input$daterange_from),"'",
      " and date <= '", isolate(input$daterange_until),"'",
      " and input_id in (",paste(isolate(input$graph_checkbox),collapse = ","),")",
      " and complaint_id in (",paste(isolate(input$graph_checkbox_complaint),collapse = ","),")"
      )
      df_plot <- dbGetQuery(db, query)
      dbDisconnect(db)
      
      g <- ggplot(df_plot)
      #lines
      g <- g+ geom_line(aes(x=date, y=value_in, group=what_in),colour="black")
      g <- g+ geom_point(aes(x=date, y=value_in, group=what_in),colour="black")
      g <- g+ geom_line(aes(x=date, y=value_out, group=what_out),colour="red")
      g <- g+ geom_point(aes(x=date, y=value_out, group=what_out),colour="red")
      g <- g + facet_grid(what_in ~ what_out,scales = "free") #scales gaat om de assen. kan alles vrij, alleen x of y of fixed.
      
      #layout
      g <- g + labs(title="Analysis: I DO (black) & I FEEL (red)", x="Date", y="Quantity")
      g <- g + scale_color_discrete(name="What:")
      g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      g <- g + theme(legend.position="none") #removes all legends
      
      #filtering
      g <- g + scale_y_discrete(limits = 0:3) #y as
      return(g)
    })
  #})
  
  ############################################ PAGE 4: Add new types of input
  #add button check
  observeEvent(input$addbutton_input_types, {
    output$outputtext_input_types <- renderText({ 
      paste0("You have added ",isolate(input$input_types_name)," (",isolate(input$input_types_group),").")
    })
  })
  
  #add button schrijf naar mysql
  observeEvent(input$addbutton_input_types, {
    #clean wat er wordt toegevoegd:
    input_name <-  isolate(as.character(input$input_types_name))
    input_group <- isolate(as.character(input$input_types_group))
    
    #write to database
    db <- connect()
    query <- sprintf(
      "INSERT INTO input_info (input_name,input_group) VALUES ('%s')",
      paste(c(input_name,input_group), collapse = "', '")
    )
    tryCatch(
      dbGetQuery(db, query),
      error=function(cond) {
        errormessage <- paste(cond,query)
        print(errormessage) #print on screen liever
      }
    )
    dbDisconnect(db)
  })
  
  ######### LOG TABLE
  
  #eenmalig de tabel data klaarzetten
  db <- connect() #connect met database
  query <- "select input_name as Name, input_group as Category from input_info order by input_group, input_name"
  values$output_table_input_types <- dbGetQuery(db, query)
  dbDisconnect(db)
  
  #updaten tabel data als iets wordt toegevoegd
  observeEvent(input$addbutton_input_types, {
    db <- connect() #connect met database
    query <- "select input_name as Name, input_group as Category from input_info order by input_group, input_name"
    values$output_table_input_types <- dbGetQuery(db, query)
    dbDisconnect(db)
  })
  
  #tabel
  output$table_input_types <- renderDataTable(values$output_table_input_types,
                                              options = list(
                                                pageLength = 50,
                                                initComplete = I("function(settings, json) {;}")
                                              )
  )
  
  ############################################ PAGE 5: Add new types of complaint
  #add button check
  observeEvent(input$addbutton_complaint_types, {
    output$outputtext_complaint_types <- renderText({ 
      paste0("You have added ",isolate(input$complaint_types_name)," (",isolate(input$complaint_types_group),").")
    })
  })
  
  #add button schrijf naar mysql
  observeEvent(input$addbutton_complaint_types, {
    #clean wat er wordt toegevoegd:
    complaint_name <-  isolate(as.character(input$complaint_types_name))
    complaint_group <- isolate(as.character(input$complaint_types_group))
    
    #write to database
    db <- connect()
    query <- sprintf(
      "INSERT INTO complaint_info (complaint_name,complaint_group) VALUES ('%s')",
      paste(c(complaint_name,complaint_group), collapse = "', '")
    )
    tryCatch(
      dbGetQuery(db, query),
      error=function(cond) {
        errormessage <- paste(cond,query)
        print(errormessage) #print on screen liever
      }
    )
    dbDisconnect(db)
  })
  
  ######### LOG TABLE
  
  #eenmalig de tabel data klaarzetten
  db <- connect() #connect met database
  query <- "select complaint_name as Name, complaint_group as Category from complaint_info order by complaint_group, complaint_name"
  values$output_table_complaint_types <- dbGetQuery(db, query)
  dbDisconnect(db)
  
  #updaten tabel data als iets wordt toegevoegd
  observeEvent(input$addbutton_complaint_types, {
    db <- connect() #connect met database
    query <- "select complaint_name as Name, complaint_group as Category from complaint_info order by complaint_group, complaint_name"
    values$output_table_complaint_types <- dbGetQuery(db, query)
    dbDisconnect(db)
  })
  
  #tabel
  output$table_complaint_types <- renderDataTable(values$output_table_complaint_types,
                                                  options = list(
                                                    pageLength = 50,
                                                    initComplete = I("function(settings, json) {;}")
                                                  )
  )
  
  
  ##end
})