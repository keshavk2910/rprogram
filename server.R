library(plotly)
library(shiny)
library(shinyjs)
library(DBI)

conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest"
)


conn_db=dbConnect(RSQLite::SQLite(), "my-db.sqlite")

gen_uuid <- function(){paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse="")}

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

function(input, output, session){

  global_username <- reactiveVal(value = "")
  global_uuid <- reactiveVal(value = "")
  
  # output$loginInfo <- renderUI({ 
  #   paste0("Username: ", input$username, "\n",
  #          "Password: ",input$password)  
  # })

  
  observeEvent(input$log_in, {
    sql <- "SELECT * FROM users WHERE username = ?username AND password = ?password;"
    query <- sqlInterpolate(conn_db, sql,username=input$username, password=input$password)
    QueryResult=dbGetQuery(conn_db, query)

    if(nrow(QueryResult)>0){
      js$setcookie_username(QueryResult[1,"username"])
      js$setcookie_uuid(QueryResult[1,"uuid"])
      global_username (QueryResult[1,"username"])
      global_uuid (QueryResult[1,"uuid"])
      updateTabsetPanel(session, "Log_in_Tabset",selected = "panel_logged")
      hideTab("Log_in_Tabset","panel_login")
      shinyalert::shinyalert("OK!", "Log in Successful.", type = "success",timer = 2500)
    }else{
      shinyalert::shinyalert("Oops!", "Check your input. ", type = "error")
    }
     click("logged_in_refresh")
  })
  
  observeEvent(input$register, {
    users=data.frame(
      username=c(input$reg_username), 
      password=c(input$reg_password),
      email=c(input$reg_email),
      uuid=c(gen_uuid()),
      first.name=c(input$reg_firstname),
      last.name=c(input$reg_lastname),
      org=c(input$reg_org),
      org.id=c(input$reg_orgid),
      enabled=c(TRUE),
      password.reset=c(FALSE),
      auth.campaigns=c(input$auth_campaign),
      auth.classes=c(input$auth_clases),
      auth.users=c(input$auth_users),
      auth.Admin=c(input$auth_admin)
    )
    if(stringr::str_length(input$reg_username)>5 && 
       stringr::str_length(input$reg_password)>5 && 
       input$reg_password==input$reg_password_confirm &&
       isValidEmail(input$reg_email)
       ){
      dbWriteTable(conn_db, "users", users, append=TRUE)
      output$registerInfo <- renderUI({ 
        paste0("USER ADDED", "\n",
               "Username: ", input$reg_username, "\n",
               "E-mail: ", input$reg_email, "\n",
               "Password: ",input$reg_password, "\n",
               "Confirm Password: ",input$reg_password_confirm)
      })
    }else{
      output$registerInfo <- renderUI({ 
        paste0("CHECK INPUT!", "\n",
               "Username: ", input$reg_username, "\n",
               "E-mail: ", input$reg_email, "\n",
               "Password: ",input$reg_password, "\n",
               "Confirm Password: ",input$reg_password_confirm)
      })
    }
    if(!isValidEmail(input$reg_email)){
      shinyalert::shinyalert("Oops!", "Check your email. ", type = "error")
    }
    if(input$reg_password!=input$reg_password_confirm){
      shinyalert::shinyalert("Oops!", "Password and confirm password doesn't match. ", type = "error")
    }
  })
  
  observeEvent(input$no_account, {
    updateTabsetPanel(session, "Log_in_Tabset",
                      selected = "panel_register"
    )
  })
  
  observeEvent(input$log_out, {
    print("                       event1")
    
    js$setcookie_uuid(gen_uuid())
    
    runjs('var today = Cookies.get("username");Shiny.onInputChange("js_username", today);')  
    print(toString(input$js_username))
    global_username(toString(input$js_username))
    
    runjs('var today = Cookies.get("uuid");Shiny.onInputChange("js_uuid", today);')  
    print(toString(input$js_uuid))
    global_uuid(toString(input$js_uuid))
    
    click("logged_in_refresh")
  })
  
  observe({
    print("                       event")
    runjs('var today = Cookies.get("username");Shiny.onInputChange("js_username", today);')  
    print(toString(input$js_username))
    global_username(toString(input$js_username))
    
    runjs('var today = Cookies.get("uuid");Shiny.onInputChange("js_uuid", today);')  
    print(toString(input$js_uuid))
    global_uuid(toString(input$js_uuid))
    
    sql <- "SELECT * FROM users WHERE username = ?username AND uuid = ?uuid;"
    query <- sqlInterpolate(conn_db, sql,username=global_username(), uuid=global_uuid())
    QueryResult=dbGetQuery(conn_db, query)
    if(nrow(QueryResult)>0){
      js$setcookie_username(QueryResult[1,"username"])
      js$setcookie_uuid(QueryResult[1,"uuid"])
      global_username (QueryResult[1,"username"])
      global_uuid (QueryResult[1,"uuid"])
      #showTab("Log_in_Tabset","panel_logged")
      updateTabsetPanel(session, "Log_in_Tabset",selected = "panel_logged")
      #hideTab("Log_in_Tabset","panel_login")
    }else{
      #showTab("Log_in_Tabset","panel_login")
      updateTabsetPanel(session, "Log_in_Tabset",selected = "panel_login")
      #hideTab("Log_in_Tabset","panel_logged")
    }
    click("logged_in_refresh")
  })
  
  observeEvent(input$logged_in_refresh,{
    print("logged_in_refresh event")
    print(global_username())
    print(global_uuid())
    sql <- "SELECT * FROM users WHERE username = ?username AND uuid = ?uuid;"
    query <- sqlInterpolate(conn_db, sql,username=global_username(), uuid=global_uuid())
    QueryResult=dbGetQuery(conn_db, query)
    if(nrow(QueryResult)>0){
      js$setcookie_username(QueryResult[1,"username"])
      js$setcookie_uuid(QueryResult[1,"uuid"])
      global_username (QueryResult[1,"username"])
      global_uuid (QueryResult[1,"uuid"])
      #showTab("Log_in_Tabset","panel_logged")
      #updateTabsetPanel(session, "Log_in_Tabset",selected = "panel_logged")
      #hideTab("Log_in_Tabset","panel_login")
    }else{
      #showTab("Log_in_Tabset","panel_login")
      #updateTabsetPanel(session, "Log_in_Tabset",selected = "panel_login")
      #hideTab("Log_in_Tabset","panel_logged")
    }
    output$loggedInfo<- DT::renderDataTable({ 
      DT::datatable(QueryResult)
    })
  })
  

  
  observeEvent(input$username,{
    if(stringr::str_length(input$username)<5){
      updateTextInput(session, "username", "Username:")
    }else{
      updateTextInput(session, "username", "Username:")
    }
  }
  )
  
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
  
  
  # choose columns to display
  diamonds2 = diamonds[sample(nrow(diamonds), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
  })
  
  
  
  output$plotx <- renderPlotly({
    # specify some map projection/options
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      lakecolor = toRGB('white')
    )
    plot_ly(z = state.area, text = state.name, locations = state.abb,
            type = 'choropleth', locationmode = 'USA-states') %>%
      layout(geo = g)
  })
  
  output$clickx <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view event data" else d
  })
  
  
  output$tbl <- renderTable({
    sql <- "SELECT * FROM City WHERE ID = ?id;"
    query <- sqlInterpolate(conn, sql, id = input$ID)
    dbGetQuery(conn, query)
  })
  output$popPlot <- renderPlot({
    query <- paste0("SELECT * FROM City LIMIT ",
                    as.integer(input$nrows)[1], ";")
    df <- dbGetQuery(conn, query)
    pop <- df$Population
    names(pop) <- df$Name
    barplot(pop)
  }) 
  
  init_db_user <- function(){
    if(dbExistsTable(conn_db, "users")==TRUE){
      dbRemoveTable(conn_db,'users')
    }
    users=data.frame(
      username=c("guest1","guest2","guest3"), 
      password=c("ids-guest1","ids-guest2","ids-guest3"),
      email=c("guest1@gmail.com","guest2@gmail.com","guest3@gmail.com"),
      uuid=c(gen_uuid(),gen_uuid(),gen_uuid()),
      first.name=c("guest1","guest2","guest3"),
      last.name=c("ids","ids","ids"),
      org=c("UCLA","UCLA","UCLA"),
      org.id=c("001","002","003"),
      enabled=c(TRUE,TRUE,FALSE),
      password.reset=c(FALSE,FALSE,FALSE),
      auth.campaigns=c(TRUE,TRUE,FALSE),
      auth.classes=c(TRUE,TRUE,FALSE),
      auth.users=c(TRUE,TRUE,FALSE),
      auth.Admin=c(TRUE,TRUE,FALSE)
    )
    #dbWriteTable(conn_db, "users", users[NULL, ])
    #dbGetQuery(conn_db, "ALTER TABLE users ADD PRIMARY KEY (username);")
    dbWriteTable(conn_db, "users", users)
    
  }
  
  init_db_class <- function(){
    if(dbExistsTable(conn_db, "classes")==TRUE){
      dbRemoveTable(conn_db,'classes')
    }
    classes=data.frame(
      classname=c("class1","class2","class3"), 
      curriculum=c("ids","ids","ids"),
      semester=c("Spring 2019","Spring 2019","Spring 2019"),
      classuuid=c(gen_uuid(),gen_uuid(),gen_uuid()),
      classurn=c("Spring 2019 A","Spring 2019 B","Spring 2019 C"),
      enabled=c(TRUE,TRUE,FALSE)
    )
    dbWriteTable(conn_db, "classes", classes)
  }
  
  init_db_class2user <- function(){
    if(dbExistsTable(conn_db, "classes2users")==TRUE){
      dbRemoveTable(conn_db,'classes2users')
    }
    classes2users=data.frame(
      classname=c("class1","class2","class3"), 
      username=c("guest1","guest2","guest3"),
      role=c("teacher","student","admin"),
      enabled=c(TRUE,TRUE,FALSE)
    )
    dbWriteTable(conn_db, "classes2users", classes2users)
  }
  
  
  

  observeEvent(input$init_users, {
    init_db_user()
    output$query_users <- DT::renderDataTable({
      sql <- "SELECT * FROM users;"
      query <- sqlInterpolate(conn_db, sql)
      DT::datatable(dbGetQuery(conn_db, query))
    }) 
  })
  
  observeEvent(input$refresh_users, {
    output$query_users <- DT::renderDataTable({
      sql <- "SELECT * FROM users;"
      query <- sqlInterpolate(conn_db, sql)
      DT::datatable(dbGetQuery(conn_db, query))
    }) 
  })
  
  output$query_users <- DT::renderDataTable({
    DT::datatable(dbReadTable(conn_db, "users"))
  })  
  
  output$query_tbl <- DT::renderDataTable({
    sql <- "SELECT * FROM users WHERE username = ?username;"
    query <- sqlInterpolate(conn_db, sql, username = input$query_username)
    DT::datatable(dbGetQuery(conn_db, query))
  })  
  
  
  observeEvent(input$init_classes, {
    init_db_class()
    output$query_classes <- DT::renderDataTable({
      sql <- "SELECT * FROM classes;"
      query <- sqlInterpolate(conn_db, sql)
      DT::datatable(dbGetQuery(conn_db, query))
    })
  })
  output$query_classes <- DT::renderDataTable({
    sql <- "SELECT * FROM classes;"
    query <- sqlInterpolate(conn_db, sql)
    DT::datatable(dbGetQuery(conn_db, query))
  }) 
  
  observeEvent(input$init_classes2users, {
    init_db_class2user()
    output$query_classes2users <- DT::renderDataTable({
      sql <- "SELECT * FROM classes2users;"
      query <- sqlInterpolate(conn_db, sql)
      DT::datatable(dbGetQuery(conn_db, query))
    },editable = TRUE) 
  })
  output$query_classes2users <- DT::renderDataTable({
    sql <- "SELECT * FROM classes2users;"
    query <- sqlInterpolate(conn_db, sql)
    DT::datatable(dbGetQuery(conn_db, query))
  },editable = TRUE) 
  
  #session$onSessionEnded(function(){
  #  dbDisconnect(conn_db)
  #}
  #)
}
