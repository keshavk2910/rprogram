library(markdown)
library(shiny)
library(shinyjs)
library(ggplot2)
library(shinyalert)

jsCode <- '
  shinyjs.getcookie_username = function(params) {
    var cookie = Cookies.get("username");
    if (typeof cookie !== "undefined") {
      //Shiny.onInputChange("jscookie_username", cookie);
    } else {
      var cookie = "";
      //Shiny.onInputChange("jscookie_username", cookie);
    }
  }
  shinyjs.setcookie_username = function(params) {
    Cookies.set("username", escape(params), { expires: 0.5 });  
    Shiny.onInputChange("jscookie_username", params);
    //Shiny.onInputChange("jscookie", "");
  }
  shinyjs.rmcookie_username = function(params) {
    Cookies.remove("username");
    Shiny.onInputChange("jscookie_username", "");
    //Shiny.onInputChange("jscookie", "");
  }
  shinyjs.getcookie_uuid = function(params) {
    var cookie = Cookies.get("uuid");
    if (typeof cookie !== "undefined") {
      //Shiny.onInputChange("jscookie_uuid", cookie);
    } else {
      var cookie = "";
      //Shiny.onInputChange("jscookie_uuid", cookie);
    }
  }
  shinyjs.setcookie_uuid = function(params) {
    Cookies.set("uuid", escape(params), { expires: 0.5 });  
    Shiny.onInputChange("jscookie_uuid", params);
    Shiny.onInputChange("jscookie", "");
  }
  shinyjs.rmcookie_uuid = function(params) {
    Cookies.remove("uuid");
    Shiny.onInputChange("jscookie_uuid", "");
    Shiny.onInputChange("jscookie", "");
  }

'


fluidPage(
  tags$head(
    tags$script(src = "js.cookies.js")
  ),
  useShinyjs(),
  useShinyalert(),
  extendShinyjs(text = jsCode),
  navbarPage(id='navbarPage_id', "IntroDataScience Tools",collapsible=TRUE,
             tabPanel("Account",
                      tabsetPanel(id = "Log_in_Tabset",
                                  tabPanel("Log in",  value = "panel_login",
                                           fluidPage(
                                             titlePanel("Log in"),
                                             textInput("username", "Username:"),
                                             passwordInput("password", "Password:"),
                                             actionButton("log_in", "Log In"),
                                             actionLink("no_account", "No Account? Register")
                                             
                                           )
                                           
                                  ),
                                  tabPanel("Logged In",  value = "panel_logged",
                                           fluidPage(
                                             tags$h2("Logged in"),
                                             actionButton("logged_in_refresh", "Refresh"),
                                             DT::dataTableOutput("loggedInfo"),
                                             actionButton("log_out", "Log Out")
                                           ),
                                           tags$p(),
                                           fluidPage(
                                             #DT::dataTableOutput("loginInfo"),
                                             #htmlOutput("loginInfohtml")
                                           )
                                  ),
                                  tabPanel("Register",  value = "panel_register",
                                           fluidPage(
                                             titlePanel("Register"),
                                             textInput("reg_firstname", "Firstname:"),
                                             textInput("reg_lastname", "Lastname:"),
                                             textInput("reg_username", "Username:"),
                                             textInput("reg_email", "E-mail:"),
                                             textInput("reg_org", "Organization:"),
                                             textInput("reg_orgid", "your Org ID:"),
                                             passwordInput("reg_password", "Password:"),
                                             passwordInput("reg_password_confirm", "Confirm Password:"),
                                             print("This is user agreement"),
                                             shinyWidgets::switchInput("user_aagree","Agree?", value = FALSE),
                                             shinyWidgets::switchInput("auth_campaign","Auth Campaign", value = FALSE),
                                             shinyWidgets::switchInput("auth_clases","Auth Clases", value = FALSE),
                                             shinyWidgets::switchInput("auth_users","Auth Users", value = FALSE),
                                             shinyWidgets::switchInput("auth_admin","Auth Admin", value = FALSE),
                                             actionButton("register", "Register")
                                           ),
                                           tags$p(),
                                           fluidPage(
                                             htmlOutput("registerInfo")
                                           )
                                  ),
                                  tabPanel("Edit User",  value = "panel_editusers",
                                           fluidPage(
                                             actionButton("init_users", "Init Users"),
                                             actionButton("refresh_users", "Refrds Users"),
                                             DT::dataTableOutput("query_users")
                                           )
                                           ),
                                  tabPanel("Init Users",  value = "panel_init_users",
                                           fluidPage(
                                             actionButton("init_users", "Init Users"),
                                             actionButton("refresh_users", "Refrds Users"),
                                             DT::dataTableOutput("query_users")
                                           )
                                  )
                                  
                      )
             ),
             tabPanel("Class",
                      tabsetPanel(id = "Class_Tabset",
                                  tabPanel("My Class",  value = "panel_my_class",
                                           fluidPage(
                                           )
                                  ),                                  
                                  tabPanel("Add Class",  value = "panel_edit_class",
                                           fluidPage(
                                                                               )
                                  ),
                                  tabPanel("Edit Class",  value = "panel_edit_class",
                                           fluidPage(
                                           )
                                  ),
                                  tabPanel("Init Class",  value = "panel_init_class",
                                           fluidPage(
                                             fluidPage(
                                               actionButton("init_classes", "Init Classes"),
                                               DT::dataTableOutput("query_classes")
                                             )
                                           ),
                                           tags$p(),
                                           fluidPage(
                                             fluidPage(
                                               actionButton("init_classes2users", "Init Classes/Users"),
                                               DT::dataTableOutput("query_classes2users")
                                             )
                                           )
                                  )                                  
                      )
             ),
             
             
             tabPanel("MySQL",
                      
                      fluidPage(
                        tags$p(),
                        textInput("query_username", "Enter your username:", "guest1"),
                        DT::dataTableOutput("query_tbl"),
                        textInput("ID", "Enter your ID:", "5"),
                        tableOutput("tbl"),
                        numericInput("nrows", "How many cities to show?", 10),
                        plotOutput("popPlot")
                      )
                      
             ),
             
             
             tabPanel("WWW",
                      fluidPage(style="padding-left: 0px; padding-right: 0px; position: absolute; top: 50px; left: 0; right: 0; width: 100%; height: calc(100% - 50px)",
                                tags$div(style="position: absolute; top: 0; left: 0; right: 0; width: 100%; height: 100%;",tags$iframe(style="position: absolute; width: 100%; height: 100%;", scrolling="yes",frameborder="no", src="https://www.mobilizingcs.org"))
                      )
             ),
             tabPanel("RStudio",
                      fluidPage(style="padding-left: 0px; padding-right: 0px; position: absolute; top: 50px; left: 0; right: 0; width: 100%; height: calc(100% - 50px)",
                                tags$div(style="position: absolute; top: 0; left: 0; right: 0; width: 100%; height: 100%;",tags$iframe(style="position: absolute; width: 100%; height: 100%;", scrolling="yes",frameborder="no", src="https://ids.mobilizingcs.org/navbar/rstudio"))
                      )
             ),

             tabPanel("Campaign Editor",
                      fluidPage(
                        titlePanel("Campaign Editor"),
                        hr(),
                        sidebarLayout(
                          sidebarPanel(
                            titlePanel("Campaign Info"),
                            textInput("campaign_name", "Campaign Name:"),
                            textInput("class", "Class:"),
                            textInput("description", "Description:"),
                            shinyWidgets::switchInput("campaign_status","Campaign Status", value = TRUE)
                            
                          ),
                          mainPanel( titlePanel("Campaign Editor2") 
                          ) 
                          
                        )
                        
                        
                        
                      )
             ),
             navbarMenu("dataset",
                        tabPanel("Diamonds", fluidPage(sidebarLayout(sidebarPanel(
                          checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                                             names(diamonds), selected = names(diamonds))),
                          mainPanel(DT::dataTableOutput("mytable1") ) ) ) )
             ),
             
             navbarMenu("Help",
                        tabPanel("Curriculum",
                                 fluidPage(style="padding-left: 0px; padding-right: 0px; position: absolute; top: 50px; left: 0; right: 0; width: 100%; height: calc(100% - 50px)",
                                           tags$div(style="position: absolute; top: 0; left: 0; right: 0; width: 100%; height: 100%;",tags$iframe(style="position: absolute; width: 100%; height: 100%;", scrolling="yes",frameborder="no", src="https://curriculum.idsucla.org"))
                                 )
                        ),
                        tabPanel("Wiki",
                                 fluidPage(
                                   titlePanel("Help: Wiki"),
                                   tags$a(href="https://wiki.idsucla.org/", "https://wiki.idsucla.org/" )
                                 )
                        ),                      
                        tabPanel("Contact",
                                 fluidPage(
                                   titlePanel("Help: Contact"),
                                   "contact_email:",
                                   tags$a(href="mailto:support@idsucla.org", "support@idsucla.org" )
                                 )
                        )
                        
             ),
             navbarMenu("More",
                        tabPanel("Summary",
                                 verbatimTextOutput("summary")
                        ),
                        tabPanel("Plot",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("plotType", "Plot type",
                                                  c("Scatter"="p", "Line"="l")
                                     )
                                   ),
                                   mainPanel(
                                     plotOutput("plot")
                                   )
                                 )
                        ),
                        tabPanel("plotly",
                                 plotly::plotlyOutput("plotx"),
                                 verbatimTextOutput("clickx")
                        ),
                        tabPanel("Table",
                                 DT::dataTableOutput("table")
                        ),
                        tabPanel("About",
                                 fluidRow(
                                   column(6,
                                          includeMarkdown("about.md")
                                   ),
                                   column(3,
                                          img(class="img-polaroid",
                                              src=paste0("http://upload.wikimedia.org/",
                                                         "wikipedia/commons/9/92/",
                                                         "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                          tags$small(
                                            "Source: Photographed at the Bay State Antique ",
                                            "Automobile Club's July 10, 2005 show at the ",
                                            "Endicott Estate in Dedham, MA by ",
                                            a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                              "User:Sfoskett")
                                          )
                                   )
                                 )
                        )
             )
  )
)
