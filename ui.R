 # checkPassword <<- dbGetQuery(con, "select * from cont")
require(shinydashboard)
require(ggplot2)
require(shiny)
require(shinydashboard)
require(ggplot2)
require(DT)
require(sqldf)
require(networkD3)
require(leaflet)
require(shinyBS)
require(googleAuthR)
require("RPostgreSQL")
library(shinyjs)
source("connectDatabase.R")
con <<- connectDatabase()

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

print(getwd())
navbarPage("Superzip", id="nav",
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                        )
                    )
          ),
tabPanel("Plot", plotOutput("plot")))
dashboardPage(
  skin = "red",
  dashboardHeader(
   

    title = "Scoala de soferi",
    tags$li(class = "dropdown",
            tags$a(
                   tags$img(height='50',width='85', alt="SNAP Logo", src="car.png")
            ))
                  ),

  

  dashboardSidebar(
    sidebarSearchForm(label = "Enter a word", "searchText", "searchButton"),
    conditionalPanel(
      condition = "output.conditionLogin",
    sidebarMenu(
      menuItem("Pagina Principala", tabName = "PaginaPrincipala", icon = icon("globe")),
      menuItem("Baza de date", tabName = "BD", icon = icon("database")),
      menuItem("Inscriere elev", tabName = "registerStudent", icon = icon("male")),
      menuItem("Programare la examen", tabName = "programStudent", icon = icon("car")))
    )
  ),
  dashboardBody(


    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

  fluidPage(
    tabItems(
      tabItem(tabName = "PaginaPrincipala",
              fluidRow(
              conditionalPanel(
                
                condition = "output.conditionLogin",
              column(2,div(id = "login_link",
                                     actionButton("login_leave", "Leave", icon = icon("sign-out"), width = "100px", style ="color: #ffe; background-color: #337ab7"))),
              column(4,leafletOutput("map", width=1000, height=500),
                     mainPanel(
                     )
                     )),
              conditionalPanel(
                condition = "!output.conditionLogin",
                column(3, offset = 4,
                       wellPanel(
                         br(),
                         h4("LOGIN", align = "center"),
                         textInput("login_username", "User name"),
                         div(class = "input_msg", textOutput("login_username_msg")),
                         passwordInput("login_password", "Password"),
                         div(class = "input_msg", textOutput("login_password_msg")),
                         conditionalPanel(
                           condition = "input.login_registerForm > 0",  
                           textInput("login_mail", "Email"),
                           textInput("login_name", "Nume"),
                           textInput("login_lastname", "Prenume"),
                           actionButton("login_register", "Sign up", icon = icon("user-plus"), width = "100px")
                         ),
                         conditionalPanel(
                           condition = "input.login_registerForm == 0",  
                           actionButton("login_login", "Log in", icon = icon("sign-in"), width = "100px"),
                           actionButton("login_registerForm", "Create user", icon = icon("user"), width = "100px")),
                         br(),
                         div(class = "input_fail", textOutput("login_fail")),
                         uiOutput("login_more")
                       )
                ),
                forceNetworkOutput("networkD3")
              ))),
             
      
      tabItem(tabName = "BD",
        
          fluidRow(
            conditionalPanel(
              condition = "output.conditionLogin",
              actionButton("studentsAge", "Modifica Telefon", width = "130px", style ="color: #ffe; background-color: #337ab7", icon = icon("pencil")),
              actionButton("deleteStudent", "Sterge Elev", style ="color: #ffe; background-color: #337ab7", icon = icon("trash")),
              actionButton("studentsName", "Modifica Nume Instructor", width = "190px", style ="color: #ffe; background-color: #337ab7", icon = icon("pencil")),
              actionButton("carStudents", "Masini Elevi", style ="color: #ffe; background-color: #337ab7", icon = icon("car")),
              actionButton("mailEmployees", "Mail Instructori", style ="color: #ffe; background-color: #337ab7", icon = icon("envelope")),
              actionButton("carEmployees", "Masini Instructori", style ="color: #ffe; background-color: #337ab7", icon = icon("car")),
              bsModal("modalExample1", "Informatii Elev", "studentsAge", 
                      uiOutput("studentSelect"),
                      textInput("phoneNumber", "", placeholder = 'Telefon'),
                      actionButton("submitNumber", "Modifica", icon = icon("pencil"))
                      ),
              bsModal("modalExample2", "Informatii Elev", "studentsName", 
                      uiOutput("instructorSelect"),
                      textInput("firstName", "", placeholder = 'Nume de Familie'),
                      actionButton("submitName", "Modifica", icon = icon("pencil"))
              ),
              
              bsModal("modalExample3", "Informatii Elev", "deleteStudent", 
                      uiOutput("studentSelect2"),
                      actionButton("deleteButton", "Sterge", icon = icon("trash"))
              ),
              bsModal("modalExample4", "Informatii Instructori", "mailEmployees", 
                      dataTableOutput("tableMails")
              ),
              bsModal("modalExample5", "Masini Instructori", "carEmployees", 
                      dataTableOutput("tableCars")
              ),
              bsModal("modalExample6", "Masini Elevi", "carStudents", 
                      dataTableOutput("tableCarsStudents")
              ),
              br(),br(),
              strong("Lista Studenti"),
              br(),br(),
              DT::dataTableOutput("tableStudents"),
              strong("Lista Instructori"),
              br(),br(),
              DT::dataTableOutput("tableEmployees")),
              useShinyjs(),                                           
              extendShinyjs(text = jsResetCode))
            ),
      
      tabItem(tabName = "registerStudent",
              fluidRow(
                conditionalPanel(
                  condition = "output.conditionLogin",
                  column(3,offset=4,
                  textInput("numeElev", "Nume Elev"),
                  textInput("prenumeElev", "Prenume Elev"),
                  textInput("varsta", "Varsta Elev"),
                  textInput("cnp", "CNP"),
                  textInput("telefon", "Telefon"),
                  br(),
                  uiOutput("instructor"),
                  uiOutput("masina"),
                  column(3,offset=2,actionButton("addStudent", "Adauga Student", icon = icon("plus"), width = "130px"))),
                  useShinyjs(),                                           
                  extendShinyjs(text = jsResetCode)
                  )
                )
              ),
      tabItem(tabName = "programStudent",
                fluidRow(
                  conditionalPanel(
                    condition = "output.conditionLogin",
                    column(3,offset=4,
                           dateInput("examDate","Data examen: "),
                           uiOutput("examHour"),
                           uiOutput("nameStudentForExam"),
                           br(),
                           column(3,offset=2,actionButton("addExam", "Adauga Programare", icon = icon("plus"), width = "150px"),
                                  br(),br(),
                                  actionButton("listExams", "Lista Programari", icon = icon("list"), width = "150px"),
                                  br(),br(),
                                  actionButton("listCarsEx", "Rezervarile Masinilor" ,icon = icon("car"), width = "170px"),
                          bsModal("modalList", "Lista Programari", "listExams", 
                                  dataTableOutput("tableExams")),
                          bsModal("modalList2", "Rezervarile Masinilor", "listCarsEx", 
                                  dataTableOutput("tableExamsCars")))
                     )
                  )
                )
              ) 
      )
    )
  )
)

