
library(ggplot2)
source("connectDatabase.R")
con <<- connectDatabase()

function(input, output, session) {
  
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  
    
    output$tabelInstructori<-renderText({
      "Lista Instructori"

    })
  output$tableEmployees <- DT::renderDataTable(DT::datatable({

    data <- df_postgresEmployees
    data
  }))

  con <- connectDatabase()
  
  df_postgresEmployees <- dbGetQuery(con, "select numeangajat as Nume, prenumeAngajat as Prenume from angajat")
  df_postgresStudents <- dbGetQuery(con, "select elev.numeElev as Nume, elev.prenumeElev as Prenume, elev.varsta as Varsta, elev.CNP, elev.telefon from elev")
  
  userVect <<- as.vector(dbGetQuery(con, "SELECT cont.user FROM public.cont;"))
  passwordVect <<- as.vector(dbGetQuery(con, "SELECT cont.parola FROM public.cont;"))
  
  pos <- reactive({
    userVect$user == input$login_username
  })
  
  
  output$conditionLogin <- reactive({
    if(length(passwordVect$parola[pos()]) > 0 & passwordVect$parola[pos()] == input$login_password & input$login_login > 0 ){
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      TRUE
    }else{
      FALSE
    }
  })
  outputOptions(output, 'conditionLogin', suspendWhenHidden = FALSE)
  
  output$instructor<-renderUI({
    con <- connectDatabase()
    queryAngajati <- dbGetQuery(con, "SELECT textcat(textcat(numeAngajat, text ' '), prenumeAngajat) FROM public.angajat;")
    dbDisconnect(conn = con)
    selectInput("instructorSelect", "Instructor:", choices = c("", queryAngajati$textcat))
    
  })
  
  output$masina<-renderUI({
    con <- connectDatabase()
    
    queryMasina <- dbGetQuery(con, paste0("select nrinmatriculare
      from masina
      inner join(
        select idmasina 
        from angajat_masina
        where idangajat = (select idangajat from angajat
                           where  textcat(textcat(numeAngajat, text ' '), prenumeAngajat) = '",input$instructorSelect,"')) idMasinaFromAM
      on masina.idmasina = idMasinaFromAM.idmasina;"))
    dbDisconnect(conn = con)
    selectInput("masinaSelect", "Masina:", choices = c("", queryMasina$nrinmatriculare))
    
  })
 
  
  observeEvent(input$login_leave, {
    js$reset()
    }) 
  
  observeEvent(input$addStudent, {
    con <- connectDatabase()
    queryidAM <- dbGetQuery(con, paste0("SELECT idam as idamasina
    FROM public.angajat_masina
    where idmasina = (SELECT idmasina as idM
                    FROM public.masina
                    where nrinmatriculare = '",input$masinaSelect,"') and idangajat = (SELECT idangajat as idA FROM public.angajat where textcat(textcat(numeangajat, text ' '), prenumeangajat) = '",input$instructorSelect,"');"))

    nextIDAM <- as.numeric(queryidAM$idamasina)
    elevi <- as.vector(dbGetQuery(con, "SELECT elev.numeelev FROM public.elev;"))
    idElev <- length(elevi$numeelev) + 1
    queryElev <- paste0("INSERT into elev VALUES (",idElev,",'",input$numeElev,"','", input$prenumeElev ,"','", input$varsta ,"','", input$cnp ,"','", input$telefon ,"',", nextIDAM,");")
    dbGetQuery(conn = con, statement = queryElev)
    
    showModal(modalDialog(
      title = "Message",
      "Datele au fost introduse!"
    ))
    df_postgresStudents <- dbGetQuery(con, "select elev.numeElev as Nume, elev.prenumeElev as Prenume, elev.varsta as Varsta, elev.CNP, elev.telefon from elev")
    output$tableStudents <- DT::renderDataTable(DT::datatable({
      data <- df_postgresStudents
      data
    }))
    dbDisconnect(conn = con)
    
  })
  
  
  observeEvent(input$login_register, {

    con <- connectDatabase()
    nrId <- length(passwordVect$parola) + 1
    query <- paste0("INSERT into cont VALUES (",nrId,",'",input$login_username,"','", input$login_password ,"','", input$login_mail,"');")
    dbGetQuery(conn = con, statement = query)
    queryAngajati <- dbGetQuery(con, "SELECT textcat(textcat(numeAngajat, text ' '), prenumeAngajat) FROM public.angajat;")
    nrIdAngajat <- length(queryAngajati$textcat) + 1
    queryAngajatiLogin <- paste0("INSERT into angajat VALUES (",nrIdAngajat,",'",input$login_name,"','", input$login_lastname ,"',", nrId,");")
    dbGetQuery(conn = con, statement = queryAngajatiLogin)
    dbDisconnect(conn = con)
    js$reset()
    
    })  
  
    output$tabelElevi<-renderText({
      
      "Lista Elevi"
      
    })

    output$tableStudents <- DT::renderDataTable(DT::datatable({

      data <- df_postgresStudents

      data
    }))
  
  
  output$examHour<-renderUI({
    
    selectInput("hourSelect", "Ora examen: ", choices = c("", "7:00", "8:00", "9:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00"))

  })
  
  output$nameStudentForExam <- renderUI({
    con <- connectDatabase()
    queryStudentiEx <- dbGetQuery(con, "SELECT textcat(textcat(numeelev, text ' '), prenumeelev) FROM public.elev;")
    dbDisconnect(conn = con)
    selectInput("studentSelect", "Student: ", choices = c("", queryStudentiEx$textcat))
  })
  
  observeEvent(input$addExam, {
    con <- connectDatabase()
    queryidStudent <- dbGetQuery(con, paste0("SELECT idelev
                                             FROM public.elev
                                             where idelev = (SELECT idelev
                                             FROM public.elev
                                             where  textcat(textcat(numeelev, text ' '), prenumeelev) = '",input$studentSelect,"');"))
    
    idStudentEx <- as.numeric(queryidStudent$idelev)
    ex <- as.vector(dbGetQuery(con, "SELECT idexamen FROM public.examen;"))
    idEx <- length(ex$idexamen) + 1
    queryExamen <- paste0("INSERT into examen VALUES (",idEx,",'",input$examDate,"','", input$hourSelect,"',", idStudentEx,");")
    dbGetQuery(conn = con, statement = queryExamen)
    
    df_listCarsEx <- dbGetQuery(con, "select masina.nrinmatriculare as masina, examen.dataexamen as data, examen.oraexamen as ora
                                  from masina
                                inner join angajat_masina on angajat_masina.idmasina = masina.idmasina
                                inner join elev on elev.idam = angajat_masina.idam
                                inner join examen on examen.idelev = elev.idelev")
    output$tableExamsCars <- DT::renderDataTable(DT::datatable({
      data <- df_listCarsEx
      data
    }))
    dbDisconnect(conn = con)

    showModal(modalDialog(
      title = "Message",
      "Datele au fost introduse!"
    ))
    
  })

  output$studentSelect<-renderUI({
    con <- connectDatabase()
    queryStudent <- dbGetQuery(con, "SELECT textcat(textcat(numeelev, text ' '), prenumeelev) FROM public.elev;")
    dbDisconnect(conn = con)
    selectInput("studSelect", "Alege Elev", choices = c("", queryStudent$textcat))
  })
  
  
  observeEvent(input$submitNumber, {
    con <- connectDatabase()
    queryNumber <- dbGetQuery(con, paste0("UPDATE public.elev
                                              SET telefon = '", input$phoneNumber, "'
                                              WHERE textcat(textcat(numeelev, text ' '), prenumeelev) = '", input$studSelect,"';"))

    df_postgresStudents <- dbGetQuery(con, "select elev.numeElev as Nume, elev.prenumeElev as Prenume, elev.varsta as Varsta, elev.CNP, elev.telefon from elev")
    output$tableStudents <- DT::renderDataTable(DT::datatable({
      data <- df_postgresStudents
      data
    }))
    dbDisconnect(conn = con)

    showModal(modalDialog(
      title = "Message",
      "Datele au fost modificate!"
    ))
  })
  
  output$instructorSelect <- renderUI({
    con <- connectDatabase()
    queryInstructor <- dbGetQuery(con, "SELECT textcat(textcat(numeangajat, text ' '), prenumeangajat) FROM public.angajat;")
    dbDisconnect(conn = con)
    selectInput("instructorName", "Alege Instructor", choices = c("", queryInstructor$textcat))
  })
  
  
  observeEvent(input$submitName, {
    con <- connectDatabase()
    queryNumber <- dbGetQuery(con, paste0("UPDATE public.angajat
                                          SET numeangajat = '", input$firstName, "'
                                          WHERE textcat(textcat(numeangajat, text ' '), prenumeangajat) = '", input$instructorName,"';"))
    
    df_postgresEmployees <- dbGetQuery(con, "select numeangajat as Nume, prenumeAngajat as Prenume from angajat")
    output$tableEmployees <- DT::renderDataTable(DT::datatable({
      data <- df_postgresEmployees
      data
    }))
    queryInstructor <- dbGetQuery(con, "SELECT textcat(textcat(numeangajat, text ' '), prenumeangajat) FROM public.angajat;")
    output$instructorSelect <- renderUI({
      
      selectInput("instructorName", "Alege Instructor", choices = c("", queryInstructor$textcat))
    })
    dbDisconnect(conn = con)
    showNotification("Datele au fost modificate!", type = "message" )
  })
  
  
  output$studentSelect2<-renderUI({
    con <- connectDatabase()
    queryStudent2 <- dbGetQuery(con, "SELECT textcat(textcat(numeelev, text ' '), prenumeelev) FROM public.elev;")
    dbDisconnect(conn = con)
    selectInput("studSelect2", "Alege Elev", choices = c("", queryStudent2$textcat))
  })
  
  

  observeEvent(input$deleteButton, {
    con <- connectDatabase()
    queryNumber <- dbGetQuery(con, paste0("DELETE FROM public.elev
                                          WHERE textcat(textcat(numeelev, text ' '), prenumeelev) = '", input$studSelect2,"';"))
    
    df_postgresStudents <- dbGetQuery(con, "select elev.numeElev as Nume, elev.prenumeElev as Prenume, elev.varsta as Varsta, elev.CNP, elev.telefon from elev")
    output$tableStudents <- DT::renderDataTable(DT::datatable({
      data <- df_postgresStudents
      data
    }))
    
    queryStudent2 <- dbGetQuery(con, "SELECT textcat(textcat(numeelev, text ' '), prenumeelev) FROM public.elev;")
    output$studentSelect2 <- renderUI({
      selectInput("studSelect2", "Alege Elev", choices = c("", queryStudent2$textcat))
    })
    
    queryStudent <- dbGetQuery(con, "SELECT textcat(textcat(numeelev, text ' '), prenumeelev) FROM public.elev;")
    output$studentSelect <- renderUI({
      selectInput("studSelect", "Alege Elev", choices = c("", queryStudent$textcat))
    })
    
    dbDisconnect(conn = con)
    showNotification("Elevul a fost sters!", type = "message" )
  })
  
  
  
  output$tableExams <- DT::renderDataTable(DT::datatable({
    con <- connectDatabase()
    df_listExams <- dbGetQuery(con, "SELECT numeelev as Nume, prenumeelev as Prenume, examen.dataexamen as Data, examen.oraexamen as Ora FROM elev
                               inner join examen on examen.idelev = elev.idelev")
    data <- df_listExams
    dbDisconnect(conn = con)
    data
  }))
  
  output$tableMails <- DT::renderDataTable(DT::datatable({
    con <- connectDatabase()
    df_listMails <- dbGetQuery(con, "SELECT  mail, angajat.numeangajat as Nume, angajat.prenumeangajat as Prenume FROM cont
                               inner join angajat on angajat.idcont = cont.idcont")
    data <- df_listMails
    dbDisconnect(conn = con)
    data
  }))
  
  output$tableCars <- DT::renderDataTable(DT::datatable({
    con <- connectDatabase()
    df_listCars <- dbGetQuery(con, "select string_agg(nrinmatriculare, ',  ') as masini, angajat.numeangajat as nume, angajat.prenumeangajat as prenume
                              from masina
                              inner join angajat_masina on angajat_masina.idmasina = masina.idmasina
                              inner join angajat on angajat_masina.idangajat = angajat.idangajat
                              group by angajat.numeangajat, angajat.prenumeangajat")
    data <- df_listCars
    dbDisconnect(conn = con)
    data
  }))
  
  
  output$tableCarsStudents <- DT::renderDataTable(DT::datatable({
    con <- connectDatabase()
    df_listCarsStud <- dbGetQuery(con, "select nrinmatriculare as masini, elev.numeelev as nume, elev.prenumeelev as prenume
                                  from masina
                                  inner join angajat_masina on angajat_masina.idmasina = masina.idmasina
                                  inner join elev on angajat_masina.idam = elev.idam
                                  order by elev.numeelev ASC")
    data <- df_listCarsStud
    dbDisconnect(conn = con)
    data
  }))
  
  
  output$tableExamsCars <- DT::renderDataTable(DT::datatable({
    con <- connectDatabase()
    df_listCarsEx <- dbGetQuery(con, "select masina.nrinmatriculare as masina, examen.dataexamen as data, examen.oraexamen as ora
                                  from masina
                                  inner join angajat_masina on angajat_masina.idmasina = masina.idmasina
                                  inner join elev on elev.idam = angajat_masina.idam
                                  inner join examen on examen.idelev = elev.idelev")
    data <- df_listCarsEx
    dbDisconnect(conn = con)
    data
  }))
  
  
  dbDisconnect(conn = con)
  
  rm(pw) 
  output$map <- renderLeaflet({


    leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(lng=26.045858, lat=44.446455,
                 popup="Scoala de Soferi dRive")  


  })
   
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    dbDisconnect(con)
  })
  
  cancel.onSessionEnded()

  
}