#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(httr)
library(jsonlite)
#library(magick)
library(bnlearn)
library(dplyr)
library(googlesheets)
library(shiny.semantic)
library(shinyjqui)





ui <- fluidPage(
        
        # shinyjs
        shinyjs::useShinyjs(),
        
        # Application title
        titlePanel("Más películas"),
        p("Este es un proyecto que busca recomendar las mejores películas para ti en cada momento.
          Por eso te pedimos de favor que contestes las siguientes preguntas, y después valora que tan útiles, 
          interesantes o atractivas te parecieron las películas propuestas."),
        
        
        sidebarLayout(
                
                ## ----- side bar: input ----------------
                sidebarPanel(
                        div(id = "form",
                            
                            
                            # place
                            radioButtons("place","¿En dónde te gustaría ver la película?",
                                         choices = list("Sin respuesta" = "",
                                                        "Sala de cine comercial" = "CM",
                                                        "Plataforma digital" = "DP",
                                                        "Sala de exhibición independiente" = "NC",
                                                        "Copia física (o TV)" = "PH")),
                            
                            # regular_user
                            radioButtons("regular_user", "¿Usas regularmente este formato/lugar para ver películas?",
                                         choices = list("Sin respuesta" = "",
                                                        "Sí" = "t",
                                                        "No" = "f")),
                            
                            # tickets
                            radioButtons("tickets", "Sí seleccionaste 'Sala de cine comercial', 
                                         ¿en dónde compras los boletos regularmente?",
                                         choices = list("No aplica" = "",
                                                        "En taquilla" = "OA",
                                                        "En taquilla automática" = "ET",
                                                        "En la página web del cine" = "CW",
                                                        "En la aplicación del cine" = "MA")),
                            
                            # motivation
                            radioButtons("motivation", "¿Qué te motiva generalmente a ver una película?",
                                         choices = list("Sin respuesta" = "",
                                                        "Pasatiempo (o distracción)" = "DG",
                                                        "Quiero ver esa película en específico" = "DM",
                                                        "Recomendaciones o críticas" = "RQ",
                                                        "Es la única disponible (debido a horario u otro)" = "UN",
                                                        "Sin motivo especial" = "DN")),
                            
                            # movie_attributes
                            checkboxGroupInput("movie_attributes", "¿En qué te fijas regularmente para ver la película?",
                                               choices = list("Reparto" = "cast",
                                                              "Director" = "director",
                                                              "Recomendaciones de conocidos" = "recommendations",
                                                              "Nominaciones y/o premios" = "awards",
                                                              "Género" = "genre",
                                                              "Efectos especiales" = "special_effects") ),
                            
                            # language
                            radioButtons("language", "¿En qué idioma te gustaría ver la película?",
                                         choices = list("Sin respuesta" = "",
                                                        "Idioma original SIN subtítulos" = "O",
                                                        "Idioma original CON subtítulos" = "S",
                                                        "Doblada" = "D")),
                          
                            
                            # movie_showtime
                            radioButtons("movie_showtime", "¿En qué horario te gustaría ver la película?",
                                         choices = list("Sin respuesta" = "",
                                                        "Antes del medio día" = "BN",
                                                        "Por la tarde" = "AN",
                                                        "De noche" = "NT",
                                                        "Muy de noche" = "LT")),
                            
                            # companion
                            radioButtons("companion", "¿En compañia de quién te gustaría ver la película?",
                                         choices = list("Sin respuesta" = "",
                                                        "Sola (o)" = "AL",
                                                        "Pareja" = "SO",
                                                        "Amigos" = "FR",
                                                        "Familia" = "FA")),
                            
                            # movie_decision
                            radioButtons("movie_decision", "Cuando estás acompañada, regularmente 
                                         ¿quién elige qué película ver?",
                                         choices = list("Sin respuesta" = "",
                                                        "Yo" = "ME",
                                                        "Acompañante(s)" = "CO",
                                                        "Entre todos" = "AL")),
                            
                            # bought_food
                            radioButtons("bought_food", "¿Te gustaría consumir alimentos durante la película?",
                                         choices = list("Sin respuesta" = "",
                                                        "Sí" = "t",
                                                        "No" = "f")),
                            
                            # food_source
                            radioButtons("food_source", "En caso de que la respuesta pasada fuera afirmativa,
                                         ¿de dónde consigues los alimentos regularmente?",
                                         choices = list("No aplica" = "NA",
                                                        "Del lugar donde veré la película (cine u otro)" = "CI",
                                                        "De mi casa" = "HO",
                                                        "De un comercio cercano" = "AC")),
                            
                            
                            # available_time
                            radioButtons("available_time", "¿De cuánto tiempo dispones para ver la película?",
                                         choices = list("Sin respuesta" = "",
                                                        "Tiempo de sobra" = "EES",
                                                        "Justo el necesario" = "JEA")),
                            
                            
                            # mobility
                            radioButtons("mobility", "¿Estás dispuesto a desplazarte (en caso de ser necesario)
                                         para ver la película?",
                                         choices = list("Sin respuesta" = "",
                                                        "Sí" = "t",
                                                        "No" = "f")),
                            
                            # distance
                            radioButtons("distance", "¿Cuánto tiempo estás dispuesto a trasladarte para
                                         llegar al lugar de proyección?",
                                         choices = list("No aplica" = "NA",
                                                        "Menos de 10 minutos" = "RC",
                                                        "De 10 a 20 minutos" = "NE",
                                                        "De 20 a 30 minutos" = "NC",
                                                        "Más de 30 minutos" = "FA")),
                            
                            
                            # transportation
                            radioButtons("transportation", "¿Qué medio de transporte utilizarías?",
                                         choices = list("No aplica" = "NA",
                                                        "A pie" = "OF",
                                                        "En bicicleta" = "BI",
                                                        "En automóvil" = "CA",
                                                        "Transporte público" = "PT")),
                            
                            
                            
                            # okGenres
                            selectInput("okGenres", "¿Qué géneros prefieres ver? (Selecciona máximo dos)",
                                        choices = generos, 
                                        multiple = T),
                            
                            # notGenres
                            selectInput("notGenres", "¿Qué géneros prefieres evitar? (Selecciona máximo dos)",
                                        choices = generos, 
                                        multiple = T),
                            
                            
                            # send responses
                            actionButton("ask", "Obtener recomendaciones"),
                            div(id="scrollup",
                                span(textOutput("scrollUP"), style = "color:red")
                            )
                        ) #end div form            
                ), # end sidebar panel
                
                ## ------------------- show recommendations ----------------
                # Show the results
                mainPanel(
                        shinyjs::hidden(
                                div(id = "rec", 
                                    fluidRow(h4(textOutput("title1")),
                                             p(uiOutput("error1")),
                                             column(2, textOutput("r1")),
                                             column(2, textOutput("r2")),
                                             column(2, textOutput("r3")),
                                             column(2, textOutput("r4")),
                                             column(2, textOutput("r5")), 
                                             align = "center"),
                                    fluidRow(column(2, htmlOutput("outPoster1")),
                                             column(2, htmlOutput("outPoster2")),
                                             column(2, htmlOutput("outPoster3")),
                                             column(2, htmlOutput("outPoster4")),
                                             column(2, htmlOutput("outPoster5")),
                                             align = "center"),
                                    br(),
                                    fluidRow(column(2, textOutput("r6")),
                                             column(2, textOutput("r7")),
                                             column(2, textOutput("r8")),
                                             column(2, textOutput("r9")),
                                             column(2, textOutput("r10")), 
                                             align = "center"),
                                    fluidRow(column(2, htmlOutput("outPoster6")),
                                             column(2, htmlOutput("outPoster7")),
                                             column(2, htmlOutput("outPoster8")),
                                             column(2, htmlOutput("outPoster9")),
                                             column(2, htmlOutput("outPoster10")),
                                             align = "center"),
                                    br(),
                                    br(),
                                    br(),
                                    #fluidRow( 
                                            #column(4, uiOutput( "recommender" ) ) ,
                                    #        column(12, uiOutput( "valScale" ))),
                                    #fluidRow(column(8, uiOutput("helpValScale"))),
                                    br(),
                                    fluidRow(column(3, uiOutput( "age" )),
                                             column(9, uiOutput("user_val"))),
                                    fluidRow(column(3, textOutput("")),
                                             column(9, uiOutput("helpUserVal"))),
                                    br(),
                                    br(),
                                    br(),
                                    fluidRow(uiOutput("submit")),
                                    fluidRow(textOutput("test_data")),
                                    shinyjs::hidden(
                                            span(id = "submit_msg", "Enviando..."),
                                            div(id = "error",
                                                div(br(), tags$b("Error: "), span(id = "error_msg"))
                                            )
                                    ) # end shinyjs::hidden
                                ) # end div rec
                        ) # end hidden div=rec
                ) # end main panel
                
        ), # end side bar layout
        
        shinyjs::hidden(
                div(id = "thankyou_msg",
                    h3("¡Gracias! Tus respuestas se han enviado correctamente"),
                    actionLink("submit_another", "Enviar otra"))
        ) # end side bar layout
        ) # end UI
# ---------------- server ------------------------
# Define server logic 
server <- function(input, output) {
        
        ## ----------- get recommendations ---------------
        # list of observed variables to input as evidence for the
        # inference on the model
        #observeEvent(input$ask)
        evid <- eventReactive(input$ask, {
                list(awards = movie_attributes()[4],
                     cast = movie_attributes()[1],
                     director = movie_attributes()[2],
                     special_effects = movie_attributes()[6],
                     place = input$place,
                     recommendations = movie_attributes()[3],
                     regular_user = input$regular_user,
                     available_time = input$available_time,
                     motivation = input$motivation,
                     companion = input$companion,
                     movie_showtime = input$movie_showtime,
                     language = input$language,
                     motivation = input$motivation,
                     tickets = input$tickets,
                     movie_decision = input$movie_decision,
                     bought_food = input$bought_food,
                     food_source = input$food_source,
                     distance = input$distance,
                     transportation = input$transportation)
        })
                
        okGenres <- reactive({c(input$okGenres)})
        notGenres <- reactive({c(input$notGenres)})
        ## get rated movie list according to input and evid
        ratedMovieList <- reactive({
                req(evid())
                getCombinedMovies(evidence.gn = evid()[nodes.ke], evidence.cmpr = evid()[nodes.cmpr],
                                  okGenres = okGenres(), notGenres = notGenres())
                #getMovieList.user(okGenres = okGenres(), notGenres = notGenres())
        })
        
        # scroll up message
        observeEvent(input$ask,{
                shinyjs::show("rec")
                output$scrollUP <- renderText({"Sube al inicio de la página para ver tus recomendaciones"})
                shinyjs::show("scrollup")
        })
        
        ## ---------- Display in  Main panel ---------------
        # numbers of movies
        output$r1 <- renderText({as.character(ratedMovieList()$title[1])})
        output$r2 <- renderText({as.character(ratedMovieList()$title[2])})
        output$r3 <- renderText({as.character(ratedMovieList()$title[3])})
        output$r4 <- renderText({as.character(ratedMovieList()$title[4])})
        output$r5 <- renderText({as.character(ratedMovieList()$title[5])})
        output$r6 <- renderText({as.character(ratedMovieList()$title[6])})
        output$r7 <- renderText({as.character(ratedMovieList()$title[7])})
        output$r8 <- renderText({as.character(ratedMovieList()$title[8])})
        output$r9 <- renderText({as.character(ratedMovieList()$title[9])})
        output$r10 <- renderText({as.character(ratedMovieList()$title[10])})
        
        
        # determine poster size
        ps <- poster_sizes[1]
        # movie 1
        output$movieList <- renderTable({
                ratedMovieList()$title
        }, colnames = F)
        
        ## posters for recommended Movies
        # movie 1
        poster1 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[1], poster.size = ps)
        })
        output$outPoster1 <- renderText({
                c('<img src="', poster1(), '">')
        })
        
        # movie 2
        poster2 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[2], poster.size = ps)
        })
        output$outPoster2 <- renderText({
                c('<img src="', poster2(), '">')
        })
        
        # movie 3
        poster3 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[3], poster.size = ps)
        })
        output$outPoster3 <- renderText({
                c('<img src="', poster3(), '">')
        })
        
        # movie 4
        poster4 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[4], poster.size = ps)
        })
        output$outPoster4 <- renderText({
                c('<img src="', poster4(), '">')
        })
        
        # movie 5
        poster5 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[5], poster.size = ps)
        })
        output$outPoster5 <- renderText({
                c('<img src="', poster5(), '">')
        })
        
        # movie 6
        poster6 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[6], poster.size = ps)
        })
        output$outPoster6 <- renderText({
                c('<img src="', poster6(), '">')
        })
        
        # movie 7
        poster7 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[7], poster.size = ps)
        })
        output$outPoster7 <- renderText({
                c('<img src="', poster7(), '">')
        })
        
        # movie 8
        poster8 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[8], poster.size = ps)
        })
        output$outPoster8 <- renderText({
                c('<img src="', poster8(), '">')
        })
        
        # movie 9
        poster9 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[9], poster.size = ps)
        })
        output$outPoster9 <- renderText({
                c('<img src="', poster9(), '">')
        })
        
        # movie 10
        poster10 <- eventReactive(input$ask,{
                getPosterML(ratedMovieList()$poster[10], poster.size = ps)
        })
        output$outPoster10 <- renderText({
                c('<img src="', poster10(), '">')
        })
        ## create title for section/row
        rec1 <- eventReactive(input$ask, {"Recomendaciones"})
        output$title1 <- renderText({rec1()})
        #output$error1 <- renderUI({
        #        if (!recGnrs1()) {
        #                helpText("Lo sentimos. No tenemos una recomendación por el momento.")
        #        }
        #})
        
        
        ## ---------- survey ------------
        output$user_val <- renderUI({
                if ( input$ask ) {
                        sliderInput("user_val", "¿Qué tan interesantes o atractivas te parecieron las recomendaciones?",
                                    min = 1,
                                    max = 4, value = 3, step = 1, ticks = F)
                }
        })
        output$helpUserVal <- renderUI({
                if (input$ask) {
                        helpText("1 es nada atractivas y 4 es muy atractivas")
                }
        })
        
        orderedMovies <- reactive({
                movies <- ratedMovieList()$id
                names(movies) <- ratedMovieList()$title
                movies
        })
        
        output$valScale <- renderUI({
                if (input$ask) {
                        orderInput(inputId = "valScale", label = "Coloca las recomendaciones de arriba de acuerdo al orden en que te gusten más", 
                                   items = orderedMovies(), 
                                   item_class = "success")
                }
        })
        
        output$helpValScale <- renderUI({
                if (input$ask) {
                        helpText("Primero coloca la recomendación que más te haya gustado")
                }
        })
        
        output$age <- renderUI({
                if ( input$ask ) {
                        numericInput("age", "Edad", value = character(0), 
                                     min = 1, max = 99, step = 1)
                }
        })
        
        
        ## -------- submit button -------------
        fieldsMandatory <- c("place", "motivation", "regular_user",
                             "language", "movie_showtime", "companion", "bought_food",
                             "available_time", "user_val",
                             "age", "movie_decision", "mobility")
        observe({
                # check if all mandatory fields have a value
                mandatoryFilled <-
                        vapply(fieldsMandatory,
                               function(x) {
                                       !is.null(input[[x]]) && input[[x]] != ""
                               },
                               logical(1))
                mandatoryFilled <- all(mandatoryFilled)
                
                # enable/disable the submit button
                shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
        
        
        output$submit <- renderUI({
                if ( input$ask ) {
                        actionButton("submit", "Enviar respuestas", 
                                     class = "btn-primary")
                }
        })
        
        
        
        ## -------- send data -----------
        #test first if char vector is being created
        
        movie_attributes <- reactive({
                match_movie_attributes(input$movie_attributes)
        })
        
        fields <- c("place", "motivation", "regular_user",
                    "first_time", "language", 
                    "movie_showtime", "companion", "bought_food",
                    "food_source", "exp_rating", "available_time",
                    "mobility", "distance", "transportation",
                    "movie_decision", "tickets")
        formData <- reactive({
                data <- unlist(sapply(fields, function(x) input[[x]]))
                data <- c(data, movie_attributes(), 
                          input$age, input$user_val, input$valScale_order, ratedMovieList()$id)
                data
        })
        
        
        output$test_data <- renderText({formData()})
        observeEvent(input$submit, {
                shinyjs::disable("submit")
                shinyjs::show("submit_msg")
                shinyjs::hide("error")
                
                tryCatch({
                        save_new_data(formData())
                        shinyjs::reset("form")
                        shinyjs::hide("form")
                        shinyjs::hide("rec")
                        shinyjs::show("thankyou_msg")
                },
                error = function(err) {
                        shinyjs::html("error_msg", err$message)
                        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
                },
                finally = {
                        shinyjs::enable("submit")
                        shinyjs::hide("submit_msg")
                })
        })
        
        observeEvent(input$submit_another, {
                shinyjs::show("form")
                #shinyjs::show("rec")
                shinyjs::hide("thankyou_msg")
                shinyjs::hide("scrollup")
        })
        
        
        
        
        
}

# Run the application 
shinyApp(ui = ui, server = server)

