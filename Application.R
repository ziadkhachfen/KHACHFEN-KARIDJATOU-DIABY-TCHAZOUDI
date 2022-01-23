### Application 
library(dplyr)
library(ggplot2)
library(ggstream)
library(lubridate)
library(shinydashboard)
library(shiny)


shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = "Analyse des consommations et productions regionales au pas demi horaire",
      titleWidth = 700),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Consommation < 36 kVA", tabName = "consommationinf36", icon = icon("dashboard")),
        menuItem("Consommation > 36 kVA", tabName = "consommationsup36", icon = icon("dashboard")),
        menuItem("Production", tabName = "production", icon = icon("chart-line")))
    ),
    dashboardBody(
      h1("Agrégats segmentés de consommation et production électriques au pas 1/2 h"),
      strong("Les données publiées donnent une vision de la consommation d'électricité au pas 1/2 h des points de soutirage < 36kVA. La liste déroulante permet de sélectionner l'agrégat souhaité : total de l'énergie consommée, courbes de charges moyennes ou nombre de points de soutirage."),
      tabItems(
        tabItem("consommationinf36",align="center",
                column(width = 6, align="center",
                       box(
                         title = "Filtres",
                         em("Deux axes d'analyse sont disponibles (par profil ou plage de puissance) sélectionnables par un bouton."),
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,align="center",
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "cat_inf", label = "Selectionner une categorie : ", choices= unique(data_inf_verticale$variable), selected ='Nb.points.soutirage')),
                         
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "Region_inf", label = "Sélectionner le national ou une région", choices= c(unique(data_inf_verticale$Region),"National", selected ='National') )),
                         
                         column(5,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "Profil_inf", label = "Profil", choices= c(unique(data_inf_verticale$Profil),"Tous les profils", selected ='Tous les profils' ))),
                         
                         column(5,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "plage_puissance_inf", label = "Plage de puissance souscrite", choices=unique(data_inf_verticale$Plage.de.puissance.souscrite))),
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                dateRangeInput(inputId = "dates_inf", label = "Période", start = "2021-06-01",end = "2021-09-30")), 
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                
                                radioButtons(inputId = "pas_inf", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire")
                         )
                       )),
                column(width=6,
                       box(
                         title = "Evolution du volume de consommation",
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,
                         plotOutput("inf")
                       ), 
                       valueBox(value=textOutput("consinf"), subtitle = "La somme de consommation (< 36Kva)"), downloadButton("Download", "Telecharger"))),
        tabItem("consommationsup36",align="center",
                column(width = 6, align="center",
                       box(
                         title = "Filtres",
                         em("Deux axes d'analyse sont disponibles (par profil ou plage de puissance) sélectionnables par un bouton."),
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,align="center",
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "cat_sup", label = " ", choices= unique(data_sup_verticale$variable) )),
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "nat_reg_sup", label = "Selectionner le national ou une région", choices=c(unique(data_sup_verticale$Region), "National"))),
                         
                         column(4,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "secteur_activite_sup", label = "Secteur d'activité", choices=c(unique(data_sup_verticale$Secteur.activite), "Tous les secteurs")  )),
                         
                         column(4,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "profil_sup", label = "Profil", choices= c( unique(data_sup_verticale$Profil), "Tous les profils"))),
                         
                         column(4,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "plage_puissance_sup", label = "Plage de puissance souscrite", choices= unique(data_sup_verticale$Plage.de.puissance.souscrite))),
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                dateRangeInput(
                                  inputId = "dates_sup",
                                  label = "Période",
                                  start = "2021-06-01",
                                  end = "2021-09-30")), 
                         column(10,align="center",style=list("padding-right: 3px;"),
                                radioButtons(inputId = "pas_sup", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire"))
                       )),
                
                column(width = 6,
                       
                       box(
                         title = "Evolution de la consommation > 36 KVA",
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,
                         plotOutput("sup")
                       ), valueBox(value=textOutput("conssup"), subtitle = "La somme de consommation (> 36Kva)"), downloadButton("Download1", "Telecharger"))
        ),
        tabItem("production",align="center",
                column(width = 6,align="center",
                       box(
                         title = "Filtres",
                         em("Deux axes d'analyse sont disponibles (par profil ou plage de puissance) sélectionnables par un bouton."),
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,align="center",
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "cat_prod", label = " ", choices= unique(data_11_verticale$variable))),
                         
                         column(10,align="center",style=list("padding-right: 3px;"),          
                                selectInput(inputId = "nat_reg_prod", label = "Sélectionner le national ou une région", choices= c(unique(data_11_verticale$Region), "National"), selected ='National' )),
                         
                         column(5,align="center",style=list("padding-right: 3px;"),           
                                selectInput(inputId = "filiere_prod", label = "Filiere", choices= c(unique(data_11_verticale$Filiere.de.production), "Toutes les filieres" ), selected = "Toutes les filieres" )),
                         
                         column(5,align="center",style=list("padding-right: 3px;"),
                                selectInput(inputId = "plage_puissance_prod", label = "Plage de puissance d'injection", choices= unique(data_11_verticale$Plage.de.puissance.injection))),
                         
                         column(10,align="center",style=list("padding-right: 3px;"),
                                dateRangeInput(
                                  inputId = "dates_prod",
                                  label = "Période",
                                  start = "2021-06-01",
                                  end = "2021-09-30")),
                         column(10,align="center",style=list("padding-right: 3px;"),   
                                radioButtons(inputId = "pas_prod", label = "Pas", choices = list("pas demi horaire", "pas quotidien"), selected = "pas demi horaire"))
                       )),
                column(width = 6,
                       
                       box(
                         title = "Evolution de la production",
                         status = "info",
                         solidHeader = TRUE,
                         width = 10,
                         plotOutput('prod')
                       ),  valueBox(value=textOutput("consprod"), subtitle = "La somme de la quantite produite "), downloadButton("Download2", "Telecharger")
                )     
                
                
        )
      )),
    title = "Analyse des consommations et productions régionales au pas demi horaire",
    skin = "black"
  ),
  
  
  server = function(input, output, session) {
    
    ### courbes
    
    construit_df_inf <- reactive({
      
      ## commencer par les filtres communs a tous
      new_data <- data_inf_verticale  %>% 
        filter(variable %in% input$cat_inf,
               Plage.de.puissance.souscrite == input$plage_puissance_inf,
               Horodate <= as.POSIXct(input$dates_inf[2]) &
                 Horodate >= as.POSIXct(input$dates_inf[1])
        )
      
      # filtrer les regions si besoin
      if (input$Region_inf != "National"){
        new_data <- new_data %>% 
          filter(Region == input$Region_inf)
      }
      # filtrer les profils si besoin
      if (input$Profil_inf != "Tous les profils"){
        new_data <- new_data  %>% filter(Profil %in% input$Profil_inf) 
      }
      # filtrer les profils si besoin
      if (input$Profil_inf != "Tous les profils"){
        new_data <- new_data  %>% filter(Profil %in% input$Profil_inf) 
      }
      if(input$pas_inf != 'pas demi horaire')
      {
        new_data$Horodate  <- date(new_data$Horodate)
      }
      ## agregation commune a tous
      new_data_grouped <- new_data %>% 
        group_by(Horodate, Profil) %>%
        summarise(value = sum(value)) %>%
        ungroup()
      
      new_data_grouped
      
      
    })
    
    
    
    
    
    plot_df_inf <- reactive({
      ggplot(data= construit_df_inf()) +
        aes(x=as.POSIXct(Horodate), y = value , fill = Profil) +
        geom_stream(type = "ridge") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Evolution de la quantité d'énergie soutirée moyenne (MWh)")+
        ylab(" ")+
        xlab("Temps ") + 
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color = "grey"))
    })
    
    output$inf <- renderPlot({plot_df_inf()})
    
    
    
    construit_dt_sup <- reactive({
      new_data1 <- data_sup_verticale %>% filter( variable %in% input$cat_sup, 
                                                  Plage.de.puissance.souscrite == input$plage_puissance_sup, 
                                                  Horodate <= as.POSIXct(input$dates_sup[2])
                                                  & Horodate >= as.POSIXct(input$dates_sup[1])) 
      if (input$nat_reg_sup != "National")
      {
        new_data1 <- new_data1 %>%  filter(Region == input$nat_reg_sup)
      }
      if (input$profil_sup != "Tous les profils")
      {
        new_data1 <- new_data1 %>%  filter(Profil == input$profil_sup) 
      }
      if (input$secteur_activite_sup != "Tous les secteurs")
      {
        new_data1 <- new_data1 %>%  filter(Secteur.activite == input$secteur_activite_sup)  
      }
      
      if(input$pas_sup != 'pas demi horaire')
      {
        new_data1$Horodate  <- date(new_data1$Horodate)
      }
      
      new_data_grouped1 <- new_data1 %>% 
        group_by(Horodate, Secteur.activite) %>%
        summarise(value = sum(value)) %>%
        ungroup()
      
      new_data_grouped1
      
    })
    
    
    
    plot_dt_sup <- reactive({
      ggplot(data= construit_dt_sup ()) +
        aes(x=as.POSIXct(Horodate), y = value , fill = Secteur.activite) +
        geom_stream(type = "ridge") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Evolution de la quantité d'énergie soutirée moyenne (MWh)")+
        ylab(" ")+
        xlab("Temps ") + 
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color = "grey"))
    })
    
    output$sup <- renderPlot({plot(plot_dt_sup ())})   
    
    construit_prod <- reactive({
      
      new_data2 <-  data_11_verticale  %>% filter(variable %in% input$cat_prod, Plage.de.puissance.injection == input$plage_puissance_prod,
                                                  Horodate <= as.POSIXct(input$dates_prod[2]) &
                                                    Horodate >= as.POSIXct(input$dates_prod[1]) ) 
      if (input$nat_reg_prod != "National")
      {
        new_data2 <- new_data2 %>% filter(Region == input$nat_reg_prod )
      }
      if (input$filiere_prod != "Toutes les filieres")
      {
        new_data2 <- new_data2 %>% filter(Filiere.de.production == input$filiere_prod )
      }
      if(input$pas_prod != 'pas demi horaire')
      {
        new_data2$Horodate  <- date(new_data2$Horodate)
      }
      
      ## agregation commune a tous
      new_data_grouped2 <- new_data2 %>% 
        group_by(Horodate, Filiere.de.production) %>%
        summarise(value = sum(value)) %>%
        ungroup()
      
      new_data_grouped2
    })
    plot_dt_prod <- reactive({
      ggplot(data= construit_prod ()) +
        aes(x=as.POSIXct(Horodate), y = value , fill = Filiere.de.production) +
        geom_stream(type = "ridge") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Evolution de la quantité d'énergie soutirée moyenne (MWh)")+
        ylab(" ")+
        xlab("Temps ") + 
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(color = "grey"))
    })
    
    output$prod <- renderPlot({plot(plot_dt_prod())}) 
    
    ### Telechargement des donnees
    output$Download <- downloadHandler(
      filename = function() {
        paste("Consommationinf36", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(construit_df_inf(), file, row.names = FALSE)
      }
    )
    output$Download1 <- downloadHandler(
      filename = function() {
        paste("Consommationsup36", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(construit_df_sup(), file, row.names = FALSE)
      }
    )
    output$Download2 <- downloadHandler(
      filename = function() {
        paste("production", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(construit_prod(), file, row.names = FALSE)
      }
    )
    
    
    output$consinf <- renderText({
      data <- construit_df_inf() 
      paste(sum(data$value))
    })
    output$conssup <- renderText({
      data <- construit_dt_sup() 
      paste(sum(data$value))
    })
    output$consprod <- renderText({
      data <- construit_prod() 
      paste(sum(data$value))
    })
  }
)
