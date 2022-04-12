#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# avec accent
l_activites <- c("suivi-gestion","baigneur","bateau","cavalier","cerf-volant","char à voile",
                 "chasseur","tir entendu","chien seul","conchiliculteur","cueillette algues",
                 "goemonier","jet-ski","jeu de plage","jogger","kayak","kite surf","longe côte",
                 "naturaliste","paddle","pêche à la ligne","pêche à pied professionnelle",
                 "pêche à pied recréative","pêche d'appats","photographe","planche à voile",
                 "promeneur","promeneur chien en laisse","promeneur chien sans laisse",
                 "quad","skimboard","speed sail","sulky","véhicule","VTT")
l_especes <- c("Huîtrier Pie","Barge rousse","Barge à queue noire","Bécasseau maubèche",
               "Bécasseau sanderling","Bécasseau variable","Bécasseau violet","Courlis cendré",
               "Courlis corlieu","Grand gravelot","Gravelot à collier interrompu",
               "Pluvier argenté","Tournepierre à collier")
# sans accent
# l_activites <- c("suivi-gestion","baigneur","bateau","cavalier","cerf-volant","char a voile",
#                  "chasseur","tir entendu","chien seul","conchiliculteur","cueillette algues",
#                  "goemonier","jet-ski","jeu de plage","jogger","kayak","kite surf","longe cote",
#                  "naturaliste","paddle","peche a la ligne","peche a pied professionnelle",
#                  "peche a pied recreative","peche d'appats","photographe","planche a voile",
#                  "promeneur","promeneur chien en laisse","promeneur chien sans laisse",
#                  "quad","skimboard","speed sail","sulky","vehicule","VTT")
# l_especes <- c("Huitrier Pie","Barge rousse","Barge a queue noire","Becasseau maubeche",
#                "Becasseau sanderling","Becasseau variable","Becasseau violet","Courlis cendre",
#                "Courlis corlieu","Grand gravelot","Gravelot a collier interrompu",
#                "Pluvier argente","Tournepierre a collier")

# Define UI for application
shinyUI(fixedPage(
  
  # Application title
  #h4(strong("LIMIVIEW")),
  
  fixedRow(
    column(4, 
      h4(strong("LIMIVIEW")),
      wellPanel(
        fileInput(
          'fichier_xls', 
          label='Charger les données :', 
          accept=c('application/vnd.ms-excel', '.xlsx')
        )
      )),
    column(8, 
      img(src="rnf_logo.jpg", style="margin: 1rem;"),
      img(src="AESN_logo.jpg", style="margin: 1rem;"),
      img(src="AFB_logo.png", style="margin: 1rem;"),
      img(src="CEFE_logo.jpg", style="margin: 1rem;"),
      img(src="logo-ephe-coul-1.jpg", style="margin: 1rem;")
      )
  ),

  fixedRow(
    column(
      12, 
      # Output: Tabset
      tabsetPanel(type = "tabs",
                  tabPanel("Campagnes de terrain", 
                           h1("Suivi des scans réalisés"),
                           h3("Marées:"),
                           checkboxGroupInput("maree", label = NULL, 
                                              choices = list("Vives-eaux" = "V", "Mortes-eaux" = "M"),
                                              selected = "V", inline=TRUE),
                           leafletOutput('carte_suivi', height=500),
                           h3("Détail:"),
                           uiOutput('resume_scans')
                  ),
                  tabPanel("Distributions observées", 
                           fixedRow(
                             column(3,
                                    checkboxInput("filtre_date", label="Filtrer par date", value=FALSE)),
                             column(9,
                                    conditionalPanel("input$filtre_date == TRUE",
                                                     uiOutput("dyn_slider"))
                             )
                           ),
                           #verbatimTextOutput("test"),
                           h3("Limicoles"),
                           leafletOutput("carte_limicoles", height=400),
                           h3("Activités humaines"),
                           leafletOutput("carte_activites", height=400)
                  ),
                  tabPanel("Abondances", 
                           h3("Toutes espèces de limicoles et catégories d'activités humaines confondues"),
                           fixedRow(
                             column(3, radioButtons("choix_effectif1", label="Somme des effectifs de limicoles",
                                                    choices=list("Total"=1, "En alimentation"=2),
                                                    selected=1)),
                             column(9, plotOutput("graph_tous"))
                           ),
                           h3("Sélection par espèce ou catégorie d'activité"),
                           fixedRow(
                             column(3, 
                                    selectInput("choix_activites", "Activités", c(TOUTES="",l_activites),selectize=FALSE),
                                    selectInput("choix_especes", "Espèces", c(TOUTES="",l_especes),selectize=FALSE),
                                    radioButtons("choix_effectif2", label="Effectif par espèce de limicoles",
                                                 choices=list("Total"=1, "En alimentation"=2),
                                                 selected=1)
                             ),
                             column(9, plotOutput("graph_select"))
                           )
                  )
                  
      )
    ))
))
