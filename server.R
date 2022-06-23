#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Lien vers l'appli en ligne: https://shiny.cefe.cnrs.fr/limiview/

library(shiny)
library(sp)
library(rgeos)
library(readxl)
library(leaflet)
library(dplyr)

# Define server logic
shinyServer(function(input, output, clientData, session) {
  
  # cette fonction reagit au choix d'un fichier XLS en entree et renvoie une liste avec les 3 tableaux
  tableau_xls <- reactive({
    in_file <- input$fichier_xls
    if (is.null(in_file))
      return(NULL)
    try({
      sscan <- read_excel(in_file$datapath, sheet = "dataScan",na="NA")
      #sscan <- subset(sscan, !(is.na(Xobs)|is.na(Yobs)))
      slimi <- read_excel(in_file$datapath, sheet = "dataLimi")
      sacti <- read_excel(in_file$datapath, sheet = "dataActiv")
      return(list(scan=sscan, limi=slimi, acti=sacti))
    })
  })
  
  
  # fonction pour determiner la position des activites / limicoles
  # suivant position de l'observateur, azimuth et distance
  f_position_obs <- function(data_obs) {
    sp_wgs84 <- SpatialPoints(data_obs[c("Xobs","Yobs")], CRS("+init=epsg:4326"))
    # conversion des (x, y) de WGS84 vers L93 pour appliquer la formule de localisation des limi
    sp_l93 <- spTransform (sp_wgs84, CRS("+init=epsg:2154")) # convertir en L93 pour appliquer la formule
    mat_coords_l93 <- sp_l93@coords
    mat_dist_xy <- cbind(sin(data_obs$Azimut*pi/180), cos(data_obs$Azimut*pi/180))
    mat_coords_l93 <- mat_coords_l93 + (mat_dist_xy * data_obs$Distance)
    sp_l93 <- SpatialPoints(mat_coords_l93, CRS("+init=epsg:2154"))
    # conversion pour projection sur carte
    sp_wgs84 <- spTransform (sp_l93, CRS("+init=epsg:4326"))
    mat_coords_wgs84 <- sp_wgs84@coords
    data_obs$Xobs2 <- mat_coords_wgs84[,1]
    data_obs$Yobs2 <- mat_coords_wgs84[,2]
    data_obs
  }
  
  
  
  # curseur date pour filtrer la carte
  output$dyn_slider <- renderUI({
    list_data <- tableau_xls()
    # donnees valides ?
    validate(need(!inherits(list_data, "try-error"), "Format du jeu de données invalide. Vérifier le fichier en entrée."),
             need(try(!is.null(list_data) && length(list_data)==3), "Sélectionnez un fichier de données pour afficher le curseur.")
    )
    data_scan <- list_data$scan
    v_dates <- data_scan$Date
    
    selectInput("date_slider", "Date d'observation",
                choices = v_dates)
  })
  
  
  ########## carte limicoles ##########
  output$carte_limicoles <- renderLeaflet({
    list_data <- tableau_xls()
    # donnees valides ?
    validate(need(!inherits(list_data, "try-error"), "Format du jeu de données invalide. Vérifier le fichier en entrée."),
             need(try(!is.null(list_data) && length(list_data)==3), "Sélectionnez un fichier de données pour afficher la carte.")
    )
    data_limi <- list_data$limi
    data_scan <- list_data$scan
    xmin <- min(data_scan$Xobs)
    ymin <- min(data_scan$Yobs)
    xmax <- max(data_scan$Xobs)
    ymax <- max(data_scan$Yobs)
    
    data_scan$Date2 <- as.Date(as.character(data_scan$Date))
    
    # jointure + filtre
    data_merg <- merge(data_limi, data_scan, by.x = "Nscan", by.y = "Nscan", all.x=T, all.y=T) # tableau avec les coordonnées de l'observateur pour localiser les activit?s humaines
    data_merg <- subset(data_merg, Espèce!="aucune") # jdd excluant les scans sans espece inventoriee
    # obtenir position des observations avec azimut et distance
    data_merg2 <- f_position_obs(data_merg)
    # legende
    nb_colL <- length(unique(data_merg2$Espèce)) # defini le nb d'especes
    #coulL <- colorFactor(rainbow(nb_colL), domain = data_merg2$Espèces) # palette de couleur 
    # palette personnalisée
    esp <- unique(data_merg$Espèce)
    # # esp <- c('Barque à queue noire','Barge rousse','Bécasseau maubèche','Bécasseau sanderling',
    #          'Bécasseau variable','Bécasseau violet','Courlis cendré','Courlis corlieu',
    #          'Grand gravelot','Gravelot à collier interrompu','Huîtrier Pie','Pluvier argenté',
    #          'Tournepierre à collier')
    couleur <-c("#FEBFD2","#CF0A1D","#003366","#BBD2E1","#660099","#80D0D0","#ED7F10","#856D4D",
                "#FEE347","#FFF0BC","#096A09","#B3B191","#9FE855")
    coulL <- colorFactor(couleur, domain = esp)
    
    # filtrer par date (case filtrer cochée ?)
    if (input$filtre_date) {
      date_choisie <- input$date_slider
      data_merg2 <- subset(data_merg2, Date2==as.Date(date_choisie))
    }
    # plot
    leaflet() %>% addTiles() %>% fitBounds(xmin, ymin, xmax, ymax) %>%
      addCircles(data_merg2$Xobs2, data_merg2$Yobs2, radius=data_merg2$NbTOT, 
                 popup = paste(data_merg2$Espèce, "N =", data_merg2$NbTOT), 
                 color=coulL(data_merg2$Espèce), weight = 1, opacity = 10)  %>%
      addScaleBar(position = c("bottomright")) %>%
      addLegend(pal=coulL, values=esp, opacity = 0.9)
  })
  
  ########## carte activite ##########
  output$carte_activites <- renderLeaflet({
    list_data <- tableau_xls()
    # donnees valides ?
    validate(need(!inherits(list_data, "try-error"), "Format du jeu de données invalide. Vérifier le fichier en entrée."),
             need(try(!is.null(list_data) && length(list_data)==3), "Sélectionnez un fichier de données pour afficher la carte.")
    )
    data_acti <- list_data$acti
    data_scan <- list_data$scan
    xmin <- min(data_scan$Xobs)
    ymin <- min(data_scan$Yobs)
    xmax <- max(data_scan$Xobs)
    ymax <- max(data_scan$Yobs)
    data_scan$Date2 <- as.Date(as.character(data_scan$Date))
    
    # jointure + filtre
    data_merg <- merge(data_acti, data_scan, by.x = "Nscan", by.y = "Nscan", all.x=T, all.y=T) # tableau avec les coordonnées de l'observateur pour localiser les activités humaines
    data_merg <- subset(data_merg, Activité!="aucune") # jdd excluant les scans sans activite inventoriee
    # obtenir position des observations avec azimut et distance
    data_merg2 <- f_position_obs(data_merg)
    # legende
    nb_col <- length(unique(data_merg2$Activité)) # defini le nb d'activités
    coul <- colorFactor(rainbow(nb_col), domain = data_merg2$Activité) # palette de couleur pour chaque type d'activite
    # filtrer par date (case filtrer cochée ?)
    if (input$filtre_date) {
      date_choisie <- input$date_slider
      data_merg2 <- subset(data_merg2, Date2==as.Date(date_choisie))
    }
    # plot
    leaflet() %>% addTiles() %>% fitBounds(xmin,ymin,xmax,ymax) %>%
      addCircles(data_merg2$Xobs2, data_merg2$Yobs2, radius=data_merg2$Nb*10, 
                 popup = paste(data_merg2$Activité, "N =", data_merg2$Nb), 
                 color=coul(data_merg2$Activité), weight = 1, opacity = 1)  %>%
      addScaleBar(position = c("bottomright")) %>%
      addLegend(pal=coul, values=data_merg2$Activité, opacity = 0.9)
    
  })
  
  ########## carte suivi ##########
  output$carte_suivi <- renderLeaflet({
    # creation / mise à jour carte leaflet
    list_data <- tableau_xls()
    chkbox <- input$maree
    # donnees valides ?
    validate(need(!inherits(list_data, "try-error"), "Format du jeu de données invalide. Vérifier le fichier en entrée."),
             need(try(!is.null(list_data) && length(list_data)==3), "Sélectionnez un fichier de données pour afficher la carte.")
    )
    data_input <- list_data$scan
    xmin <- min(data_input$Xobs)
    ymin <- min(data_input$Yobs)
    xmax <- max(data_input$Xobs)
    ymax <- max(data_input$Yobs)
    
    
    filtre <- rep(FALSE, nrow(data_input))
    if (any(chkbox == "V")) {
      filtre <- filtre | (data_input$Coef >= 80)
    }
    if (any(chkbox == "M")) {
      filtre <- filtre | (data_input$Coef < 80)
    }
    data_input <- data_input[filtre,]
    
    if (nrow(data_input)>0) {
      # jdd spatialise en WGS84
      space_tab <- as.data.frame(data_input)
      coordinates(space_tab) <- ~Xobs+Yobs
      proj4string(space_tab) <- "+init=epsg:4326"	#projection WGS84
      # Generation des buffers
      space_tab2 <- spTransform (space_tab, CRS("+init=epsg:2154"))
      buf <- gBuffer(spgeom = space_tab2, width = 400, byid=TRUE)
      coul <- rgb(0.5, 0, 0, alpha=0.1) # gerer la transparence des buffers qui se superposent
      
      ZE <- gUnaryUnion(spgeom = buf)
      ZE_WGS84 <- spTransform(ZE,CRS("+init=epsg:4326"))
      
      
      
      # Pour une carte avec zoom sur R
      leaflet() %>% addTiles() %>% fitBounds(xmin, ymin, xmax, ymax) %>% 
        addPolygons(data=ZE_WGS84, color="black", weight=1, fillOpacity=0) %>%
        addCircles(data_input$Xobs, data_input$Yobs, weight = 0,radius =400, color ="brown", opacity = 0.2,
                   popup = paste("N° scan:", data_input$Nscan, "(",data_input$Date, ")")) %>%
        addScaleBar(position = c("bottomright"))
    } else {
      leaflet() %>% addTiles() %>% fitBounds(xmin, ymin, xmax, ymax)
    }
  })
  
  ########## calcul et affiche chiffres suivi ##########
  output$resume_scans <- renderUI({
    # creation / mise à jour carte leaflet
    list_data <- tableau_xls()
    # donnees valides ?
    validate(need(!inherits(list_data, "try-error"), "Format du jeu de données invalide. Vérifier le fichier en entrée."),
             need(try(!is.null(list_data) && length(list_data)==3), "Sélectionnez un fichier de données pour afficher la carte.")
    )
    data_input <- list_data$scan
    
    nb_scan <- length(unique(data_input$Nscan))		# nb de scan realises
    nb_jours <- length(unique(data_input$Date))		# nb de jours de terrain
    moy_scan <- sum(data_input$Durée)/nb_scan			# duree moyenne d'un scan
    
    data_VE <- subset(data_input, Coef>=80)
    data_ME <- subset(data_input, Coef<80)
    nb_joursVE <- length(unique(data_VE$Date))
    nb_joursME <- length(unique(data_ME$Date))
    
    notify <- p("Nb de scan réalisés : ", 
                strong(nb_scan), br(),
                "Nb de jours de terrain :", 
                strong(nb_jours), br(),
                "Durée moyenne d'un scan (en minute):",
                strong(sprintf("%.2f",moy_scan)), br(),
                "Nb de jours de terrain réalisé en vives-eaux : ",
                strong(nb_joursVE), br(),
                "Nb de jours de terrain réalisé en mortes-eaux : ",
                strong(nb_joursME))
    notify
  })
  
  ########## graphique : toutes especes + toutes activite ##########
  output$graph_tous <- renderPlot({
    list_data <- tableau_xls()
    choix_eff1 <- input$choix_effectif1
    # donnees valides ?
    validate(need(!inherits(list_data, "try-error"), "Format du jeu de données invalide. Vérifier le fichier en entrée."),
             need(try(!is.null(list_data) && length(list_data)==3), "Sélectionnez un fichier de données pour afficher la carte.")
    )
    data_acti <- list_data$acti
    data_limi <- list_data$limi
    data_scan <- list_data$scan
    # jointure + filtre
    data_mergL <- merge(data_limi, data_scan, by.x = "Nscan", by.y = "Nscan", all.x=T, all.y=T) # tableau avec les coordonn?es de l'observateur pour localiser les activit?s humaines
    data_mergL <- subset(data_mergL, Espèce!="aucune") # jdd excluant les scans sans espece inventoriee
    data_mergA <- merge(data_acti, data_scan, by.x = "Nscan", by.y = "Nscan", all.x=T, all.y=T) # tableau avec les coordonn?es de l'observateur pour localiser les activit?s humaines
    data_mergA <- subset(data_mergA, Activité!="aucune") # jdd excluant les scans sans activite inventoriee
    
    ### Données d'abondance au cours du temps
    # avec ttes les dates
    tableau <- data.frame(Datescan = unique(data_scan$Date))
    if (nrow(data_mergL) > 0) {
      if (choix_eff1==1) {
        ab_limi <- aggregate(NbTOT ~ Date, data=data_mergL, sum)
      } else {
        ab_limi <- aggregate(NbALIM ~ Date, data=data_mergL, sum)
        ab_limi <- setNames(ab_limi,c("Date", "NbTOT"))
      }
      tableau <- merge(tableau, ab_limi, by.x = "Datescan", by.y = "Date", all.x=T, all.y=T) 
    } else {
      tableau$NbTOT <- NA
    }
    if (nrow(data_mergA) > 0) {
      ab_acti <- aggregate(Nb ~ Date, data_mergA, sum)
      tableau <- merge(tableau, ab_acti, by.x = "Datescan", by.y = "Date", all.x=T, all.y=T) 
    } else {
      tableau$Nb <- NA
    }
    
    
    # Graphe sur toutes les données de comptage
    par(mar=c(4,4,4,5)) 
    if (all(is.na(tableau$NbTOT))) {
      # effectif total
      plot(NbTOT ~ Datescan, data=tableau, type = "h", col='grey', ylim=c(0,1000), ylab = "", yaxt = "n", xlab="Date du suivi", lwd=4)
    } else {
      # effectif en alimentation
      plot(NbTOT ~ Datescan, data=tableau, type = "h", col='grey', ylab = "", yaxt = "n", xlab="Date du suivi", lwd=4)
    }
    axis(2, col.axis="grey")
    mtext("Nb limicoles comptés", col = "grey", side = 2, padj=-4)
    par(new = T) 
    if (all(is.na(tableau$Nb))) {
      # effectif total
      plot(Nb ~ Datescan, data=tableau, col = "red", pch=16, ylim=c(0,100), yaxt = "n", xaxt = "n", ylab = "", xlab="")
    } else {
      # effectif en alimentation
      plot(Nb ~ Datescan, data=tableau, col = "red", pch=16, yaxt = "n", xaxt = "n", ylab = "", xlab="")
    }
    axis(4, col.axis="red")
    mtext("Nb d'activités humaines", col = "red", side = 4, padj=4)
  })
  
  ########## graphique : par especes + par activite ##########
  output$graph_select <- renderPlot({
    list_data <- tableau_xls()
    choix_eff2 <- input$choix_effectif2
    nom_espece <- input$choix_especes
    nom_activite <- input$choix_activites
    # donnees valides ?
    validate(need(!inherits(list_data, "try-error"), "Format du jeu de données invalide. Vérifier le fichier en entrée."),
             need(try(!is.null(list_data) && length(list_data)==3), "Sélectionnez un fichier de données pour afficher la carte.")
    )
    
    data_acti <- list_data$acti
    data_limi <- list_data$limi
    data_scan <- list_data$scan
    
    # jointure + filtre
    data_mergL <- merge(data_limi, data_scan, by.x = "Nscan", by.y = "Nscan", all.x=T, all.y=T) # tableau avec les coordonn?es de l'observateur pour localiser les activit?s humaines
    if (nom_espece=="") {
      data_mergL <- subset(data_mergL, Espèce!="aucune") # jdd excluant les scans sans espece inventoriee
    } else {
      data_mergL <- subset(data_mergL, Espèce==nom_espece) # jdd excluant les scans sans espece inventoriee
    }
    data_mergA <- merge(data_acti, data_scan, by.x = "Nscan", by.y = "Nscan", all.x=T, all.y=T) # tableau avec les coordonn?es de l'observateur pour localiser les activit?s humaines
    if (nom_activite=="") {
      data_mergA <- subset(data_mergA, Activité!="aucune") # jdd excluant les scans sans activite inventoriee
    } else {
      data_mergA <- subset(data_mergA, Activité==nom_activite)
    }
    
    ### Données d'abondance au cours du temps
    # avec ttes les dates
    tableau <- data.frame(Datescan = unique(data_scan$Date))
    if (nrow(data_mergL) > 0) {
      if (choix_eff2==1) {
        ab_limi <- aggregate(NbTOT ~ Date, data=data_mergL, sum)
      } else {
        ab_limi <- aggregate(NbALIM ~ Date, data=data_mergL, sum)
        ab_limi <- setNames(ab_limi,c("Date", "NbTOT"))
      }
      tableau <- merge(tableau, ab_limi, by.x = "Datescan", by.y = "Date", all.x=T, all.y=T) 
    } else {
      tableau$NbTOT <- NA
    }
    if (nrow(data_mergA) > 0) {
      ab_acti <- aggregate(Nb ~ Date, data_mergA, sum)
      tableau <- merge(tableau, ab_acti, by.x = "Datescan", by.y = "Date", all.x=T, all.y=T) 
    } else {
      tableau$Nb <- NA
    }
    
    
    # Graphe sur toutes les données de comptage
    par(mar=c(4,4,4,5)) 
    if (all(is.na(tableau$NbTOT))) {
      # effectif total
      plot(NbTOT ~ Datescan, data=tableau, type = "h", col='grey', ylim=c(0,1000), ylab = "", yaxt = "n", xlab="Date du suivi", lwd=4)
    } else {
      # effectif en alimentation
      plot(NbTOT ~ Datescan, data=tableau, type = "h", col='grey', ylab = "", yaxt = "n", xlab="Date du suivi", lwd=4)
    }
    axis(2, col.axis="grey")
    mtext("Nb limicoles comptés", col = "grey", side = 2, padj=-4)
    par(new = T) 
    if (all(is.na(tableau$Nb))) {
      # effectif total
      plot(Nb ~ Datescan, data=tableau, col = "red", pch=16, ylim=c(0,100), yaxt = "n", xaxt = "n", ylab = "", xlab="")
    } else {
      # effectif en alimentation
      plot(Nb ~ Datescan, data=tableau, col = "red", pch=16, yaxt = "n", xaxt = "n", ylab = "", xlab="")
    }
    axis(4, col.axis="red")
    mtext("Nb d'activités humaines", col = "red", side = 4, padj=4)
  })
  
})
