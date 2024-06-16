library(shiny)
library(xml2)
library(dplyr)
library(classInt)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
library(readr)
library(readr)
library(xml2)
library(DT)
xml_dir <- "/Users/fabiengouez/Documents/GitHub/blog/shiny_depute/xml"
deputes_path <- "/Users/fabiengouez/Documents/GitHub/blog/shiny_depute/listedeputes.csv"
# Lire le fichier CSV des députés
deputes <- read.csv(deputes_path)

# Fonction pour lire tous les fichiers XML dans un répertoire
read_all_xml <- function(directory) {
  files <- list.files(directory, pattern = "\\.xml$", full.names = TRUE)
  xml_list <- lapply(files, read_xml)
  return(xml_list)
}

# Lire tous les fichiers XML dans le répertoire
scrutins_list <- read_all_xml(xml_dir)

#Définir l’interface utilisateur (UI)

ui <- fluidPage(
  titlePanel("Analyse des Votes des Députés"),
  sidebarLayout(
    sidebarPanel(
      helpText("Cette application visualise les votes des députés.")
    ),
    mainPanel(
      plotOutput("votePlot"),
      plotOutput("mcaPlot")
    )
  )
)

#Définir le serveur

server <- function(input, output) {
  
  #Analyser les données XML
  
  observe({
    res <- data.frame(identifiant = character(), nomOrgane = character(), nbVote = integer(), oui = integer(), non = integer(), abst = integer(), stringsAsFactors = FALSE)
    for (scrutins in scrutins_list) {
      liste <- xml_children(scrutins)
      for (a in liste) {
        if (length(xml_children(a)) >= 14 && xml_text(xml_children(a)[14]) == "DecompteNominatif") {
          numero <- xml_text(xml_children(a)[2])
          ventil <- xml_children(a)[16]
          groupe <- xml_children(xml_children(xml_children(ventil)))
          tempDataFrame <- data.frame(identifiant = res$identifiant, vote = NA)
          
          for (b in groupe) {
            intermediaire <- xml_children(xml_children(b))
            nomGroupe <- xml_text(xml_children(b)[1])
            
            for (i in 1:4) {
              if (length(xml_children(intermediaire[3])) >= i) {
                df3 <- data.frame(xml_text(xml_children(xml_children(intermediaire[3])[i])))
                if (nrow(df3) != 0) {
                  for (j in df3[,]) {
                    if (i == 1) {
                      j <- strsplit(strsplit(strsplit(j, "MG")[[1]], "PSE")[[1]], "PAN")[[1]]
                    }
                    res[res$identifiant == j, "nomOrgane"] <- as.character(nomGroupe)
                    if (i != 1) res[res$identifiant == j, "nbVote"] <- res[res$identifiant == j, "nbVote"] + 1
                    if (i == 1) tempDataFrame[tempDataFrame$identifiant == j, "vote"] <- "NV"
                    if (i == 2) {
                      res[res$identifiant == j, "oui"] <- res[res$identifiant == j, "oui"] + 1
                      tempDataFrame[tempDataFrame$identifiant == j, "vote"] <- "POUR"
                    }
                    if (i == 3) {
                      res[res$identifiant == j, "non"] <- res[res$identifiant == j, "non"] + 1
                      tempDataFrame[tempDataFrame$identifiant == j, "vote"] <- "CTRE"
                    }
                    if (i == 4) {
                      res[res$identifiant == j, "abst"] <- res[res$identifiant == j, "abst"] + 1
                      tempDataFrame[tempDataFrame$identifiant == j, "vote"] <- "ABST"
                    }
                  }
                }
              }
            }
          }
          res <- cbind(res, tempDataFrame$vote)
        }
      }
    }
    
    res2 <- res %>% filter(dept < 96) %>% mutate(
      circo = ifelse(nchar(circo) == 1, paste0("0", circo), circo),
      dept = ifelse(nchar(dept) == 1, paste0("0", dept), dept),
      iden = paste0(dept, circo)
    ) %>% filter(!is.na(iden)) %>% arrange(iden)
    
    parIden <- aggregate(nbVote ~ iden, data = res2, sum)
    
    # Assigner les noms des groupes depuis result_groupe
    parIdenScrutin <- aggregate(res2[, 9:ncol(res2)], by = list(res2$iden), na.omit)
    parIdenScrutin <- left_join(parIdenScrutin, result_groupe, by = c("nomOrgane" = "uid"))
    parIdenScrutin$nomOrgane <- ifelse(is.na(parIdenScrutin$libelle), parIdenScrutin$nomOrgane, parIdenScrutin$libelle)
    parIdenScrutin <- select(parIdenScrutin, -libelle)
    
    parIdenScrutin$Group.1 <- as.factor(parIdenScrutin$Group.1)
    parIdenScrutin$nomOrgane <- as.factor(parIdenScrutin$nomOrgane)
    
    acm <- MCA(parIdenScrutin, quali.sup = 2, graph = FALSE)
    
    output$votePlot <- renderPlot({
      nuancier <- findColours(classIntervals(parIden$nbVote, 6, style = "quantile"), smoothColors("white", 98, "#0C3269"))
      plot(fdc, col = nuancier)
      leg <- findColours(classIntervals(round(parIden$nbVote), 6, style = "quantile"), smoothColors("white", 98, "#0C3269"), under = "moins de", over = "plus de", between = "–", cutlabels = FALSE)
      legend("bottomleft", fill = attr(leg, "palette"), legend = names(attr(leg, "table")), title = "Nombre de Votes", bty = "n")
      title(main = "Nombre de votes par circonscription", cex.main = 1.5)
    })
    
    output$mcaPlot <- renderPlot({
      plot(acm$ind$coord[, 1:2], type = "n", xlab = paste0("Axe 1 (", round(acm$eig[1, 2], 1), " %)"), ylab = paste0("Axe 2 (", round(acm$eig[2, 2], 1), " %) "), main = "Nuage des individus selon les partis", cex.main = 1, cex.axis = 1, cex.lab = 1, font.lab = 3)
      abline(h = 0, v = 0, col = "grey", lty = 3, lwd = 1)
      points(acm$ind$coord[, 1:2], col = as.numeric(parIdenScrutin$nomOrgane), pch = 19, cex = 0.5)
      legend("topleft", legend = levels(parIdenScrutin$nomOrgane), bty = "o", text.col = 1:10, col = 1:10, pch = 18, cex = 0.8)
      text(acm$ind$coord[, 1:2], labels = rownames(acm$ind$coord), col = as.numeric(parIdenScrutin$nomOrgane), cex = 0.7, pos = 4)
    })
  })
}
#Lancer l’application Shiny

shinyApp(ui = ui, server = server)