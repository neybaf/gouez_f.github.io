
# Charger les packages
library(shiny)
library(xml2)
library(dplyr)
library(DT)

# Fonction pour extraire les votes d'un fichier XML
# Charger les packages
library(shiny)
library(xml2)
library(dplyr)
library(DT)

# Fonction pour extraire les votes d'un fichier XML
extract_votes <- function(file) {
  xml_data <- read_xml(file)
  
  ns <- xml_ns(xml_data)  # Récupérer les espaces de noms pour les requêtes XPath
  default_ns <- ns[""]    # Prendre l'espace de noms par défaut
  
  dateScrutin <- xml_text(xml_find_first(xml_data, ".//d1:dateScrutin", ns))
  libelle <- xml_text(xml_find_first(xml_data, ".//d1:titre", ns))
  votes <- data.frame(dateScrutin = character(), libelle = character(), acteurRef = character(), vote = character(), stringsAsFactors = FALSE)
  
  groupes <- xml_find_all(xml_data, ".//d1:groupe", ns)
  
  for (groupe in groupes) {
    # Pour votes
    pour_votants <- xml_find_all(groupe, ".//d1:decompteNominatif/d1:pours/d1:votant", ns)
    if (length(pour_votants) > 0) {
      pour_votants_df <- data.frame(
        dateScrutin = rep(dateScrutin, length(pour_votants)),
        libelle = rep(libelle, length(pour_votants)),
        acteurRef = xml_text(xml_find_all(pour_votants, ".//d1:acteurRef", ns)),
        vote = rep("Pour", length(pour_votants)),
        stringsAsFactors = FALSE
      )
      votes <- rbind(votes, pour_votants_df)
    }
    
    # Contre votes
    contre_votants <- xml_find_all(groupe, ".//d1:decompteNominatif/d1:contres/d1:votant", ns)
    if (length(contre_votants) > 0) {
      contre_votants_df <- data.frame(
        dateScrutin = rep(dateScrutin, length(contre_votants)),
        libelle = rep(libelle, length(contre_votants)),
        acteurRef = xml_text(xml_find_all(contre_votants, ".//d1:acteurRef", ns)),
        vote = rep("Contre", length(contre_votants)),
        stringsAsFactors = FALSE
      )
      votes <- rbind(votes, contre_votants_df)
    }
    
    # Abstention votes
    abstention_votants <- xml_find_all(groupe, ".//d1:decompteNominatif/d1:abstentions/d1:votant", ns)
    if (length(abstention_votants) > 0) {
      abstention_votants_df <- data.frame(
        dateScrutin = rep(dateScrutin, length(abstention_votants)),
        libelle = rep(libelle, length(abstention_votants)),
        acteurRef = xml_text(xml_find_all(abstention_votants, ".//d1:acteurRef", ns)),
        vote = rep("Abstention", length(abstention_votants)),
        stringsAsFactors = FALSE
      )
      votes <- rbind(votes, abstention_votants_df)
    }
    
    # Non votants
    non_votants <- xml_find_all(groupe, ".//d1:decompteNominatif/d1:nonVotants/d1:votant", ns)
    if (length(non_votants) > 0) {
      non_votants_df <- data.frame(
        dateScrutin = rep(dateScrutin, length(non_votants)),
        libelle = rep(libelle, length(non_votants)),
        acteurRef = xml_text(xml_find_all(non_votants, ".//d1:acteurRef", ns)),
        vote = rep("Non Votant", length(non_votants)),
        stringsAsFactors = FALSE
      )
      votes <- rbind(votes, non_votants_df)
    }
  }
  
  return(votes)
}
# Définir l'interface utilisateur
ui <- fluidPage(
  titlePanel("Votes d'un Député"),
  sidebarLayout(
    sidebarPanel(
      textInput("acteurRef", "Référence de l'acteur:", ""),
      actionButton("search", "Rechercher"),
      downloadButton("downloadData", "Télécharger les données")
    ),
    mainPanel(
      DTOutput("votesTable")
    )
  )
)

# Définir le serveur
server <- function(input, output) {
  votesData <- eventReactive(input$search, {
    files <- list.files(path = "/Users/fabiengouez/Desktop/R_ET_ET_RMD/pol_depute/src16/votes/", pattern = "*.xml", full.names = TRUE)
    votes_list <- lapply(files, function(file) {
      tryCatch({
        extract_votes(file)
      }, error = function(e) {
        message(paste("Erreur dans le fichier :", file))
        message(e)
        return(data.frame(dateScrutin = character(), libelle = character(), acteurRef = character(), vote = character(), stringsAsFactors = FALSE))
      })
    })
    votes <- do.call(rbind, votes_list)
    votes <- votes %>% filter(acteurRef == input$acteurRef)
    return(votes)
  })

  output$votesTable <- renderDT({
    votesData()
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("votes_", input$acteurRef, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(votesData(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)