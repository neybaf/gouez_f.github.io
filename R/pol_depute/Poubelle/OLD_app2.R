library(shiny)
library(dplyr)
library(readr)
library(xml2)
library(DT)
listedeputes <- read.csv("/Users/fabiengouez/Documents/GitHub/blog/shiny_depute/listedeputes.csv")

# Vérification des colonnes dans listedeputes
if (!("acteurRef" %in% colnames(listedeputes)) | !("uid_groupe" %in% colnames(listedeputes))) {
  stop("Les colonnes acteurRef ou uid_groupe ne sont pas présentes dans listedeputes")
}

# Fonction pour extraire les votes d'un fichier XML
extract_votes <- function(file) {
  xml_data <- read_xml(file)
  
  ns <- xml_ns(xml_data)
  
  dateScrutin <- xml_text(xml_find_first(xml_data, ".//d1:dateScrutin", ns))
  libelle <- xml_text(xml_find_first(xml_data, ".//d1:titre", ns))
  
  votes <- data.frame(
    dateScrutin = character(),
    libelle = character(),
    acteurRef = character(),
    vote = character(),
    organeRef = character(),
    positionMajoritaire = character(),
    stringsAsFactors = FALSE
  )
  
  groupes <- xml_find_all(xml_data, ".//d1:groupe", ns)
  for (groupe in groupes) {
    organeRef <- xml_text(xml_find_first(groupe, ".//d1:organeRef", ns))
    positionMajoritaire <- xml_text(xml_find_first(groupe, ".//d1:positionMajoritaire", ns))
    
    # Pour votes
    pour_votants <- xml_find_all(groupe, ".//d1:decompteNominatif/d1:pours/d1:votant", ns)
    if (length(pour_votants) > 0) {
      pour_votants_df <- data.frame(
        dateScrutin = rep(dateScrutin, length(pour_votants)),
        libelle = rep(libelle, length(pour_votants)),
        acteurRef = xml_text(xml_find_all(pour_votants, ".//d1:acteurRef", ns)),
        vote = rep("Pour", length(pour_votants)),
        organeRef = rep(organeRef, length(pour_votants)),
        positionMajoritaire = rep(positionMajoritaire, length(pour_votants)),
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
        organeRef = rep(organeRef, length(contre_votants)),
        positionMajoritaire = rep(positionMajoritaire, length(contre_votants)),
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
        organeRef = rep(organeRef, length(abstention_votants)),
        positionMajoritaire = rep(positionMajoritaire, length(abstention_votants)),
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
        organeRef = rep(organeRef, length(non_votants)),
        positionMajoritaire = rep(positionMajoritaire, length(non_votants)),
        stringsAsFactors = FALSE
      )
      votes <- rbind(votes, non_votants_df)
    }
  }
  
  return(votes)
}

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Votes d'un Député"),
  sidebarLayout(
    sidebarPanel(
      textInput("acteurRef", "Référence de l'acteur:", "PA721024"),
      actionButton("search", "Rechercher"),
      selectInput("group_compare", "Choisissez un groupe à comparer:", choices = unique(listedeputes$uid_groupe)),
      selectInput("group_approx", "Choisissez un groupe pour le rapprochement:", choices = unique(listedeputes$uid_groupe)),
      downloadButton("downloadData", "Télécharger les données")
    ),
    mainPanel(
      DTOutput("votesTable")
    )
  )
)

# Serveur
server <- function(input, output) {
  votesData <- eventReactive(input$search, {
    files <- list.files(path = "/Users/fabiengouez/Documents/GitHub/blog/shiny_depute/src/xml/", pattern = "*.xml", full.names = TRUE)
    votes_list <- lapply(files, function(file) {
      tryCatch({
        extract_votes(file)
      }, error = function(e) {
        message(paste("Erreur dans le fichier :", file))
        message(e)
        return(data.frame(
          dateScrutin = character(),
          libelle = character(),
          acteurRef = character(),
          vote = character(),
          organeRef = character(),
          positionMajoritaire = character(),
          stringsAsFactors = FALSE
        ))
      })
    })
    votes <- do.call(rbind, votes_list)
    
    # Ajouter les colonnes pour les votes éloignés ou rapprochés
    votes <- votes %>%
      left_join(listedeputes, by = c("acteurRef" = "acteurRef")) %>%
      mutate(
        vote_diff_group = ifelse(vote != positionMajoritaire, 1, 0),
        vote_diff_compare = ifelse(vote != input$group_compare, 1, 0),
        vote_approx_compare = ifelse(vote == input$group_approx, 1, 0)
      )
    
    if (nrow(votes) == 0 || !input$acteurRef %in% votes$acteurRef) {
      votes <- data.frame(
        dateScrutin = NA,
        libelle = NA,
        acteurRef = input$acteurRef,
        vote = "Absent",
        stringsAsFactors = FALSE
      )
    } else {
      votes <- votes %>% filter(acteurRef == input$acteurRef)
    }
    return(votes)
  })
  
  output$votesTable <- renderDT({
    datatable(votesData(), filter = "top")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("votes_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(votesData(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
