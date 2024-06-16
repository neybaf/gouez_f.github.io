library(shiny)
library(xml2)
library(dplyr)

# Fonction pour lire et analyser les fichiers XML
read_vote_data <- function(file_path) {
  xml_data <- read_xml(file_path)
  
  # Identifier et extraire les informations pertinentes
  date <- xml_find_first(xml_data, "//dateScrutin") %>% xml_text()
  title <- xml_find_first(xml_data, "//titre") %>% xml_text()
  votes_pour <- xml_find_all(xml_data, "//pours/votant") %>% xml_length()
  votes_contre <- xml_find_all(xml_data, "//contres/votant") %>% xml_length()
  votes_abstention <- xml_find_all(xml_data, "//abstentions/votant") %>% xml_length()
  votes_non_votant <- xml_find_all(xml_data, "//nonVotants/votant") %>% xml_length()
  
  data.frame(
    Date = date,
    Titre = title,
    Pour = votes_pour,
    Contre = votes_contre,
    Abstention = votes_abstention,
    NonVotant = votes_non_votant,
    stringsAsFactors = FALSE
  )
}

# Lire tous les fichiers XML
xml_files <- list.files(path = "/Users/fabiengouez/Desktop/R_ET_ET_RMD/pol_depute/src16/votes", pattern = "\\.xml$", full.names = TRUE)
vote_data_list <- lapply(xml_files, read_vote_data)
vote_data <- bind_rows(vote_data_list)

# Interface utilisateur de Shiny
ui <- fluidPage(
  titlePanel("Votes des Députés"),
  mainPanel(
    tableOutput("voteTable")
  )
)

# Serveur de Shiny
server <- function(input, output) {
  output$voteTable <- renderTable({
    vote_data
  })
}

# Exécuter l'application
shinyApp(ui = ui, server = server)
