# app2.R

# Charger les bibliothèques nécessaires
library(shiny)
library(dplyr)
library(dplyr)
library(readr)
library(xml2)
library(purrr) 
library(jsonlite)

# Charger les données des députés
deputes <- read.csv("/Users/fabiengouez/Documents/GitHub/blog/shiny_depute/listedeputes.csv")
# Fonction pour charger les données de votes à partir du fichier XML
load_votes <- function(file) {
  xml_data <- read_xml(file)
  votes <- xml_data %>%
    xml_find_all("//scrutin") %>%
    map_df(~{
      data.frame(
        vote_id = xml_attr(.x, "id"),
        group = xml_attr(.x, "groupe"),
        vote = xml_text(.x),
        stringsAsFactors = FALSE
      )
    })
  return(votes)
}
# Charger les votes
votes <- load_votes("~/Documents/GitHub/blog/shiny_depute/src/xml/VTANR5L16V19.xml")
## Afficher les premières lignes des données pour vérifier le chargement
print(head(deputes))
print(head(votes))

# Fonction pour comparer les votes
compare_votes <- function(votes, group1, group2) {
  votes_group1 <- votes %>% filter(group == group1)
  votes_group2 <- votes %>% filter(group == group2)
  common_votes <- intersect(votes_group1$vote_id, votes_group2$vote_id)
  
  if (length(common_votes) == 0) {
    return(NA) # Pas de votes communs
  }
  
  identical_votes <- sum(votes_group1 %>% filter(vote_id %in% common_votes) %>% pull(vote) ==
                           votes_group2 %>% filter(vote_id %in% common_votes) %>% pull(vote))
  return(identical_votes / length(common_votes) * 100)
}

# Créer l'application Shiny
ui <- fluidPage(
  titlePanel("Comparaison des Votes des Groupes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group1", "Sélectionner le premier groupe:", choices = unique(deputes$libelle_groupe)),
      selectInput("group2", "Sélectionner le deuxième groupe:", choices = unique(deputes$libelle_groupe))
    ),
    mainPanel(
      textOutput("result")
    )
  )
)

server <- function(input, output) {
  output$result <- renderText({
    group1 <- input$group1
    group2 <- input$group2
    if (group1 != group2) {
      result <- compare_votes(votes, group1, group2)
      if (is.na(result)) {
        "Aucun vote commun entre les groupes sélectionnés."
      } else {
        paste("Les votes sont identiques à", round(result, 2), "% entre", group1, "et", group2)
      }
    } else {
      "Veuillez sélectionner deux groupes différents."
    }
  })
}

shinyApp(ui = ui, server = server)