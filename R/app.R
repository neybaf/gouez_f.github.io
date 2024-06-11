# app. R de concordancier, collocations et ngrams
install.packages(c("shinylive"))

library(shiny)
library(readr)
library(quanteda)
library(DT)
library(udpipe)
library(igraph)
library(ggraph)
library(quanteda.textstats)
library(knitr)
library(R.temis)
library(plotly)
library(dplyr)
library(shinylive)
library(httpuv)
# CSS pour agrandir la fenêtre
css <- "
#ngramTable, #ngramPlot, #semanticNetwork {
  height: 1600;
}
.shiny-output-error-validation {
  color: red;
}
"

ui <- fluidPage(
  tags$head(
    tags$style(HTML(css))
  ),
  titlePanel("Analyse de Co-texte"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choisir le corpus",
                multiple = TRUE,
                accept = c("text/plain", ".txt")),
      checkboxGroupInput("clean_options", "Nettoyer le texte", 
                         choices = list("Lemmatiseur" = "lemma", 
                                        "Stopwords" = "stopwords", 
                                        "Chiffres" = "numbers", 
                                        "Symboles" = "symbols", 
                                        "Minuscules" = "tolower")),
      actionButton("clean_text", "Nettoyer le texte"),
      radioButtons("analysis_type", "Type d'analyse", choices = c("Concordance", "Co-occurrences", "Ngrams")),
      conditionalPanel(
        condition = "input.analysis_type == 'Concordance'",
        textInput("n_words", "Co-texte:", value = 5), 
        textInput("myterm", "Mot clé:", value = "nation")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Co-occurrences'",
        textInput("myterm", "Mot clé:", value = "texte")
      ),
      actionButton("update", "Mettre à jour"),
      downloadButton("downloadData", "Télécharger les données")
    ),
    mainPanel(
      DTOutput("ngramTable"),
      uiOutput("cooccurrenceTable"),
      plotlyOutput("ngramPlot", height = "700px") # Ajout de height pour agrandir le graphique
    )
  )
)

server <- function(input, output, session) {
  raw_corpus_data <- reactiveVal(NULL)
  cleaned_corpus_data <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    req(input$file1)
    
    # Charger les données et créer le corpus combiné
    combine_files <- function(filepaths) {
      texts <- lapply(filepaths, readLines, encoding = "UTF-8")
      combined_text <- unlist(texts)
      return(combined_text)
    }
    
    files_comb <- input$file1$datapath
    combined_text <- combine_files(files_comb)
    raw_corpus_data(combined_text)
    cleaned_corpus_data(combined_text) # Initialiser avec les textes bruts
  })
  
  observeEvent(input$clean_text, {
    req(raw_corpus_data())
    
    combined_text <- raw_corpus_data() # Utiliser les textes bruts pour chaque nettoyage
    
    # Nettoyage du texte selon les options sélectionnées
    tokens_comb <- tokens(combined_text)
    if ("lemma" %in% input$clean_options) {
      # Charger le modèle UDPipe
      ud_model <- udpipe_load_model("/Users/fabiengouez/Library/Mobile Documents/com~apple~CloudDocs/Downloads/french-gsd-ud-2.5-191206(1).udpipe")
      
      # Fonction pour annoter et lemmatiser un document
      annotate_and_lemmatize <- function(text, ud_model) {
        annotations <- udpipe_annotate(ud_model, x = text)
        annotations <- as.data.frame(annotations)
        annotations$lemma <- ifelse(is.na(annotations$lemma) | annotations$lemma == "", annotations$token, annotations$lemma)
        modified_sentence <- paste(annotations$lemma, collapse = " ")
        return(modified_sentence)
      }
      
      # Assurer la correspondance des longueurs pour udpipe_annotate
      lemmatized_texts <- sapply(seq_along(combined_text), function(i) {
        text <- combined_text[i]
        annotate_and_lemmatize(text, ud_model)
      })
      
      # Recréer le corpus avec les textes lemmatisés
      corpus_comb <- corpus(lemmatized_texts, docnames = paste0("doc", seq_along(lemmatized_texts)))
      tokens_comb <- tokens(corpus_comb)
    }
    if ("stopwords" %in% input$clean_options) {
      tokens_comb <- tokens_remove(tokens_comb, pattern = stopwords("fr"))
    }
    if ("numbers" %in% input$clean_options) {
      tokens_comb <- tokens_remove(tokens_comb, pattern = "\\d+", valuetype = "regex")
    }
    if ("symbols" %in% input$clean_options) {
      tokens_comb <- tokens_keep(tokens_comb, pattern = "[\\p{L}\\p{N}]+", valuetype = "regex")
    }
    if ("tolower" %in% input$clean_options) {
      tokens_comb <- tokens_tolower(tokens_comb)
    }
    cleaned_corpus_data(tokens_comb)
  })
  
  observeEvent(input$update, {
    req(cleaned_corpus_data())
    
    if (input$analysis_type == "Concordance") {
      extract_concordance <- function(doc, term, n_words) {
        words <- unlist(strsplit(doc, "\\s+"))
        term_positions <- which(words == term)
        concordances <- sapply(term_positions, function(pos) {
          start <- max(1, pos - n_words)
          end <- min(length(words), pos + n_words)
          paste(words[start:end], collapse = " ")
        })
        return(concordances)
      }
      
      tokens_comb <- cleaned_corpus_data()
      
      concordances_list <- lapply(tokens_comb, function(doc) {
        extract_concordance(as.character(doc), input$myterm, as.numeric(input$n_words))
      })
      
      concordances_df <- data.frame(text = unlist(concordances_list))
      
      # Tableau interactif
      output$ngramTable <- renderDT({
        datatable(concordances_df)
      })
      
      # Télécharger les données
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("ngram_data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(concordances_df, file, row.names = FALSE)
        }
      )
      
      # Cacher le tableau de co-occurrences
      output$cooccurrenceTable <- renderUI(NULL)
      output$ngramPlot <- renderPlotly(NULL)
    }  else if (input$analysis_type == "Co-occurrences") {
      req(cleaned_corpus_data())
      
      # Créer une matrice terme-document
      dtm <- dfm(tokens(cleaned_corpus_data()))
      
      # Calculer les co-occurrences avec le terme spécifié
      term_fcm <- fcm(dtm)
      
      ## Vérifier si le terme spécifié est présent dans la matrice
      if (!(input$myterm %in% colnames(term_fcm))) {
        cooc_df <- data.frame(message = "No co-occurrences found for the specified term.")
      } else {
        # Extraire la colonne des co-occurrences pour le terme spécifié
        cooc_vector <- as.vector(term_fcm[, input$myterm])
        cooc_terms <- colnames(term_fcm)
        
        # Créer un DataFrame à partir du vecteur de co-occurrences
        cooc_df <- data.frame(Term = cooc_terms, Cooccurrences = cooc_vector)
        
        # Filtrer les termes avec au moins une co-occurrence
        cooc_df <- cooc_df[cooc_df$Cooccurrences > 5, ]
        
        # Vérifier si des co-occurrences ont été trouvées après filtrage
        if (nrow(cooc_df) == 0) {
          cooc_df <- data.frame(message = "No co-occurrences found for the specified term.")
        } else {
          # Trier le DataFrame par nombre de co-occurrences, décroissant
          cooc_df <- cooc_df[order(cooc_df$Cooccurrences, decreasing = TRUE), ]
        }
      }
      
      # Générer le titre et la légende du tableau
      title <- paste("Co-occurrences de:", input$myterm)
      # Générer le tableau et l'afficher
      output$cooccurrenceTable <- renderUI({
        x1 <- knitr::kable(cooc_df, caption = title, format = "html")
        HTML(x1)
      })
      
      # Cacher le tableau de concordance
      output$ngramTable <- renderDT(NULL)
      output$ngramPlot <- renderPlotly(NULL)
    }
    else if (input$analysis_type == "Ngrams") {
      req(cleaned_corpus_data())
      
      # Obtenir les tokens à partir des données nettoyées
      tokens_comb <- tokens(cleaned_corpus_data())
      
      # Calculer les collocations (ngrams) avec quanteda.textstats
      collocations <- textstat_collocations(tokens_comb, size = 2:5)
      
      # Filtrer les collocations pour n'afficher que les plus fréquentes
      collocations <- collocations[order(collocations$count, decreasing = TRUE), ]
      
      # Limiter le nombre de collocations affichées (par exemple, les 20 premières)
      top_collocations <- head(collocations, 50)
      
      p <- ggplot(top_collocations, aes(x = reorder(collocation, count), y = count)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(
          title = "Top 20 Collocations (2-3 words)",
          x = "Collocations",
          y = "Frequency"
        ) +
        theme_minimal()
      
      # Convertir le graphique ggplot en un graphique interactif avec plotly et ajouter des annotations
      output$ngramPlot <- renderPlotly({
        ggplotly(p) 
      })
      # Cacher les autres tableaux
      output$ngramTable <- renderDT(NULL)
      output$cooccurrenceTable <- renderUI(NULL)
    }
  })
}

shinyApp(ui, server)
shinylive::export(appdir = "/Users/fabiengouez/Documents/GitHub/blog/shinylive", destdir = "docs")
