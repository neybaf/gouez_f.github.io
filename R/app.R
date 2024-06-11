# app. R de concordancier, collocations et ngrams
library(shiny)
library(shinylive)
library(readr)
library(DT)
library(udpipe)
library(ggplot2)

ui <- fluidPage(
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
      conditionalPanel(
        condition = "input.analysis_type == 'Ngrams'",
        textInput("n_mini", "Ngrams minimum", value = 2),
        textInput("n_maxi", "Ngrams maximum", value = 5)
      ),
      actionButton("update", "Mettre à jour"),
      downloadButton("downloadData", "Télécharger les données")
    ),
    mainPanel(
      DTOutput("ngramTable"),
      uiOutput("cooccurrenceTable"),
      plotOutput("ngramPlot", height = "700px") # Ajout de height pour agrandir le graphique
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
      ud_model <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")
      
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
      output$concordance <- renderDT({
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
      
      # Convert tokens to plain text
      text_data <- sapply(cleaned_corpus_data(), function(tokens) paste(as.character(tokens), collapse = " "))
      text_data_df <- data.frame(doc_id = 1:length(text_data), text = text_data, stringsAsFactors = FALSE)
      
      # Annotate the cleaned corpus data
      ud_model <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")
      annotations <- udpipe_annotate(ud_model, x = text_data_df$text)
      annotations_df <- as.data.frame(annotations)
      
      # Filter annotations based on parts of speech if needed
      relevant_annotations <- annotations_df[annotations_df$upos %in% c("NOUN", "ADJ", "VERB"), ]
      
      # Calculate co-occurrences
      cooc_df <- cooccurrence(relevant_annotations, group = "doc_id", term = "lemma")
      
      # Filter for co-occurrences with the specified term
      term_cooc <- subset(cooc_df, term1 == input$myterm | term2 == input$myterm)
      term_cooc <- term_cooc[order(term_cooc$cooc, decreasing = TRUE), ]
      
      # Display co-occurrence table
      output$cooccurrenceTable <- renderUI({
        x1 <- knitr::kable(term_cooc, caption = paste("Co-occurrences de", input$myterm), format = "html")
        HTML(x1)
      })
      
      # Hide other outputs
      output$concordance <- renderDT(NULL)
      output$ngramPlot <- renderPlotly(NULL)
    }
    else if (input$analysis_type == "Ngrams") {
      req(cleaned_corpus_data())
      # Préparer les données textuelles pour l'annotation
      text_data <- unlist(lapply(cleaned_corpus_data(), function(x) paste(x, collapse = " ")))
      text_data_df <- data.frame(doc_id = 1:length(text_data), text = text_data, stringsAsFactors = FALSE)
      
      # Annoter les données du corpus nettoyé
      ud_model <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")
      annotations <- udpipe_annotate(ud_model, x = text_data_df$text)
      annotations_df <- as.data.frame(annotations)
      
      # Calculer les collocations
      collocations <- keywords_collocation(annotations_df, term = "lemma", group = c("doc_id", "sentence_id"), ngram_max = as.integer(input$n_maxi), n_min = as.integer(input$n_mini))
      
      # Filtrer et trier les collocations
      top_collocations <- head(collocations[order(collocations$freq, decreasing = TRUE), ], 50)
      
      # Afficher les collocations
      output$ngramPlot <- renderPlot({
        ggplot(top_collocations, aes(x = reorder(keyword, freq), y = freq)) +
          geom_bar(stat = "identity", fill = "steelblue") +
          coord_flip() +
          labs(
            title = paste("Top 50 Collocations", input$n_mini, "-", input$n_maxi),
            x = "Collocations",
            y = "Fréquence"
          ) +
          theme_minimal()
      })
      
      output$ngramTable <- renderTable({
        top_collocations
      })
      
      output$concordance <- renderTable(NULL)
      output$cooccurrenceTable <- renderUI(NULL)
    }
  })
}

shinyApp(ui, server)



