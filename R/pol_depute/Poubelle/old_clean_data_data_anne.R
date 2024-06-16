library(dplyr)
library(udpipe)
library(igraph)
library(ggraph)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(ggplot2)

all_data_unique <- distinct(all_data)

df <- data.frame(
  content = all_data_unique$content,
  id = all_data_unique$title)

# Charger le modèle UDPipe (mettre le chemin correct vers votre modèle)
model_path <- "/Users/fabiengouez/Library/Mobile Documents/com~apple~CloudDocs/Downloads/french-gsd-ud-2.5-191206(1).udpipe"
ud_model <- udpipe_load_model(model_path)
# Annoter et lemmatiser les textes
annotate_and_lemmatize <- function(text, ud_model) {
  annotations <- udpipe_annotate(ud_model, x = text)
  annotations <- as.data.frame(annotations)
  annotations$lemma <- ifelse(is.na(annotations$lemma) | annotations$lemma == "", annotations$token, annotations$lemma)
  modified_sentence <- paste(annotations$lemma, collapse = " ")
  return(modified_sentence)
}


# Créer le corpus et tokeniser
df$content <- ifelse(is.na(df$content), "", df$content)
df$content <- gsub("0 0", " ", df$content)
df$content <- gsub("votes évaluation l'article", " ", df$content)
df$content <- gsub("0 votes", " ", df$content)
df$content <- gsub("1 vote", " ", df$content)
df$content <- gsub("1 vote", " ", df$content)

# Créer le corpus
corpus <- corpus(df$content)

# Nettoyer le corpus
tokens <- tokens(corpus, remove_punct = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(pattern = stopwords("fr")) %>%
  tokens_remove(pattern = "\\d+", valuetype = "regex") %>%
  tokens_keep(pattern = "[\\p{L}\\p{N}]+", valuetype = "regex")

# Convertir les tokens en texte après nettoyage
cleaned_texts <- sapply(tokens, function(x) paste(x, collapse = " "))

# Mettre à jour le DataFrame avec les textes nettoyés
df$content <- cleaned_texts

write.csv(all_data, "all_data.csv", row.names = FALSE)
write.csv(df, "all_data_clean.csv", row.names = FALSE)

# Créer une matrice terme-document et une matrice de co-occurrences de termes
dtm <- dfm(tokens)

toks_stats <- textstat_collocations(
  tokens,
  method = "lambda",
  size = 2:5,
  min_count = 20,
  smoothing = 0.5)


n_words <- 15  # Nombre de mots avant et après le terme recherché
myterm <- "Chine"
# Fonction pour extraire les concordances
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
# Créer un vecteur source pour VCorpus
source <- VectorSource(df$content)

# Créer un VCorpus
myVcorpus <- VCorpus(source)
myterm <- "chine"
concordances_list <- lapply(myVcorpus, function(doc) {
  extract_concordance(as.character(content(doc)), myterm, n_words)
})
 
concordances_df <- data.frame(text = unlist(concordances_list))
concordances_df
  
