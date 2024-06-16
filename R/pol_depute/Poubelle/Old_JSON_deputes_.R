


```{r}

# Load necessary libraries
library(jsonlite)
library(dplyr)

# Define the directory containing JSON files for organes
json_dir_orga <- "/Users/fabiengouez/Documents/GitHub/blog/shiny_depute/src/AMO10/organes/"  # Change this to your actual directory

# Function to extract required information from a JSON file
extract_organe_info <- function(json_file) {
  json_data <- fromJSON(json_file)
  
  # Check if the organe has codeType "PARPOL"
  if (json_data$codeType == "PARPOL") {
    codeType <- json_data$uid
    libelle <- json_data$libelle
    libelleAbrege <- json_data$libelleAbrege
    
    # Create a data frame with the extracted information
    return(data.frame(codeType = codeType, libelle = libelle, libelleAbrege = libelleAbrege, stringsAsFactors = FALSE))
  }
  
  # Return NULL if the condition is not met
  return(NULL)
}

# List all JSON files in the directory
json_files <- list.files(json_dir_orga, pattern = "\\.json$", full.names = TRUE)

# Apply the function to each JSON file and combine results into a single data frame
results_orga <- do.call(rbind, lapply(json_files, extract_organe_info))

# Display the results
print(results_orga)
```
```{r}
# Load necessary libraries
library(jsonlite)
library(dplyr)

# Define the directories containing JSON files
json_dir_acteurs <- "/Users/fabiengouez/Documents/GitHub/blog/shiny_depute/src/AMO10/acteurs/"
json_dir_organes <- "/Users/fabiengouez/Documents/GitHub/blog/shiny_depute/src/AMO10/organes/"

# Function to extract civ, prenom, nom, and organe_ref from a JSON file
extract_basic_info <- function(json_file) {
  json_data <- fromJSON(json_file)
  
  # Extracting civ, prenom, and nom
  civ <- json_data$etatCivil$ident$civ
  prenom <- json_data$etatCivil$ident$prenom
  nom <- json_data$etatCivil$ident$nom
  organe_refs <- character(0)
  
  # Create a data frame with the extracted information
  data.frame(civ = civ, prenom = prenom, nom = nom, organe_refs = I(list(organe_refs)), stringsAsFactors = FALSE)
}

# Function to extract required information from a JSON file in organes
extract_organe_info <- function(json_file) {
  json_data <- fromJSON(json_file)
  
  uid <- json_data$uid
  libelle <- json_data$libelle
  
  # Create a data frame with the extracted information
  data.frame(uid = uid, libelle = libelle, stringsAsFactors = FALSE)
}

# List all JSON files in the directories
json_files_acteurs <- list.files(json_dir_acteurs, pattern = "\\.json$", full.names = TRUE)
json_files_organes <- list.files(json_dir_organes, pattern = "\\.json$", full.names = TRUE)

# Apply the function to each JSON file and combine results into single data frames
results <- do.call(rbind, lapply(json_files_acteurs, extract_basic_info))
results_orga <- do.call(rbind, lapply(json_files_organes, extract_organe_info))

# Function to find the parti name for given organe_refs by searching in results_orga
find_parti <- function(organe_refs, orga_data) {
  parti_list <- orga_data %>% filter(uid %in% organe_refs) %>% select(libelle) %>% unlist() %>% as.character()
  if (length(parti_list) == 0) {
    return(NA)
  }
  return(paste(parti_list, collapse = ", "))
}

# Apply the find_parti function to each row in results
results$parti <- sapply(results$organe_refs, find_parti, orga_data = results_orga)

# Display the results
print(results)
```

