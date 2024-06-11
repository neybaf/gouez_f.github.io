# Function to fetch the file list from the URL
getFileList <- function() {
  url <- "http://neybaf.github.io/gouez_f/R/data"
  page <- read_html(url)
  files <- page %>%
    html_nodes("a") %>% # Adjust the selector based on the HTML structure of the directory listing
    html_text() %>%
    grep("\\.txt$", ., value = TRUE) # Filter for .txt files
  return(files)
}

ui <- fluidPage(
  selectInput("file1", "Choisir le corpus",
              choices = getFileList(),
              multiple = TRUE),
  textOutput("selected_files")
)

server <- function(input, output, session) {
  output$selected_files <- renderText({
    paste("Fichiers sélectionnés:", input$file1)
  })
}

shinyApp(ui, server)