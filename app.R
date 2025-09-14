library(shiny)
library(bslib)
library(ellmer)
library(shinychat)

# source("R/met_setup.R")
met_api <- "https://collectionapi.metmuseum.org/public/collection/v1/"

ui <- page_sidebar(
  title = "ArtRogue",
  sidebar = sidebar(
    actionButton("surprise_btn", "Surprise Me!"),
    textInput(
      "met_search_string",
      "Search for an artwork, artist, or keyword:",
      placeholder = "e.g. van gogh, armor, magpie"
    ),
    actionButton("search_btn", "Search"),
    actionButton("interpret_btn", "Interpret This")
  ),
  layout_column_wrap(
    width = 1/3,
    height = "100vh",
    fill = TRUE,
    gap = "1rem",
    
    # Column 1: Search Results
    card(
      header = "Search Results",
      uiOutput("met_search_results")
    ),
    
    # Column 2: Selected Artwork Display
    card(
      header = "Selected Artwork",
      uiOutput("selected_artwork_display")
    ),
    
    # Column 3: Chatbot
    card(
      header = "ArtRogue Chat",
      shinychat::chat_mod_ui("chat_ui")
    )
  )
)

server <- function(input, output, session) {
  chat <- ellmer::chat_openai(
    system_prompt = paste(
      "You are ArtRogue, an art museum chatbot inspired by Waldemar ",
      "Januszczak. You respond to questions about artworks, artists,",
      "and museum collections with historical insight and cultural ",
      "comparisons. Avoid a dry academic tone - be bold and conversational. ",
      "If possible, include an unexpected detail or interpretation."
    ),
    model = "gpt-4.1"
  )
  
  shinychat::chat_mod_server("chat_ui", chat)
  
  bot_prompt <- function(prompt_str) {
    print(glue::glue("bot prompt: {prompt_str}"))
    ns_id <- shiny::NS("chat_ui")("chat")
    shinychat::chat_clear(ns_id, session = session)
    stream <- chat$stream_async(prompt_str)
    shinychat::chat_append(ns_id, stream, session = session)
  }
  
  render_met_results <- function(object_ids, output_id = "met_search_results") {
    if (length(object_ids) == 0) {
      output[[output_id]] <- renderUI("No results found.")
      return()
    }
    
    artworks <- lapply(object_ids, function(id) {
      obj_url <- paste0(met_api, "objects/", id)
      jsonlite::fromJSON(obj_url)
    })
    
    output[[output_id]] <- renderUI({
      tagList(lapply(seq_along(artworks), function(i) {
        art <- artworks[[i]]
        actionButton(
          inputId = paste0("art_select_", i),
          label = HTML(paste0(
            "<div class='artwork-card'>",
            "<h4>", art$title, "</h4>",
            "<p>Artist: ", art$artistDisplayName, "</p>",
            "<p>Date: ", art$objectDate, "</p>",
            if (!is.null(art$primaryImageSmall) && art$primaryImageSmall != "")
              paste0("<img src='", art$primaryImageSmall, "' height='150px'>"),
            "</div>"
          )),
          style = "width: 100%; text-align: left;"
        )
      }))
    })
    
    return(artworks)
  }
  
  artworks <- reactiveVal(NULL)

  selected_artwork <- reactiveVal(NULL)
  
  observeEvent(input$search_btn, {
    req(input$met_search_string)
    search_url <- paste0(met_api, "search?", "q=", URLencode(input$met_search_string))
    search_results <- jsonlite::fromJSON(search_url)
    object_ids <- head(search_results$objectIDs, 5)
    artworks <<- render_met_results(object_ids)
  })
  
  observeEvent(input$surprise_btn, {
    search_url <- paste0(met_api, URLencode("search?isHighlight=true&isOnView=true&hasImages=true&q=*"))
    search_results <- jsonlite::fromJSON(search_url)
    object_ids <- sample(search_results$objectIDs, size = 5)
    artworks <<- render_met_results(object_ids)
  })
    
  observe({
    lapply(seq_along(artworks), function(i) {
      observeEvent(input[[paste0("art_select_", i)]], {
        selected_artwork(artworks[[i]])
        
        # Format metadata for LLM
        art <- artworks[[i]]
        summary_prompt <- paste(
          "I am looking an image of a work of art. What should I ",
          "understand about it? Here is the metadata: ",
          paste("Title:", art$title),
          paste("Artist:", art$artistDisplayName),
          paste("Date:", art$objectDate),
          paste("Medium:", art$medium),
          paste("Culture:", art$culture),
          paste("Description:", art$creditLine)
        )
        
        bot_prompt(summary_prompt)
      })
    })
  })
    
  observeEvent(input$surprise_btn, {
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
