library(shiny)
library(bslib)
library(ellmer)
library(markdown)
library(shinychat)
library(httr)

# ---- Setup ----
met_api <- "https://collectionapi.metmuseum.org/public/collection/v1/"

# ---- Theme ----
my_theme <- bs_theme(
  version = 5,
  bootswatch = "lux",      # or another Bootswatch
  primary = "#8B4513",     # warm brown
  secondary = "#C0A060",   # gold accent
  success = "#3B7A57",
  base_font = font_google("Lato"),
  heading_font = font_google("Merriweather"),
  code_font = font_google("Fira Code")
)

tags$style(HTML("
  .card {
    border-radius: 12px;
    box-shadow: 0 4px 10px rgba(0,0,0,0.1);
  }
  .accordion-button {
    font-weight: bold;
  }
  .artwork-card img {
    margin-top: 0.5em;
    border-radius: 6px;
  }
"))

ui <- page_sidebar(
  theme = my_theme,
  title = "ArtRogue",
  
  tags$head(
    tags$style(HTML("
      .card {
        border-radius: 12px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.1);
      }
      .accordion-button {
        font-weight: bold;
      }
      .artwork-card img {
        margin-top: 0.5em;
        border-radius: 6px;
      }
      .art-card:hover {
        transform: scale(1.03);
        transition: transform 0.2s ease-in-out;
        cursor: pointer;
      }
    "))
  ),
  
  sidebar = sidebar(
    accordion(
      accordion_panel(
        "Find Artworks",
        textInput(
          "met_search_string",
          "Search for an artwork, artist, or keyword:",
          placeholder = "e.g. van gogh, armor, magpie"
        ),
        actionButton("search_btn", "Search", class = "btn-primary"),
        div(class = "my-2 text-center", "- or -"),
        actionButton("surprise_btn", "Surprise Me!")
      ),
      accordion_panel(
        "Results",
        uiOutput("met_search_results")
      ),
      open = c("Find Artworks", "Results")
    ),
    # actionButton("interpret_btn", "Interpret This"),
  ),
  layout_columns(
    col_widths = c(8, 4),

    # Left column: artwork image + metadata
    card(
      header = "Selected Artwork",
      uiOutput("selected_artwork_display"),
      uiOutput("selected_artwork_metadata")
    ),
    
    # Right column: chatbot
    card(
      header = "ArtRogue Chat",
      shinychat::chat_mod_ui("chat_ui"),
      full_screen = TRUE
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
  
  output$selected_artwork_display <- renderUI({
    includeMarkdown("intro.md")
  })
  
  bot_prompt <- function(prompt_str) {
    print(glue::glue("bot prompt: {prompt_str}"))
    ns_id <- shiny::NS("chat_ui")("chat")
    shinychat::chat_clear(ns_id, session = session)
    stream <- chat$stream_async(prompt_str)
    shinychat::chat_append(ns_id, stream, session = session)
  }
  
  artworks <- reactiveVal(NULL)
  
  render_met_results <- function(object_ids, output_id = "met_search_results") {
    if (length(object_ids) == 0 || is.null(object_ids)) {
      output[[output_id]] <- renderUI("No results found.")
      return()
    }

    # For each artwork in result set, pull its details from the API.
    arts <- list()
    for (id in object_ids) {
      obj_url <- paste0(met_api, "objects/", id)
      # defensive fetch: skip if not 200
      resp <- tryCatch(httr::GET(obj_url, timeout(5)), error = function(e) NULL)
      if (is.null(resp)) next
      status <- httr::status_code(resp)
      if (status != 200) {
        # skip 404 and other errors
        next
      }
      # safe parse
      parsed <- tryCatch(
        jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")),
        error = function(e) NULL
      )
      if (!is.null(parsed)) arts[[length(arts) + 1]] <- parsed
    }
    
    artworks(arts)  # store in the reactive val
    
    output[[output_id]] <- renderUI({
      div(
        style = "display: grid;
               grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
               gap: 1rem;
               max-height: 500px;
               overflow-y: auto;
               padding: 0.5rem;",
        lapply(seq_along(arts), function(i) {
          art <- arts[[i]]
          actionLink(
            inputId = paste0("art_select_", i),
            label = div(
              class = "art-card",
              style = "border: 1px solid #ccc; border-radius: 8px; 
                     padding: 0.5rem; text-align: center; 
                     background: #fff; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
              if (!is.null(art$primaryImageSmall) && art$primaryImageSmall != "")
                img(src = art$primaryImageSmall,
                    style = "max-height: 120px; width: auto; border-radius: 4px;"),
              div(style = "font-size: 0.8em; margin-top: 0.5rem;",
                  strong(art$title), br(),
                  if (nzchar(art$artistDisplayName)) art$artistDisplayName else "Unknown",
                  br(), art$objectDate
              )
            )
          )
        })
      )
    })
    
    return(artworks)
  }
  
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
    arts <- artworks()
    req(arts)  # ensure it’s not NULL

    lapply(seq_along(arts), function(i) {
      observeEvent(input[[paste0("art_select_", i)]], {
        selected_artwork(arts[[i]])
        
        output$selected_artwork_display <- renderUI({
          HTML(glue::glue("<img src='{arts[[i]]$primaryImageSmall}'>"))
        })
        
        # Tells shiny, 'update the UI before running this code'
        later::later(function() {
          summary_prompt <- paste(
            "I am looking an image of a work of art. What should I ",
            "understand about it? No need to show me the image since ",
            "I am already looking at it. Here is the metadata: ",
            jsonlite::toJSON(arts[[i]])
          )
          
          bot_prompt(summary_prompt)
        })
      })
    })
  })
  
  output$selected_artwork_metadata <- renderUI({
    req(selected_artwork())
    art <- selected_artwork()
    tagList(
      h4(art$title),
      p(strong("Artist: "), art$artistDisplayName),
      p(strong("Date: "), art$objectDate),
      p(strong("Medium: "), art$medium),
      p(strong("Dimensions: "), art$dimensions)
    )
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
