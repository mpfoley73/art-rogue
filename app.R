library(shiny)
library(bslib)
library(ellmer)
library(markdown)
library(shinychat)
library(httr)

# ---- Setup ----
source("R/fx_api.R")
source("R/cma_api.R")
source("R/met_api.R")

# ---- Theme ----
my_theme <- bs_theme(
  version = 5,
  bootswatch = "lux",
  primary = "#8B4513",
  secondary = "#C0A060",
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

ui <- page_navbar(
  theme = my_theme,
  title = "ArtRogue",
  header = tags$head(
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
  nav_panel(
    title = tagList(icon("home"), "Home"),
    layout_columns(
      col_widths = c(8, 4),

      # Left column: group Search Results above Selected Artwork
      div(
        accordion(
          id = "search_accordion",
          accordion_panel(
            "What's there to see at the museum",
            uiOutput("intro"),
            card(
              card_body(
                fluidRow(
                  column(
                    width = 9,
                    textInput(
                      "search_string",
                      label = NULL, # "Search artworks / artist / keyword",
                      placeholder = "e.g. van gogh, baseball", width = "100%"
                    )
                  ),
                  column(
                    width = 3,
                    actionButton("search_btn", "Search", class = "btn-primary"),
                  )
                )
              ),
              card_footer(
                div(
                  class = "d-flex gap-2",
                  "- or -",
                  actionButton("surprise_btn", "Surprise Me!", class = "btn-secondary"),
                  actionButton("new_btn", "What's New?", class = "btn-secondary"),
                )
              )
            ),
            # card(
            uiOutput("search_results"),
            max_height = "250px"
            # ),
          )
        ),
        # Artwork Display
        card(
          uiOutput("selected_artwork_display"),
          uiOutput("selected_artwork_metadata")
        )
      ),

      # Right column: chatbot
      card(
        card_header("ArtRogue Chat"),
        shinychat::chat_mod_ui("chat_ui"),
        full_screen = TRUE
      )
    )
  ),
  nav_spacer(),
  nav_menu(
    "More",
    nav_panel(
      title = tagList(icon("gear"), "Settings"),
      card(
        selectizeInput(
          "museum",
          "Museum:",
          choices = c(met_label, cma_label),
          selected = cma_label,
          # width = "100%"
        )
      )
    ),
    nav_panel(
      title = tagList(icon("info"), "About"),
      card(
        card_header("About this app"),
        card_body(includeMarkdown("about.md"))
      )
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
    model = "gpt-4.1-mini"
  )

  shinychat::chat_mod_server("chat_ui", chat)

  # Default to a welcome screen.
  output$intro <- renderUI({
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

  render_results <- function(artworks_list) {
    if (length(artworks_list) == 0 || is.null(artworks_list)) {
      output$search_results <- renderUI("No results found.")
      return()
    }

    artworks(artworks_list) # store in the reactive val

    output$search_results <- renderUI({
      div(
        style = "display: grid;
               grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
               gap: 1rem;
               max-height: 500px;
               overflow-y: auto;
               padding: 0.5rem;",
        lapply(seq_along(artworks_list), function(i) {
          artwork <- fx_search_result(input$museum, artworks_list[[i]], verbose = TRUE)
          if (is.null(artwork) || is.null(artwork$title)) {
            return(div(class = "art-card", "Invalid artwork"))
          }
          actionLink(
            inputId = paste0("art_select_", i),
            label = div(
              class = "art-card",
              style = "border: 1px solid #ccc; border-radius: 8px;
                     padding: 0.5rem; text-align: center;
                     background: #fff; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
              if (!is.null(artwork$img_url) && artwork$img_url != "") {
                img(
                  src = artwork$img_url,
                  style = "max-height: 120px; max-width: 75px; width: auto; border-radius: 4px;"
                )
              },
              div(
                style = "font-size: 0.8em; margin-top: 0.5rem;",
                strong(artwork$title), br(),
                artwork$artist, br(),
                artwork$creation_date
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
    req(input$search_string)
    artworks_list <- fx_search(input$museum, input$search_string)
    render_results(artworks_list)
  })

  observeEvent(input$surprise_btn, {
    artworks_list <- fx_search(input$museum, "*")
    render_results(artworks_list)
  })

  observeEvent(input$new_btn, {
    artworks_list <- fx_search(input$museum, "*", highlight = TRUE)
    render_results(artworks_list)
  })

  # ---------------------------------------------------------------------------
  # App displays up to 5 artwork tiles. If user clicks a tile, kick off a
  # dialogue with the LLM.

  chat_about_artwork <- function(i) {
    arts <- artworks()
    req(arts) # ensure it’s not NULL
    selected_artwork(arts[[i]])

    artwork <- fx_search_result(input$museum, arts[[i]], verbose = TRUE)

    output$selected_artwork_display <- renderUI({
      HTML(glue::glue("<img src='{artwork$img_url}' style='max-width:800px; max-height:800px;'>"))
    })

    output$intro <- renderUI({
      ""
    })

    accordion_panel_close("search_accordion", values = TRUE)

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
  }

  observeEvent(input$art_select_1, {
    chat_about_artwork(1)
  })

  observeEvent(input$art_select_2, {
    chat_about_artwork(2)
  })

  observeEvent(input$art_select_3, {
    chat_about_artwork(3)
  })

  observeEvent(input$art_select_4, {
    chat_about_artwork(4)
  })

  observeEvent(input$art_select_5, {
    chat_about_artwork(5)
  })

  #----------------------------------------------------------------------------

  output$selected_artwork_metadata <- renderUI({
    req(selected_artwork())
    art <- selected_artwork()
    artwork <- fx_search_result(input$museum, art, verbose = TRUE)
    tagList(
      h4(artwork$title),
      p(strong("Artist: "), artwork$artist),
      p(strong("Date: "), artwork$creation_date)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
