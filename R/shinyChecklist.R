#' shinyChecklist
#'
#' @description Simple Checklist that can be run as a Rstudio plugin.
#'
#' @export shinyChecklist
#'
#' @import shiny miniUI rstudioapi
#'
shinyChecklist <- function()
{



  ui <- miniPage(
    gadgetTitleBar("Checklist"),
    miniContentPanel(
      textInput("newitem", "Add a new item"),
      actionButton("send", "Add"),
      uiOutput("list"),
      hr(),
      textInput("data", "load list from", ".shinychecklist")
    )
  )

  server <- function(input, output, session) {

    ### reactive values
    react <- reactiveValues(list = c(), selected = NULL)

    ### Two Observer for new and old list input
    observe(
      {
        req(file.exists(input$data))
        bckp <- readRDS(input$data)
        react$selected <- bckp$selected
        react$list <- bckp$list
      }
    )



    observeEvent(
      input$send,
      {
        react$list <- c(react$list, input$newitem)
      }
    )

    observeEvent(
      input$listout,
      {
        react$selected <- input$listout
      }
    )

    ### Render the output
    output$list <- renderUI(
      {
        checkboxGroupInput(
          "listout", "", react$list, react$selected
        )
      }
    )

    # End the app
    observeEvent(
      input$done,
      {
        saveRDS(reactiveValuesToList(react), input$data)
        stopApp()
      }
    )

  }

  viewer <- dialogViewer("ShinyChecklist")
  runGadget(ui, server, viewer = viewer)

}
