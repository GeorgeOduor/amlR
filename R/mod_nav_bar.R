#' nav_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nav_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('navbar'))
  )
}

#' nav_bar Server Functions
#'
#' @noRd
mod_nav_bar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$navbar <- renderUI({
      tagList(
        tabPanel(title = "Disbursement"),
        navbarMenu(title = "Transaction Monitoring",
                   tabPanel("Transaction Monitoring1"),
                   tabPanel("Transaction Monitoring2")
                   )
      )
    })
  })
}

## To be copied in the UI
# mod_nav_bar_ui("nav_bar_1")

## To be copied in the server
# mod_nav_bar_server("nav_bar_1")
