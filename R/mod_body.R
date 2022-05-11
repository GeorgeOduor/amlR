#' body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_body_ui <- function(id){
  ns <- NS(id)
uiOutput(ns('body'))
}

#' body Server Functions
#'
#' @noRd
mod_body_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
output$body <- renderUI({
  tagList(
    tabItems(
      tabItem(tabName = 'home',"Home"),
      tabItem(tabName = 'disbmonitoring',mod_loanlimits_ui("loanlimits_1")),
      tabItem(tabName = 'trx_monitor',mod_trx_monitoring_ui("trx_monitoring_1")),
      tabItem(tabName = 'overdrawn',mod_over_drawn_accs_ui("over_drawn_accs_1")),
      tabItem(tabName = 'clients_monitor',mod_clients_monitor_ui("clients_monitor_1")),
      tabItem(tabName = 'settings',mod_settings_ui("settings_1"))
    )
  )
})
  })
}

## To be copied in the UI
# mod_body_ui("body_1")

## To be copied in the server
# mod_body_server("body_1")
