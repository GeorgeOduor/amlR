#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    menuItemOutput(ns('sidebar_menu'))
  )
}

#' sidebar Server Functions
#'
#' @import shinydashboard
#'
#' @noRd
mod_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$sidebar_menu <- renderMenu({
      isolate({updateTabItems(session,inputId =  "sidebarmenu")})
      usermenu <- function() {
        sidebarMenu(id = "sidebarmenu",
                    menuItem(text = "Home",tabName = "home",icon = icon('home'),selected = T),
                    menuItem(text = "Disbursement Monitoring",tabName = "disbmonitoring",icon = icon('money')),
                    menuItem(text = 'Trx Monitoring',tabName = "trx_monitor",icon=icon('chart-line')),
                    menuItem(text = 'Overdrawn Accounts',tabName = 'overdrawn',icon=icon('minus')),
                    menuItem(text = 'Clients Monitoring',tabName = 'clients_monitor',icon=icon('users')),
                    menuItem(text = "Settings",tabName = "settings",icon = icon('gears'))

        )
      }
      usermenu()
    })

  })
}

## To be copied in the UI
# mod_sidebar_ui("sidebar_1")

## To be copied in the server
# mod_sidebar_server("sidebar_1")
