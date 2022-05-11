#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd

ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),
    dashboardPage(header = shinydashboard::dashboardHeader(),
                  sidebar = shinydashboardPlus::dashboardSidebar(
                    mod_sidebar_ui("sidebar_1"),collapsed = T,minified = T),
                  body = shinydashboard::dashboardBody(
                    mod_body_ui("body_1")),
                  footer = shinydashboardPlus::dashboardFooter(left = HTML("&copy; 2022")))
  )
}

app_ui <- shinymanager::secure_app(ui = ui,enable_admin = T,theme = "united")
#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'aml'
    )
  )
}
