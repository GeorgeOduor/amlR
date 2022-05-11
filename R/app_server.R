#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinymanager
#' @noRd
#' app_server
app_server <- function( input, output, session ) {

    res_auth <- secure_server(keep_token = T,
                              check_credentials = check_credentials(
                                  db = "inst/temp/amldb.sqlite"))


    mod_sidebar_server("sidebar_1")
    mod_body_server("body_1")
    mod_loanlimits_server("loanlimits_1")
    mod_trx_monitoring_server("trx_monitoring_1")
    mod_over_drawn_accs_server("over_drawn_accs_1")
    mod_clients_monitor_server("clients_monitor_1")
    mod_settings_server("settings_1")

# "334"+45 + 7

}
