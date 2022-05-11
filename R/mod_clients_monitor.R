#' clients_monitor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shinydashboard
#' @import shinyWidgets
#' @importFrom shiny NS tagList
mod_clients_monitor_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      uiOutput(ns('controles')),
      box(title = "Client Transactions Monitoring",status='success',width=9,
          h4("Flagged transactions Summary"),
          panel(uiOutput(ns('trx_summaries'))),
          hr(),
          h4("Flagged Cases"),
          tabsetPanel(type = 'pills',id = ns("risk_status"),
                      tabPanel(title = "Amounts Transacted",value = 'amount',
                               DT::dataTableOutput(ns('amounts_flagged'))),
                      tabPanel(title = "Transactions Count",value = 'count',
                               DT::dataTableOutput(ns('counts_flagged')))
                      )
          )
    ),

  )
}

#' clients_monitor Server Functions
#' @import tibble
#' @import dplyr
#' @import highcharter
#' @import kableExtra
#' @noRd
mod_clients_monitor_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$controles <- renderUI({
      box(title = "Settings",status="success",width=3,
          dateRangeInput(ns('period'),
                         "Select Reporting Period",start = Sys.Date()-7,
                         end = Sys.Date()-1,max = Sys.Date()),
          hr(),
          actionBttn(ns('submit_checks'),"Submit",style = 'material-flat',block = T,size = 'sm',color = 'success'),
          hr(),
          uiOutput(ns('controls'))
      )
    })
    observeEvent(input$submit_checks,{
      cbs_creds = tbl(con,"CBS_Creds") %>% as_tibble()
      cbs = maisha_conn(cbs_creds$username,enc_dec(cbs_creds$password,"dec"))
      df = client_transactions(session,con = cbs,start_date = input$period[1],end_date = input$period[2] )

      # load("inst/temp/clients_trx_summary.Rdata")
      # render data tables here ====
      # 1 amounts flagged
      output$amounts_flagged <- DT::renderDataTable({
        req(input$trx_type)
        data = df$trx_long %>% ungroup()%>%
          arrange(ClientID) %>%
          filter(Check_amount=="High",TransactionType==input$trx_type,TransactionCategory==input$product) %>%
          select(ClientID,AccountID,AccountName,TrxDate,TransactionCategory,TransactionType,Amount) %>%
          bind_cols(tibble(View = shinyInput(FUN = actionButton,
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                             n = nrow(.),id = 'view_more_',icon=icon("eye"),
                                             class="action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                             label = "",onclick = 'Shiny.setInputValue(\"clients_monitor_1-view_more\", this.id, {priority: \"event\"})')
                           )
                    )
        datatable(data,escape = F,selection="none",options = list(scrollY = 300,scroller = T))
      })
      # 2 counts flagged

      output$counts_flagged <- DT::renderDataTable({
        req(input$trx_type)
        data = df$trx_long %>% ungroup()%>%
          arrange(ClientID) %>%
          filter(Check_count=="High",TransactionType==input$trx_type,TransactionCategory==input$product) %>%
          select(ClientID,AccountID,AccountName,TrxDate,TransactionCategory,TransactionType,Count) %>%
          bind_cols(tibble(View = shinyInput(FUN = actionButton,
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                             n = nrow(.),id = 'view_more_',icon=icon("eye"),
                                             class="action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                             label = "",onclick = 'Shiny.setInputValue(\"clients_monitor_1-view_more\", this.id, {priority: \"event\"})')
          )
          )
        datatable(data,escape = F,selection="none",options = list(scrollY = 300,scroller = T))
      })
      # render value boxes here ====
      output$trx_summaries <- renderUI({
        req(input$trx_type)
        # fetch required Data
        trx_data = df$trx_long_summary %>%
          filter(TransactionType==input$trx_type,TransactionCategory==input$product) %>%
          mutate(Amount=format(Amount,big.mark=','))
        tagList(
          infoBox(title = "High Risk",subtitle = paste("By Volumes:",pull(filter(trx_data,value=="High"),Count)),value = paste("By Values:",pull(filter(trx_data,value=="High"),Amount)),color = "red",icon = icon('money-bill-alt')),
          infoBox(title = "Low Risk",subtitle = paste("By Volumes:",pull(filter(trx_data,value=="Very Low"),Count)),value = paste("By Values:",pull(filter(trx_data,value=="Very Low"),Amount)),color = "orange",icon = icon('money-bill-alt')),
          infoBox(title = "Normal Range",subtitle = paste("By Volumes:",pull(filter(trx_data,value=="Normal"),Count)),value = paste("By Values:",pull(filter(trx_data,value=="Normal"),Amount)),color = "green",icon = icon('money-bill-alt'))
        )
      })
      # render controls here ====
      output$controls <- renderUI({
        tagList(
          selectInput(ns('product'),"Bank Product",
                      choices = c('Branch','Mobile Banking','Mfanisi Airtel','Mfanisi Safaricom')),
          hr(),
          selectInput(ns('trx_type'),"Transaction Category",
                      c('Deposit','Withdrawal'))
        )
      })
    })
    observeEvent(input$view_more,{
      load("inst/temp/clients_trx_summary.Rdata")

      # pull the selected client id and account number====
      selectedRow <<-  readr::parse_number(input$view_more)
      data <- switch(input$risk_status,
                      "count" = {df$trx_long %>% filter(Check_count=="High")},
                      "amount" = {df$trx_long  %>% filter(Check_amount=="High")}
                      ) %>%
        arrange(ClientID) %>%
        filter(TransactionType==input$trx_type,
               TransactionCategory==input$product) %>%
        rowid_to_column() %>%
        filter(rowid == selectedRow)
      # show clients progress ======
      output$progress <- renderHighchart({
        flagged_trx_chart(dataset = df$trx_long1,mode = input$risk_status,
                          AccID = data$AccountID,
                          TrxCategory = input$product,
                          TrxType =  input$trx_type)
      })
      # show statements here =====
      output$statement_table <- function(){
        trx_statements(trx_data = df$statements,
                       AccID = data$AccountID,
                       trx_category = input$product,
                       trx_type = input$trx_type)
      }
     # show clients statement here ====
      output$statement <- DT::renderDataTable({
        datatable(head(iris),options=list(scrollX=T,scrollY=300))
      })
      showModal(modalDialog(title = "Client Transactions Report",size = 'l',
                            trans_summary(ns,TransactionType=input$trx_type,
                                          AccountID = data$AccountID,
                                          AccountName = data$AccountName,
                                          ClientID = data$ClientID
                                          ),
                            easyClose = T,
                            footer = HTML("&copy; Maisha Bank")))
    })
  })
}

## To be copied in the UI
# mod_clients_monitor_ui("clients_monitor_1")

## To be copied in the server
# mod_clients_monitor_server("clients_monitor_1")
