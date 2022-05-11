#' trx_monitoring UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets
#' @import shinydashboard
mod_trx_monitoring_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('trx_controls')),
    fluidRow(box(width = 12,title = "Transaction Montoring",status="success",
                 tabsetPanel(type = "pills",
                             tabPanel(title = 'Daily Trx Counts',
                                      column(4,
                                             helpText("Use the controls to set flag rules"),
                                             uiOutput(ns('flagoutput')),
                                             textOutput(ns("flag1"))
                                      ),
                                      column(8,DT::dataTableOutput(ns("trx_counts1")))),
                             tabPanel(title = 'Daily Trx Amounts',
                                      column(4,
                                             helpText("Use the controls to set flag rules"),
                                             column(6,numericInput(ns("CC"),"CC",value = 1000000),
                                                    numericInput(ns("CD"),"CD",value = 1000000)),
                                             column(6,numericInput(ns("TD"),"TD",value = 1000000),
                                                    numericInput(ns("TC"),"TC",value = 1000000))
                                      ),
                                      column(8,DT::dataTableOutput(ns("trx_amounts_")))),
                             tabPanel(title = 'Trx Cummulative',
                                      column(3,
                                             selectInput(ns("mode"),label = "Select Param",choices = c("Amount","Count"))),
                                      column(9,DT::dataTableOutput(ns("trx_weekly")))
                             )
                 )))
  )
}

#' trx_monitoring Server Functions
#' @import shinyWidgets
#' @import glue
#' @import openxlsx
#' @noRd
mod_trx_monitoring_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$flag1 <- renderText({
      paste("Total transactions between",glue_collapse(input$totaltrxs,last = " and "))

    })

    output$flag2 <- renderText({
      paste("Total transaction Amounts between",glue_collapse(input$totaltrxs_amount,last = " and "))

    })

    output$trx_controls <- renderUI({
      fluidRow(
        panel(
          column(width = 3,
                 airDatepickerInput(inputId = ns("trxdates"),placeholder = Sys.Date(),multiple = T,label = "Period",
                                    range = TRUE,maxDate = Sys.Date(),clearButton = T),
                 col_5(actionBttn(ns("submit_trx"),"Submit",icon = icon("send"),style = "material-flat",size = "xs",color = "success")),
                 col_7(uiOutput(ns('reports_buttn')))
          ),
          column(width = 9,uiOutput(ns("infoboxes")))
        ))
    })

    observeEvent(input$submit_trx,{

      progressSweetAlert(session = session, id = "trx_monitoring",title = "Start Transaction monitoring",display_pct = TRUE, value = 0)
      cbs_creds = tbl(con,"CBS_Creds") %>% as_tibble()
      db_con = maisha_conn(cbs_creds$username,enc_dec(cbs_creds$password,'dec'))
      updateProgressBar(session = session,id = "trx_monitoring",value = 20,status = "success",title = "Reading Transactions Listing")
      large_trx_data = large_trx(db_con)
      updateProgressBar(session = session,id = "trx_monitoring",value = 40,status = "success",title = "Generating daily aggregates")
      dailytrx_monitoring = maisha_dailytrx_monitor(large_trx_data,valuevar = "NumerOfTransactions",trxamount_flag = 10,day = Sys.Date())
      updateProgressBar(session = session,id = "trx_monitoring",value = 80,status = "success",title = "Checking Daily transactions anomalies")
      dailytrx_monitoring2 = maisha_dailytrx_monitor(large_trx_data,valuevar = "Amount",trxamount_flag = 10,day = Sys.Date())
      updateProgressBar(session = session,id = "trx_monitoring",value = 90,status = "success",title = "Generating weekly aggregates")
      weeklytrx_monitoring = list(Count = maisha_weeklytrx_monitor(trxData = large_trx_data,"NumerOfTransactions"),
                                  Amount = maisha_weeklytrx_monitor(trxData = large_trx_data,"Amount"))
      valueboxfigs = valueboxes(trxData = large_trx_data)
      trx_report <- list(
        valueboxfigs=valueboxfigs,
        dailytrx_monitoring = dailytrx_monitoring,
        dailytrx_monitoring2 = dailytrx_monitoring2,
        weeklytrx_monitoring = weeklytrx_monitoring
      )
      save(trx_report,file = "inst/temp/trx_monitoring.Rdata")
      updateProgressBar(session = session,id = "trx_monitoring",value = 100,status = "success",title = "Done")
      closeSweetAlert(session = session)
      sendSweetAlert(session = session,title = "Done",type = "success")
      # render info boxes here =====
      output$infoboxes <- renderUI({
        tagList(
          column(4,infoBox(width = 12,value = format(valueboxfigs$TotalCredit,big.mark = ","),title  = "CREDIT",icon = icon("coins"))),
          column(4,infoBox(width = 12, value = format(valueboxfigs$TotalDebit,big.mark = ","),title  = "DEBIT",icon = icon("coins"))),
          column(4,infoBox(width = 12,value  = valueboxfigs$Totatrans,title  = "Trx Counts",icon = icon("bank")))
        )
      })
      # render flagging output here ====
      output$flagoutput <- renderUI({
        sliderInput(inputId = ns("totaltrxs"),label = "Total Transactions",
                    min = min(dailytrx_monitoring$transactions$Total,na.rm = T),
                    max = max(dailytrx_monitoring$transactions$Total,na.rm = T),
                    value = c(min(dailytrx_monitoring$transactions$Total)+10,
                              max(dailytrx_monitoring$transactions$Total)),step = 1
                    )
      })
      # render transactions counts here =====
      output$trx_counts1 <- DT::renderDataTable({
        min = ifelse(is.null(input$totaltrxs) ,0,min(input$totaltrxs))
        max = ifelse(is.null(input$totaltrxs) ,0,max(input$totaltrxs))
        dailytrx_monitoring$transactions %>%
          filter(Total >= min ,Total<= max) %>%
          datatable(extensions = "Buttons",options = list(dom = "Bfrtip",buttons = "excel",scrollY = "250px"))

      })
      # render transactions amounts here =====
      output$trx_amounts_ <- DT::renderDataTable({
        dailytrx_monitoring2$transactions %>%
          filter(TD >= ifelse(is.null(input$TD) ,0,as.numeric(input$TD))|
                   TC >= ifelse(is.null(input$TC) ,0,as.numeric(input$TC)) |
                   CC >= ifelse(is.null(input$CC) ,0,as.numeric(input$CC)) |
                   CD >= ifelse(is.null(input$CD) ,0,as.numeric(input$CD)) ) %>%
          datatable(extensions = "Buttons",
                    options = list(dom = "Bfrtip",buttons = "excel",scrollY = "250px"))
      })
      # render cumulative counts here ====
      output$trx_weekly <- DT::renderDataTable({
        produt = input$mode
        pluck(weeklytrx_monitoring,produt) %>%
          datatable(extensions = "Buttons",options = list(dom = "Bfrtip",buttons = "excel",scrollY = "250px"))


      })
      # insert reports button
      output$reports_buttn <- renderUI({
        actionBttn(ns("pullreports"),icon = icon("download"),style = "material-flat",
                   size = "xs",color = "warning",label = "Download")
      })
      })
    # start downloading =====
    observeEvent(input$pullreports,{
      showModal(
        modalDialog(title = "Report type",size = "s",easyClose = T,footer = HTML("&copy; Maisha Bank"),
                    fluidRow(
                      column(width = 12,align="center",
                             downloadBttn(ns("excel_download"),"Create Excel Report",style = "material-flat",size = "xs"))
                    ))
      )

    })
    output$excel_download <- downloadHandler(

      filename = function() {
        paste('Trx_Monitoring_', format(input$trxdates[2],"%d.%m.%Y"), '.xlsx', sep='')
      },
      content <- function(file) {
        shiny::withProgress(
          message = "Generating Transaction Reports",value = 0,
          expr = {
            removeModal()
            start = ifelse(is.null(input$trxdates[1]),as.character(Sys.Date()),as.character(input$trxdates[1]))
            stop = ifelse(is.null(input$trxdates[2]),as.character(Sys.Date()),as.character(input$trxdates[2]))
            load("inst/temp/trx_monitoring.Rdata")
            dailytrx_monitoring = trx_report$dailytrx_monitoring
            dailytrx_monitoring2 = trx_report$dailytrx_monitoring2
            weeklytrx_monitoring = trx_report$weeklytrx_monitoring


            wb = createWorkbook()
            addWorksheet(wb,"TrxCounts_all")
            addWorksheet(wb,"TrxAmount_ll")
            addWorksheet(wb,"TrxCount_check")
            addWorksheet(wb,"TrxAmount_Check")
            addWorksheet(wb,"Cumulative",gridLines = F,zoom = 80)
            writeDataTable(wb,1,dailytrx_monitoring$transactions)
            writeDataTable(wb,2,dailytrx_monitoring2$transactions)
            writeDataTable(wb,3,dailytrx_monitoring$transactions_check)
            writeDataTable(wb,4,dailytrx_monitoring2$transactions_check)
            # writeDataTable(wb,5,report)
            writeData(wb,5,x = sprintf("Cumulative Report From %s to %s",start,stop),xy = c(7,1))
            writeData(wb,5,x = sprintf("TRANSACTION COUNTS"),xy = c(2,2))
            writeData(wb,5,x = sprintf("TRANSACTION AMOUNTS"),xy = c(11,2))
            writeDataTable(wb,5,x = weeklytrx_monitoring$Count,xy = c(1,3))
            writeDataTable(wb,5,x = weeklytrx_monitoring$Amount,xy = c(10,3))
            saveWorkbook(wb, file,overwrite = T)
          }
        )
      }
    )
  })
}

## To be copied in the UI
# mod_trx_monitoring_ui("trx_monitoring_1")

## To be copied in the server
# mod_trx_monitoring_server("trx_monitoring_1")
