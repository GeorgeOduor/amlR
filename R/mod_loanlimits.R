#' loanlimits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shinyWidgets
#' @import highcharter
#' @importFrom shiny NS tagList
mod_loanlimits_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      uiOutput(ns('controls1'))
    ),
    fluidRow(
      box(title = "Disbursements and Limits Reporting",width = 12,status='success',
          tabsetPanel(type = "pills",
                      tabPanel(title = "Disbursement Listing",
                               DT::dataTableOutput(ns("disbursement_listing_day"))),
                      tabPanel(title = "Limits & Multiple Loans",
                               tagList(
                                 column(6,
                                        textOutput(ns("limitsreport")),
                                        tableOutput(ns("abovelimtable"))),
                                 column(6,
                                        selectInput(ns('multi_cat'),"Duplicates By:",
                                                    choices = c("IDs" ,"Phone Number" ,"Account Number")),
                                        tableOutput(ns("multiloans"))
                                        # tabsetPanel(type = "pills",
                                        #             tabPanel(title = "IDs" , tableOutput(ns("dupsbyID"))),
                                        #             tabPanel(title = "Phone Number" , tableOutput(ns("phone_duplicates"))),
                                        #             tabPanel(title = "Account Number" , tableOutput(ns("dupsbyAcc")))
                                        # )
                                 )
                               )),
                      tabPanel(title = "Disbursements Summary",
                               column(2,
                                      selectInput(ns("disb_period"),"Select report period",
                                                  choices =c('Day Disbursements','Last 7 Days','This Month') )),
                               column(4, tableOutput(ns("disbursemet_sum_table"))),
                               column(6, highchartOutput(ns("daily_trend"), height = "420px"))),
          )
      )

    )
  )
}

#' loanlimits Server Functions
#' @import shinydashboard
#' @import highcharter
#' @import DT
#' @import purrr
#' @import kableExtra
#' @import readr
#' @import openxlsx
#' @import shiny
#' @noRd
mod_loanlimits_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$controls1 <- renderUI({
      tagList(

        panel(class = "input-panel",
              col_4(
                dateInput(ns("reportDate"),"Report Date")
              ),
              col_2(
                actionButton(ns('submit_data1'),"Submit",icon = icon('send'),class="start_button")
              ),
              col_2(
                actionButton(ns('update_limits'),"Update Limits",icon = icon('upload'),class="start_button")
              ),
              col_2(
                uiOutput(ns('report_writer'))
              ),
              col_2(
                uiOutput(ns('report_sharer'))
              )
        )

      )
    })
    observeEvent(input$submit_data1,{
      tryCatch(
      expr = {
        cbs_creds = tbl(con,"CBS_Creds") %>% as_tibble()
        cbs = maisha_conn(cbs_creds$username,enc_dec(cbs_creds$password,'dec'))
        reports <-  loanlimits(session=session,con=con,dates = input$reportDate,cbs=cbs)
        # print(reports)

        # render disbursement listing=======
        output$disbursement_listing_day <- DT::renderDT({
          datatable(data = reports %>% pluck('disbursements'),
                    options = list(scrollY=T,paging = TRUE, searching = TRUE, info = FALSE,height="300",
                                   sort = TRUE, scrollX = TRUE,scrollY=T, fixedColumns = list(rightColumns = 2)))
        })
        # render above limits listing=======
        output$limitsreport <- renderText({
          above = nrow(reports$limits_check)
          if (above > 0) {
            paste("There are ",above," cases above limit.")
          }else{
            "No case above limit"
          }
        })
        output$abovelimtable <- function(){
          reports$limits_check %>%
            kable() %>%
            kable_styling(full_width = F) %>%
            scroll_box(height = "300px",width = "100%",fixed_thead = T)
        }
        # multiple loans check ====
        output$multiloans <- function(){
          multiple_data <- switch (input$multi_cat,
                                   'IDs' = reports$duplicates_summary$idnumber,
                                   "Phone Number" = reports$duplicates_summary$mobile,
                                   "Account Number" = reports$duplicates_summary$accountid
          )
          multiple_data %>%
            kable(caption = paste("Duplicates By:",input$multi_cat,";",nrow(multiple_data),"cases.")) %>%
            kable_styling(full_width = F) %>%
            scroll_box(height = "240px",width = "100%",fixed_thead = T)
        }
        # disbursements summary====
        output$disbursemet_sum_table <- function(){
          disb_summary = switch (input$disb_period,
                                 'Day Disbursements' = {reports$disb_summ$day_disb},
                                 'Last 7 Days' = {reports$disb_summ$week_disb},
                                 'This Month' = {reports$disb_summ$month_disb}
          )

          disb_summary %>%
            kable() %>%
            kable_styling(full_width = F)%>%
            collapse_rows(columns = 1) %>%
            scroll_box(width = "100%",fixed_thead = T)
        }

        output$daily_trend = renderHighchart({
          chart = switch (input$disb_period,
                          'Day Disbursements' = {reports$disb_summ$day_disb },
                          'Last 7 Days' = {reports$disb_summ$week_disb },
                          'This Month' = {reports$disb_summ$month_disb}
          ) %>% filter(!grepl('Total',Channel)) %>%
            mutate(Amount=parse_number(Amount)) %>%
            hchart('column',hcaes(x='Product',y = "Amount")) %>%
            hc_add_theme(hc_theme_ffx())
          chart
        })

        # show report writer button====
        output$report_writer <- renderUI({
          downloadButton(ns('write_reports'),"Save Reports",class="start_button")

        })
        # show report share button====
        output$report_sharer <- renderUI({
          actionButton(ns('share_report_button'),"Share",class="start_button")

        })

      },
        error = function(e){
          shinyalert::shinyalert(title = "Error",text = paste(e),type = 'error')
        }
      )
    })
# share reports logic here =====
    observeEvent(input$share_report_button,{
      output$message_body <- renderUI({
        htmlTemplate("inst/temp/email2.html")
      })
      showModal(ui = modalDialog(title = 'Dibursement Monitoring Report',
                                 report_sharer(ns),
                                 footer = HTML("&copy; Maisha Bank 2022"),
                                 size = 'l',easyClose = T),
                )
    })
    # download reports====
    output$write_reports <- downloadHandler(
      filename = function() {
        paste("DisbursementReport-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        load("inst/temp/limits_report.Rdata")
        data <- list(
          LimitsCheck = reports$limits_check,
          MultipleLoans = reports$duplicates_summary %>% bind_rows(),
          DayDisbursement =  reports$disb_summ$day_disb,
          MonthDisbursement =  reports$disb_summ$month_disb
        )
        write.xlsx(data, file)
      })

    # send email=====
    observeEvent(input$send_mail,{
      req(input$to)
      email_creds = tbl(con,"EMAIL_Creds") %>% as_tibble()
      credentials = list(
        password = enc_dec(email_creds$password,dir = 'dec'),
        username = email_creds$username
      )
      files = input$atachments$filepath
      push_email(to = input$to,
                 cc = NULL,
                 from = credentials$username,
                 creds = credentials,subject = "DISBURSEMENT MONITORING REPORT",
                 body = "inst/temp/email2.html",attachments = files)
      shinyjs::refresh()
    })
  })
}

## To be copied in the UI
# mod_loanlimits_ui("loanlimits_1")

## To be copied in the server
# mod_loanlimits_server("loanlimits_1")
