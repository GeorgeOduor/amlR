#' over_drawn_accs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydashboardPlus
mod_over_drawn_accs_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(uiOutput(ns('controlers'))),
    fluidRow(
      box(title = "Overdrawn Accounts",width=12,status="success",
          col_12(tabsetPanel(type = "pills",
                            tabPanel(title = "No Changes Observed",DT::dataTableOutput(ns("accountactivity"))),
                            tabPanel(title = "Existing Overdrawn With changes",DT::dataTableOutput(ns("overdrawn_1"))),
                            tabPanel(title = "New Overdrawn Accounts",DT::dataTableOutput(ns("overdrawn_2")))
          )))
      # shinycssloaders::withSpinner(uiOutput(ns('overdrawnreport')))
    ),
  )


}

#' over_drawn_accs Server Functions
#' @import shinydashboardPlus
#' @noRd
mod_over_drawn_accs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    if (file.exists("inst/temp/overdrawn_reports_check.Rdata") ) {
      load("inst/temp/overdrawn_reports_check.Rdata")
      if (time$reportdate != Sys.Date()) {
        output$controlers <- renderUI({
          panel(
            # column(3,airDatepickerInput(inputId = ns("reportingdate1"),label = "Reporting Date",placeholder = Sys.Date()-2,
            #                             value = Sys.Date()-1,maxDate = Sys.Date()-1)),
            tagList(
              column(3,br(),aling="center",actionBttn(ns("check_overdrawn"),"Check Overdrawn Accounts",block=T,icon = icon("send"),style = "material-flat",size = "xs",color = "success")),
              column(3,br(),aling="center",uiOutput(ns("downloadbutton")))
            )
          )
        })
      }else{
        output$controlers <- renderUI({
          panel(tagList(
            column(3,h3('Reports are up to Date')),
            column(3,br(),aling="center",actionBttn(ns("view_overdrawn"),"View",icon = icon("send"),style = "material-flat",size = "xs",color = "success")),
            column(3,br(),aling="center",uiOutput(ns("downloadbutton")))
          ))
        })
      }
    }

    observeEvent(input$check_overdrawn,{
      cbs_creds = tbl(con,"CBS_Creds") %>% as_tibble()
      cbscon = maisha_conn(cbs_creds$username,enc_dec(cbs_creds$password,"dec"))
      out = overdrawn_function_app(session,con1=con,con2=cbscon,final=F)
      newnegative_count = nrow(out$Newneg)
      newnegative_amount = abs(out$Newneg$Active_ClearBalance) %>% sum()
      all_negative_accounts = out %>% bind_rows() %>% nrow()
      change = round(newnegative_count/all_negative_accounts*100,2)
      newnegative_amount = abs(out$Newneg$Active_ClearBalance) %>% sum()
      output$accountactivity <- DT::renderDataTable({
        out$Nochange %>% datatable(options = list(scrollY = 300,scroller = T))
      })
      output$overdrawn_1 <- DT::renderDataTable({
        out$Newdep %>% datatable(options = list(scrollY = 300,scroller = T))
      })
      output$overdrawn_2 <- DT::renderDataTable({
        out$Newneg %>% datatable(options = list(scrollY = 300,scroller = T))
      })
      output$downloadbutton <- renderUI({
        actionBttn(ns("download_overdrawn"),label = "Download Overdrawn reports",style = "material-flat",size = "xs",color = "success",icon = icon("download"))
      })
    })
    observeEvent(input$view_overdrawn,{

      load("inst/temp/overdrawn_reports.Rdata")
      newnegative_count = nrow(out$Newneg)
      newnegative_amount = abs(out$Newneg$Active_ClearBalance) %>% sum()
      all_negative_accounts = out %>% bind_rows() %>% nrow()
      change = round(newnegative_count/all_negative_accounts*100,2)
      newnegative_amount = abs(out$Newneg$Active_ClearBalance) %>% sum()

      output$accountactivity <- DT::renderDataTable({
        out$Nochange %>% datatable(options = list(scrollY = 300,scroller = T))
      })
      output$overdrawn_1 <- DT::renderDataTable({
        out$Newdep %>% datatable(options = list(scrollY = 300,scroller = T))
      })
      output$overdrawn_2 <- DT::renderDataTable({
        out$Newneg %>% datatable(options = list(scrollY = 300,scroller = T))
      })
      output$downloadbutton <- renderUI({
        actionBttn(ns("download_overdrawn"),label = "Download Overdrawn reports",style = "material-flat",size = "xs",color = "success",icon = icon("download"))
      })
    })
    observeEvent(input$download_overdrawn,{
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
        paste('Overdrawn_reports_', format(as.Date(input$reportingdate1),"%d.%m.%Y"), '.xlsx', sep='')
      },
      content <- function(file) {
        shiny::withProgress(
          message = "Generating Overdrawn Reports",value = 0,
          expr = {
            removeModal()
            load("inst/temp/overdrawn_reports.Rdata")

            wb = createWorkbook()
            # create worksheets
            addWorksheet(wb,sheetName = "No Change Observed",gridLines = F)
            addWorksheet(wb,sheetName = "With changes",gridLines = F)
            addWorksheet(wb,sheetName = "New Negative Accounts",gridLines = F)
            writeDataTable(wb,1,x = as.data.frame(head(out$Nochange,100)),tableStyle = "TableStyleMedium21",withFilter = F)
            writeDataTable(wb,2,x = out$Newdep,tableStyle = "TableStyleMedium21",withFilter = F)
            writeDataTable(wb,3,x = out$Newneg,tableStyle = "TableStyleMedium21",withFilter = F)
            saveWorkbook(wb, file,overwrite = T)
          }
        )
      }
    )

  })
}

## To be copied in the UI
# mod_over_drawn_accs_ui("over_drawn_accs_1")

## To be copied in the server
# mod_over_drawn_accs_server("over_drawn_accs_1")
