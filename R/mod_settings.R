#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinydashboard
mod_settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 3,title='Settings',status='success',
        selectInput(ns('setting_module'),'Module',c('CBS Connection','Email','BDO Listing'))),
    box(width = 9,title='Settings',status='success',
        uiOutput(ns('setting_module_ui')))
  )
}

#' settings Server Functions
#' @import DBI
#' @noRd
mod_settings_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  output$setting_module_ui <- renderUI({
    switch (input$setting_module,
      'CBS Connection' = cbs_con_ui(ns),
      'Email' = email_settings_ui(ns),
      'BDO Listing' = bdo_listing_ui(ns)
    )
  })

  # save cbs password here =====
  observeEvent(input$save_cbs,{
    req(input$username,input$password_cbs)
    if (input$save_cbs < 3) {
      tryCatch(
        expr = {
          username = input$username
          pass = input$password_cbs
          maisha_conn(username,pass)
          dbWriteTable(con,"CBS_Creds",tibble(username = username,password = enc_dec(pass)),overwrite=T)
          shinyalert::shinyalert(title = "Success",text = "Username and password combination is correct!",type = 'success')
        },
        error = function(e){
          shinyalert::shinyalert(title = "Error",text = "Username and password combination is NOT correct!Confirm with ICT department the correct credentials or check your connection to LAN.",type = 'error')
        },
        warning = function(w){

        },
        finally = {

        }
      )
    }else{
      shinyalert::shinyalert(title = "STOP",text = "Maximum trials exceeded!Confirm with IT the correct credentials",type = 'error')

    }
  })
  # save emailpass ====
  observeEvent(input$save,{
    req(input$email,input$password_email)
    tryCatch(
      expr = {
        credentials=list(username = input$email,password = input$password_email)
        shinyWidgets::sendSweetAlert(session = session,title = "Hold On",text = "Please wait,Checking email credentials",type = "info")
        push_email(to = credentials$username,
                   cc = NULL,
                   from = credentials$username,
                   creds = credentials,subject = "Test",
                   body =HTML("<p>This is a test email from AML app.It's a confirmation that you entered correct email and password combination.</p>"))
        shinyWidgets::closeSweetAlert()
        dbWriteTable(con,"EMAIL_Creds",tibble(username = credentials$username,
                                              password = enc_dec(credentials$password)),overwrite=T)
        shinyalert::shinyalert(title = "Success",text = "Great!Username and password combination is correct!",type = 'success')
      },
      error = function(e){
        shinyWidgets::closeSweetAlert()
        shinyalert::shinyalert(title = "Error",text = "Username and password combination is NOT correct!",type = 'error')
        # shinyalert::shinyalert(title = "Error",text = paste(e),type = 'error')
      },
      warning = function(w){

      },
      finally = {

      }
    )

  })
  # render BDO ====
  bdos <- reactiveVal(
    tbl(con,"BDODetails") %>%
      as_tibble() %>%
      bind_cols(
        tibble(Edit = shinyInput(FUN = actionButton,
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                 n = nrow(.),id = 'edit_bdo_',icon=icon("edit"),
                                 class="action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                 label = "",onclick = 'Shiny.setInputValue(\"settings_1-select_button\", this.id, {priority: \"event\"})'),
               Delete = shinyInput(FUN = actionButton,
                                   style="color: #fff; background-color: red; border-color: #2e6da4",
                                   n = nrow(.),id = 'delete_bdo_',icon = icon("trash"),
                                   class = "action-button bttn bttn-material-flat bttn-xs bttn-success bttn-no-outline",
                                   label = "",onclick = 'Shiny.setInputValue(\"settings_1-select_button_1\", this.id, {priority: \"event\"})')
        )
      )
  )
  output$bdos_listing <- DT::renderDT(
    bdos(),escape = F,selection = 'none'
  )
  # edit bdos ===
  observeEvent(input$select_button,{
    selectedRow_ <<-  readr::parse_number(input$select_button)
    product <<- bdos()[selectedRow_,'Product'] %>% pull('Product')
    channel = bdos()[selectedRow_,'Channel'] %>% pull('Channel')


    showModal(ui = modalDialog(size = "s",title = "Edit",easyClose = F,
                               footer = tagList(
                                 col_6(float="left",align="left","Maisha Bank"),
                                 col_6(float="right",align="right",modalButton(label = "Close",icon = icon("close")))
                               ),bdo_listing_ui1(ns,product = product,channel=channel)
    ))
  })
  observeEvent(input$edit,{
    req(input$product,input$channel)
    sql = sprintf("UPDATE BDODetails SET Product = '%s' ,Channel = '%s' WHERE Product = '%s'",input$product,input$channel,product)
    dbSendQuery(con,statement = sql)
    shinyalert::shinyalert(title = "Success",text = paste(stringr::str_to_title(input$hod),"Opdated Successfully."),
                           type = "success",timer = 3000)
    shinyjs::refresh()
  })
# delete bdos ==---
  observeEvent(input$select_button_1,{
    selectedRow_ <<-  readr::parse_number(input$select_button_1)
    product <<- bdos()[selectedRow_,'Product'] %>% pull('Product')
    shinyWidgets::confirmSweetAlert(session = session,
                                    inputId = ns("remove_bdo"),
                                    title = "Attention",
                                    text = list_to_p(
                                      list = list(paste("Are you sure you want to permanently remove ",product,"from the database?"),
                                                  "Please note that this step is irreversible.")
                                    ),
                                    btn_labels = c("No","Yes"),
                                    btn_colors = c("green","red"),
                                    closeOnClickOutside = T,html = T,type = 'warning')
  })
  observeEvent(input$remove_bdo,{
    product <- bdos()[selectedRow_,'Product'] %>% pull('Product')
    action_sql = sprintf("delete from BDODetails WHERE Product = '%s'",product)
    if (input$remove_bdo) {
      DBI::dbSendQuery(con,statement = action_sql)
      shinyalert::shinyalert(title = 'Done',text = paste(product,' was deleted successfully.'),type = 'success')
      shinyjs::refresh()
    }

  })
  # new bdo ====
  observeEvent(input$new_bdo,{
    showModal(modalDialog(size = 's',title = 'NEW BDO /Product',footer = 'Maisha Bank',
                          bdo_listing_ui1(ns,"new")))
  })
  observeEvent(input$new_bdo_submit,{
    req(input$product,input$channel)
    tryCatch(
    expr = {
      new_bdo = tibble(Product = input$product,
                             Channel = input$channel)
    dbWriteTable(con,"BDODetails",new_bdo,append=T)
    shinyalert::shinyalert(title = 'Done',type = 'success')
    shinyjs::refresh()
    },
      error = function(e){
        shinyalert::shinyalert(title = 'Error',text=paste(e),type = 'error')

      },
      warning = function(w){

      },
      finally = {

      }
    )

  })
  })
}

## To be copied in the UI
# mod_settings_ui("settings_1")

## To be copied in the server
# mod_settings_server("settings_1")
