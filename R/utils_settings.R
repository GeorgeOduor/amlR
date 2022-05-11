#' settings
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import shinyWidgets
email_settings_ui <- function(ns) {
    fluidRow(
        col_12(tagList(
            textInputIcon(ns('email'),'EmailAdress',size = 'sm',icon = icon('at')),
            passwordInput(ns('password_email'),'Email Password'),
            # col_6(actionButton(ns('test_pass'),"Test",width = '100%',class= 'start_button')),
            col_6(actionButton(ns('save'),"Save",width = '100%',class= 'start_button'))
        ))
    )
}
#' settings
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import shinyWidgets
cbs_con_ui <- function(ns){
    fluidRow(
        col_12(
            tagList(
                textInputIcon(ns('username'),'CBD USER',size = 'sm',icon = icon('at')),
                passwordInput(ns('password_cbs'),'CBS DB password'),
                # col_6(actionButton(ns('test_pass_cbs'),"Test",width = '100%',class= 'start_button')),
                col_6(actionButton(ns('save_cbs'),"Save",width = '100%',class= 'start_button'))
            )
        )
    )
}
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import shinyWidgets
bdo_listing_ui <- function(ns){
    tagList(
        fluidRow(
            col_12(
                actionButton(ns('new_bdo'),"Add New",width = '97%',class= 'start_button',icon = icon('plus'))
                )
        ),
        hr(),
        br(),
        fluidRow(
            col_12(
                DT::dataTableOutput(ns('bdos_listing'))
            )
        )
    )
}

#' @description Edit bdo details
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import shinyWidgets
bdo_listing_ui1 <- function(ns,type="edit",...){
    kwargs = list(...)
    fluidRow(
        textInput(ns('product'),label = "BDO/Product Name",value = kwargs$product),
        textInput(ns('channel'),label = "Channel",value = kwargs$channel),
        if (type=='edit') {
            actionButton(ns('edit_bdo'),"Edit")
        }else{
            actionButton(ns('new_bdo_submit'),"Submit")
        }
    )
}

#' @import safer
#' @name enc_dec
#' @title  enc_dec
enc_dec <- function(text,dir = 'enc') {
    out = switch (dir,
        'enc' = safer::encrypt_string(string = text,key = "george"),
        'dec' = safer::decrypt_string(string = text,key = "george")
    )
    return(out)
}

