#' report_sharer
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import shinyWidgets
#'
#' @noRd
report_sharer <- function(ns) {
    ui = tagList(
        fluidRow(
            col_2(
                actionButton(ns('send_mail'),'Send',icon = icon('send'),width = '100%',class = 'start_button')
            ),
            col_10(
                panel(
                    col_6(textInputIcon(ns('to'),"To",width = '100%',value = "management@maishabank.com",icon = icon('user'),size = 'sm')),
                    col_6(textInputIcon(ns('cc'),"Cc",width = '100%',size = 'sm',icon = icon('users'))),
                    fileInput(ns('attachments'),'Attachments',multiple = T,width = "100%"),
                ),
                hr(),
                wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 270px",
                          htmlTemplate(filename = "inst/temp/email2.html")
                          )
                )
        )
    )
    return(ui)
}

#' @import kableExtra
emailbody <- function(limits_analysis) {
    # load( paste0("files/limits_analysis_",format(Sys.Date()-1,'%d.%m.%Y'),".Rdata"))
    # load("inst/temp/limits_report.Rdata")
    # limits_analysis = reports
    abovelimit = limits_analysis$limits_check %>%
        dplyr::filter(as.Date(DisbursedOn) == max(as.Date(DisbursedOn)))
    duplicates =   limits_analysis$duplicates_summary %>% bind_rows()
    cummulativedisb = limits_analysis$disb_summ$month_disb
    daydisb = limits_analysis$disb_summ$day_disb
    markers1 = grep("Total",daydisb$Channel)
    markers2 = grep("Total",cummulativedisb$Channel)

    HTML( paste0("<p>Please find attached Loan limits and multiple loans analysis.</p>
                  <h3><u><b>Insights</b></u></h3>",
                 "<h4>Loan limits.</h4>",
                 if (nrow(abovelimit) > 0) {
                     paste("<p>There ", ifelse(nrow(abovelimit)>1,
                                               paste(" were ",nrow(abovelimit),"disbursments above limit as at ",Sys.Date()-1,"</p><p><u></u></p>"),
                                               paste(" was ",nrow(abovelimit),"disbursment above limit as at ",Sys.Date()-1,"</p><p><u></u></p>" ))

                     )


                 }else{
                     paste("<p>There were no disbursments records above limit on ",Sys.Date()-1,"</p>")
                 },
                 "<h4>Duplicates</h4>",
                 if (sum(nrow(duplicates)) > 0) {
                     paste0("<p> There were a total of ",sum(nrow(duplicates)), " duplicate disbursment.Details in the attachment.</p>"                     )
                 }else{
                     paste("<p>There were no duplicated disbursment records by National IDs,Phone Numbers or Account IDs</p>")
                 },
                 "<h4><b>Cumulative Disbursements as at ",Sys.Date()-1,"<h4></b>",
                 # pipelines()$cumulative %>% htmlTable::htmlTable(caption = " Cummulative Disbursements",total= T),
                 cummulativedisb %>%  kable() %>% kable_styling(font_size = 11) %>% collapse_rows(1) %>%
                     column_spec(column = 1:2,border_right = T,color = "black") %>%
                     row_spec(c(0,markers2),bold = T,underline = T,background = "lightgray",italic = T),
                 "<h4><b>Daily Disbursements for ",Sys.Date()-1,"<h4></b>",
                 # pipelines()$daily %>% htmlTable::htmlTable(caption = "Daily Disbursements",total= T),
                 daydisb %>% kable() %>% kable_styling(font_size = 11) %>% collapse_rows(1) %>%
                     column_spec(column = 1:2,border_right = T,color = "black") %>%
                     row_spec(c(0,markers1),bold = T,underline = T,background = "lightgray",italic = T)




    ))
}

#' @import emayili
#' @import glue

push_email <- function(to,cc,from,creds,subject,body,test=T,
                       attachments = NULL) {
    msg <- envelope(to = to,
                    from = from,
                    cc = cc,
                    subject = subject,
                    html = body)

    if (!is.null(attachments)) {
        for (i in attachments) {
            msg = attachment(msg = msg,path = i)
        }
    }
    smtp <- server(
        host = "smtp-mail.outlook.com",
        port = 587,
        username = creds$username,
        password = creds$password
    )

    if (test) {
        smtp(msg, verbose = T)
    }else{
        "Done"
    }
}



