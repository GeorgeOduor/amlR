#' @name overdrawn_function
#' @import tidyr
#' @import dplyr
overdrawn_function <- function(prev_depfile,active_depfile) {
    accounts = prev_depfile %>% full_join(active_depfile,by=c("ClientID"),all=T) %>%
        rename(Prev_ClearBalance = ClearBalance.x,Active_ClearBalance = ClearBalance.y)

    accounts_check <- accounts %>%
        mutate(AccountsCheck = ifelse(is.na(Prev_ClearBalance),"New Negative Account",
                                      ifelse(abs(Active_ClearBalance) - abs(Prev_ClearBalance)< 0 | is.na(Active_ClearBalance),"New Deposit",
                                             ifelse(abs(Active_ClearBalance) - abs(Prev_ClearBalance)> 0,"Overdrawn With Changes","No Change"))),
               AccountName = ifelse(is.na(AccountName.y),as.character(AccountName.x),as.character(AccountName.y)),
               OpenedDate = ifelse(is.na(OpenedDate.y),as.character(OpenedDate.x),as.character(OpenedDate.y)),
               AccountID = ifelse(is.na(AccountID.y),as.character(AccountID.x),as.character(AccountID.y)),
               ProductName = ifelse(is.na(ProductName.y),as.character(ProductName.x),as.character(ProductName.y))
        ) %>%
        select( ClientID,AccountID,ProductName,OpenedDate,Prev_ClearBalance,Active_ClearBalance,AccountName,OpenedDate,AccountsCheck) %>%
        split(.$AccountsCheck)

    return(accounts_check)
}
#' @name overdrawn_report
#' @import tidyr
#' @import dplyr
overdrawn_report <- function(prev_depfile,active_depfile) {


    # asign files
    prev_depfile  = prev_depfile %>%
        select(ClientID,AccountID,ClearBalance,AccountName,OpenedDate,ProductName) %>%
        filter(ClearBalance < 0) %>% mutate(ClientID = as.numeric(ClientID),AccountID = as.character(AccountID))
    active_depfile  =  active_depfile%>%
        select(AccountID,AccountName,ProductName,ClientID,ClearBalance,OpenedDate) %>%
        filter(ClearBalance < 0)%>% mutate(ClientID = as.numeric(ClientID),AccountID = as.character(AccountID))
    # program starts here ==================
    overdrawn_reports <- overdrawn_function(prev_depfile = prev_depfile,active_depfile = active_depfile)
    out = list(Nochange = overdrawn_reports$`No Change`,
               Newdep = overdrawn_reports$`New Deposit`,
               Newneg = overdrawn_reports$`New Negative Account`)
    return(out)
}



#' @name  overdrawn_function_app
#'
#' @import DBI

overdrawn_function_app <- function(session,con1=NULL,con2=NULL,final=F) {
    progressSweetAlert(session = session, id = "overdrawn_accs",title = "Start Overdrawn Accounts monitoring",display_pct = TRUE, value = 0)
    updateProgressBar(session = session,id = "overdrawn_accs",value = 0,status = "success",title = "Reading Previous Deposits Listing")
    prev_depfile = tbl(con1,"Deposits") %>% collect()
    updateProgressBar(session = session,id = "overdrawn_accs",value = 40,status = "success",title = "Reading Active Deposits Listing")
    active_depfile = dbSendQuery(con2,sprintf("EXECUTE rw_ACBalance @ReportDate='%s',@SkipZero='false',@Branches ='000,001',@DepositProducts ='MSS902,MFF804,MFS803,MFT801,MA01,MA02,MA03,MA04,MA05,MB01,MB02,MB03,MC01,MC02,MF01,MF02,MR01,MR02,MS01,MAMA01,MFS903,MFS902,MTS901'",format(Sys.Date(),"%d %b %Y"))) %>%
        dbFetch()
    updateProgressBar(session = session,id = "overdrawn_accs",value = 80,status = "success",title = "Checking Overdrawn Accounts")

    out = overdrawn_report(prev_depfile,active_depfile)

    # accounts = prev_depfile %>%
    #     full_join(active_depfile,by=c("ClientID"),all=T) %>%
    #     rename(Prev_ClearBalance = ClearBalance.x,Active_ClearBalance = ClearBalance.y)
    # accounts_check <- accounts %>%
    #     mutate(AccountsCheck = ifelse(is.na(Prev_ClearBalance),"New Negative Account",
    #                                   ifelse(abs(Active_ClearBalance) - abs(Prev_ClearBalance)< 0 | is.na(Active_ClearBalance),"New Deposit",
    #                                          ifelse(abs(Active_ClearBalance) - abs(Prev_ClearBalance)> 0,"Overdrawn With Changes","No Change"))),
    #            AccountName = ifelse(is.na(AccountName.y),as.character(AccountName.x),as.character(AccountName.y)),
    #            OpenedDate = ifelse(is.na(OpenedDate.y),as.character(OpenedDate.x),as.character(OpenedDate.y)),
    #            AccountID = ifelse(is.na(AccountID.y),as.character(AccountID.x),as.character(AccountID.y)),
    #            ProductName = ifelse(is.na(ProductName.y),as.character(ProductName.x),as.character(ProductName.y))
    #     ) %>%
    #     select( ClientID,AccountID,ProductName,OpenedDate,Prev_ClearBalance,Active_ClearBalance,AccountName,OpenedDate,AccountsCheck) %>%
    #     split(.$AccountsCheck)
    # if report is marked as final then we will overwrite
    if (final) {
        new_deposit = active_depfile %>% select(ProductID,ProductName,AccountID,AccountName,ClientID,AccountStatus = `Account Status`,ClearBalance,OpenedDate,salesOfficerName) %>% mutate(ReportDate = as.character(Sys.Date()))
        dbWriteTable(conn = con1,name = "Deposits",value = new_deposit)
    }

    time = list(reportdate = Sys.Date()-1)
    updateProgressBar(session = session,id = "overdrawn_accs",value = 950,status = "success",title = "Pre saving reports")
#
# # save temp files=====
    save(out,file = "inst/temp/overdrawn_reports.Rdata")
    # save(time,file = "inst/temp/overdrawn_reports_check.Rdata")
    closeSweetAlert(session = session)
    sendSweetAlert(session = session,title = "Done",type = "success")
    return(out)

}

# overdrawn_function(prev_depfile,active_depfile)

overdrawn_UI <- function(ns,reports) {

    box(title = "Overdrawn Accounts",width=12,status="success",
        col_8(tabsetPanel(type = "pills",
                          tabPanel(title = "No Changes Observed",23,dataTableOutput(ns("accountactivity"))),
                          tabPanel(title = "Existing Overdrawn With changes",23,dataTableOutput(ns("overdrawn_1"))),
                          tabPanel(title = "New Overdrawn Accounts",23,dataTableOutput(ns("overdrawn_2")))
        )),
        col_4(tagList(
            descriptionBlock(number = paste('change',"%"),header = 'all_negative_accounts',text = "All Overdrawn Accounts"),
            descriptionBlock(number ="",header = format('newnegative_amount',big.mark = ","),text = "New Overdrawn Amount"),
            descriptionBlock(number ="",header = format('newnegative_count',big.mark = ","),text = "New Overdrawn Accounts")
        )))

}
