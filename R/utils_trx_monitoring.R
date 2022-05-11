#' trx_monitoring
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import DBI
#' @noRd
large_trx <- function(db_con,trxdates=NULL) {
    # start = ifelse(is.null(trxdates[1]),as.character(Sys.Date()),as.character(input$trxdates[1]))
    #
    # stop = ifelse(is.null(trxdates[2]),as.character(Sys.Date()),as.character(input$trxdates[2]))
    start = Sys.Date()-5
    stop = Sys.Date()

    largetrx_query = sprintf("EXECUTE rw_LargeTransactionsReport @FromDate='%s',@ToDate='%s',@FromAmount='-99999999',@Branches ='000,001',@Products ='001,MA01,MA02,MA03,MA04,MA05,MAMA01,MB01,MB02,MB03,MC01,MC02,MF01,MF02,MFC802,MFC805,MFC806,MFC807,MFF804,MFS803,MFS903,MFT801,ML101,ML102,ML103,ML104,ML105,ML106,ML107,ML108,ML109,ML110,ML111,ML113,ML114,ML115,ML116,ML117,ML118,ML119,ML121,ML122,MLS904,MR01,MR02,MS01,MSS902,MTS901'",
                             format(as.Date(start),"%d/%b/%Y"),
                             format(as.Date(stop),"%d/%b/%Y"))

    largeTransactions = dbGetQuery(db_con,largetrx_query) %>% mutate_all(as.character())

    return(largeTransactions)
}

#' maisha_dailytrx_monitor
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
maisha_dailytrx_monitor <- function(trxData,valuevar,trxamount_flag,trx_countflag,day) {
    PastDays= as.numeric(Sys.Date()-as.Date(day))
    start = Sys.time()
    message("Generating daily aggregates")
    transactions = trxData %>%select(-1) %>%
        filter(`TrxDate` <= Sys.Date()-PastDays) %>%
        group_by(`AccountID`, `TrxTypeID`) %>%
        mutate(AmountTrans = as.numeric(Debit)+as.numeric(Credit)) %>%
        summarise(NumerOfTransactions = n(),Amount = sum(AmountTrans)) %>%
        arrange(desc(NumerOfTransactions)) %>%
        reshape2::dcast(.,`AccountID`~`TrxTypeID`,value.var = valuevar) %>%
        mutate(Name = trxData$`AccountName`[match(`AccountID`,trxData$`AccountID`)]) %>%
        select(Name,everything()) %>%
        mutate_all(.funs = function(x)ifelse(is.na(x),0,x)) %>%
        janitor::adorn_totals("col")
    # accounts check
    message("Checking transactions anomalies")
    if (valuevar == "Amount") {
        transactions_check <- transactions %>% filter(TD >= as.numeric(trxamount_flag)|TC >= as.numeric(trxamount_flag) | CC>=as.numeric(trxamount_flag) | CD >= as.numeric(trxamount_flag) )

    }else{
        transactions_check = transactions %>% filter(Total >= 10)
    }

    output = list("transactions" = transactions,"transactions_check"=transactions_check)


    return(output)


}
#' maisha_weeklytrx_monitor
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
maisha_weeklytrx_monitor <- function(trxData,valuevar){
    trasndata = trxData  %>%
        group_by(`AccountID`, `TrxTypeID`) %>%
        mutate(AmountTrans = as.numeric(Debit)+as.numeric(Credit)) %>%
        summarise(NumerOfTransactions = n(),Amount = sum(AmountTrans)) %>%
        reshape2::dcast(.,`AccountID`~`TrxTypeID`,value.var = valuevar) %>%
        mutate(Name = trxData$`AccountName`[match(`AccountID`,trxData$`AccountID`)]) %>%
        select(Name,everything()) %>%
        mutate_all(.funs = function(x)ifelse(is.na(x),0,x)) %>%
        janitor::adorn_totals("col") %>% filter(Total >= 20)%>%
        arrange(desc(Total))



    if (valuevar == "Amount") {
        trasndata = trasndata %>% filter(Total >= 1000000)
    }



    return(trasndata)
}
#' valueboxes
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
valueboxes = function(trxData){
    summary_ = trxData %>% summarise(
        TotalCredit = format(sum(as.numeric(Credit),na.rm = T),big.mark=","),
        TotalDebit = format(sum(as.numeric(Debit),na.rm = T),big.mark=","),
        Totatrans = format(n(),big.mark=",")
    )
}
#' valueboxes
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

