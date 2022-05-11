#' @import DBI
#' @import dplyr

client_transactions <- function(session,con,start_date,end_date) {
  progressSweetAlert(session = session, id = "client_monitoring",title = "Start Clients monitoring",display_pct = TRUE, value = 0)
  updateProgressBar(session = session,id = "client_monitoring",value = 10,status = "success",title = "Reading Large Transactions")
  largetrx_query = sprintf(
    "EXECUTE rw_LargeTransactionsReport @FromDate='%s',@ToDate='%s',@FromAmount='-99999999',@Branches ='000,001',
  @Products ='001,MA01,MS01,MA02,MA03,MA04,MA05,MAMA01,MB01,MB02,MB03,MC01,MC02,MF01,MF02,MFC802,MFC805,MFC806,MFC807,MFF804,MFS803,MFS903,MFT801,ML101,ML102,ML103,ML104,ML105,ML106,ML107,ML108,ML109,ML110,ML111,ML113,ML114,ML115,ML116,ML117,ML118,ML119,ML121,ML122,MLS904,MR01,MR02,MS01,MSS902,MTS901'",
    format(start_date, "%d/%b/%Y"),
    format(end_date, "%d %b %Y")
  )
  largeTransactions = dbGetQuery(con, largetrx_query)
  updateProgressBar(session = session,id = "client_monitoring",value = 70,status = "success",title = "Calculating daily lags")

  trx_long1 <- largeTransactions %>%
    arrange(TrxDate) %>%
    dplyr::filter(!OperatorID %in% c("SYS","SYS_FD")
                  ) %>%
    mutate(TransactionCategory = ifelse(OperatorID %in% c('ELMA'),"Mobile Banking",
                                        ifelse(OperatorID == "ONFON","Mfanisi Safaricom",
                                               ifelse(OperatorID == "ELMOBILE","Mfanisi Airtel","Branch")))) %>%
    select(Credit,Debit,ClientID,TrxDate,AccountName,AccountID,ProductID,OperatorID,TransactionCategory) %>%
    reshape2::melt(id.vars=c('TrxDate','AccountID','AccountName','ClientID','ProductID','OperatorID','TransactionCategory')) %>%
    mutate(variable = ifelse(variable == "Credit","Deposit","Withdrawal")) %>%
    dplyr::filter(value != 0) %>%
    dplyr::rename("TransactionType"="variable") %>%
    group_by(ClientID,AccountID,AccountName,TransactionCategory,TransactionType,TrxDate) %>%
    summarise(
      Count = n(),
      Amount = sum(value)
    )  %>%
    mutate(
      Lag_Count = ifelse(is.na(Count - lag(Count)),0,Count - lag(Count)),
      Lag_propotion_Count = ifelse(is.na(round(Count/(lag(Count)) * 100,2)),0,round(Lag_Count/(lag(Count)) * 100,2)),
      Lag_Amount = ifelse(is.na(Amount-lag(Amount)),0,Amount-lag(Amount)),
      Lag_propotion_Amount = ifelse(is.na(round(Lag_Amount/(lag(Amount)) * 100,2)),0,round(Lag_Amount/(lag(Amount)) * 100,2)),
      mean_count = mean(Lag_Count),
      mean_amount = mean(Lag_Amount),
      sd_count = ifelse(is.na(sd(Lag_Count)),0,sd(Lag_Count)),
      sd_amount = ifelse(is.na(sd(Lag_Amount)),0,sd(Lag_Amount)),
      ucl_count = mean_count + 1 * sd_count,
      lcl_count = mean_count - 1 * sd_count,
      ucl_amount = mean_amount + 1 * sd_amount,
      lcl_amount = mean_amount - 1 * sd_amount,
      CheckAmount1 = Lag_Amount > ucl_amount,
      Check_count = ifelse(Lag_Count > ucl_count,"High",
                           ifelse(Lag_Count < lcl_count,"Very Low","Normal")),
      Check_count = ifelse((lcl_count == 0 & ucl_count == 0 ),"Normal",Check_count),
      Check_amount = ifelse(Lag_Amount > ucl_amount,"High",
                            ifelse(Lag_Amount < lcl_amount,"Very Low","Normal")),
      Check_amount = ifelse((lcl_amount == 0 & ucl_amount == 0 ),"Normal",Check_amount)
    ) #%>% select(contains('amount'))

  trx_long = trx_long1 %>% dplyr::filter(as.Date(TrxDate) == max(TrxDate))
  updateProgressBar(session = session,id = "client_monitoring",value = 90,status = "success",title = "Calculating daily transaction trends")

  statements = trx_long %>% select(ClientID,AccountID,AccountName) %>%
    left_join(largeTransactions %>%
                select(AccountID,ClientID,AccountName,ProductName,ProductID,TrxDate,ClearBalance,TrxTypeID,Debit,Credit,TrxNarration))
  #
  # mutate(KPI_Category = ifelse(grepl("Count",variable),"Count","Amount"))

  trx_long_summary <- trx_long %>% ungroup() %>%
    # select(-Amount) %>%
    select(ClientID,TransactionCategory,TransactionType,TrxDate,contains('Check')) %>% #view()
    reshape2::melt(id.vars=c('ClientID','TransactionCategory','TransactionType','TrxDate')) %>%
    mutate(KPI_Category = ifelse(grepl("count",variable),"Count","Amount")) %>%
    # dplyr::filter(variable %in% c("Amount","Count")) %>%
    group_by(ClientID,TransactionCategory,TransactionType,value,KPI_Category) %>%
    tally(name = "Count") %>%
    reshape2::dcast(TransactionCategory + TransactionType + value ~ KPI_Category,
                    value.var = "Count",fun.aggregate = sum)

  df = list(
    'trx_long'=trx_long,
    'trx_long_summary' = trx_long_summary,
    'trx_long1' = trx_long1,
    'statements' = statements
  )
  updateProgressBar(session = session,id = "client_monitoring",value = 100,status = "success",title = "Finishing")

  save(df,file = "inst/temp/clients_trx_summary.Rdata")
  closeSweetAlert(session = session)
  sendSweetAlert(session = session,title = "Done",type = "success")
  return(df)
}
#' @import kableExtra
#'
trx_statements <- function(trx_data,AccID,trx_category,trx_type) {
  statement = trx_data %>%
    ungroup() %>%
    filter(AccountID == AccID,TransactionCategory == trx_category,
           TransactionType==trx_type) %>%

  # statement = trx_statements(trx_data = statements,trx_category = 'Mobile Banking',
  #                trx_type = 'Withdrawal',AccID='0012070000008') %>%
    select(ClientID,AccountID,AccountName,ProductID,ProductName,TrxDate,TrxTypeID,ClearBalance,Debit,Credit,TrxNarration) %>%
    kable(format.args = list(big.mark=",")) %>%
    kable_styling(full_width = T,font_size = 10) %>%
    scroll_box(width = "100%",fixed_thead = T)

  statement
}

#' @import shiny
trans_summary <- function(ns,...) {
  kwargs <- list(...)
  tagList(fluidRow(
    HTML(paste(tags$b("Client Name"),paste(tags$u(class="desc",kwargs$AccountName)))),
    hr(),
    col_12(
      col_6(HTML(paste(tags$b("Account ID"),paste(tags$u(class="desc",kwargs$AccountID))))),
      col_6(HTML(paste(tags$b("Client ID"),paste(tags$u(class="desc",kwargs$ClientID))))),
    ),
    br(),
    hr(),
    HTML(paste(tags$b("Flagged Transaction Type"),paste(tags$u(class="desc",kwargs$TransactionType)))),
    hr(),
    HTML(paste(tags$b("Reporting Period"),paste(tags$u(class="desc",kwargs$Reporting)))),
    hr()
  ),
  fluidRow(
    box(width = 12,title= 'Statement for the time period selected',
        status='success',
        tabsetPanel(type = "pills",
          tabPanel(title = "Transactions Trend",shinycssloaders::withSpinner(highchartOutput(ns('progress')))),
          tabPanel(title = "Statement",shinycssloaders::withSpinner(tableOutput(ns('statement_table'))))
        )
        )
  )
  )

}

#' flagged trx charts
#' @import highcharter
flagged_trx_chart <- function(dataset,mode,AccID,TrxCategory,TrxType) {
  chart <- switch (mode,
                   "amount" = {
                     dataset %>%
                       ungroup() %>%
                       filter(AccountID == AccID,
                              TransactionCategory == TrxCategory,
                              TransactionType == TrxType) %>%
                       mutate(Trend = paste0("Amount Transacted ",ifelse(Lag_propotion_Amount > 0," increased by ",ifelse(Lag_propotion_Amount == 0,''," reduced by ")),Lag_propotion_Amount,"% \n from the previous.")) %>%
                       select(TrxDate,Lag_Amount,ucl_amount,lcl_amount,mean_amount,Trend)%>%
                       reshape2::melt(id.vars=c('TrxDate','Trend')) %>%
                       mutate(TrxDate = as.Date(TrxDate),
                              Trend = ifelse(variable == 'ucl_amount',paste("Maximum transacted Amount change Ksh ",value," ."),
                                             ifelse(variable == 'lcl_amount',paste("Minimum transacted Amount change Ksh ",round(value)," ."),
                                                    ifelse(variable == 'mean_amount',paste0("Average Transactions Change ",value," ."),
                                                           Trend)))) %>%
                       hchart("spline",hcaes(x='TrxDate',y='value',group='variable')) %>%
                       hc_colors(c('red','green','green','blue'))%>%
                       hc_title(text="Transactions Trend") %>%
                       hc_yAxis(title = list(text = "Amount Transacted"))%>%
                       hc_yAxis(title = list(text = "Number Of Transactions")) %>%
                       hc_tooltip(crosshairs = TRUE,formatter = JS("function(){
                                                        return (
                            '<table>'+
                            '<tr> <br><b>Reporting Date</b> <td> ' + this.point.TrxDate + '</td> '+
                            '<tr><td> <br>---- <br> </td>  <td> </td>  </tr> '+
                            '<tr> <td>' + this.point.Trend + '</td></tr> </table>')}")) %>%
                       hc_add_theme(hc_theme_538())
                   },
                   "count" = {dataset %>% ungroup() %>%
                       filter(AccountID == AccID,
                              TransactionCategory == TrxCategory,
                              TransactionType == TrxType) %>%
                       mutate(Trend = paste0("Number of transactions ",ifelse(Lag_propotion_Count > 0," increased by ",ifelse(Lag_propotion_Count == 0,''," reduced by ")),Lag_propotion_Count,"% \n from the previous.")) %>%
                       select(TrxDate,Lag_Count,ucl_count,lcl_count,mean_count,Trend)%>%
                       reshape2::melt(id.vars=c('TrxDate','Trend')) %>%
                       mutate(TrxDate = as.Date(TrxDate),
                              Trend = ifelse(variable == 'ucl_count',paste("Upper control limit ",value,"transactions"),
                                             ifelse(variable == 'lcl_count',paste("Lower control limit ",round(value),"transactions"),
                                                    ifelse(variable == 'mean_count',paste0("Average Transactions Change ",value,"transactions"),
                                                           Trend)))) %>%
                       hchart("spline",hcaes(x='TrxDate',y='value',group='variable')) %>%
                       hc_colors(c('red','green','green','blue'))%>%
                       hc_title(text="Transactions Trend") %>%
                       hc_yAxis(title = list(text = "Number Of Transactions")) %>%
                         hc_tooltip(crosshairs = TRUE,formatter = JS("function(){
                                                        return (
                            '<table>'+
                            '<tr> <br><b>Reporting Date</b> <td> ' + this.point.TrxDate + '</td> '+
                            '<tr><td> <br>---- <br> </td>  <td> </td>  </tr> '+
                            '<tr> <td>' + this.point.Trend + '</td></tr> </table>')}")) %>%
                       hc_add_theme(hc_theme_538())
                     }
  )
  return(chart)
}
