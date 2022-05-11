#' loan_limits
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import dplyr
#' @import janitor
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import openxlsx
#' @import lubridate
#'
#' @noRd
loanlimits <- function(session,con,dates,cbs) {
    reportingDate1 <- format(as.Date(dates),"%d %b %Y")#//////////////////////
    reportingDate2 <- format(floor_date(Sys.Date()-1,"month"),"%d/%b/%Y")#/
    # start progress bar =====
    progressSweetAlert(session = session, id = "disb_monitoring",title = "Start Disbursement monitoring",display_pct = TRUE, value = 0)
    # read files =====
    updateProgressBar(session = session,id = "disb_monitoring",value = 20,status = "success",title = "Reading Disbursement Listing")
    disbursement_file <-dbSendQuery(cbs,sprintf("EXECUTE Rpt_Loandisburseddetails @FromDate='%s',@ToDate='%s',@FromAmount='-99999999',@ToAmount='99999999',@FromCreditOfficerID='0',@ToCreditOfficerID='zzzz',@Branches ='000,001',@LoanProducts ='MFC802,MFC805,MFC806,ML101,ML102,ML103,ML104,ML105,ML106,ML107,ML108,ML109,ML110,ML111,ML113,ML114,ML115,ML116,ML117,ML118,ML119,ML121,ML122,MLS904'",
                                                reportingDate2,gsub(" ","/",reportingDate1))) %>% dbFetch()

    updateProgressBar(session = session,id = "disb_monitoring",value = 50,status = "success",title = "Reading Loan Limits File")
    LoanLimits = tbl(con,"LoanLimits") %>% collect()
# limits check ===========
    updateProgressBar(session = session,id = "disb_monitoring",value = 70,status = "success",title = "Checking Loans Above Limit")
    limits_check <- disbursement_file %>%
        filter(grepl('MFC|MLS',ProductID),
               as.Date(DisbursedOn) >= Sys.Date()-1) %>%
        select(AccountID,IDNumber,PhoneNumber,DisbursedOn,LoanAmount,ProductID) %>%
        left_join(LoanLimits %>%
                      distinct(MSISDN,.keep_all=T) %>%
                      select(PhoneNumber=MSISDN,Final_Limit)) %>%
        mutate(
            Net_Loan = ifelse(grepl("MFC802|MLS904",ProductID,T),LoanAmount/1.12,
                              ifelse(ProductID == "MFC805",LoanAmount/1.048,LoanAmount/1.072)),
            IDNumber = trimws(gsub("_","",IDNumber)),
            Final_Limit = round(as.numeric(Final_Limit)/50) * 50,
            Limits_Check = ifelse(as.numeric(Final_Limit) > Final_Limit,"Above Limit","Within Limit")
        ) %>% filter(Limits_Check == "Above Limit")

# duplicates analysis =======
    updateProgressBar(session = session,id = "disb_monitoring",value = 70,status = "success",title = "Checking Multiple Loans")
    duplicates = disbursement_file %>%
        filter(grepl('MFC|MLS',ProductID),
               as.Date(DisbursedOn) >= Sys.Date()-1) %>%
        group_by(IDNumber) %>%
        mutate(DuplicatedByID = n()) %>% ungroup() %>%
        group_by(PhoneNumber) %>%
        mutate(DuplicatedByPhone = n()) %>% ungroup() %>%
        group_by(AccountID) %>%
        mutate(DuplicatedByAccountID = n(),
               ReportDate = Sys.Date())
   duplicates_summary <-  list(mobile = duplicates %>% filter(DuplicatedByPhone>1) %>% select(PhoneNumber,IDNumber,AccountID,TitleOfAccount,DisbursedOn,LoanAmount),
                               accountid = duplicates %>% filter(DuplicatedByAccountID>1) %>% select(PhoneNumber,IDNumber,AccountID,TitleOfAccount,DisbursedOn,LoanAmount),
                               idnumber = duplicates %>% filter(DuplicatedByID>1) %>% select(PhoneNumber,IDNumber,AccountID,TitleOfAccount,DisbursedOn,LoanAmount))
# disbursements summaries ==========
   bdo_products = tbl(con,"BDODetails") %>% collect()
   updateProgressBar(session = session,id = "disb_monitoring",value = 90,status = "success",title = "Disbursement Summaries")
   disb_grouped <- disbursement_file %>%
       separate(CreditOfficerName,into = c(NA,'CreditOfficerName'),sep = "-")%>%
       mutate(
           CreditOfficerName = str_to_title(trimws(CreditOfficerName)),
           Product = ifelse(grepl('MFC',ProductID),"Mfanisi Airtel",
                            ifelse(grepl('MLS',ProductID),"Mfanisi Safaricom",CreditOfficerName)),
           Channel  = ifelse(grepl('fanisi',Product),"M-Fanisi","Branch")
       )
   daily_disb = disb_grouped %>%
       group_by(Channel,Product,DisbursedOn = as.Date(DisbursedOn)) %>%
       summarise(Amount = sum(as.numeric(LoanAmount)),Count = n()) %>%
       mutate(Amount=format(Amount,big.mark=","))

   day_disb = disb_grouped %>%
       filter(as.Date(DisbursedOn)==Sys.Date()-1) %>%
       group_by(Channel,Product) %>%
       summarise(Amount = sum(as.numeric(LoanAmount)),Count = n()) %>%
       merge(bdo_products,all.y = T) %>%
       mutate_all(.funs = function(x)ifelse(is.na(x),0,x)) %>%
       split(.$Channel) %>%
       map_df(~adorn_totals(.) %>% as_tibble())%>%
       bind_rows(.,select(filter(.,Channel=="Total"),-Channel,-Product) %>%
                     colSums()) %>%
       mutate(Channel = ifelse(is.na(Channel),"Grand Total",Channel)) %>%
       mutate(Amount=format(Amount,big.mark=","))



   month_disb = disb_grouped %>%
       group_by(Channel,Product) %>%
       summarise(Amount = sum(as.numeric(LoanAmount)),Count = n()) %>%
       merge(bdo_products,all.y = T) %>%
       mutate_all(.funs = function(x)ifelse(is.na(x),0,x)) %>%
       split(.$Channel) %>%
       map_df(~adorn_totals(.))%>%
       bind_rows(.,select(filter(.,Channel=="Total"),-Channel,-Product) %>%
                     colSums()) %>%
       mutate(Channel = ifelse(is.na(Channel),"Grand Total",Channel))%>%
       mutate(Amount=format(Amount,big.mark=","))

   # last 7 days
   week_disb = daily_disb %>%
       mutate(Amount = readr::parse_number(Amount)) %>%
       filter(DisbursedOn >= Sys.Date()-8) %>%
       group_by(Channel,Product) %>%
       summarise(Amount = sum(as.numeric(Amount),na.rm = T),Count = n()) %>%
       merge(bdo_products,all.y = T) %>%
       mutate_all(.funs = function(x)ifelse(is.na(x),0,x)) %>%
       split(.$Channel) %>%
       map_df(~adorn_totals(.))%>%
       bind_rows(.,select(filter(.,Channel=="Total"),-Channel,-Product) %>%
                     colSums()) %>%
       mutate(Channel = ifelse(is.na(Channel),"Grand Total",Channel))%>%
       mutate(Amount=format(Amount,big.mark=","))

   disb_summ = list(daily_disb = daily_disb,day_disb=day_disb,month_disb=month_disb,week_disb=week_disb)
# outputs =========
   reports = list(
       limits_check = limits_check,
       duplicates_summary = duplicates_summary,
       disb_summ = disb_summ,
       disbursements = disbursement_file %>%
           filter(as.Date(DisbursedOn)==max(as.Date(DisbursedOn)))
   )
   updateProgressBar(session = session,id = "disb_monitoring",value = 99,status = "success",title = "Pre Saving Output")
   save(reports,file = paste0(getwd(),"/inst/temp/limits_report.Rdata"))
   knitr::knit2html(input = "inst/temp/email.Rmd",output = "inst/temp/email2.html",quiet = T)
   closeSweetAlert(session = session)
   sendSweetAlert(session = session,title = "Done",type = "success")

   return(reports)
}

#' disbursement files
#'
#' @description A disbursement files function
#'
#' @return The return value, if any, from executing the utility.
#' @import dplyr
#' @import janitor
#' @import tidyr
#' @import stringr
#' @import purrr
#' @import openxlsx
#'
#' @noRd

get_files <- function(report_date=NULL,pathto_limits="inst/temp/") {

}
