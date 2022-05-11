
con = DBI::dbConnect(RSQLite::SQLite(),"inst/temp/amldb.sqlite")
#'
#' @import DBI
maisha_conn <- function(userName = NULL,password = NULL) {
    if (is.null(userName) |
        is.null(password)) {
        userName = stringr::str_to_upper(getPass::getPass("Enter your Username "))
    }
    if (is.null(password) |
        is.null(password)) {
        password = (getPass::getPass("Enter your password "))
    }

    con =  DBI::dbConnect(
        odbc::odbc(),
        driver = "SQL Server",
        server = "172.16.200.6",
        database = "BRNET_MAISHA",
        uid = userName,
        pwd = password
    )

    # con <- DBI::dbConnect(odbc::odbc(), driver = "SQL Server", server = "172.16.200.6",
    #                       database = "BRNET_MAISHA", uid = keyring::key_get("db_user"),
    #                       pwd = "test.test21")
    return(con)
}

