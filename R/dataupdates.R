# detach("package:reticulate", unload = TRUE)
library(R6);library(mbanalytics)

MfanisiData <- R6Class(classname = "MfanisiData",public = list(
    email = NA,
    password = NA,
    api_base_endpoint = "https://mfan.maishabank.com",
    initialize = function(email,password){
        self$email = email
        self$password = password
    },
    api_authenticate = function(){
        url <- paste(self$api_base_endpoint,"/authentication", sep="")

        pc_json <- list(
            strategy = "local",
            email = self$email,
            password = self$password
        )
        print(paste("Starting Authentication...Please Wait"))
        result <- httr::POST(url
                       , body = pc_json
                       , encode = "json",
                       httr::add_headers(.headers = c("Content-Type"="application/json"))
        )
        print(paste("Authentication Ended."))
        output <- httr::content(result)
    },
    api_fetch = function(token,service="/core/customer"){
        url <- paste(self$api_base_endpoint,service, sep="")
        result <-
            httr::GET(url, httr::add_headers(
                .headers = c(
                    "Content-Type" = "application/json",
                    "Authorization" = paste("Bearer", token)
                )
            ))
        output <- httr::content(result,"text")
        return(output)
    },
    fetch_data = function(accessToken,util='customer',limit=100,...) {

        txt = tibble()
        datatable <- init$api_fetch(token = accessToken,service = glue("/core/{util}?%24limit={limit}"))
        total = as.numeric(pull(head(as_tibble(jsonlite::fromJSON(datatable)),1),total))
        cuts = (cut(x = 1:total,breaks = seq(0,total,by=100),right = T) %>% levels())
        for (i in 1:length(cuts)) {
            values = eval(parse(text = gsub("[(]|]", "", gsub(',', ":", cuts))[i]))
            if (i == 1) {
                value = values
            }else{
                min_ = min(values)+1
                max_ = max(values)
                value = min_:max_
            }
            ids = paste0("&id=",value,collapse = "")
            customers <- self$api_fetch(token = accessToken,
                                        service = glue("/core/{util}?%24limit=100{ids}"))
            new_row = jsonlite::fromJSON(customers,flatten = T)

            txt <- bind_rows(txt,as_tibble(new_row))
        }
        return(txt$data)
    }
))
init = MfanisiData$new(email = 'george.oduor@maishabank.com',password = 'test.test1')
accessToken <- init$api_authenticate()$accessToken
customers = init$fetch_data(accessToken = accessToken,util = "customer",limit = 100)
