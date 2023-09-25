##This code loops through a list of CQC webaddresses to pull out the review date. 
##This code is based on using the table from UK Health Dimensions, but if you 
##have a list of the required GP practices and their CQC location ID it would
##be easy to create these yourself.


library(dplyr)
library(odbc)
library(dbplyr)
library(DBI)
library(rvest)
library(stringr)
library(purrr)



CQC_Data <- tbl(UK_Health, in_schema("cqc","Locations_SCD")) |> 
  filter(Is_Latest == 1,
         odsCcgCode == "36L",
         is.na(deregistrationDate)) |> 
  select(odsCode, URL) |> 
  collect() |> 
  mutate(URL = gsub("api.","", URL)) |> 
  mutate(URL = gsub("public/v1/","", URL)) |> 
  mutate(URL = gsub("locations","location", URL))

ReviewDatesList <- list()

for (url in CQC_Data$URL) {


#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
web_data_html <- html_nodes(webpage,'.service-overview-meta')

#Converting the ranking data to text
web_data <- html_text(web_data_html) 

ReviewDatesList[[url]] <- c(web_data, "NULL")
}


ReviewDate <- data.frame(ReviewDates2) |> 
  t() |> data.frame() |> 
  mutate(X1 = gsub("Latest review:\n","",X1)) |> 
  mutate(X1 = trimws(X1)) |> 
  mutate(X1 = sub("^(\\S*\\s+\\S+\\s+\\S+).*", "\\1", X1)) |> 

  
  CQC_Data <- CQC_Data |> mutate(ReviewDate = ReviewDate$X1 )  
  


