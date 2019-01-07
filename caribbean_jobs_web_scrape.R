# Packages
library(tidyverse) 
library(dplyr)
library(rvest)    
library(stringr)
library(purrr)

#read in web page
cj_urlbase = "https://www.caribbeanjobs.com/ShowResults.aspx?Keywords=&Location=&Category=&Recruiter=Company%2cAgency&btnSubmit=+"
cj_urlbasepg = read_html((cj_urlbase))

#get the total number of results so we can find out number of iterations needed
total_results = cj_urlbasepg %>% 
  html_nodes(xpath = '//*[@id="page"]/div[2]/div/div[2]/div[2]/div/label[1]')%>% 
  html_text()%>%
  sub("Total Jobs Found: ", "", .) %>% 
  as.numeric()

max_pg = ceiling(total_results/25) #since there are 25 results per page

#create data frame to append each iteration to
full_df <- data.frame()
for(i in 1:max_pg) {
  
cj_urlbase = "https://www.caribbeanjobs.com/ShowResults.aspx?Keywords=&Location=&Category=&Recruiter=Company%2cAgency&btnSubmit=+"
cj_url<-paste0(cj_urlbase,"&Page",i)
cj_page = xml2::read_html(cj_url)


job_title <- cj_page %>% 
  html_nodes(".job-result-title") %>% 
  html_text() %>% 
  subset(. != '') 

base_salary <- cj_page %>% 
  html_nodes(".salary") %>% 
  html_text() %>% 
  subset(. != '') 

post_date  <- cj_page %>% 
  html_nodes(".updated-time") %>% 
  html_text() %>% 
  subset(. != '') 

job_location <-cj_page %>% 
  html_nodes(".location") %>% 
  html_text() %>% 
  subset(. != '') 

job_links <- cj_page %>%
  html_nodes(".show-more") %>% 
  html_attr('href')

job_description <- c()
for(i in seq_along(job_links)) {
  
  url <- paste0("https://www.caribbeanjobs.com", job_links[i])
  page <- xml2::read_html(url)
  
  job_description[[i]] <- page %>%
    rvest::html_nodes('.job-details') %>%
    rvest::html_text()
}
df <- data.frame(job_title, base_salary, post_date, job_location, job_links, job_description)
full_df <- rbind(full_df, df)
}


agrep("information technology", full_df$job_description, max = 2, ignore.case = TRUE)



