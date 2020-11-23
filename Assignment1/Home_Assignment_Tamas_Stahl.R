library(data.table)
library(rvest)

rm(list=ls())

# The website we are interested in is financial times and big data
my_url <- 'https://www.ft.com/search?&q=big+data'

# Creating a function that downloads the specified information (title, link and teaser)
get_one_page_from_ft <- function(my_url) {
  
  print(my_url)
  t <- read_html(my_url)
  boxes <- t %>% 
    html_nodes('.js-teaser')
  x <- boxes[[1]]
  boxes_df <- lapply(boxes, function(x){
    t_list <- list()
    t_list[['title']] <- x %>% html_nodes('.o-teaser__heading') %>% html_text()
    t_list[['link']] <- paste0('https://www.ft.com/', x %>% html_nodes('.js-teaser-heading-link') %>% html_attr('href'))
    t_list[['teaser']] <-  gsub('\t', '', gsub('\n', '', x %>% html_nodes('.js-teaser-standfirst-link') %>% html_text(), fixed = TRUE), fixed = TRUE)
    return(data.frame(t_list))  
  })
  df <- rbindlist(boxes_df)
  return(df)
}

# The resulting data frame of getting one page from ft.com
df <- get_one_page_from_ft(my_url)
# Writing out to CSV and RDS
write.csv(df, 'FT_one_page.csv')
saveRDS(df, 'FT_one_page.rds')


# With this function we could use two arguments, keyword and number of pages to download
# For example
## search term <- 'big data'
## page_to_download <- 3 

get_ft <- function(searchterm, page_to_download) {
  
  searchterm <- gsub(' ', '+', searchterm, fixed = T)
  # create links
  links_to_get <- 
    paste0('https://www.ft.com/search?q=', searchterm, '&page=', seq(1, (page_to_download)))
  ret_df <- rbindlist(lapply(links_to_get, get_one_page_from_ft))
  return(ret_df)
  
}


# The resulting data frame of getting data frame from ft.com using two arguments
df_arguments <- get_ft(searchterm = 'big data', 3)
# Writing out to CSV and RDS
write.csv(df_arguments, 'FT_arguments.csv')
saveRDS(df_arguments, 'FT_arguments.rds')


