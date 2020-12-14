# Webscraping final project
# I intended to have a look into the abnormal 2019-2020 season of the NBA. The major league American sports like the NBA, NFL, NHL and MLB are the heaven for data and sort enthusiasts.
# My task was to scrape a website which we were interested in and then prepare a small analysis.
# I got the data for each month of the 2019-2020 season with the help of a function in R.

library(rvest)
library(lubridate)
library(dplyr)
library(moments)
library(ggplot2)

# 2020 was a really strange year due to the pandemic
# It was the first time that basketball players played on October in two distinct years but was in the same season
# in order to get the whole data for the year we had to include october like october-2019 and october-2020 in order to get the data correctly
# Other note is that there was a break during the year due to covid, therefore, there is no data between april and june
# If you would like to check out different years just please change the paratmeters year and months
year <- "2020"
months <- c("october-2019", "november", "december", "january","february", "march", "july", "august", "september", "october-2020")
playoffs <- ymd("2020-08-17")
outputfile <- "NBA-2020_game_data.rds"

########
# Scraping basketball-reference
########
df <- data.frame()
for (month in months) {
  # get webpage
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                "_games-", month, ".html")
  webpage <- read_html(url)

  # get column names
  col_names <- webpage %>% 
    html_nodes("table#schedule > thead > tr > th") %>% 
    html_attr("data-stat")    
  col_names <- c("game_id", col_names)
  
  # The Playoffs section divider messes up our table so we will remove that from the dates
  dates <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>% 
    html_text()
  dates <- dates[dates != "Playoffs"]
  
  # Again, the Playoffs divider needs to be removed from the game ID, more preciselz the NA value that is given
  game_id <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > th") %>%
    html_attr("csk")
  game_id <- game_id[!is.na(game_id)]
  
  # extract all columns (except date)
  data <- webpage %>% 
    html_nodes("table#schedule > tbody > tr > td") %>% 
    html_text() %>%
    matrix(ncol = length(col_names) - 2, byrow = TRUE)
  
  # combine game IDs, dates and columns in dataframe for this month, add col names
  month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
  names(month_df) <- col_names
  
  # add to overall dataframe
  df <- rbind(df, month_df)
}

## Data Cleaning
# Correct values for the certain columns are changed
df$visitor_pts <- as.numeric(df$visitor_pts)
df$home_pts    <- as.numeric(df$home_pts)
df$attendance  <- as.numeric(gsub(",", "", df$attendance))
df$date_game   <- mdy(df$date_game)

# Dropping the useless data
df$box_score_text <- NULL
df$game_remarks <- NULL

# Dropping the NA values (in attendance)
# Due to the pandemic there were lot of games with zero attendance in person (the whole playoffs was behind closed doors)
df[is.na(df)] <- 0


## Data Enrichment
# Adding a new column for categorizing a game for Regular Season or Playoffs
df$game_type <- with(df, ifelse(date_game >= playoffs, 
                                "Playoff", "Regular Season"))

# Getting both the winner and loser team for each game
df$winner <- with(df, ifelse(visitor_pts > home_pts, 
                             visitor_team_name, home_team_name))
df$loser <- with(df, ifelse(visitor_pts < home_pts, 
                            visitor_team_name, home_team_name))

# Getting the difference score from the view of the home and visitor team sa well
df <- df %>% mutate(home_game_diff = home_pts - visitor_pts)
df <- df %>% mutate(away_game_diff = visitor_pts - home_pts)

# save to file
saveRDS(df, outputfile)
write.csv(df, "clean_data.csv")

getwd()

########
# Analysis
########

df %>% summarise(
  variable = 'Home game advantage',
  mean     = mean(home_game_diff),
  median   = median(home_game_diff),
  std      = sd(home_game_diff),
  min      = min(home_game_diff),
  max      = max(home_game_diff),
  skew     = skewness(home_game_diff),
  numObs   = sum( !is.na( home_game_diff ) ) )

df %>% summarise(
  variable = 'Attendance',
  mean     = mean(attendance),
  median   = median(attendance),
  std      = sd(attendance),
  min      = min(attendance),
  max      = max(attendance),
  skew     = skewness(attendance),
  numObs   = sum( !is.na( attendance ) ) )



#Creating subset for Los Angeles Lakers home games
s1 <- subset(df,home_team_name %in% c("Los Angeles Lakers"))
#Creating subset for Los Angeles Lakers away games
s2 <- subset(df,visitor_team_name %in% c("Los Angeles Lakers"))

# Checking on a line chart how did Los Angeles Lakers fared on home court and away
ggplot()+
  geom_point(data = s1, aes(x = date_game, y = home_game_diff), color = "blue", size = 2)+
  geom_point(data = s2, aes(x = date_game, y = away_game_diff), color = "red", size = 2)+
  geom_hline(yintercept = 0, size = 1)+
  xlab('Dates') +
  ylab('Score difference')+
  ggtitle('LAL performance')


#Creating subset for Boston Celtics home games
s3 <- subset(df,home_team_name %in% c("Boston Celtics"))
#Creating subset for Boston Celtics Lakers away games
s4 <- subset(df,visitor_team_name %in% c("Boston Celtics"))


# Checking on a line chart how did Boston Celtics fared on home court and away
ggplot()+
  geom_point(data = s3, aes(x = date_game, y = home_game_diff), color = "blue", size = 2)+
  geom_point(data = s4, aes(x = date_game, y = away_game_diff), color = "red", size = 2)+
  geom_hline(yintercept = 0, size = 1)+
  xlab('Dates') +
  ylab('Score difference')+
  ggtitle('BOS performance')
  
#Creating subset for Golden State Warriors home games
s5 <- subset(df,home_team_name %in% c("Golden State Warriors"))
#Creating subset for Golden State Warriors away games
s6 <- subset(df,visitor_team_name %in% c("Golden State Warriors"))

# Checking on a line chart how did Golden State Warriors fared on home court and away
ggplot()+
  geom_point(data = s5, aes(x = date_game, y = home_game_diff), color = "blue", size = 2)+
  geom_point(data = s6, aes(x = date_game, y = away_game_diff), color = "red", size = 2)+
  geom_hline(yintercept = 0, size = 1)+
  xlab('Dates') +
  ylab('Score difference')+
  ggtitle('GSW performance')


# Checking on a line chart how did Los Angeles Lakers and Golden State fared on home court
LAL_BOS_GSW_home <- ggplot()+
  geom_point(data = s1, aes(x = date_game, y = home_game_diff), color = "blue", size = 2)+
  geom_point(data = s3, aes(x = date_game, y = home_game_diff), color = "green", size = 2)+
  geom_point(data = s5, aes(x = date_game, y = home_game_diff), color = "red", size = 2)+
  geom_hline(yintercept = 0, size = 1)+
  xlab('Dates') +
  ylab('Score difference')+
  ggtitle('Difference between performance of LAL, BOS and GSW on home court')


# Checking on a line chart how did Los Angeles Lakers Golden State fared on home court and away
LAL_BOS_GSW_away <- ggplot()+
  geom_line(data = s2, aes(x = date_game, y = away_game_diff), color = "blue")+
  geom_line(data = s4, aes(x = date_game, y = away_game_diff), color = "green")+
  geom_line(data = s6, aes(x = date_game, y = away_game_diff), color = "red")+
  xlab('Dates') +
  ylab('Score difference')+
  ggtitle('Difference between performance of LAL, BOS and GSW on away court')

ggarrange(LAL_BOS_GSW_home, LAL_BOS_GSW_away, ncol = 2, nrow = 1)

# Creating a subset of playoffs
playoffs <- subset(df,game_type %in% c("Playoff"))

# Plotting the score differences over time
ggplot()+
  geom_line(data = playoffs, aes(x = date_game, y = home_game_diff))+
  geom_point(data = playoffs, aes(x = date_game, y = home_game_diff))+
  xlab('Dates') +
  ylab('Score difference')+
  ggtitle('Score difference in the playoffs (home court view)')

# Number of games won in playoffs 2019-2020
ggplot(playoffs, aes(winner))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab('Teams') +
  ylab('Number of games won')+
  ggtitle('Number of games won in playoffs 2019-2020')

# Change of attendance in the regular season
regular_season <- subset(df,game_type %in% c("Regular Season"))
ggplot()+
  geom_point(data = regular_season, aes(x = date_game, y = attendance))+
  xlab('Dates') +
  ylab('Attendance')+
  ggtitle('Attendance in the regular season 2019-2020')


