
    
---
title: "Final_project_final_edition"
author: "Daren Ansher"
date: "5/21/2019"
output:
  pdf_document: default
  html_document: default
---

A Little Context:
Sports tickets tend to be sold in three ways: single game tickets, multi-game plans, and season tickets. 
Since teams would rather sell more tickets at once, the best seating is generally reserved for those willing 
to purchase season tickets. Once a team has sold as many season tickets as they can, the leftover seats are 
sold in multi-game plans. Once no more people want to buy those, the remaining seats are sold as single game 
tickets. Because of this, single game tickets are either the seats nobody else wanted, which can be bought at 
face value via the team, or, they are good seats, bought on the secondary market for a sizable markup from a 
season ticketholder that can't go to a particular game. 

Due to this model, folks will often form a group to purchase and split season tickets - espically for sports 
with longer seasons. For instance, a four person group might split the cost of NBA season tickets, so that 
each person gets ten of the 41 home games in a season. This way, the best seats can be purchased at the best 
price, and nobody has to pay the entire cost of season tickets themselves. Also no one needs to attend 41 NBA 
games a year.

However, the one downside of this method is that you don't have the freedom to purchase tickets for only the
games against oppenents that you want to see. Purchasing NBA season tickets requries purchasing tickets to 
every game and thus every opponent. Therefore groups need a way to divide tickets amongst themselves fairly so 
that each person gets some good games, and some bad ones.

Often, groups will remedy this by having a draft at the beginning of each season. Each person gets ten picks, 
and they take turns selecting which games they want. Each person considers the outcomes of prior seasons, this 
year's offseason acquisitions, and their anticipation of which teams will improve or regress, and combine them 
to make predictions about what opponents will be most worth spending a draft pick (and money) to go see. Since 
NBA team success is heavily dependent on a few of the best players, and since teams that keep their star players 
tend to be pretty consistent over 3-5 year spans, this is generally a pretty successful way to go about choosing 
tickets.

However, we think there might be a better way. While drafters often consider who the best teams will be, they 
rarely consider which teams will make for the most entertaining game. With the plethora of NBA data out there,
we'd like to see if we can scrape some key statistics to predict which teams will make for the most entertaining 
game.


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(rvest)
library(tidyverse)
library(stringr)
library(ggplot2)
library(randomForest)
```


First things first, we'll need to find statistcs for all of the NBA teams. ESPN.com has NBA team statistics for each of 
the past five years in a variety of useful categories. For each year, we can get general team stats, team's offensive and 
defensive ratings, and a few other useful stats categorized under misc. We'll identify the URLs for the tables that we 
want to use and then use the read_html function to scrape the pages for our data. We notice that for all three pages, 
statistics are held in the "mod-content" class located inside a div with ID "my-teams-table". We'll use the # CSS selector 
to select by ID and the . CSS selector to select by class. Finally, we notice that thet relevant statistics are all table 
row elements so we'll use tr to extract all table rows. More info on CSS Selectors can be viewed here: 
https://www.w3.org/TR/CSS2/selector.html.


```{r Pull ESPN Data}
off_url <- "http://www.espn.com/nba/statistics/team/_/stat/offense-per-game/year/2015/seasontype/2"
d_url <- "http://www.espn.com/nba/statistics/team/_/stat/defense-per-game/year/2015/seasontype/2"
misc_url <- "http://www.espn.com/nba/statistics/team/_/stat/miscellaneous-per-game/year/2015/seasontype/2"
  
  html_data <- read_html(off_url) %>% 
  html_node("#my-teams-table") %>% 
  html_nodes(".mod-content") %>% 
  html_nodes("tr") %>% 
  html_text()
  html_data
```



The HTML is a bit messy, so the pages will require a little cleanup. However it appears that each of the three pages are set
up the same. Therefore, we should be able to scrape the statistics from the other two pages using the exact same commands. 
Further, we notice that the URL for the statistics pages of other years is exactly the same except for replacinig "2015" with 
whatever year we want the statistics for. Given how similar these tasks are, we can make things a little easier by writing 
a few 
functions!

We'll create two functions. The first will take a url, scrape the page for statistical information, and return the 
information we want. The second will take in a year, use the gsub function to replace the part of the URLs that say 
"20xx" with whatever year is provded, and then call the first function, inputting the modified URLs, to scrape that year's
pages. The gsub function will use Regular Expressions to search for a given pattern and, wherever it finds that pattern, 
replace it with a different pattern. In this case, the pattern we are searching for is "20\\d{2}" aka the exact number 
"20" followed by exactly two additional digits ("\\d{2}"). Whenever that group of characters is found, it will replace 
those characters with the input year. For more information about Regular Expressions and text manipulation, 
see here: https://r4ds.had.co.nz/strings.html)


```{r Write some functions}
scrape_html <- function(url) {
  read_html(url) %>% 
  html_node("#my-teams-table") %>% 
  html_nodes(".mod-content") %>% 
  html_nodes("tr") %>% 
  html_text()
}
get_year_data <- function(year) {
  off_url <- off_url %>%  gsub(pattern = "20\\d{2}", replacement = year)
  d_url <- d_url %>%  gsub(pattern = "20\\d{2}", replacement = year)
  misc_url <- misc_url %>%  gsub(pattern = "20\\d{2}", replacement = year)
  
  off_html_data <- scrape_html(off_url)
  off_stats <- get_offensive_stats(off_html_data) 
  
  d_html_data <- scrape_url(d_url)
  d_stats <- get_defensive_stats(d_html_data)
  
  misc_html_data <- scrape_html(misc_url)
  misc_stats <- get_misc_stats(misc_html_data) 
}
```

Before we jump to the next part we'll have a brief interlude to talk about some nifty regular expressions. Square brackets 
can be thought of as match any of these characters. So [AB] will match the following strings: "A", "B", "AB", "AC", "BC", 
"ABC". It will not match the string "CDE". \\d will match any digit 0-9 and \\s will match any whitespace character (tab, 
space, etc.). Each of the regular expressions mentioned will match exactly once. The function str_extract() takes a regular 
expression and extracts any corresponding strings matching that pattern. So using str_extract() with the pattern "[AB]" and 
the string "ABC" will return just "A". Using the same on the string "CBC" will return just "B". 

We can use numeric operators as well. Appending an expression with ? equates to match whenever that expression ocurrs zero 
or one times. "*" means match zero or more times, and "+" means match one or more times. Appending with {x,y} where x and y 
are numbers, means match any amount between x and y times. Examples can be found in the following paragraph, and additional
reading about regular expressions can be found here: https://r4ds.had.co.nz/strings.html
Since team names haven't changed since 2014, and since they are in the same place (albeit in different orders) for all three 
webpages, we can write a functiion that will take in the html_data for a partcular page and extract the team names. We'll use
str_extract() to pass in another regular expression and rerturn any strings that match that pattern. Since we notice that 
most of the team names are preceded by a one or two digit number, we'll extract any strings that have one or zero digits 
(\\d?), followed by any capital letter ([A-Z]), followed by one or more lowercase letters ([a-z]+), followed by zero or 
more spaces (\\s\*), followed by zero or more upperr or lowercase letters ([A-Za-z]\*), followed by a digit. Then we'll 
pass the resulting string into str_extract() again to discard the numbers we swept up. Finally, we'll use gsub() to get 
rid of any spaces, and na.omit() to get rid of any rows with NA. Both of these final operations will be useful to us 
later on. 

```{r Separate Team Names}
extract_team_names <- function(html_data) {
  html_data %>% 
    str_extract("\\d?[A-Z][a-z]+\\s*[A-Za-z]*\\d") %>% 
    str_extract("[A-Z][a-z]+\\s*[A-Za-z]*") %>% 
    gsub(pattern = "\\s", replacement = c("")) %>% 
    na.omit()
}
```

Now that we have our html data, and a way to pull the team names, its time to do the heavy lifting. We'll write three more 
functions; one function will be for offensive statistics, one for defensive, and one for misc. They will all have similar 
methodology, but with slight variations due to the dataset. The general idea is that we'll pass in the html data for the 
page with the corresponding stats, use the extract_team_names() function we wrote and save it for later. Next we'll use 
str_remove_all to remove all non-numeric values from the data.

We'll then create a dataframe with each team being an entity and each statistical category being an attribute. Because there
is no clear delimiter in the data, we'll need to treat it as a giant string and create substrings based on the length of the 
corresponding statistics. Because PTS can be over or under 100, and because 3PM can be over or under 10, there is no way to 
separate each row at the same places. We have to get a little hacky here but bear with me. 

We'll have to set up a function that will consider the four possibilities and adjust the cutoffs accordingly. We'll 
helpfully name this function adjust_accordingly. Each row can have a total of 47-49 characters depending on these two 
values. If PTS >= 100 and 3pm >= 10, there will be 49 characters. If PTS < 100 and 3PM >= 10 or if PTS >= 100 and 3PM <= 10,
we'll have 48 characters, and if both are less, we'll have 47. So we'll create a flag that will be true if the line consists
of exactly 48 characters and false otherwise. Next, we notice that if PTS >= 100, it will have to start with a 1. So we can 
make another flag that will be true if the first number is a 1 and false otherwise. Combining the two flags, we can
determine where each of the cutoffs will be for every row!

Next we'll use the slice() function to get rid of the rows that have attribute headers instead of statistics. Finally, 
we'll add the team names back to the statistics dataframe with the mutate() function and then use select to move team
name to the front so that the team name is the first attribute in the table.

```{r MORE FUNCTIONS!!!}
get_offensive_stats <- function(html_data) {
  team_names_data <-  extract_team_names(html_data)
  off_stat_data <- html_data %>%
    str_remove_all("^\\d*\\s*[A-Za-z]*\\s?[A-Za-z]*")
  off_stat_tab <- adjust_accordingly(off_stat_data)
  #off_stat_tab
  
  off_stat_tab <- off_stat_tab %>% 
      slice(-1) %>% 
      slice(-11) %>% 
      slice(-21) %>% 
      mutate(Team_Name = team_names_data) %>% 
      select(-str, -length)
  off_stat_tab <- off_stat_tab %>% 
    select(Team_Name, everything()) 
  
}
get_defensive_stats <- function(html_data) {
  team_names_data <-  extract_team_names(html_data)
  def_stat_data <- html_data %>% 
    str_remove_all("^\\d*\\s*[A-Za-z]*\\s?[A-Za-z]*")
  
  def_stat_tab <- adjust_accordingly(def_stat_data)
  def_stat_tab <- def_stat_tab %>% 
      slice(-1) %>% 
      slice(-11) %>% 
      slice(-21) %>% 
      mutate(Team_Name = team_names_data) %>% 
      select(-str, -length)
  def_stat_tab <- def_stat_tab %>% 
    select(Team_Name, everything()) %>% 
    rename(Opp_PPG = PPG, Opp_FGM = FGM, Opp_FGA = FGA, Opp_FGP = FGP, 
           Opp_TPM = TPM, Opp_TPA = TPA, Opp_TPP = TPP,Opp_FTM = FTM, 
           Opp_FTA = FTA, Opp_FTP = FTP, Opp_PPS = PPS,Opp_AFG = AFG) 
}
    
adjust_accordingly <- function(off_stat_data) {
  # off_stat_tab <- data.frame(PPG = "", FGM = "", FGA = "", FGP = "", 
  #          TPM = "", TPA = "",TPP = "", FTM = "", FTA = "", FTP = "",
  #          PPS = "", AFG = "")
  rowlen <- nchar(off_stat_data)
  off_stat_frame <- data.frame(str = off_stat_data, len = rowlen)
  off_stat_frame$flag_tot <- ifelse(rowlen == 48, TRUE, FALSE)
  off_stat_frame$flag_first <- ifelse(as.numeric(substr(off_stat_data,1,1) == 1), TRUE, FALSE)
  for(row in 1:nrow(off_stat_frame)){
    if(off_stat_frame$flag_tot[row] == TRUE & off_stat_frame$flag_first[row] == TRUE) {
      off_stat_tab$PPG[row] <- substr(off_stat_frame$str[row],1,5)
      off_stat_tab$FGM[row] <- substr(off_stat_frame$str[row],6,9)
      off_stat_tab$FGA[row] <- substr(off_stat_frame$str[row],10,13)
      off_stat_tab$FGP[row] <- substr(off_stat_frame$str[row],14,17)
      off_stat_tab$TPM[row] <- substr(off_stat_frame$str[row],18,20)
      off_stat_tab$TPA[row] <- substr(off_stat_frame$str[row],21,24)
      off_stat_tab$TPP[row] <- substr(off_stat_frame$str[row],25,28)
      off_stat_tab$FTM[row] <- substr(off_stat_frame$str[row],29,32)
      off_stat_tab$FTA[row] <- substr(off_stat_frame$str[row],33,36)
      off_stat_tab$FTP[row] <- substr(off_stat_frame$str[row],37,40)
      off_stat_tab$PPS[row] <- substr(off_stat_frame$str[row],41,44)
      off_stat_tab$AFG[row] <- substr(off_stat_frame$str[row],45,49)
    }
    else if(off_stat_frame$flag_tot[row] == TRUE & off_stat_frame$flag_first[row] == FALSE) {
      off_stat_tab$PPG[row] <- substr(off_stat_frame$str[row],1,4)
      off_stat_tab$FGM[row] <- substr(off_stat_frame$str[row],5,8)
      off_stat_tab$FGA[row] <- substr(off_stat_frame$str[row],9,12)
      off_stat_tab$FGP[row] <- substr(off_stat_frame$str[row],13,16)
      off_stat_tab$TPM[row] <- substr(off_stat_frame$str[row],17,20)
      off_stat_tab$TPA[row] <- substr(off_stat_frame$str[row],21,24)
      off_stat_tab$TPP[row] <- substr(off_stat_frame$str[row],25,28)
      off_stat_tab$FTM[row] <- substr(off_stat_frame$str[row],29,32)
      off_stat_tab$FTA[row] <- substr(off_stat_frame$str[row],33,36)
      off_stat_tab$FTP[row] <- substr(off_stat_frame$str[row],37,40)
      off_stat_tab$PPS[row] <- substr(off_stat_frame$str[row],41,44)
      off_stat_tab$AFG[row] <- substr(off_stat_frame$str[row],45,49)
    }
    else if(off_stat_frame$flag_tot[row] == FALSE & off_stat_frame$flag_first[row] == FALSE) {
      off_stat_tab$PPG[row] <- substr(off_stat_frame$str[row],1,4)
      off_stat_tab$FGM[row] <- substr(off_stat_frame$str[row],5,8)
      off_stat_tab$FGA[row] <- substr(off_stat_frame$str[row],9,12)
      off_stat_tab$FGP[row] <- substr(off_stat_frame$str[row],13,16)
      off_stat_tab$TPM[row] <- substr(off_stat_frame$str[row],17,19)
      off_stat_tab$TPA[row] <- substr(off_stat_frame$str[row],20,23)
      off_stat_tab$TPP[row] <- substr(off_stat_frame$str[row],24,27)
      off_stat_tab$FTM[row] <- substr(off_stat_frame$str[row],28,31)
      off_stat_tab$FTA[row] <- substr(off_stat_frame$str[row],32,35)
      off_stat_tab$FTP[row] <- substr(off_stat_frame$str[row],36,39)
      off_stat_tab$PPS[row] <- substr(off_stat_frame$str[row],40,43)
      off_stat_tab$AFG[row] <- substr(off_stat_frame$str[row],44,48)
    }
    else {
      off_stat_tab$PPG[row] <- substr(off_stat_frame$str[row],1,5)
      off_stat_tab$FGM[row] <- substr(off_stat_frame$str[row],6,9)
      off_stat_tab$FGA[row] <- substr(off_stat_frame$str[row],10,13)
      off_stat_tab$FGP[row] <- substr(off_stat_frame$str[row],14,17)
      off_stat_tab$TPM[row] <- substr(off_stat_frame$str[row],18,21)
      off_stat_tab$TPA[row] <- substr(off_stat_frame$str[row],22,25)
      off_stat_tab$TPP[row] <- substr(off_stat_frame$str[row],26,29)
      off_stat_tab$FTM[row] <- substr(off_stat_frame$str[row],30,33)
      off_stat_tab$FTA[row] <- substr(off_stat_frame$str[row],34,37)
      off_stat_tab$FTP[row] <- substr(off_stat_frame$str[row],38,41)
      off_stat_tab$PPS[row] <- substr(off_stat_frame$str[row],42,45)
      off_stat_tab$AFG[row] <- substr(off_stat_frame$str[row],46,49)
    }
  }
  off_stat_tab
}
```


Finally, we'll put all of the functions we've written into one giant function that takes in a year and outputs the combined 
statistics for that year. We'll start by using gsub() exactly as we did before to get the desired urls. We'll then call our 
html_scrape() function with each url to get the appropriate HTML info, and pipe that into the corresponding get_stats() 
function to clean everything up and put it into a dataframe. Finally, we'll call full_join() on the Team_Name attribute 
to combine the three tables into one mega table. Full join will find the entities in each table with matching team names, 
and concatenate all of those entities' attributes together. 
For more detail on joins, see here: http://www.hcbravo.org/IntroDataSci/bookdown-notes/two-table-operations.html

```{r Put it all together}
get_year_data <- function(year) {
  off_url <- off_url %>%  gsub(pattern = "20\\d{2}", replacement = year)
  d_url <- d_url %>%  gsub(pattern = "20\\d{2}", replacement = year)
  misc_url <- misc_url %>%  gsub(pattern = "20\\d{2}", replacement = year)
  
  off_stats <- scrape_html(off_url) %>% 
    get_offensive_stats()
  
  d_stats <- scrape_html(d_url) %>% 
    get_defensive_stats()
  
  stats_tab <- full_join(off_stats, d_stats, by = "Team_Name") %>% 
      type_convert()
  
}
stats_tab <- get_year_data(2017)
stats_tab
```

Now that we've gathered all these statistics, lets briefly summarize what each of these things actually mean so we can know 
which stats to choose.

It is worth noting two things before diving in. First, all of these stats are calculated as per game averages. So PPG, or 
points per game, is the total number of points the team scored over the season divided by 82, the number of games in a 
season. Second, you'll notice that each attribute in the offensive stats table has an identical attribute, prepended by 
Opp_, in the defensive stats table. The first measures how good the team is at offensive things while the second measures
how good that team is defensively. So if PPG is the amount of points a team scores in an average game, Opp_PPG is the
amount of points that same team allows their opponents to score in an average game.

To the specifics! 

In basketball, a field goal is any basket made during the course of normal play (so basically anything other than a free 
throw, which we'll explain soon). FGM, FGA, and FGP is field goals made, field goals attempted, and field goal percent. 
FGM is the number of baskets made during normal play, FGA is the number of times the team shot a basket, regardless of
whether or not the shot is made, and FGP is FGM/FGA.

While most baskets count for two points, if the shot is taken 24 feet from the hoop or further, the team is awarded three
points instead of two. TPM, TPA, and TPP, is three pointers made, three pointers attempted, and three point percent and it
is exactly the same as field goals, but only for shots taken from the requisite distance. 

When an offensive player is contacted illegally while shooting a basket, we call this a foul and the player is awarded two
or three free throws depending on if the foul was committed against a player while that player was taking a shot from a 
distance less than or more than 24 feet from the hoop (there is quite a bit of additional nuance to fouling in basketball 
that I won't get into. If you are interested, this site gives a lovely overview of the types and terminology:
https://www.breakthroughbasketball.com/basics/basics.html). When a free throw is awarded, the player will be allowed to 
shoot two or three uncontested shots from the free throw line, a designated location 15 feet from the center of the basket.
Each free throw is worth one point. FTM, FTA, and FTP is exactly like field goals and  three pointers, but only for these 
special shots.

PPS and AFG are points per shot and adjusted field goal percent respectively. They try to measure efficiency by calculating 
the how many points are scored per possession instead of per game. They're not very useful for our purposes so I won't go 
into any additional detail. However if you'd like to know more about them, you can start 
here: http://pages.stat.wisc.edu/~wardrop/articles/3point_html.

As noted above, each of the previously discussed statistics have an identical stat, prepended with Opp_. These are the
exact same stats, but instead of calculating the per game averages of the team, they calculate the per game averages for 
the team's opponents. These are used to measure the quality of a team's defense.

Okay! Let's use our functions to pull together all of last year's data and select
the attributes we discussed.

```{r Get 2018 data}
stats_2018 <- get_year_data(2018)
```


Now that we have our scraped data, let's pull another dataset in from
https://www.kaggle.com/pablote/nba-enhanced-stats/downloads/nba-enhanced-stats.zip/27 
to view boxscores for the same time periods. For our purposes, we'll define an exciting
game as a game where the losing team score is within ten points of the winning team's score.

We'll use filter() to grab only games for the year we scraped the data from. Then we'll use 
select() to grab the date of the game, which teams played, and each team's score and then
mutate() to create a boolean column reflecting whether or not the absolute value of difference
in the two team's scores is less than ten.

```{r get boxscore data}
boxscores <- "~/Documents/UMD/courses/spring2019/cmsc320/Projects/final_project/nba-enhanced-stats/2012-18_teamBoxScore.csv"
boxscore_tab <- read_csv(boxscores)
boxscore_tab_2018 <- boxscore_tab %>% 
  filter(gmDate >= "2017-10-30") %>% 
  select(gmDate, teamAbbr, opptAbbr, teamPTS, opptPTS) %>%
  mutate(diff = abs(as.numeric(teamPTS) - as.numeric(opptPTS)), close_game = 
    ifelse((abs(as.numeric(teamPTS) - as.numeric(opptPTS))) < 10, TRUE, FALSE))
boxscore_tab_2018
```

Now that we've obtained all of our data, let's start looking at it! We'll start with a boxplot to 
see if any teams tend to have a lower point differential on average.

```{r visualizations}
boxscore_tab_2018 %>% 
  ggplot(aes(x=teamAbbr, y=diff)) + 
    geom_boxplot() +
      labs(title="Point Differential per Team in 2017-2018 Season",
         x = "Team",
         y = "Differential") +
    theme(plot.title=element_text(hjust = .5), 
          legend.title = element_blank())
```

Since the other dataset uses team abbreviations instead of team names, let's add the appropriate
abbreviation for each team so we can refer to the same team in both tables.

```{r Add Team Abbreviations }
stats_2018 <- stats_2018 %>% 
  mutate(teamAbbr = c("GS", "HOU", "NO", "TOR", "CLE", "DEN", 
                      "PHI", "MIN", "LAC", "CHA", "LAL", "OKC", 
                      "WAS", "BKN", "MIL", "POR", "IND", "NY",
                      "UTA", "BOS", "PHO", "DET", "MIA", "ORL",
                      "ATL", "CHI", "SA", "DAL", "MEM", "SAC")) 
stats_2018 <- stats_2018 %>% 
  select(Team_Name, teamAbbr, everything())
stats_2018
```


Then, let's use group_by() to combine the boxscore stats for each team and then summarize() to calculate the 
average point differential per team. Next we'll use a join to combine our scraped data with the average 
differentials.
```{r }
team_diffs_tab <- boxscore_tab_2018 %>% 
  group_by(teamAbbr) %>% 
  summarize(avg_diff = mean(diff), num_close_games = sum(ifelse(close_game == TRUE, 1, 0)))
team_diffs_tab
diffstats_tab <- full_join(stats_2018, team_diffs_tab, by="teamAbbr")
diffstats_tab
```


lets make a few plots considering different stats and how they relate to average differential


```{r }
diffstats_tab %>% 
  ggplot(aes(x=PPG, y=num_close_games)) + 
    geom_point()+
      labs(title="Number Close Games vs Points Per Game",
           x = "Points Per Game",
           y = "Number Close Games") +
    theme(plot.title=element_text(hjust = .5), 
          legend.title = element_blank())
```



```{r }
diffstats_tab %>% 
  ggplot(aes(x=Opp_PPG, y=num_close_games)) + 
    geom_point()+
      labs(title="Number Close Games vs Opponent Points Per Game",
           x = "Opponent Points Per Game",
           y = "Number Close Games") +
    theme(plot.title=element_text(hjust = .5), 
          legend.title = element_blank())
```


```{r }
diffstats_tab %>% 
  ggplot(aes(x=TPP, y=num_close_games)) + 
    geom_point() +
      labs(title="Number Close Games vs Three Point Shot Percent",
           x = "Three Point Shot Percent",
           y = "Number Close Games") +
    theme(plot.title=element_text(hjust = .5), 
          legend.title = element_blank())
```


```{r }
diffstats_tab %>% 
  ggplot(aes(x=TPA, y=num_close_games)) + 
    geom_point() +
      labs(title="Number Close Games vs Three Point Shot Attempts",
           x = "Three Point Shot Attempts Per Game",
           y = "Number Close Games") +
    theme(plot.title=element_text(hjust = .5), 
          legend.title = element_blank())
```
```{r }
diffstats_tab %>% 
  ggplot(aes(x=FTA, y=num_close_games)) + 
    geom_point() +
      labs(title="Number Close Games vs Free Throw Attempts Per Game",
           x = "Free Throw Attempts Per Game",
           y = "Number Close Games") +
    theme(plot.title=element_text(hjust = .5), 
          legend.title = element_blank())
```

While most plots show nothing of interest correlation, the last two show a small amount of promise.
Let's hypothesize that three point attempts and free throw attempts per game have some statistically
significant effect on the number of close games and see if we can reject the null hypothesis that they
have no effect. We'll fit these to a linear regression model and find out if there is validity to our 
hypothesis

```{r Linear Regression}
model <- lm(num_close_games~TPA*FTA, data = diffstats_tab) %>% 
          broom::tidy() 
model
```

Unfortunately, it appears we are unable to reject the null hypothesis. Although conditioning 
on three point attempts and free throw attempts indicated a possible decrease in the
number of close games of approximately 14.9%, the pvalue of 0.15 indcates that this is well 
above the 0.05 threshhold we would need to reject the null hypothesis. The statistics appear 
to have a nonlinear relationship to the number of close games.


