
library(xml2)
library(rvest)
library(tidyverse)

a <- read_html("https://en.wikipedia.org/wiki/1955%E2%80%9356_European_Cup")


df <- get_match_df(a)

# Time

get_match_time(a)

# Team Name

get_team_names(a)


# Team Link

get_team_full_names(a)

get_team_links(a)

# Country Link


get_country_full_names(a)

get_country_links(a)



# Scores
scores <- a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.fscore") %>%
        html_text()

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.fscore") %>%
        html_text() %>%
        str_split("[–-]") %>%
        map_chr(.f = function(x) {x[1]}) %>%
        as.integer()

str_split(scores[1], "[–-]")

get_match_scores(a)


# Goals
goals <- a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fhgoal") %>%
        html_text()

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fagoal") %>%
        html_text()


# Goalscorer Names
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fhgoal") %>%
        `[[`(16) %>%
        html_nodes("a") %>%
        html_text()

ns <- a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fhgoal")

ns[[3]] %>%
        html_nodes("a") %>%
        html_text() %>%
        length()

ns[[22]] %>%
        html_nodes("a") %>%
        html_text()

ns[[22]] %>%
        html_nodes("a") %>%
        html_attr("title")

ns[[22]] %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_remove("/wiki/")

node_to_plyr_df(ns[[1]])
node_to_goal_df(ns[[1]])

inner_join(node_to_goal_df(ns[[1]]), node_to_plyr_df(ns[[1]]), by = 'Player')


get_goals_df(a)




a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fagoal") %>%
        `[[`(16) %>%
        html_nodes("a") %>%
        html_text()


# Location
txt <- a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("[class='mobile-float-reset fright']") %>%
        html_nodes("div") %>%
        html_text() %>%
        `[`(seq(1,length(.),3)) %>%
        str_remove_all("\\[.*\\]")


do.call("c", str_split(txt, ",")) %>%
        `[`(seq(1,length(.),2)) %>%
        str_trim()

do.call("c", str_split(txt, ",")) %>%
        `[`(seq(2,length(.),2)) %>%
        str_trim()

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("[class='mobile-float-reset fright']") %>%
        html_nodes("div") %>%
        `[`(seq(1,length(.),3)) %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset("/wiki/") %>%
        `[`(seq(1,length(.),2)) %>%
        str_remove("/wiki/")

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("[class='mobile-float-reset fright']") %>%
        html_nodes("div") %>%
        `[`(seq(1,length(.),3)) %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_subset("/wiki/") %>%
        `[`(seq(2,length(.),2)) %>%
        str_remove("/wiki/")

# Attendance
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("[class='mobile-float-reset fright']") %>%
        html_nodes("div") %>%
        html_text() %>%
        `[`(seq(2,length(.),3)) %>%
        str_extract("[0-9,]+") %>%
        str_remove_all(",") %>%
        as.integer()

# Referee
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("[class='mobile-float-reset fright']") %>%
        html_nodes("div") %>%
        `[`(seq(3,length(.),3)) %>%
        html_nodes("a") %>%
        `[`(seq(1,length(.),2)) %>%
        html_attr("href") %>%
        str_remove("/wiki/")

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("[class='mobile-float-reset fright']") %>%
        html_nodes("div") %>%
        html_text() %>%
        `[`(seq(3,length(.),3)) %>%
        str_remove("Referee:") %>%
        str_extract("\\(.*\\)") %>%
        str_remove_all("[()]")

test <- get_match_meta(a)


s <- a %>%
        html_nodes(xpath = "//h2/span[@class='mw-headline'] | //h3/span[@class='mw-headline'] | //div[@class='footballbox']") %>%
        html_text() %>%
        str_subset("(round|leg|(F|f)inal|^\n)")


get_stages(a)


test <- df[1:16,]

x <- test$Home_Score
names(x) <- test$Home_Team
y <- test$Away_Score
names(y) <- test$Away_Team

as.integer((x[order(names(x))] + y[order(names(y))])['Milan'])




test <- test %>%
        add_column(Home_Agg = 0, .before = "Home_Team_Full") %>%
        add_column(Away_Agg = 0, .before = "Home_Team_Full")

for (i in seq_along(1:nrow(test))) {
        
        if (test$Leg[i] == "Second leg") {
                test$Home_Agg[i] <- as.integer((x[order(names(x))] + y[order(names(y))])[test$Home_Team[i]])
                test$Away_Agg[i] <- as.integer((x[order(names(x))] + y[order(names(y))])[test$Away_Team[i]])
        } else {
                test$Home_Agg[i] <- test$Home_Score[i]
                test$Away_Agg[i] <- test$Away_Score[i]
        }
}



process_df <- function(df) {
        
        x <- df$Home_Score
        names(x) <- df$Home_Team
        y <- df$Away_Score
        names(y) <- df$Away_Team
        
        s <- x[order(names(x))] + y[order(names(y))]

        df <- df %>%
                add_column(Home_Agg = 0, .before = "Home_Team_Full") %>%
                add_column(Away_Agg = 0, .before = "Home_Team_Full")
        
        for (i in seq_along(1:nrow(df))) {
                
                if (df$Leg[i] == "Second leg") {
                        df$Home_Agg[i] <- as.integer(s[df$Home_Team[i]])
                        df$Away_Agg[i] <- as.integer(s[df$Away_Team[i]])
                } else {
                        df$Home_Agg[i] <- df$Home_Score[i]
                        df$Away_Agg[i] <- df$Away_Score[i]
                }
        }
        return(df)
}


process_df_list <- function(df) {
        
        levels <- unique(df$Stage)
        
        df_list <- list()
        
        for (i in seq_along(1:length(levels))) {
                
                df_list[[i]] <- df %>% 
                        filter(Stage == levels[i]) %>% 
                        process_df()
        }
        
        df_new <- do.call("rbind", df_list)
        
        return(df_new)
}


test <- process_df_list(df)



