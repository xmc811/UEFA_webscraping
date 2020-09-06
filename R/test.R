
library(xml2)
library(rvest)
library(tidyverse)

a <- read_html("https://en.wikipedia.org/wiki/1955%E2%80%9356_European_Cup")


get_match_df(a)

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


# Goalscorer Links
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fhgoal") %>%
        `[[`(16) %>%
        html_nodes("a") %>%
        html_attr("title")

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fagoal") %>%
        `[[`(16) %>%
        html_nodes("a") %>%
        html_attr("title")

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fhgoal") %>%
        `[[`(16) %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_remove("/wiki/")

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fagoal") %>%
        `[[`(16) %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        str_remove("/wiki/")

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


get_stages <- function(txt) {
        
        r_list <- c()
        l_list <- c()
        
        r <- c()
        l <- c()
        
        m <- 0
        
        for (i in seq_along(1:length(s))) {
                
                if (str_detect(txt[i], "round|finals")) {
                        r <- txt[i]
                } else if (str_detect(txt[i], "leg")) {
                        l <- txt[i]
                } else if (txt[i] == "Final") {
                        r <- txt[i]
                        l <- txt[i]
                } else if (str_detect(txt[i], "^\n")) {
                        m <- m + 1
                        r_list[m] <- r
                        l_list[m] <- l
                } else {
                }
        }
        return(list(r_list, l_list))
}


get_stages(s)





test <- a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fhgoal") %>%
        html_text() %>%
        `[`(22)

txt <- test %>%
        str_split("(?<=('|\\)))(?=[A-Za-z])") %>%
        `[[`(1)

str_count(txt, "[0-9]{1,3}'")

str_remove_all(txt, "[0-9]{1,3}'|,|\\(o.g.\\)|\\(pen.\\)") %>%
        str_trim()

do.call("c", str_split(txt, ",")) %>%
        str_extract("[0-9]{1,3}'([[:space:]]\\(.*\\))?")

do.call("c", str_split(txt, ",")) %>%
        str_extract("[0-9]{1,3}'([[:space:]]\\(.*\\))?") %>%
        str_extract("[0-9]{1,3}") %>%
        as.integer()

do.call("c", str_split(txt, ",")) %>%
        str_detect("\\(o.g.\\)")

do.call("c", str_split(txt, ",")) %>%
        str_detect("\\(pen.\\)")


txt_score_to_df("")


