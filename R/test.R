
library(xml2)
library(rvest)
library(tidyverse)

a <- read_html("https://en.wikipedia.org/wiki/1955%E2%80%9356_European_Cup")


# Time
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("time") %>%
        html_attr("datetime")

t <- get_match_time(a)

strptime(t[1], format = '%Y-%m-%dT%H:%M:%S', tz = "UTC")

# Team Name
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.fhome") %>%
        html_text() %>%
        str_trim()

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.faway") %>%
        html_text() %>%
        str_trim()

get_team_names(a)


# Team Link
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.fhome") %>%
        html_nodes("a") %>%
        html_attr("title") %>%
        `[`(seq(1,length(.),2))

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.faway") %>%
        html_nodes("a") %>%
        html_attr("title") %>%
        `[`(seq(2,length(.),2))

get_team_full_names(a)


a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.fhome") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        `[`(seq(1,length(.),2)) %>%
        str_remove("/wiki/")

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.faway") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        `[`(seq(2,length(.),2)) %>%
        str_remove("/wiki/")


get_team_links(a)

# Country Link
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.fhome") %>%
        html_nodes("a") %>%
        html_attr("title") %>%
        `[`(seq(2,length(.),2))

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.faway") %>%
        html_nodes("a") %>%
        html_attr("title") %>%
        `[`(seq(1,length(.),2))

get_country_full_names(a)

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.fhome") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        `[`(seq(2,length(.),2)) %>%
        str_remove("/wiki/")

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.faway") %>%
        html_nodes("a") %>%
        html_attr("href") %>%
        `[`(seq(1,length(.),2)) %>%
        str_remove("/wiki/")

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
a %>%
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
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("[class='mobile-float-reset fright']") %>%
        html_nodes("div") %>%
        html_text() %>%
        `[`(seq(1,length(.),3))

# Attendance
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("[class='mobile-float-reset fright']") %>%
        html_nodes("div") %>%
        html_text() %>%
        `[`(seq(2,length(.),3))

# Referee
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("[class='mobile-float-reset fright']") %>%
        html_nodes("div") %>%
        html_text() %>%
        `[`(seq(3,length(.),3))


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
        str_detect("\\(o.g.\\)")

do.call("c", str_split(txt, ",")) %>%
        str_detect("\\(pen.\\)")


