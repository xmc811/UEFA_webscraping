
library(xml2)
library(rvest)
library(tidyverse)

a <- read_html("https://en.wikipedia.org/wiki/1955%E2%80%9356_European_Cup")


# Time
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("time") %>%
        html_attr("datetime")

# Team Name
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.fhome") %>%
        html_text()

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.faway") %>%
        html_text()


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

# Scores
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("th.fscore") %>%
        html_text()

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
        `[[`(1) %>%
        html_nodes("a") %>%
        html_text()

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fagoal") %>%
        `[[`(1) %>%
        html_nodes("a") %>%
        html_text()


# Goalscorer Links
a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fhgoal") %>%
        `[[`(1) %>%
        html_nodes("a") %>%
        html_attr("title")

a %>%
        html_nodes("div.footballbox") %>%
        html_nodes("td.fagoal") %>%
        `[[`(1) %>%
        html_nodes("a") %>%
        html_attr("title")

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






