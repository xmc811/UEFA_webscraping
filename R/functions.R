

library(xml2)
library(rvest)
library(tidyverse)


get_match_time <- function(page) {
    
        time <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("time") %>%
                html_attr("datetime")    
        
        return(strptime(time, format = '%Y-%m-%dT%H:%M:%S', tz = "UTC"))
}

get_team_names <- function(page) {
        
        h_teams <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.fhome") %>%
                html_text() %>%
                str_trim()
        
        a_teams <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.faway") %>%
                html_text() %>%
                str_trim()
        
        return(list(h_teams, a_teams))
}


get_team_full_names <- function(page) {
        
        h_teams <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.fhome") %>%
                html_nodes("a") %>%
                html_attr("title") %>%
                `[`(seq(1,length(.),2))
        
        a_teams <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.faway") %>%
                html_nodes("a") %>%
                html_attr("title") %>%
                `[`(seq(2,length(.),2))
        
        return(list(h_teams, a_teams))
}


get_team_links <- function(page) {
        
        h_teams <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.fhome") %>%
                html_nodes("a") %>%
                html_attr("href") %>%
                `[`(seq(1,length(.),2)) %>%
                str_remove("/wiki/")
        
        a_teams <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.faway") %>%
                html_nodes("a") %>%
                html_attr("href") %>%
                `[`(seq(2,length(.),2)) %>%
                str_remove("/wiki/")
        
        return(list(h_teams, a_teams))
}


get_country_full_names <- function(page) {
        
        h_country <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.fhome") %>%
                html_nodes("a") %>%
                html_attr("title") %>%
                `[`(seq(2,length(.),2))
        
        a_country <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.faway") %>%
                html_nodes("a") %>%
                html_attr("title") %>%
                `[`(seq(1,length(.),2))
        
        return(list(h_country, a_country))
}


get_country_links <- function(page) {
        
        h_country <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.fhome") %>%
                html_nodes("a") %>%
                html_attr("href") %>%
                `[`(seq(2,length(.),2)) %>%
                str_remove("/wiki/")
        
        a_country <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.faway") %>%
                html_nodes("a") %>%
                html_attr("href") %>%
                `[`(seq(1,length(.),2)) %>%
                str_remove("/wiki/")
        
        return(list(h_country, a_country))
}


get_match_scores <- function(page) {
        
        scores <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("th.fscore") %>%
                html_text() %>%
                str_split("[–-]")
        
        h_scores <- scores %>%
                map_chr(.f = function(x) {x[1]}) %>%
                as.integer()
        
        a_scores <- scores %>%
                map_chr(.f = function(x) {x[2]}) %>%
                as.integer()
        
        return(list(h_scores, a_scores))
}


get_goals <- function(page) {
        
        
        
        
        
}

txt_score_to_df <- function(txt) {
        
        t <- txt %>%
                str_split("(?<=('|\\)))(?=[A-Za-z])") %>%
                `[[`(1)
        
        times <- str_count(t, "[0-9]{1,3}'")
        
        plys <- str_remove_all(t, "[0-9]{1,3}'|,|\\(o.g.\\)|\\(pen.\\)") %>% str_trim()
        
        ply_list <- map2(.x = plys, .y = times, .f = rep) %>% do.call("c", .)
        
        mins <- do.call("c", str_split(t, ",")) %>%
                str_extract("[0-9]{1,3}'([[:space:]]\\(.*\\))?") %>%
                str_extract("[0-9]{1,3}") %>%
                as.integer()
        
        og <- do.call("c", str_split(t, ",")) %>%
                str_detect("\\(o.g.\\)")
        
        pen <- do.call("c", str_split(t, ",")) %>%
                str_detect("\\(pen.\\)")
        
        df <- tibble(Player = ply_list,
                     Minutes = mins,
                     OG = og,
                     PK = pen)
        
        return(df)
}




