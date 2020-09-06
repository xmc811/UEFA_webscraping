

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


get_match_meta <- function(page) {
        
        txt <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("[class='mobile-float-reset fright']") %>%
                html_nodes("div") %>%
                html_text() %>%
                `[`(seq(1,length(.),3)) %>%
                str_remove_all("\\[.*\\]")
        
        stadium <- do.call("c", str_split(txt, ",")) %>%
                `[`(seq(1,length(.),2)) %>%
                str_trim()
        
        city <- do.call("c", str_split(txt, ",")) %>%
                `[`(seq(2,length(.),2)) %>%
                str_trim()
        
        stadium_link <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("[class='mobile-float-reset fright']") %>%
                html_nodes("div") %>%
                `[`(seq(1,length(.),3)) %>%
                html_nodes("a") %>%
                html_attr("href") %>%
                str_subset("/wiki/") %>%
                `[`(seq(1,length(.),2)) %>%
                str_remove("/wiki/")
        
        city_link <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("[class='mobile-float-reset fright']") %>%
                html_nodes("div") %>%
                `[`(seq(1,length(.),3)) %>%
                html_nodes("a") %>%
                html_attr("href") %>%
                str_subset("/wiki/") %>%
                `[`(seq(2,length(.),2)) %>%
                str_remove("/wiki/")
        
        att <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("[class='mobile-float-reset fright']") %>%
                html_nodes("div") %>%
                html_text() %>%
                `[`(seq(2,length(.),3)) %>%
                str_extract("[0-9,]+") %>%
                str_remove_all(",") %>%
                as.integer()
        
        ref <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("[class='mobile-float-reset fright']") %>%
                html_nodes("div") %>%
                html_text() %>%
                `[`(seq(3,length(.),3)) %>%
                str_remove("Referee:") %>%
                str_remove("\\(.*\\)") %>%
                str_trim()
        
        ref_country <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("[class='mobile-float-reset fright']") %>%
                html_nodes("div") %>%
                html_text() %>%
                `[`(seq(3,length(.),3)) %>%
                str_remove("Referee:") %>%
                str_extract("\\(.*\\)") %>%
                str_remove_all("[()]")
        
        ref_link <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("[class='mobile-float-reset fright']") %>%
                html_nodes("div") %>%
                `[`(seq(3,length(.),3)) %>%
                html_nodes("a") %>%
                `[`(seq(1,length(.),2)) %>%
                html_attr("href") %>%
                str_remove("/wiki/")
        
        return(list(stadium, city, stadium_link, city_link, att, ref, ref_country, ref_link))
}


ns_to_df <- function(ns, team) {
        
        goals_df <- map(.x = ns, .f = node_to_goal_df, team = team)
        plyrs_df <- map(.x = ns, .f = node_to_plyr_df)
        
        goals_df <- map2(.x = goals_df, .y = plyrs_df, .f = inner_join, by = "Player")

        return(goals_df)
}


get_goals_df <- function(page) {
        
        h_df <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("td.fhgoal") %>%
                ns_to_df(team = "H")
        
        a_df <- page %>%
                html_nodes("div.footballbox") %>%
                html_nodes("td.fagoal") %>%
                ns_to_df(team = "A")
        
        df <- map2(.x = h_df, .y = a_df, .f = function(x, y) {rbind(x,y) %>% arrange(Minute)})
        
        return(df)
        
}

node_to_goal_df <- function(node, team) {
        
        txt <- node %>%
                html_text()
        
        df <- tibble(Player = character(),
                     Minute = integer(),
                     OG = logical(),
                     PK = logical(),
                     Team = character())
        
        if (txt == "") {
                return(df)
        }
        
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
                     Minute = mins,
                     OG = og,
                     PK = pen,
                     Team = team)
        
        return(df)
}



node_to_plyr_df <- function(node) {
        
        df <- tibble(Player = character(),
                     Player_Name = character(),
                     Player_Link = character())
        
        if (node %>%
            html_nodes("a") %>%
            html_text() %>%
            length() == 0) {
                
                return(df)
        }
        
        plyrs <- node %>%
                html_nodes("a") %>%
                html_text()
        
        plyrs_full <- node %>%
                html_nodes("a") %>%
                html_attr("title") %>%
                str_remove("\\(page.*exist\\)|\\(.*footballer.*\\)") %>%
                str_trim()
        
        plyrs_link <- node %>%
                html_nodes("a") %>%
                html_attr("href") %>%
                str_remove("/wiki/")
        
        df <- tibble(Player = plyrs,
                     Player_Name = plyrs_full,
                     Player_Link = plyrs_link) %>%
                filter(!Player %in% c('pen.', 'o.g.'))

        return(df)
} 



get_match_df <- function(page) {
        
        df <- tibble(Time = get_match_time(page),
                     Home_Team = get_team_names(page)[[1]],
                     Away_Team = get_team_names(page)[[2]],
                     Home_Team_Full = get_team_full_names(page)[[1]],
                     Away_Team_Full = get_team_full_names(page)[[2]],
                     Home_Team_Link = get_team_links(page)[[1]],
                     Away_Team_Link = get_team_links(page)[[2]],
                     Home_Country = get_country_full_names(page)[[1]],
                     Away_Country = get_country_full_names(page)[[2]],
                     Home_Country_Link = get_country_links(page)[[1]],
                     Away_Country_Link = get_country_links(page)[[2]],
                     Home_Score = get_match_scores(page)[[1]],
                     Away_Score = get_match_scores(page)[[2]],
                     Stadium = get_match_meta(page)[[1]],
                     City = get_match_meta(page)[[2]],
                     Stadium_Link = get_match_meta(page)[[3]],
                     City_Link = get_match_meta(page)[[4]],
                     Attendance = get_match_meta(page)[[5]],
                     Referee = get_match_meta(page)[[6]],
                     Referee_Country = get_match_meta(page)[[7]],
                     Referee_Link = get_match_meta(page)[[8]]
                     )
        
        return(df)
}




