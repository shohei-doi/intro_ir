library(tidyverse)
library(rvest)

## veto

html_veto <- read_html("https://www.un.org/depts/dhl/resguide/scact_veto_table_en.htm")

df_veto <- html_veto %>% 
  html_node("table") %>% 
  html_table(header = FALSE) %>% 
  slice(-(1:3)) %>% 
  select(1:5) %>% 
  set_names(c("date", "draft", "record", "agenda", "country")) %>% 
  mutate(year = str_extract(date, "[0-9]{4}") %>% 
           parse_number(),
         period = (year - 1946) %/% 5,
         us = if_else(str_detect(country, "USA"), 1, 0),
         uk = if_else(str_detect(country, "UK"), 1, 0),
         fr = if_else(str_detect(country, "France"), 1, 0),
         ch = if_else(str_detect(country, "China"), 1, 0),
         ru = if_else(str_detect(country, "(Russia|USSR)"), 1, 0)) %>% 
  select(-country) %>% 
  pivot_longer(us:ru, names_to = "country", values_to = "veto") %>% 
  filter(veto == 1)

write_csv(df_veto, "data/unsc/veto.csv")

## pko

html_pko <- read_html("https://en.wikipedia.org/wiki/List_of_United_Nations_peacekeeping_missions")

dfs_pko <- html_pko %>% 
  html_nodes("table.wikitable") %>% 
  html_table()

region <- c("アフリカ", "アメリカ", "アジア", "ヨーロッパ", "中東", 
            "アフリカ", "アジア", "ヨーロッパ", "中東")

no_pko <- NULL

for(i in 1:length(dfs_pko)) {
  
  df_pko <- dfs_pko[[i]]
  
  for(j in 1:nrow(df_pko)) {
    year <- df_pko[j,1]
    start <- str_extract(year, "^[0-9]{4}")
    end <- str_extract(year, "[0-9]{4}$")
    
    if (i <= 5) {
      temp <- tibble(year = start:end,
                     region = region[i])
    } else {
      temp <- tibble(year = start:2022,
                     region = region[i])
    }
    
    no_pko <- bind_rows(no_pko, temp)
    
  }
  
}

type <- region[1:5]

df_pko <- left_join(expand(no_pko, year, region),
                    no_pko %>% 
                      count(year, region)) %>% 
  replace_na(list(n = 0))

write_csv(df_pko, "data/unsc/pko.csv")

## sc meeting

links_res <- read_html("https://research.un.org/en/docs/sc/quick/meetings/2022") %>% 
  html_nodes("ul.nav li ul.nav li a") %>% 
  html_attr("href")

df_sc <- NULL

for (link_res in links_res) {
  
  print(link_res)
  
  sc_tab <- read_html(link_res) %>% 
    html_node("table") %>% 
    html_table() %>% 
    set_names(1:ncol(.)) %>% 
    slice(-1)
  
  sc_tab <- sc_tab[,colSums(is.na(sc_tab)) == 0]
  
  if (ncol(sc_tab) == 4) {
    sc_tab <- sc_tab %>% 
      set_names(c("meeting", "date", "topic", "outcome"))
  } else if (ncol(sc_tab) == 5) {
    sc_tab <- sc_tab %>% 
      set_names(c("meeting", "date", "press", "topic", "outcome"))
  } else if (ncol(sc_tab) == 7) {
    sc_tab <- sc_tab %>% 
      set_names(c("meeting", "date", "record", "topic", "letter1", "letter2", "outcome"))
  } else {
    break
  }
  
  df_sc <- bind_rows(df_sc, 
                     sc_tab %>% 
                       mutate(year = str_extract(link_res, "[0-9]{4}") %>% 
                                parse_number()))
  
  Sys.sleep(1)
  
}

df_sc <- df_sc %>% 
  filter(meeting != date) %>% 
  mutate(action = case_when(str_detect(outcome, "RES") ~ "決議", 
                            str_detect(outcome, "PRST") ~ "議長声明", 
                            str_detect(outcome, "Communiqué") ~ "コミュニケ", 
                            TRUE ~ "会合") %>% 
           fct_relevel("会合", "コミュニケ", "議長声明", "決議"))

df_sc <- left_join(expand(df_sc, year, action) %>% 
                     drop_na(),
                   df_sc %>% 
                     count(year, action)) %>% 
  replace_na(list(n = 0))

write_csv(df_sc, "data/unsc/meeting.csv")
