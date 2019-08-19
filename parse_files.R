

library("rvest")
library("stringr")
library("dplyr")

positions <- c("qb", "wr", "rb", "te", "k", "def")

# QB
qbs <- list()
file_count <- 4
for(i in 1:file_count){
  raw <- read_html(paste0("data_files/qb", i, ".html"))
  tab <- raw %>% html_nodes("table") %>% .[[2]] %>% html_table
  
  # clean up header row
  tab[1, ] <- gsub("[^0-9a-zA-Z]", "", tab[1, ])
  names(tab) <- paste(names(tab), tab[1, ], sep="_")
  tab <- tab[-1, ]
  tab <- tab[, -c(1, 3)]
  
  # parse player name
  tab <- tab %>% 
    mutate(player_name = str_extract(`_Offense`, 
                                     "[A-Z]{1}.+ [A-Z]{1}.+(?=\\s[A-Z]{1}[A-Za-z]+\\s\\-\\s)"),
           position = str_extract(`_Offense`, "(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+"),
           team_abbr = str_extract(`_Offense`, "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)"),
           player_info = paste(player_name, team_abbr, position, sep=" / ")) %>%
    select(player_info,
           rank = Rankings_Proj,
           bye = `_Bye`,
           fantasy_pts_proj = Fantasy_FanPts,
           position,
           pass_td_proj = Passing_TD,
           pass_yds_proj = Passing_Yds,
           pass_int_proj = Passing_Int,
           owner_2018 = `_Owner`)
  qbs[[i]] <- tab
} 
write.csv(do.call("rbind", qbs), file="output/qbs.csv", row.names=FALSE)

# WR  
wrs <- list()
file_count <- 6
for(i in 1:file_count){
  raw <- read_html(paste0("data_files/wr", i, ".html"))
  tab <- raw %>% html_nodes("table") %>% .[[2]] %>% html_table
  
  # clean up header row
  tab[1, ] <- gsub("[^0-9a-zA-Z]", "", tab[1, ])
  names(tab) <- paste(names(tab), tab[1, ], sep="_")
  tab <- tab[-1, ]
  tab <- tab[, -c(1, 3)]
  
  # parse player name
  tab <- tab %>% 
    mutate(player_name = str_extract(`_Offense`, 
                                     "[A-Z]{1}.+ [A-Z]{1}.+(?=\\s[A-Z]{1}[A-Za-z]+\\s\\-\\s)"),
           position = str_extract(`_Offense`, "(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+"),
           team_abbr = str_extract(`_Offense`, "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)"),
           player_info = paste(player_name, team_abbr, position, sep=" / ")) %>%
    select(player_info,
           rank = Rankings_Proj,
           bye = `_Bye`,
           fantasy_pts_proj = Fantasy_FanPts,
           position,
           rec_td_proj = Receiving_TD,
           rec_yds_proj = Receiving_Yds,
           rec_tgt_proj = Receiving_Tgt,
           owner_2018 = `_Owner`)
  wrs[[i]] <- tab
} 
write.csv(do.call("rbind", wrs), file="output/wrs.csv", row.names=FALSE)

# RB  
rbs <- list()
file_count <- 6
for(i in 1:file_count){
  raw <- read_html(paste0("data_files/rb", i, ".html"))
  tab <- raw %>% html_nodes("table") %>% .[[2]] %>% html_table
  
  # clean up header row
  tab[1, ] <- gsub("[^0-9a-zA-Z]", "", tab[1, ])
  names(tab) <- paste(names(tab), tab[1, ], sep="_")
  tab <- tab[-1, ]
  tab <- tab[, -c(1, 3)]
  
  # parse player name
  tab <- tab %>% 
    mutate(player_name = str_extract(`_Offense`, 
                                     "[A-Z]{1}.+ [A-Z]{1}.+(?=\\s[A-Z]{1}[A-Za-z]+\\s\\-\\s)"),
           position = str_extract(`_Offense`, "(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+"),
           team_abbr = str_extract(`_Offense`, "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)"),
           player_info = paste(player_name, team_abbr, position, sep=" / ")) %>%
    select(player_info,
           rank = Rankings_Proj,
           bye = `_Bye`,
           fantasy_pts_proj = Fantasy_FanPts,
           position,
           fantasy_pts_proj = Fantasy_FanPts,
           rush_td_proj = Rushing_TD,
           rush_yds_proj = Rushing_Yds,
           rush_att_proj = Rushing_Att,
           owner_2018 = `_Owner`)
  rbs[[i]] <- tab
} 
write.csv(do.call("rbind", rbs), file="output/rbs.csv", row.names=FALSE)

# TE  
tes <- list()
file_count <- 4
for(i in 1:file_count){
  raw <- read_html(paste0("data_files/te", i, ".html"))
  tab <- raw %>% html_nodes("table") %>% .[[2]] %>% html_table
  
  # clean up header row
  tab[1, ] <- gsub("[^0-9a-zA-Z]", "", tab[1, ])
  names(tab) <- paste(names(tab), tab[1, ], sep="_")
  tab <- tab[-1, ]
  tab <- tab[, -c(1, 3)]
  
  # parse player name
  tab <- tab %>% 
    mutate(player_name = str_extract(`_Offense`, 
                                     "[A-Z]{1}.+ [A-Z]{1}.+(?=\\s[A-Z]{1}[A-Za-z]+\\s\\-\\s)"),
           position = str_extract(`_Offense`, "(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+"),
           team_abbr = str_extract(`_Offense`, "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)"),
           player_info = paste(player_name, team_abbr, position, sep=" / ")) %>%
    select(player_info,
           rank = Rankings_Proj,
           bye = `_Bye`,
           fantasy_pts_proj = Fantasy_FanPts,
           position,
           fantasy_pts_proj = Fantasy_FanPts,
           rec_td_proj = Receiving_TD,
           rec_yds_proj = Receiving_Yds,
           rec_tgt_proj = Receiving_Tgt,
           owner_2018 = `_Owner`)
  tes[[i]] <- tab
} 
write.csv(do.call("rbind", tes), file="output/tes.csv", row.names=FALSE)

# K
ks <- list()
file_count <- 3
for(i in 1:file_count){
  raw <- read_html(paste0("data_files/k", i, ".html"))
  tab <- raw %>% html_nodes("table") %>% .[[2]] %>% html_table
  
  # clean up header row
  tab[1, ] <- gsub("[^0-9a-zA-Z]", "", tab[1, ])
  names(tab) <- paste(names(tab), tab[1, ], sep="_")
  tab <- tab[-1, ]
  tab <- tab[, -c(1, 3)]
  
  # parse player name
  tab <- tab %>% 
    mutate(player_name = str_extract(`_Kickers`, 
                                     "[A-Z]{1}.+ [A-Z]{1}.+(?=\\s[A-Z]{1}[A-Za-z]+\\s\\-\\s)"),
           position = str_extract(`_Kickers`, "(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+"),
           team_abbr = str_extract(`_Kickers`, "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)"),
           player_info = paste(player_name, team_abbr, position, sep=" / ")) %>%
    select(player_info,
           rank = Rankings_Proj,
           bye = `_Bye`,
           fantasy_pts_proj = Fantasy_FanPts,
           position,
           pat_made_proj = PAT_Made,
           owner_2018 = `_Owner`)
  ks[[i]] <- tab
} 
write.csv(do.call("rbind", ks), file="output/ks.csv", row.names=FALSE)

# DEF
defs <- list()
file_count <- 2
for(i in 1:file_count){
  raw <- read_html(paste0("data_files/def", i, ".html"))
  tab <- raw %>% html_nodes("table") %>% .[[2]] %>% html_table
  
  # clean up header row
  tab[1, ] <- gsub("[^0-9a-zA-Z]", "", tab[1, ])
  names(tab) <- paste(names(tab), tab[1, ], sep="_")
  tab <- tab[-1, ]
  tab <- tab[, -c(1, 3)]
  
  # parse player name
  tab <- tab %>% 
    mutate(player_name = str_extract(`_DefenseSpecialTeams`, 
                                     "(?<=\\s)[A-Z]{1}[a-z]{1,}[A-Za-z ]*(?=\\s[A-Z]{1}[a-zA-Z]{1,2})"),
           position = str_extract(`_DefenseSpecialTeams`, "(?<=[A-Z]{1}[A-Za-z]{1,2} - )[A-Z,]+"),
           team_abbr = str_extract(`_DefenseSpecialTeams`, "[A-Z]{1}[a-zA-Z]{1,2}(?=\\s\\-\\s)"),
           player_info = paste(player_name, team_abbr, position, sep=" / ")) %>%
    select(player_info,
           rank = Rankings_Proj,
           bye = `_Bye`,
           fantasy_pts_proj = Fantasy_FanPts,
           position,
           to_int_proj = Turnovers_Int,
           to_fum_proj = Turnovers_FumRec,
           sack_proj = Tackles_Sack,
           owner_2018 = `_Owner`)
  defs[[i]] <- tab
} 
write.csv(do.call("rbind", defs), file="output/defs.csv", row.names=FALSE)

# All
a <- lapply(list(qbs, wrs, rbs, tes, ks, defs), 
            function(x){
              tmp <- do.call("rbind", x)
              tmp <- tmp %>% select(player_info,
                                    rank,
                                    bye,
                                    owner_2018,
                                    fantasy_pts_proj)
              return(tmp)
            })
all <- do.call("rbind", a)
all <- all[order(as.numeric(all$rank)), ]
write.csv(all, file="output/all.csv", row.names=FALSE)

# last year rosters
ly <- all[all$owner_2018 != "FA", ]
ly <- ly[order(ly$owner_2018, as.numeric(ly$rank)), ]
write.csv(ly, file="output/ly.csv", row.names=FALSE)
