library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

#import data
dt <- read.csv("C:/Users/cates/R Projects/phs-2023-000000000000 (2).csv")

#organize for summary plots
maps <- dt %>% group_by(map_name,map_type) %>% summarise(games = length(unique(esports_match_id))) %>% arrange(desc(games))

#summary by gamemode
ggplot(maps, aes(x=map_type, y=games, fill=map_type)) + geom_bar(stat = "identity", colour="black") +  
  coord_flip() + scale_fill_brewer(palette="Set3") + theme(legend.position = "none")

#summary by map/gamemode
ggplot(maps, aes(x=map_name, y=games, fill=map_type)) + geom_bar(stat = "identity", colour="black") +  
  coord_flip() + scale_fill_brewer(palette="Set3")

#intro to hero details
dt %>% filter(stat_name == 'Hero Damage Done' & hero_name != 'All Heroes' & hero_name != 'Lifeweaver' & hero_name != 'Junkrat' & hero_name != 'Mercy') %>% group_by(hero_name,esports_match_id,team_name, player_name) %>% summarise(dmg_per_game = mean(amount)) %>% ggplot(aes(x=hero_name, y=dmg_per_game, fill=hero_name)) + geom_boxplot() + theme(legend.position = "none") + coord_flip()

#subsetting hero list
dps <- dt %>% filter(hero_name != 'All Heroes' & stat_name == 'Hero Damage Done') %>% group_by(hero_name) %>% summarize(dmg = sum(amount)) %>% arrange(desc(dmg)) %>% slice(1:10) %>% select(hero_name)

dt %>% filter(hero_name != 'All Heroes' & stat_name == 'Hero Damage Done') %>% group_by(hero_name) %>% summarize(dmg = sum(amount)) %>% arrange(desc(dmg)) %>% slice(1:10) 

#re visualizing subsetted list
dt %>% filter(stat_name == 'Hero Damage Done' & hero_name %in% dps$hero_name) %>% group_by(hero_name,esports_match_id,team_name, player_name) %>% summarise(dmg_per_game = mean(amount)) %>% ggplot(aes(x=hero_name, y=dmg_per_game, fill=hero_name)) + geom_boxplot() + theme(legend.position = "none") +
  geom_jitter(alpha = 0.5) + coord_flip()

#pivoting data into more intuitive structure
dt_wider <- dt %>% filter(stat_name %in% c('Hero Damage Done','Eliminations') & hero_name != 'All Heroes') %>% pivot_wider(names_from="stat_name",values_from="amount")

dt_wider[is.na(dt_wider)] <- 0

hero_sum <- dt_wider %>% group_by(hero_name) %>% summarise(across(c(Eliminations, 'Hero Damage Done'), sum))

hero_sum$`Hero Damage Done` = hero_sum$`Hero Damage Done` / 1000

#comparing nominal performance
ggplot(hero_sum,aes(x=`Hero Damage Done`, y=Eliminations)) + geom_point() + geom_text(label=hero_sum$hero_name, nudge_x = 0.25, nudge_y = 0.25, check_overlap = T) + 
  geom_smooth(method=lm , color="red", se=FALSE)

#comparison basis
dt %>% filter(hero_name == 'Ramattra' & stat_name == 'Hero Damage Done') %>% select(tournament_title,map_type,player_name,hero_name,amount) %>% arrange(desc(amount)) %>% slice(1:15)

#comparison against
dt %>% filter(hero_name %in% c('Winston','Tracer','Sombra') & stat_name == 'Hero Damage Done') %>% select(tournament_title,map_type,player_name,hero_name,amount) %>% arrange(desc(amount)) %>% slice(1:15)

#visualizing the difference
dt %>% filter(hero_name %in% c('Ramattra','Tracer','Winston','Sombra') & stat_name == 'Hero Damage Done') %>% group_by(player_name, esports_match_id, map_name, hero_name) %>% summarise(game_damage = sum(amount)) %>% ggplot(aes(x = game_damage,fill = hero_name)) + geom_histogram(color="black") + 
  scale_fill_manual(values = c('purple','orchid','orange','wheat')) + facet_wrap(~hero_name)

#adding to pivot data
hero_time_played <- dt %>% filter(stat_name == 'Time Played' & hero_name != 'All Heroes') %>% pivot_wider(names_from="stat_name",values_from="amount") %>% rename('time' = `Time Played`)

#normalizing damage data by play time
hero_time_played[is.na(hero_time_played)] <- 0

time_sum <- hero_time_played %>% group_by(hero_name) %>% summarise(seconds_played = sum(time))

hero_pivot <- merge(x= hero_sum, y=time_sum, by='hero_name') %>% mutate(minutes_played = seconds_played / 60, damage_per_minute = `Hero Damage Done` / minutes_played, eliminations_per_minute = Eliminations / minutes_played )

hero_pivot %>% arrange(desc(Eliminations)) %>% slice(1:15) %>% ggplot(aes(x=damage_per_minute, y=eliminations_per_minute)) + geom_point() + geom_label(aes(label = hero_name))

#hardcoding hero roles for use
DPS <- c('Symmetra','Tracer','Cassidy','Sombra','Mei','Bastion','Sojourn','Pharah','Hanzo','Soldier: 76','Genji','Reaper','Widowmaker','Echo','Ashe','Torbjorn','Junkrat')
Support <- c('Ana','Lucio','Kiriko','Baptiste','Zenyatta','Mercy','Brigitte','Moira','Lifeweaver')
Tank <- c('Sigma','Winston','Ramattra','Wrecking Ball','D.Va','Doomfist','Reinhardt','Junker Queen','Roadhog','Zarya','Orisa')

hero_pivot <- hero_pivot %>% mutate(Role = ifelse(hero_name %in% DPS, 'DPS',ifelse(hero_name %in% Tank, 'Tank', 'Support')))
  
hero_pivot %>% ggplot(aes(x=damage_per_minute, y=eliminations_per_minute, color=Role, shape=Role)) + geom_point(size=6) + geom_text_repel(label=hero_pivot$hero_name, color="black")
