# required libraries
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(tidygraph)
library(ggrepel)
library(tm)
library(SnowballC)
library(lubridate)
library(ggmap)

# preparing data ####
# data frame containing dates and pdf names of blackouts
blackout_dates = readRDS("clean_dates_blackouts.rds")
blackout_dates$advert_source = paste0("Advert-", sprintf("%04d", 1:nrow(blackout_dates)))


# data frame containing power failures in nairobi
nairobi_blackouts = readRDS("nairobi_county_blackouts.rds")
nairobi_blackouts = nairobi_blackouts %>% left_join(select(blackout_dates, new_date, pdf_name, advert_source), by = "advert_source")
nairobi_blackouts$areas = trimws(nairobi_blackouts$areas, "both")
nairobi_blackouts$areas = gsub("\\s+", " ", nairobi_blackouts$areas)
nairobi_blackouts$areas = gsub(", ", ",", nairobi_blackouts$areas)
nairobi_blackouts$areas = gsub(" ", "-", nairobi_blackouts$areas)
nairobi_blackouts$areas = gsub(",", " ", nairobi_blackouts$areas)

# adding date and time of the blackouts
#adverts = readRDS("blackouts_indiv_areas_combined.rds") %>%
 # select(advert_source, date_time)
#nairobi_blackouts = nairobi_blackouts %>% left_join(adverts, by = "advert_source")

nairobi_blackouts_individual_areas =
  nairobi_blackouts %>% 
  unnest_tokens(output = individual_areas, input = areas, token = str_split, pattern = " ", to_lower = F) %>%
  count(individual_areas)

# network graph function ####
# function to transform data into an adjacency matrix and create a network graph
plot_network_graph = function(x, y, z){
  corpus = VCorpus(VectorSource(x))
  corpus = tm_map(corpus, content_transformer(tolower))
  tdm = TermDocumentMatrix(corpus, control = list(removePunctuation = FALSE, stopwords = FALSE))
  areas_matrix = as.matrix(tdm)
  areas_matrix[areas_matrix >= 1] = 1
  adjacency_matrix = areas_matrix %*% t(areas_matrix)
  graph_object = graph.adjacency(adjacency_matrix, weighted = TRUE, mode = "undirect")
  graph_object = simplify(graph_object)
  bipartite_object = get.data.frame(graph_object) %>% filter(weight >= y)
  graph_object = graph.data.frame(bipartite_object)
  V(graph_object)$label = V(graph_object)$name
  V(graph_object)$degree = degree(graph_object)
  nodes_data = z %>%
    filter(individual_areas %in% V(graph_object)$label) %>%
    arrange(individual_areas)
 tidy_graph_object = as_tbl_graph(graph_object) 
 tidy_graph_object = tidy_graph_object %>%
   arrange(label)
  V(tidy_graph_object)$counts = nodes_data$n 
  ggraph(tidy_graph_object) + geom_edge_link(col = "blue") +
    geom_node_point(aes(size = V(tidy_graph_object)$counts), col = "red") + theme_graph() +
    geom_node_text(aes(label = label), repel = TRUE) + scale_color_continuous(low = "#132B43", high = "#FF0000") +
    scale_size(range = c(0.5, 15)) + theme(legend.position = "none")
}

# all blackouts
tiff('nairobi county blackouts.tiff', units="in", width=12, height=12, res=300)
plot_network_graph(nairobi_blackouts$areas, 5, nairobi_blackouts_individual_areas)
dev.off()
# plotting per year
# 2014 blackouts ####
nairobi_blackouts_individual_areas_2014 =
  nairobi_blackouts %>% 
  filter(year(nairobi_blackouts$new_date) == 2014) %>%
  unnest_tokens(output = individual_areas, input = areas, token = str_split, pattern = " ", to_lower = F) %>%
  count(individual_areas)

nairobi_blackouts_2014 = 
  nairobi_blackouts %>% 
  filter(year(nairobi_blackouts$new_date) == 2014)
tiff('nairobi county blackouts 2014.tiff', units="in", width=12, height=12, res=300)
plot_network_graph(nairobi_blackouts_2014$areas, 2, nairobi_blackouts_individual_areas_2014)
dev.off()
# 2015 blackouts ####
nairobi_blackouts_individual_areas_2015 =
  nairobi_blackouts %>% 
  filter(year(nairobi_blackouts$new_date) == 2015) %>%
  unnest_tokens(output = individual_areas, input = areas, token = str_split, pattern = " ", to_lower = F) %>%
  count(individual_areas)

nairobi_blackouts_2015 = 
  nairobi_blackouts %>% 
  filter(year(nairobi_blackouts$new_date) == 2015)
tiff('nairobi county blackouts 2015.tiff', units="in", width=12, height=12, res=300)
plot_network_graph(nairobi_blackouts_2015$areas, 2, nairobi_blackouts_individual_areas_2015)
dev.off()

# 2016 blackouts ####
nairobi_blackouts_individual_areas_2016 =
  nairobi_blackouts %>% 
  filter(year(nairobi_blackouts$new_date) == 2016) %>%
  unnest_tokens(output = individual_areas, input = areas, token = str_split, pattern = " ", to_lower = F) %>%
  count(individual_areas)


nairobi_blackouts_2016 = 
  nairobi_blackouts %>% 
  filter(year(nairobi_blackouts$new_date) == 2016)

tiff('nairobi county blackouts 2016.tiff', units="in", width=12, height=12, res=300)
plot_network_graph(nairobi_blackouts_2016$areas, 2, nairobi_blackouts_individual_areas_2016)
dev.off()

# 2017 blackouts ####
nairobi_blackouts_individual_areas_2017 =
  nairobi_blackouts %>% 
  filter(year(nairobi_blackouts$new_date) == 2017) %>%
  unnest_tokens(output = individual_areas, input = areas, token = str_split, pattern = " ", to_lower = F) %>%
  count(individual_areas)


nairobi_blackouts_2017 = 
  nairobi_blackouts %>% 
  filter(year(nairobi_blackouts$new_date) == 2017)

tiff('nairobi county blackouts 2017.tiff', units="in", width=12, height=12, res=300)
plot_network_graph(nairobi_blackouts_2017$areas, 2, nairobi_blackouts_individual_areas_2017)
dev.off()

write.csv(nairobi_blackouts_individual_areas, "nairobi_county_blackouts_frequency.csv", row.names = F)
write.csv(nairobi_blackouts_individual_areas_2014, "nairobi_county_blackouts_frequency_2014.csv", row.names = F)
write.csv(nairobi_blackouts_individual_areas_2015, "nairobi_county_blackouts_frequency_2015.csv", row.names = F)
write.csv(nairobi_blackouts_individual_areas_2016, "nairobi_county_blackouts_frequency_2016.csv", row.names = F)
write.csv(nairobi_blackouts_individual_areas_2017, "nairobi_county_blackouts_frequency_2017.csv", row.names = F)

#nairobi county map ####
nairobi_blackouts_individual_areas_map = 
  nairobi_blackouts_individual_areas %>%
  mutate(individual_areas = gsub("-", " ", nairobi_blackouts_individual_areas$individual_areas)) %>%
  mutate_geocode(individual_areas)
#nairobi_blackouts_individual_areas_map1 = 
 # nairobi_blackouts_individual_areas %>%
  #mutate(individual_areas = paste(gsub("-", " ", nairobi_blackouts_individual_areas$individual_areas), "nairobi")) %>%
  #mutate_geocode(individual_areas)
nairobi_map = get_map(location = c(36.715, -1.41, 36.95, -1.205))
tiff('nairobi county blackouts map.tiff', units="in", width=12, height=12, res=300)
ggmap(nairobi_map) +
  geom_point(aes(x = lon, y = lat, size = n), data = nairobi_blackouts_individual_areas_map, col = "red") +
  scale_size(range = c(0.5, 15)) + theme(legend.position = "none")
dev.off()
write.csv(nairobi_blackouts_individual_areas_map, "nairobi_blackouts_individual_areas_map.csv", row.names = F)

# clean the data in excel then load it 
nairobi_blackouts_individual_areas_map = 
  read_csv("nairobi_blackouts_individual_areas_map_cleaned.csv") %>%
  filter(n > 1) %>%
  group_by(individual_areas) %>%
  filter(row_number() == 1)

#nairobi_blackouts_individual_areas_map =  nairobi_blackouts_individual_areas_map[!endsWith(nairobi_blackouts_individual_areas_map$individual_areas,"road"),] 
#top_areas = nairobi_blackouts_individual_areas_map %>%
 # arrange(desc(n)) %>%
  #filter(row_number() <= 10) %>%
  #rename("areas" = "individual_areas")

tiff('nairobi county blackouts map.tiff', units="in", width=12, height=12, res=300)
ggmap(nairobi_map) +
  geom_point(aes(x = lon, y = lat, size = n), data = nairobi_blackouts_individual_areas_map, col = "red") +
  scale_size(range = c(0.5, 20)) + theme(legend.position = "none") #+ geom_text_repel(data = top_areas, aes(label = areas))
dev.off()