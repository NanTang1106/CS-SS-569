library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(reshape2)
library(cluster)
library(circlize)

medici_dt <- read.csv('http://staff.washington.edu/kpleung/vis/data/medici.csv', sep=' ')
migrat_dt <- read.csv('http://staff.washington.edu/kpleung/vis/data/migrat2010.csv')

medici <- as.matrix(medici_dt)

medici_graph <- as_tbl_graph(medici, directed = F)
ggraph(medici_graph) +
  geom_node_point() +
  geom_edge_link() + 
  geom_node_text(aes(label = name), repel = TRUE)

medici_graph <-
  medici_graph %>%
  mutate(
    # Calculate degree centrality
    degree = centrality_degree(),
    # Implement community-detection algorithm
    community = group_edge_betweenness()
  )

ggraph(medici_graph) +
  geom_node_point(aes(size = degree, color = factor(community)),
                  show.legend = FALSE) +
  geom_edge_link() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph()

## migration 
migrat_graph <- migrat_dt %>% as_tbl_graph()
ggraph(migrat_graph)+
  geom_edge_link(alpha = 0.5) +
  geom_node_point() +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph()


migrat_dt %>%
  ggplot(aes(y = origRegion, x = destRegion, fill = flow)) +
  geom_tile(color = "white", size = 0.2) +
  coord_equal() +
  theme(panel.background = element_blank())

migrat_new <- expand(migrat_dt, origRegion, destRegion) %>%
  left_join(migrat_dt, by = c("origRegion", "destRegion"))

quantile(migrat_new$flow, na.rm = TRUE)

breaks <- c(1000, 5000, 10000, 50000, 100000, Inf)
labels <- c("1000 - 5000", "5000 - 10000", "10000 - 50000",
            "50000 - 100000", "100000 or more")

migrat_new$flowCat <- cut(migrat_new$flow, breaks, labels)

migrat_matrix <-
  migrat_new %>%
  acast(origRegion ~ destRegion, value.var = "flow")

migrat_hclust <-
  dist(migrat_matrix) %>%
  hclust(method = "ward.D") # Several other methods are available
countryOrder <- migrat_hclust$order

countryLevels <- unique(migrat_new$origRegion)[countryOrder]

migrat_new <- migrat_new %>%
  mutate(
    origRegion = factor(origRegion, levels = rev(countryLevels)),
    destRegion = factor(destRegion, levels = countryLevels)
  )

migrat_new %>%
  ggplot(aes(y = origRegion, x = destRegion, fill = flowCat)) +
  geom_tile(color = "white", size = 0.2) +
  # Scale fill values with "YlGnBu" palette and "grey90" for NAs
  scale_fill_brewer(palette = "YlGnBu", na.value = "grey90",
                    breaks = rev(labels)) +
  # Put x-axis labels on top
  scale_x_discrete(position = "top") +
  coord_equal() +
  theme(panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        # Rotate and align x-axis labels
        axis.text.x.top = element_text(angle = 90, hjust = 0),
        legend.key.height = grid::unit(0.8, "cm"),
        legend.key.width = grid::unit(0.2, "cm")
  ) +
  guides(fill = guide_legend(title = "Migration flow")) +
  labs(y = "Origin", x = "Destination")


