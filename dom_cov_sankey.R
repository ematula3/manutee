

################# sankey diagram ##################

# load libraries

library(networkD3)
library(dplyr)
library(readr)
library(tidyr)


# color stuff
my_color <- 'd3.scaleOrdinal() .domain(["submersed 1", "rice 2"]) .range(["#E78B30","#88C425","#5391BD","#F0CC99"])'

# read in snkyPdata
snkyPdata <- read_csv("data/plant_dom_cov_wr22.csv")

# for (i in 1:nrow(snkyPdata)){
#   snkyPdata[i,"target"] <- paste(snkyPdata[i,"target"], " ")
# }

# define nodes
nodes <- data.frame(
  name=c(as.character(snkyPdata$source), 
         as.character(snkyPdata$target)) %>% 
    unique())

# add source and target fields
snkyPdata$IDsource <- match(snkyPdata$source, nodes$name)-1 
snkyPdata$IDtarget <- match(snkyPdata$target, nodes$name)-1


# Make the Network
plant_dom_cov_wr22 <- sankeyNetwork(Links = snkyPdata, 
                        Nodes = nodes,
                        Source = "IDsource", 
                        Target = "IDtarget",
                        Value = "value", 
                        NodeID = "name", 
                        width = 1500,
                        height = 600, 
                        fontSize=0,
                        colourScale = my_color,
                        iterations = 0,
                        sinksRight=FALSE,
                        margin = 1,)


snkyP2


saveNetwork(plant_dom_cov_wr22, "sankey_fin_what.html", selfcontained = TRUE)


