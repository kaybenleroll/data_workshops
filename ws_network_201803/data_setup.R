### Data Setup Script

# Re-using some code from Sec 1 of the workshop to avoid repeating myself
# in later worksheets.


# Setup the Florentine marriage network
data(flo, package = 'network')

florence_igraph <- graph_from_adjacency_matrix(flo, mode = 'undirected')

florentine_fr_layout <- ggnetwork(florence_igraph, 'fruchtermanreingold')


# Setup the lazega network
lazega_igraph  <- lazega %>% upgrade_graph()
lazega_network <- lazega %>% intergraph::asNetwork()
