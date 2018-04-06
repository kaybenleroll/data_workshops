### Data Setup Script

# Re-using some code from Sec 1 of the workshop to avoid repeating myself
# in later worksheets.

data(flo, package = 'network')

florence_igraph <- graph_from_adjacency_matrix(flo, mode = 'undirected')
