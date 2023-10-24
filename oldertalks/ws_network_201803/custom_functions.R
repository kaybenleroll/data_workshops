
run_network_model_assessment <- function(data_igraph, sample_func, n_iter = 1000) {
    sim_data_tbl <- data_frame(sim_id = 1:n_iter) %>%
        mutate(graph      = rerun(n_iter, sample_func())
              ,trans      = map_dbl(graph, transitivity)
              ,diam       = map_dbl(graph, diameter)
              ,meandist   = map_dbl(graph, mean_distance)
              ,max_degree = map_dbl(graph, function(x) x %>% igraph::degree() %>% max)
              ,n_comp     = map_dbl(graph, function(x) x %>% count_components)
              ,n_clust    = map_dbl(graph, function(x) x %>% cluster_fast_greedy() %>% length)
        )

    graph_vals_tbl <- data_frame(
        parameter = c('trans','diam','meandist', 'max_degree', 'n_comp', 'n_clust')
       ,graph_val = c(data_igraph %>% transitivity
                     ,data_igraph %>% diameter
                     ,data_igraph %>% mean_distance
                     ,data_igraph %>% igraph::degree() %>% max
                     ,data_igraph %>% count_components()
                     ,data_igraph %>% cluster_fast_greedy() %>% length
        )
    )

    plot_data_tbl <- sim_data_tbl %>%
        dplyr::select(-graph) %>%
        gather('parameter','value', -sim_id)


    assess_plot <- ggplot(plot_data_tbl) +
        geom_histogram(aes(x = value), bins = 50) +
        geom_vline(aes(xintercept = graph_val), colour = 'red', data = graph_vals_tbl) +
        facet_wrap(~parameter, scales = 'free') +
        scale_y_continuous(label = comma) +
        xlab('Value') +
        ylab('Count')


    return(list(
        sim_data_tbl   = sim_data_tbl
       ,graph_vals_tbl = graph_vals_tbl
       ,plot_data_tbl  = plot_data_tbl
       ,assess_plot    = assess_plot
    ))
}
