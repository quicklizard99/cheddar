extern "C"
{

void sum_diet_gaps(const int *network, const int *nodes,
                   const int *row_order,
                   int *sum_diet_gaps,
                   int *status);

void minimise_sum_diet_gaps(const int *network,
                            const int *nodes,
                            const double *T_start,
                            const double *T_stop,
                            const double *c,
                            const int *swaps_per_T,
                            const int *trace_anneal,
                            int *best_cost,
                            int *best,
                            int *status);

void trophic_chains_size(const int *adjacency,
                     const int *adjacency_length,
                     const int *is_basal,
                     const int *node_count,
                     const int *test_overflow,
                     const int *max_queue,
                     int *n_chains,
                     int *longest,
                     int *status);

void print_chains(const int *adjacency,
                  const int *adjacency_length,
                  const int *is_basal,
                  const int *node_count,
                  const int *max_queue,
                  int *status);

void trophic_chains(const int *adjacency,
                    const int *adjacency_length,
                    const int *is_basal,
                    const int *node_count,
                    const int *ncols,
                    const int *nrows,
                    const int *max_queue,
                    int *chains,
                    int *status);

void trophic_chains_stats(const int *adjacency,
                          const int *adjacency_length,
                          const int *is_basal,
                          const int *node_count,
                          const int *n_chains,
                          const int *longest,
                          const int *max_queue,
                          int *node_pos_counts,
                          int *chain_lengths,
                          int *status);

void shortest_paths(const int *consumers, const int *consumers_length,
                    const int *resources, const int *resources_length,
                    const double *weights,
                    const int *node_count,
                    double *lengths,
                    int *status);

}
