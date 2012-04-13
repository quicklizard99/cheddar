# Stress the compiled code
R -d valgrind --vanilla < bin/minimise_diet_gaps.R && 
R -d valgrind --vanilla < bin/shortest_paths.R && 
R -d valgrind --vanilla < bin/trophic_chains.R

