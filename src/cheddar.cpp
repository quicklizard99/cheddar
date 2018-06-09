#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "cheddar.h"


extern "C"
{

R_NativePrimitiveArgType trophic_chains_size_args[] =
{
  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

R_NativePrimitiveArgType print_chains_args[] =
{
  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

R_NativePrimitiveArgType trophic_chains_args[] =
{
  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

R_NativePrimitiveArgType trophic_chains_stats_args[] =
{
  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
  INTSXP
};

R_NativePrimitiveArgType shortest_paths_args[] =
{
  INTSXP, INTSXP, INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, INTSXP
};

R_NativePrimitiveArgType sum_diet_gaps_args[] =
{
  INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
};

R_NativePrimitiveArgType minimise_sum_diet_gaps_args[] =
{
  INTSXP, INTSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP,
  INTSXP
};


static const R_CMethodDef cMethods[] = {
  {
    "trophic_chains_size", (DL_FUNC) &trophic_chains_size, 8,
    trophic_chains_size_args
  },
  {"print_chains", (DL_FUNC) &print_chains, 6, print_chains_args},
  {"trophic_chains", (DL_FUNC) &trophic_chains, 9, trophic_chains_args},
  {
    "trophic_chains_stats", (DL_FUNC) &trophic_chains_stats, 10,
    trophic_chains_stats_args
  },
  {"shortest_paths", (DL_FUNC) &shortest_paths, 8, shortest_paths_args},
  {"sum_diet_gaps", (DL_FUNC) &sum_diet_gaps, 5, sum_diet_gaps_args},
  {
    "minimise_sum_diet_gaps", (DL_FUNC) &minimise_sum_diet_gaps, 10,
    minimise_sum_diet_gaps_args
  },
  {NULL, NULL, 0, NULL},
};

void R_init_myLib(DllInfo *info) 
{
  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

} // extern C
