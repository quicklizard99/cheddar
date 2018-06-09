#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "cheddar.h"


extern "C"
{

static const R_CMethodDef cMethods[] = {
  {
    "trophic_chains_size", (DL_FUNC) &trophic_chains_size, 8,
    (R_NativePrimitiveArgType [])
    {
      INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
    }
  },
  {
    "print_chains", (DL_FUNC) &print_chains, 6,
    (R_NativePrimitiveArgType [])
    {
      INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
    }
  },
  {
    "trophic_chains", (DL_FUNC) &trophic_chains, 9,
    (R_NativePrimitiveArgType [])
    {
      INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
    }
  },
  {
    "trophic_chains_stats", (DL_FUNC) &trophic_chains_stats, 10,
    (R_NativePrimitiveArgType [])
    {
      INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
      INTSXP
    }
  },
  {
    "shortest_paths", (DL_FUNC) &shortest_paths, 8,
    (R_NativePrimitiveArgType [])
    {
      INTSXP, INTSXP, INTSXP, INTSXP, REALSXP, INTSXP, REALSXP, INTSXP
    }
  },
  {
    "sum_diet_gaps", (DL_FUNC) &sum_diet_gaps, 5,
    (R_NativePrimitiveArgType [])
    {
      INTSXP, INTSXP, INTSXP, INTSXP, INTSXP
    }
  },
  {
    "minimise_sum_diet_gaps", (DL_FUNC) &minimise_sum_diet_gaps, 10,
    (R_NativePrimitiveArgType [])
    {
      INTSXP, INTSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP, INTSXP, INTSXP,
      INTSXP
    }
  },
  {NULL, NULL, 0, NULL},
};

void R_init_myLib(DllInfo *info) 
{
  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

} // extern C
