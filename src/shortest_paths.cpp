/* Uses Dijkstra's algorithm to compute shortest paths between every node in
   a food web. Basically C but uses STL vector.
*/
#include <R.h>
#include <Rmath.h>
#include <vector>

#include "cheddar.h"

typedef std::vector<bool> BoolVector;
typedef std::vector<int> IntVector;
typedef std::vector<double> DoubleVector;

/*#define DEBUG_DIJKSTRA*/
#ifdef DEBUG_DIJKSTRA
#define DEBUG(X) { X; }
#else
#define DEBUG(X)
#endif
#ifdef DEBUG_DIJKSTRA


static void print_weights(const DoubleVector &weights, int n)
{
  for(int row=0; row<n; ++row)
  {
    for(int col=0; col<n; ++col)
    {
      Rprintf("%.0f ", weights[row + col*n]);
    }
    Rprintf("\n");
  }
}

static void print_adjacency(const IntVector &adjacency, int n)
{
  for(int i=0; i<n; ++i)
  {
    Rprintf("[%d] [%d] adjacent ->", i, adjacency[i]);
    for(int j = 1; j <= adjacency[i]; ++j)
      Rprintf(" %d", adjacency[i + n*j]);
    Rprintf("\n");
  }
}
#endif

static DoubleVector dijkstra(const IntVector &consumers,
                             const IntVector &resources,
                             const DoubleVector &weights,
                             int n)
{
  DEBUG(print_weights(weights, n));
  DEBUG(print_adjacency(resources, n));
  DEBUG(print_adjacency(consumers, n));
  DoubleVector lengths(n*n);
  /* All distances start as infinite */
  for(int i=0; i<n*n; ++i)
  {
    lengths[i] = R_PosInf;
  }
  /* Diagonal is 0.0 - no cost to moving nowhere */
  for(int i=0; i<n; ++i)
  {
    lengths[i + i*n] = 0.0;
  }
  for(int source=0; source<n; ++source)
  {
    DEBUG(Rprintf("source [%d]\n", source));
    BoolVector todo(n, true);
    while(TRUE)
    {
      /* Set u to index of node with smallest entry in dist[todo] */
      int u = -1;
      for(int i=0; i<n; ++i)
      {
        if(todo[i])
        {
          if(-1==u)   u = i;
          if(lengths[source + n*i] < lengths[source + n*u])
          {
            u = i;
          }
        }
      }
      if(-1==u || !R_FINITE(lengths[source + n*u]))
      {
        break;
      }
      todo[u] = false;
      DEBUG(Rprintf("u [%d]\n", u));
      for(int i=0; i<resources[u]; ++i)
      {
        const int v = resources[u + (1+i)*n];
        const double alt = lengths[source + n*u] + weights[v + u*n];
        if(alt<lengths[source + n*v])
        {
          DEBUG(Rprintf("res v [%d]\n", v));
          lengths[source + n*v] = alt;
        }
      }
      for(int i=0; i<consumers[u]; ++i)
      {
        const int v = consumers[u + (1+i)*n];
        const double alt = lengths[source + n*u] + weights[u + v*n];
        if(alt<lengths[source + n*v])
        {
          DEBUG(Rprintf("con v [%d]\n", v));
          lengths[source + n*v] = alt;
        }
      }
    }
  }
  return (lengths);
}

extern "C"
{
void shortest_paths(const int *consumers, const int *consumers_length,
                    const int *resources, const int *resources_length,
                    const double *weights,
                    const int *node_count,
                    double *lengths,
                    int *status)
{
  /* WARNING: Nested returns */
  /* The shortest paths between each node in a graph.
     In params:
     resources:
     consumers:   matrices of node_count rows. First column is the number of
                  resources (consumers) of that row. Subsequent columns are
                  ids of resources (consumers).
consumers_length: the number of ints in consumers
resources_length: the number of ints in resources
       weights:   n x n matrix of weights
    node_count:   Number of nodes in the network
    Out params:
       lengths:   vector of length n
        status:   0 - normal exit.
                  1 - problem with one of the parameters.
  */
  /* Quick and dirty parameter checks */
  if(0==resources || resources_length==0 || *resources_length<1 ||
     0==consumers || 0==consumers_length || *consumers_length<1 ||
     0==weights || 0==node_count || *node_count<1 || 0==lengths || 0==status)
  {
    if(0!=status)
    {
      *status = 1;
    }
    /* WARNING: Nested return */
    return;
  }
  *status = -1;
  try
  {
    IntVector con(consumers, consumers + *consumers_length);
    IntVector res(resources, resources + *resources_length);
    DoubleVector w(weights, weights + (*node_count * *node_count));
    DoubleVector r = dijkstra(con, res, w, *node_count);
    std::memcpy(lengths, &r[0], sizeof(double) * *node_count * *node_count);
    *status = 0;        // Normal exit
  }
  catch(const std::exception &e)
  {
    REprintf("Unexpected error in shortest_paths [%s]\n", e.what());
  }
  catch(...)
  {
    REprintf("Unexpected error in shortest_paths\n");
  }
}

}   // extern C
