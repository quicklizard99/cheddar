/* Enumerates every unique chain in a food web, using a breadth-first search. 

   Algorithm and first implemention by Rob Emmerson. 
   Converted to use C++ and integrated into R by Lawrence Hudson.
*/

#include <R.h>

#include <vector>
#include <queue>

typedef std::vector<int> IntVector;
typedef std::queue<IntVector> Queue;

//#define DEBUG_TC

#ifdef DEBUG_TC
#define DEBUG(X) { X; }
#else
#define DEBUG(X)
#endif

static void print_adjacency(const IntVector &adjacency, const int n)
{
  for(int i=0; i<n; ++i)
  {
    Rprintf("[%d] has [%d] consumers ->", i, adjacency[i]);
    for(int j = 1; j <= adjacency[i]; ++j)
      Rprintf(" %d", adjacency[i + n*j]);
    Rprintf("\n");
  }
}

static void print_path(const char *text, const IntVector &path)
{
  Rprintf("%s:", text);
  for(IntVector::const_iterator j=path.begin(); j!=path.end(); ++j)
    Rprintf(" %d", *j);
  Rprintf("\n");
}

static int append_chain(const IntVector &path, int *chains, int *n_chains_found, 
                        int *max_chains_length, int max_chains, int nodes)
{
  if(*n_chains_found < max_chains)
  {
    const IntVector::size_type path_length(path.size());
    memcpy(chains + *n_chains_found * nodes, &path[0], 
           path_length*sizeof(IntVector::value_type));
    *n_chains_found = *n_chains_found + 1;
    *max_chains_length = int(path_length) > *max_chains_length ? 
                         int(path_length) : 
                         *max_chains_length;
    return (1);
  }
  else
  {
    return (0);
  }
}

static int enum_chains(const IntVector &adjacency, 
                       const IntVector &is_basal, 
                       int nodes,  
                       int max_chains,  
                       int *chains,                // out
                       int *n_chains_found,        // out
                       int *max_chains_length)     // out
{
  // Enumerates unique trophic chains the network represented by adjancency.
  // Returns 0 if chains is full.

  // Initialize out params
  *n_chains_found = 0;
  *max_chains_length = 0;

  DEBUG(Rprintf("DUMP GRAPH\n"));
  DEBUG(print_adjacency(adjacency, nodes));

  Queue queue;

  for(int n = 0; n < nodes; ++n)
  {
    if(adjacency[n] == 0) continue; /* Node n has no out-edges, it cannot 
                                       start a chain */
    if(is_basal[n] == 0) continue; /* Node n has in-edges, so it cannot start 
                                      a chain */

    IntVector path;
    path.reserve(nodes);    // Will have at most this length
    path.push_back(n);

    queue = Queue();

    DEBUG(print_path("INITIAL PATH", path));

    queue.push(path);

    while(!queue.empty())
    {
      path = queue.front();
      queue.pop();

      IntVector::const_reference m = path.back();

      DEBUG(Rprintf("AT NODE [%d]\n", m));

      if(adjacency[m] == 0)
      {
        DEBUG(print_path("", path));
        if(!append_chain(path, chains, n_chains_found, max_chains_length, 
                         max_chains, nodes))
        {
          /* WARNING: Nested return */
          return (0);
        }
      }
      else
      {
        int cycle = 1;

        for(int j = 1; j <= adjacency[m]; ++j)
        {
          int found = 0;
          for(IntVector::const_iterator k=path.begin(); k!=path.end(); ++k)
          {
            if(*k == adjacency[m + nodes*j])
            {
              found = 1;
              break;
            }
          }

          if(!found)
          {
            path.push_back(adjacency[m + nodes*j]);
            DEBUG(print_path("EXTEND PATH", path));
            queue.push(path);

            path.pop_back();
            cycle = 0;
          }
        }

        if(cycle)
        {
          DEBUG(print_path("", path));
          if(!append_chain(path, chains, n_chains_found, max_chains_length, 
                           max_chains, nodes))
          {
            return (0);
          }
        }
      }
    }
  }

  return (1);
}

extern "C"
{
void trophic_chains(const int *adjacency, 
                    const int *adjacency_length,
                    const int *is_basal, 
                    const int *node_count,  
                    const int *max_chains, 
                    int *chains, 
                    int *n_chains_found, 
                    int *max_chains_length, 
                    int *status)
{
  /* WARNING: Nested returns */

  /* Enumerates every unique trophic path in a directed graph. Output is 
     written to chains. 

     In params:
     adjacency:   a matrix of node_count rows. First column is the number of 
                  consumers of that row. Subsequent columns are ids of 
                  consumers.

adjacency_length: the number of ints in adjacency

      is_basal:   an array of length node_count. 1 for nodes that have no 
                  resources, 0 otherwise. Chains start only with nodes which 
                  have a 1 in is_basal.

    node_count:   the number of nodes in the network.

    max_chains:   the number of columns in chains

    Out params:
        chains:   a matrix of node_count rows and max_chains columns. Each 
                  unique chain will be written to this matrix.

n_chains_found:   the number of unique chains found.

max_chain_length: the length of the longest chain found.    

          status: -1 - unexpected error.
                   0 - normal exit.
                   1 - problem with one of the parameters.
                   2 - chains is not large enough to enumerate all chains.
  */

  /* Quick and dirty parameter checks */
  if(0==adjacency || 0==adjacency_length || *adjacency_length<1 || 
     0==is_basal || 0==node_count || *node_count<1 || 
     0==chains || 0==max_chains || *max_chains<1 || 0==n_chains_found || 
     0==max_chains_length)
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
    IntVector adj(adjacency, adjacency + *adjacency_length);
    IntVector basal(is_basal, is_basal + *node_count);
    if(!enum_chains(adj, basal, *node_count, *max_chains, chains, 
                    n_chains_found, max_chains_length))
    {
      // No space in chains
      *status = 2;
    }
    else
    {
      // Normal exit
      *status = 0;
    }
  }
  catch(const std::exception &e)
  {
    REprintf("Unexpected error in trophic_chains() [%s]\n", e.what());
  }
  catch(...)
  {
    REprintf("Unexpected error in trophic_chains\n");
  }
}

}   // extern "c"
