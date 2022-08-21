// visit unique chains in food webs.
#include <R.h>
#include <climits>
#include <vector>
#include <queue>
#include <map>
#include <algorithm>

#include "cheddar.h"
#include "cheddar_exception.h"

typedef std::vector<int> IntVector;

// Define DEBUG_TC to get lots of debug output
//#define DEBUG_TC
#ifdef DEBUG_TC
#define DEBUG(X) { X; }
#else
#define DEBUG(X)
#endif

std::vector<IntVector> Adjacency(const int *adjacency, int node_count)
{
  // Returns a C++ representation of the adjacency list in R matrix form
  std::vector<IntVector> adj;
  for(int node=0; node<node_count; ++node)
  {
    IntVector l;
    const int len = adjacency[node];
    for(int v=1; v<=len; ++v)
    {
      l.push_back(adjacency[node + v*node_count]);
    }
    adj.push_back(l);
  }
  return (adj);
}

template <typename Visitor> class TrophicChains
{
  // Enumerates every unique chain in a food web, using a breadth-first search.
  // A visitor is shown each unique chain. Visitors must implement a method:
  // void chain(const IntVector &);
  // Algorithm and first implemention by Rob Emerson.
  // Converted to use C++ and integrated into R by Lawrence Hudson.
  // TODO Stop when queue larger than a threshold
  // TODO Respect CTRL+C
private:
  const std::vector<IntVector> &adjacency_;
  const IntVector &is_basal_;
  typedef std::queue<IntVector> Queue;
  Queue::size_type max_queue_;

private:
#ifdef DEBUG_TC
  void print_adjacency() const
  {
    for(std::vector<IntVector>::size_type i=0; i<adjacency_.size(); ++i)
    {
      Rprintf("[%d] has [%d] consumers ->", i, adjacency_[i].size());
      for(int j=0; j<adjacency_[i].size(); ++j)
        Rprintf(" %d", adjacency_[i][j]);
      Rprintf("\n");
    }
  }

  void print_path(const char *text, const IntVector &path) const
  {
    Rprintf("%s:", text);
    for(IntVector::const_iterator j=path.begin(); j!=path.end(); ++j)
      Rprintf(" %d", *j);
    Rprintf("\n");
  }
#endif

public:
  TrophicChains(const std::vector<IntVector> &adjacency,
                const IntVector &is_basal,
                Queue::size_type max_queue) :
         adjacency_(adjacency),
         is_basal_(is_basal),
         max_queue_(max_queue)
  {
  }

  void visit(Visitor &visitor) const
  {
    // Visits unique trophic chains the network represented by adjancency.
    DEBUG(Rprintf("DUMP GRAPH\n"));
    DEBUG(print_adjacency());
    bool queue_warning = false;
    for(int n = 0; n < adjacency_.size(); ++n)
    {
      if(adjacency_[n].size() == 0) continue; // Node n has no out-edges, it
                                              // cannot start a chain
      if(is_basal_[n] == 0) continue;  // Node n has in-edges, so it cannot
                                       // start a chain
      IntVector path(1, n);
      DEBUG(print_path("INITIAL PATH", path));
      Queue queue;
      queue.push(path);
      while(!queue.empty())
      {
        path = queue.front();
        queue.pop();
        R_ProcessEvents();
        if(max_queue_>0 && !queue_warning && queue.size()>max_queue_/2)
        {
          REprintf("This network has a lot of paths, possibly too many to "
                   "compute\n");
          queue_warning = true;
        }
        else if(max_queue_>0 && queue.size()>max_queue_)
        {
          throw CheddarException("Unable to compute paths - see the help for "
                                 "TrophicChains for more information.");
        }
        int m = path.back();
        DEBUG(Rprintf("AT NODE [%d]\n", m));
        if(adjacency_[m].size() == 0)
        {
          DEBUG(print_path("", path));
          visitor.chain(path);
        }
        else
        {
          bool cycle = true;
          for(int j=0; j<adjacency_[m].size(); ++j)
          {
            bool found = false;
            for(IntVector::const_iterator k=path.begin(); k!=path.end(); ++k)
            {
              if(*k == adjacency_[m][j])
              {
                found = true;
                break;
              }
            }
            if(!found)
            {
              path.push_back(adjacency_[m][j]);
              DEBUG(print_path("EXTEND PATH", path));
              queue.push(path);
              path.pop_back();
              cycle = false;
            }
          }
          if(cycle)
          {
            DEBUG(print_path("", path));
            visitor.chain(path);
          }
        }
      }
    }
  }
};


class CollectChainsVisitor
{
  // Collects chains
private:
    int *chains_;     // An array of ncols_ * nrows_
    int ncols_;
    int nrows_;
    int n_chains_found_;

public:
    CollectChainsVisitor(int *chains,  int ncols, int nrows) :
      chains_(chains),
      ncols_(ncols),
      nrows_(nrows),
      n_chains_found_(0)
  {
  }

  void chain(const IntVector &path)
  {
    if(n_chains_found_ <= ncols_ && path.size()<=IntVector::size_type(nrows_))
    {
      const IntVector::size_type path_length(path.size());
      std::memcpy(chains_ + n_chains_found_ * nrows_, &path[0],
                  path_length*sizeof(IntVector::value_type));
      n_chains_found_ += 1;
    }
    else
    {
      DEBUG(Rprintf("Chains storage space exceeded\n"));
      DEBUG(Rprintf("nrows [%d] ncols [%d]\n", nrows_, ncols_));
      DEBUG(Rprintf("n_chains_found_ [%d]\n", n_chains_found_));
      DEBUG(Rprintf("path length [%d]\n", path.size()));
      throw CheddarException("Chains storage space exceeded");
    }
  }
};


class PrintChainsVisitor
{
  // Prints chains to stdout in the same format as Rob's solution8
public:
  void chain(const IntVector &path)
  {
    // Rprintf is an order of magnitude slower that printf (on my Mac, at
    // least) and isn't relevant in this case but we have no choice but to use
    // it because of R CMD check
    Rprintf(":");
    for(IntVector::size_type j=0; j<path.size(); ++j)
      Rprintf(" %d", 1+path[j]);
    Rprintf("\n");
  }
};


class CollectChainLengthsVisitor
{
  // Records the number of chains and the length of the longest chain.
public:
  int longest_;
  int n_chains_;

public:
  CollectChainLengthsVisitor(bool test_overflow=false) : longest_(0),
                                                         n_chains_(0)
  {
    if(test_overflow)
    {
      n_chains_ = INT_MAX;
    }
  }

  void chain(const IntVector &path)
  {
    longest_ = std::max(longest_, int(path.size()));
    if(INT_MAX>n_chains_)
    {
      n_chains_ += 1;
    }
    else
    {
      throw CheddarException("Too many chains to count without overflow");
    }
  }
};


class ChainStatsVisitor
{
  // Records the length of each chains and the number of times that each node
  // appears in a position in a chain.
public:
  typedef std::vector<IntVector> CountVector;
  CountVector counts_;
  IntVector chain_lengths_;    // A vector of chain lengths

public:
  ChainStatsVisitor(int nodes, int longest) : counts_(nodes)
  {
    for(CountVector::iterator it=counts_.begin(); it!=counts_.end(); ++it)
    {
      it->resize(longest);
    }
  }

  void chain(const IntVector &path)
  {
    chain_lengths_.push_back(path.size()-1);
    for(IntVector::size_type position=0; position<path.size(); ++position)
    {
      IntVector::const_reference node = path[position];
      counts_[node][position] += 1;
    }
  }

#ifdef DEBUG_TC
  void print() const
  {
    Rprintf("Chain position counts\n");
    for(CountVector::size_type node=0; node<counts_.size(); ++node)
    {
      CountVector::const_reference v = counts_[node];
      Rprintf("[%d] ", node);
      for(CountVector::value_type::size_type pos=0; pos!=v.size(); ++pos)
      {
        Rprintf("%d:%lu, ", pos, v[pos]);
      }
      Rprintf("\n");
    }
  }
#endif
};


extern "C"
{
void trophic_chains_size(const int *adjacency,
                         const int *adjacency_length,
                         const int *is_basal,
                         const int *node_count,
                         const int *test_overflow,
                         const int *max_queue,
                         int *n_chains,
                         int *longest,
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
 test_overflow:   if non-zero then check the overflow case for counting chains.
     max_queue:   the maximum alowabe size of the queue used to compute chains.
                  0 indicates no limit.
    Out params:
      n_chains:   the number of unique chains found.
       longest:   the number of nodes in the longest chain found.
        status: -1 - unexpected error.
                 0 - normal exit.
                 1 - problem with one of the parameters.
*/
  /* Quick and dirty parameter checks */
  if(0==adjacency || 0==adjacency_length || *adjacency_length<1 ||
     0==is_basal || 0==node_count || *node_count<1 || 0==test_overflow ||
     0==max_queue || *max_queue<0 ||
     0==n_chains || 0==longest || 0==status)
  {
    if(0!=status)
    {
      *status = 1;
    }
    /* WARNING: Nested return */
    return;
  }
  *status = -1;    // Default to an error status code
  try
  {
    std::vector<IntVector> adj = Adjacency(adjacency, *node_count);
    IntVector basal(is_basal, is_basal + *node_count);
    TrophicChains<CollectChainLengthsVisitor> worker(adj, basal, *max_queue);
    CollectChainLengthsVisitor visitor(0!=*test_overflow);
    worker.visit(visitor);
    *n_chains = visitor.n_chains_;
    *longest = visitor.longest_;
    // Normal exit
    *status = 0;
  }
  catch(const std::exception &e)
  {
    REprintf("Unexpected error in trophic_chains_size [%s]\n", e.what());
  }
  catch(...)
  {
    REprintf("Unexpected error in trophic_chains_size\n");
  }
}

void print_chains(const int *adjacency,
                  const int *adjacency_length,
                  const int *is_basal,
                  const int *node_count,
                  const int *max_queue,
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
         ncols:   the number of columns in chains.
         nrows:   the number of rows in chains.
     max_queue:   the maximum alowabe size of the queue used to compute chains.
                  0 indicates no limit.
     Out params:
        status: -1 - unexpected error.
                 0 - normal exit.
                 1 - problem with one of the parameters.
  */
  /* Quick and dirty parameter checks */
  if(0==adjacency || 0==adjacency_length || *adjacency_length<1 ||
     0==is_basal || 0==node_count || 0==max_queue || *max_queue<0 ||
     0==status)
  {
    if(0!=status)
    {
      *status = 1;
    }
    /* WARNING: Nested return */
    return;
  }
  *status = -1;    // Default to an error status code
  try
  {
    std::vector<IntVector> adj = Adjacency(adjacency, *node_count);
    IntVector basal(is_basal, is_basal + *node_count);
    TrophicChains<PrintChainsVisitor> worker(adj, basal, *max_queue);
    PrintChainsVisitor visitor;
    worker.visit(visitor);
    // Normal exit
    *status = 0;
  }
  catch(const std::exception &e)
  {
    REprintf("Unexpected error in print_chains[%s]\n", e.what());
  }
  catch(...)
  {
    REprintf("Unexpected error in print_chains\n");
  }
}

void trophic_chains(const int *adjacency,
                    const int *adjacency_length,
                    const int *is_basal,
                    const int *node_count,
                    const int *ncols,
                    const int *nrows,
                    const int *max_queue,
                    int *chains,
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
         ncols:   the number of columns in chains.
         nrows:   the number of rows in chains.
     max_queue:   the maximum alowabe size of the queue used to compute chains.
                  0 indicates no limit.
    Out params:
        chains:   a matrix of node_count rows and max_chains columns. Each
                  unique chain will be written to this matrix.
          status: -1 - unexpected error.
                   0 - normal exit.
                   1 - problem with one of the parameters.
  */
  /* Quick and dirty parameter checks */
  if(0==adjacency || 0==adjacency_length || *adjacency_length<1 ||
     0==is_basal || 0==node_count || *node_count<1 ||
     0==max_queue || *max_queue<0 ||
     0==chains || 0==ncols || *ncols<1 ||
     0==nrows || *nrows<1 || 0==status)
  {
    if(0!=status)
    {
      *status = 1;
    }
    /* WARNING: Nested return */
    return;
  }
  *status = -1;    // Default to an error status code
  try
  {
    std::vector<IntVector> adj = Adjacency(adjacency, *node_count);
    IntVector basal(is_basal, is_basal + *node_count);
    TrophicChains<CollectChainsVisitor> worker(adj, basal, *max_queue);
    CollectChainsVisitor visitor(chains, *ncols, *nrows);
    worker.visit(visitor);
    // Normal exit
    *status = 0;
  }
  catch(const std::exception &e)
  {
    REprintf("Unexpected error in trophic_chains[%s]\n", e.what());
  }
  catch(...)
  {
    REprintf("Unexpected error in trophic_chains\n");
  }
}

void trophic_chains_stats(const int *adjacency,
                          const int *adjacency_length,
                          const int *is_basal,
                          const int *node_count,
                          const int *n_chains,
                          const int *longest,
                          const int *max_queue,
                          int *node_pos_counts,
                          int *chain_lengths,
                          int *status)
{
  /* WARNING: Nested returns */
  /* Enumerates every unique trophic path in a directed graph. Outputs are the
     mean, minimum and maximum position of each node in every chain.
     In params:
     adjacency:   a matrix of node_count rows. First column is the number of
                  consumers of that row. Subsequent columns are ids of
                  consumers.
adjacency_length: the number of ints in adjacency
      is_basal:   an array of length node_count. 1 for nodes that have no
                  resources, 0 otherwise. Chains start only with nodes which
                  have a 1 in is_basal.
    node_count:   the number of nodes in the network.
      n_chains:   the number of chains.
       longest:   the number of nodes in the longest chain.
     max_queue:   the maximum alowabe size of the queue used to compute chains.
                  0 indicates no limit.
    Out params:
node_pos_counts:  an array of n_chains * longest integers. Will contain
                  counts of the number of times each node appears at a given
                  position in the chain.
  chain_lengths:  an array of n_chains integers. Will contain the length of
                  each chain.
        status: -1 - unexpected error.
                 0 - normal exit.
                 1 - problem with one of the parameters.
  */
  /* Quick and dirty parameter checks */
  if(0==adjacency || 0==adjacency_length || *adjacency_length<1 ||
     0==is_basal || 0==node_count || *node_count<1 ||
     0==n_chains || *n_chains<1 || 0==longest || *longest<1 ||
     0==max_queue || *max_queue<0 ||
     0==node_pos_counts || 0==chain_lengths || 0==status)
  {
    if(0!=status)
    {
      *status = 1;
    }
    /* WARNING: Nested return */
    return;
  }
  *status = -1;    // Default to an error status code
  try
  {
    std::vector<IntVector> adj = Adjacency(adjacency, *node_count);
    IntVector basal(is_basal, is_basal + *node_count);
    TrophicChains<ChainStatsVisitor> worker(adj, basal, *max_queue);
    ChainStatsVisitor visitor(*node_count, *longest);
    worker.visit(visitor);
    if(sizeof(IntVector::value_type)!=sizeof(*chain_lengths) ||
          sizeof(IntVector::value_type)!=sizeof(*node_pos_counts))
    {
      throw CheddarException("Unexpected type size");
    }
    else if(visitor.chain_lengths_.size()!=IntVector::size_type(*n_chains))
    {
      throw CheddarException("Unexpected number of chains");
    }
    else if(visitor.counts_.size()!=IntVector::size_type(*node_count))
    {
      throw CheddarException("Unexpected number of nodes");
    }
    else
    {
      std::memcpy(chain_lengths, &visitor.chain_lengths_[0], sizeof(int) * *n_chains);
      for(ChainStatsVisitor::CountVector::size_type node=0;
          node<visitor.counts_.size(); ++node)
      {
        ChainStatsVisitor::CountVector::const_reference v = visitor.counts_[node];
        if(v.size()!=ChainStatsVisitor::CountVector::value_type::size_type(*longest))
        {
          throw CheddarException("Unexpected number of node position counts");
        }
        else
        {
          std::memcpy(node_pos_counts + node * *longest, &v[0],
                 sizeof(int) * *longest);
        }
      }
    }
    // Normal exit
    *status = 0;
  }
  catch(const std::exception &e)
  {
    REprintf("Unexpected error in trophic_chains_stats [%s]\n", e.what());
  }
  catch(...)
  {
    REprintf("Unexpected error in trophic_chains_stats\n");
  }
}

}   // extern "c"
