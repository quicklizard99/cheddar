// Minimises diet gaps in food webs using simulated annealing learning as 
// described by Stouffer et al (2006) PNAS.

#include <R.h>

#include <algorithm>
#include <functional>
#include <vector>

#include "cheddar_exception.h"

//#define DEBUG_ANNEAL

#ifdef DEBUG_ANNEAL
#define DEBUG(X) { X; }
#else
#define DEBUG(X)
#endif

//#define DEBUG_COST_FUNCTION

#ifdef DEBUG_COST_FUNCTION
#define DEBUG_COST(X) { X; }
#else
#define DEBUG_COST(X)
#endif

//#define DEBUG_RANDOM_NUMBER_POOL

#ifdef DEBUG_RANDOM_NUMBER_POOL
#define DEBUG_RAND(X) { X; }
#else

#define DEBUG_RAND(X)
#endif

typedef std::vector<int> IntVector;

class SumDietGap
{
  private:
   IntVector network_;
   IntVector::size_type nodes_;
   IntVector consumers_;

  public:
    typedef IntVector State;
    typedef int Cost;

    SumDietGap(const IntVector &network, IntVector::size_type n) : 
                                                  network_(network), nodes_(n)
    {
      // network_ should be a square matrix, i.e. of size nodes x nodes
      if((nodes_*nodes_) != network.size())
      {
        throw CheddarException("Unexpected network size");
      }
      else if(nodes_>IntVector::size_type(INT_MAX))
      {
        throw CheddarException("Network too big to guarantee avoiding overflow");
      }
      else
      {
        // Count diet gaps only for consumers with two or more resources. 
        // Those with fewer than two resources can not contribute to diet gaps. 
        for(IntVector::size_type consumer=0; consumer<nodes_; ++consumer)
        {
          if(std::count(network_.begin() + consumer*nodes_, 
                        network_.begin() + (1+consumer)*nodes_, 1)>1)
          {
            consumers_.push_back(consumer);
          }
        }

        DEBUG_COST(Rprintf("Network with [%d] nodes and [%d] consumers\n[", 
                           nodes_, consumers_.size()));
        DEBUG_COST(for(IntVector::size_type i=0; i<consumers_.size(); ++i) 
                     Rprintf("%d ",consumers_[i]));
        DEBUG_COST(Rprintf("]\n"));
      }
    }

    State initial() const
    {
      State initial(nodes_);
      for(IntVector::size_type i=0; i<nodes_; ++i) initial[i] = i;
      return (initial);
    }

    Cost cost(const State &row_order) const
    {
      /* The sum of gaps in diets over consumers in network, given row_order.
         consumers should be an an array of nodes that have two or more 
         resources. Only the diet gaps of nodes in consumers are checked. 
         Most networks will have resources and/or consumers with only one 
         resource, so this scheme gives an improvement (over checking all 
         nodes) in performance of Intervality(). These figures relate to my 
         old C implementation: 

         With consumers array:
            > system.time(a <- Intervality(TL84))
               user  system elapsed 
              0.232   0.000   0.234 
            > system.time(a <- Intervality(YthanEstuary))
               user  system elapsed 
              0.704   0.000   0.703 

         Without consumers array:
            > system.time(a <- Intervality(TL84))
               user  system elapsed 
              0.392   0.000   0.392 
            > system.time(a <- Intervality(YthanEstuary))
               user  system elapsed 
              1.124   0.004   1.131 

         The obvious, bone-headed solution of finding start and end of diet and 
         counting the number of 0s in between is the quickest of all that I 
         tried. 

         Overflow of sum_diet_gaps should never happen because a nodes_ by 
         nodes_ matrix can have at most (nodes-1)^2 gaps.
      */
      Cost sum_diet_gaps = 0;
      const IntVector::size_type n = nodes_;

      for(IntVector::size_type consumer_i=0; consumer_i < consumers_.size(); 
          ++consumer_i)
      {
        const IntVector::value_type consumer = consumers_[consumer_i];
        DEBUG_COST(Rprintf("Consumer [%d] is [%d]\n", consumer_i, consumer));
        const IntVector::value_type consumer_col_offset = consumer*n;

        /* Find the start of the diet */
        IntVector::size_type start_of_diet = network_.max_size();
        for(IntVector::size_type i=0; i<n; ++i)
        {
          if(network_[ consumer_col_offset + row_order[i] ])
          {
            start_of_diet = i;
            break;
          }
        }

        if(start_of_diet != network_.max_size())
        {
          /* Find the end of the diet */
          IntVector::size_type end_of_diet = network_.max_size();
          for(IntVector::size_type i=(n-1); i>start_of_diet; --i)
          {
            if(network_[ consumer_col_offset + row_order[i] ])
            {
              end_of_diet = i;
              break;
            }
          }

          if(end_of_diet != network_.max_size())
          {
            DEBUG_COST(Rprintf("Consumer [%d] diet range [%d] [%d]\n", consumer,
                               start_of_diet, end_of_diet));

            /* Sum number of 0s in between start and end */
            for(IntVector::size_type  i=1+start_of_diet; i<end_of_diet; ++i)
            {
              if(!network_[ consumer_col_offset + row_order[i] ])
              {
                ++sum_diet_gaps;
              }
            }
          }
        }
      }
      return (sum_diet_gaps);
    }
};


class R_RNG
{
  // A lock on the R random number generator
private:
  R_RNG(const R_RNG &);              // Not implemented
  R_RNG& operator=(const R_RNG&);    // Not implemented

public:
  R_RNG()
  {
    GetRNGstate();
  }

  ~R_RNG()
  {
    PutRNGstate();
  }
};

typedef std::vector<double> DoubleVec;
class Random
{
  // A pool of uniformly drawn random numbers
  public:
    Random(DoubleVec::size_type pool_size=1e5) : pool_size_(pool_size), 
                                                 current_(pool_.end())
    {
    }

    double fetch_rand() const
    {
      // Returns a random number. 
      if(current_==pool_.end())
      {
        populate();
      }

      return (*current_++);
    }

  private:
    DoubleVec::size_type pool_size_;
    mutable DoubleVec pool_;
    mutable DoubleVec::const_iterator current_;

    void populate() const
    {
      // Fills the array rand with pool_size_ uniformly drawn numbers
      DEBUG_RAND(Rprintf("Generating [%d] random numbers\n", pool_size_));

      DoubleVec new_pool(pool_size_);
      if(TRUE)
      {
        R_RNG rng;
        std::generate(new_pool.begin(), new_pool.end(), unif_rand);
      }
      pool_.swap(new_pool);
      current_ = pool_.begin();
    }
};

template <typename Problem> class SimulatedAnnealing
{
  // Simple implementation of simulated annealing learning. 

  public: 
    typedef std::pair<typename Problem::State, typename Problem::Cost> Result;

    SimulatedAnnealing(double T_start = 1000,
                       double T_stop = 1,
                       double c = 0.99, 
                       unsigned int swaps_per_T = 1000, 
                       bool trace_anneal = true) : T_start_(T_start), 
                                                   T_stop_(T_stop),
                                                   c_(c),
                                                   swaps_per_T_(swaps_per_T), 
                                                   trace_anneal_(trace_anneal)
    {
      if(T_start<T_stop || T_stop<=0 || c<=0 || c>=1 || swaps_per_T<=0)
      {
        throw CheddarException("Bad annealing parameters");
      }
    }

    Result optimise(SumDietGap *problem)
    {
      // Returns a pair of state and cost of best solution found.
      unsigned int state_changes_better = 0;
      unsigned int state_changes_worse = 0;
      unsigned int total_state_changes = 0;
      double T = T_start_;

      typename Problem::State current = problem->initial();
      typename Problem::Cost cost_current = problem->cost(current);

      if(trace_anneal_)
      {
        Rprintf("Simulated annealing learning starting at T [%.5f], ",T_start_);
        Rprintf("stopping at T [%.5f], ", T_stop_);
        Rprintf("cooling by [%.5f], ", c_);
        Rprintf("with maximum [%d] swaps at each T.\n", swaps_per_T_);
      }

      while(T>T_stop_ && cost_current>0)
      {
        state_changes_better = state_changes_worse = 0;

        /* Warning: non-canonical for loop */
        for(unsigned int i=0; i<swaps_per_T_ && cost_current>0; ++i)
        {
          /* Swap two randomnly selected species */
          typedef typename Problem::State::size_type swap_size;
          const swap_size a = swap_size(current.size()*rand_pool_.fetch_rand());
          const swap_size b = swap_size(current.size()*rand_pool_.fetch_rand());
          if(a!=b)
          {
            DEBUG(Rprintf("Swapping [%d] [%d]\n", a, b));

            typename Problem::State consider = current;

            /* Swap the rows */
            if(TRUE)
            {
                typename Problem::State::value_type temp = consider[a];
                consider[a] = consider[b];
                consider[b] = temp;
            }

            /* Cost of the solution under consideration */
            typename Problem::Cost cost_consider = problem->cost(consider);

            DEBUG(Rprintf("Cost current [%d] cost consider [%d]\n", 
                          cost_current, cost_consider));
            bool accept = false;
            if(cost_consider <= cost_current)
            {
              DEBUG(Rprintf("Accepting as cost_consider <= cost_current\n"));
              accept = true;
              ++state_changes_better;
            }
            else
            {
              const double unif = rand_pool_.fetch_rand();
              const double p = exp(-(cost_consider-cost_current)/T);
              DEBUG(Rprintf("Accept if p [%.6f] > unif [%.6f] at T [%.4f]\n", 
                            p, unif, T));
              if(p > unif)
              {
                ++state_changes_worse;
                accept = true;
              }
            }

            if(accept)
            {
              cost_current = cost_consider;
              std::swap(current, consider);
            }
          }
        }

        total_state_changes = total_state_changes + state_changes_better + 
                              state_changes_worse;

        if(trace_anneal_)
        {
          Rprintf("Finished at T [%.5f]. ", T);
          Rprintf("Accepted [%u] better and ",state_changes_better);
          Rprintf("[%u] worse. ", state_changes_worse);
          Rprintf("Total changes so far [%u]. ", total_state_changes);
          Rprintf("Current cost [%u].\n", cost_current);
        }

        T = c_ * T;
      }

      return Result(current, cost_current);
    }

  private:
    double T_start_;
    double T_stop_;
    double c_;
    unsigned int swaps_per_T_;
    bool trace_anneal_;

    Random rand_pool_;
};

extern "C"
{

void sum_diet_gaps(const int *network, const int *nodes, 
                   const int *row_order, 
                   int *sum_diet_gaps, 
                   int *status)
{
  /* WARNING: Nested returns */

  /* The sum of gaps in diets over consumers in network

     In params:

       network:  A predation matrix of nodes x nodes

         nodes:  The number of nodes in the predation matrix

     row_order:  An array of length nodes containing the ordering of the 
                 rows in the predation matrix

    Out params:
 sum_diet_gaps:  The sum diet gap of the best solution that was found

        status: -1 - unexpected error.
                 0 - normal exit.
                 1 - problem with one of the parameters.

  */

  if(0==network || 0==nodes || *nodes<1 || 0==row_order || 0==sum_diet_gaps || 
     0==status)
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
    if(sizeof(SumDietGap::Cost) < sizeof(*sum_diet_gaps))
    {
      // Sanity check
      throw CheddarException("Unexpected cost size");
    }
    else
    {
      SumDietGap::State roworder(row_order, row_order + *nodes);
      SumDietGap worker(IntVector(network, network + *nodes * *nodes), *nodes);
      const SumDietGap::Cost cost = worker.cost(roworder);
      if(cost>INT_MAX)
      {
        // Sanity check
        // Overflow should not be possible because a nodes by nodes matrix can 
        // have at most (nodes-1)^2 gaps.
        throw CheddarException("Too many gaps to count without overflow");
      }
      else
      {
        *sum_diet_gaps = int(cost);
        *status = 0;        // Normal exit
      }
    }
  }
  catch(const std::exception &e)
  {
    REprintf("Unexpected error in sum_diet_gap [%s]\n", e.what());
  }
  catch(...)
  {
    REprintf("Unexpected error in sum_diet_gap\n");
  }  
}

void minimise_sum_diet_gaps(const int *network, 
                            const int *nodes, 
                            const double *T_start, 
                            const double *T_stop, 
                            const double *c,
                            const int *swaps_per_T, 
                            const int *trace_anneal, 
                            int *best_cost, 
                            int *best,
                            int *status)
{
  /* WARNING: Nested returns */
  /* Miminise sum diet gaps using simulated annealing learning.
  
     In params:

       network:  A predation matrix of nodes x nodes

         nodes:  The number of nodes in the predation matrix

       T_start:  annealing starts at this temperature

        T_stop:  annealing ends when T drops below T_stop

             c:  T cools by this fraction every iteration

   swaps_per_T:  The number of network row swaps per T

  trace_anneal:  If 1 some output printed to stdout


    Out params:
     best_cost:  The sum diet gap of the best solution that was found

          best:  An array of length nodes containing the best row ordering 
                 found

        status: -1 - unexpected error.
                 0 - normal exit.
                 1 - problem with one of the parameters.
  */

  /* Quick and dirty parameter checks */
  if(0==network || 0==nodes || *nodes<1 || 
     0==T_start || 0==T_stop || *T_start<=*T_stop || 0==c || 
     *c<=0 || *c>=1 || 0==swaps_per_T || 0==*swaps_per_T || 0==trace_anneal || 
     0==best_cost || 0==best || 0==status)
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
    if(sizeof(SumDietGap::Cost) < sizeof(*best_cost) || 
              sizeof(SumDietGap::State::value_type)!=sizeof(*best))
    {
      // Sanity check
      throw CheddarException("Unexpected type size");
    }
    else
    {
      typedef SimulatedAnnealing<SumDietGap> SDG;
      SDG worker(*T_start, *T_stop, *c, *swaps_per_T, 1==*trace_anneal);

      SumDietGap problem(IntVector(network, network + *nodes * *nodes), *nodes);
      const SDG::Result res = worker.optimise(&problem);

      if(int(res.first.size())!=*nodes)
      {
        // Sanity check
        throw CheddarException("Annealing result has unexpected size");
      }
      else
      {
        memcpy(best, &res.first[0], sizeof(int) * *nodes);

        // Overflow of res.second is not possible because a matrix of size 
        // nodes by nodes matrix can have at most (nodes-1)^2 gaps.
        *best_cost = int(res.second);
        *status = 0;        // Normal exit
      }
    }
  }
  catch(const std::exception &e)
  {
    REprintf("Unexpected error in intervality() [%s]\n", e.what());
  }
  catch(...)
  {
    REprintf("Unexpected error in intervality()\n");
  }  
}

} // extern C
