print("Shortest paths")
library(cheddar)
data(Benguela, BroadstoneStream, TL84, TL86, SkipwithPond, YthanEstuary)
for(community in list(Benguela, BroadstoneStream, TL84, TL86, SkipwithPond, 
                      YthanEstuary))
{
    print(community)
    print(system.time(y <- ShortestPaths(community)))
}

