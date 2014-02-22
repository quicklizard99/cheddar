#!/usr/bin/env Rscript
print("Sum diet gaps")
library(cheddar)
data(Benguela, BroadstoneStream, TL84, TL86, SkipwithPond, YthanEstuary)
for(community in list(Benguela, BroadstoneStream, TL84, TL86, SkipwithPond, 
                      YthanEstuary))
{
    print(community)
    print(system.time(i <- MinimiseSumDietGaps(community)))
    print(paste('Diet gap:',i$sum.gaps))
}

