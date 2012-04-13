print("Trophic chains")
library(cheddar)
data(TL84, TL86, YthanEstuary, Benguela, SkipwithPond)
for(community in list(TL84, TL86, YthanEstuary, Benguela))
{
    print(community)
    print(system.time(chains <- TrophicChains(community)))
}

