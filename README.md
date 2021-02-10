# Retweeting Paths
This is an instruction to reconstruct diffusion paths based on retweeting and following on Twitter.

## Datasets

To reconstruct a diffusion cascade requires two data sets: 1, all retweets from an original tweet, which can be obtained from Twitter API - "GET statuses/retweets/:id""; 2, the following relationships among the retweeters, which can be obtained from the "GET friends/ids" API.

The retweet data set should be a dataframe with the three columns: sharers, seed_users, and time. Each row indicates a retweeting action.Retweets were ordered chronologically from earliest to most recent. The below example is from Figure 2A.

```{r}
retws = data.frame(sharers=c(1:5),seed_user=rep(6,5),time=c(1:5))
retws
```

The following relationships were stored in another dataframe with two columns: egos and followees. Egos are following followees. The below example is from Figure 2A.

```{r}
followings = data.frame(egos=c(1,2,3,3,5,5),followees=c(6,1,1,2,2,4))
followings
```

## Function

The following function can be used to reconstruct a diffusion cascade, given retweets and followings.

```{r}
reconstruct = function(retws,followings){
  library(plyr)
  if (nrow(followings)>0){
    dn = merge(followings,retws[,c('sharers','time')],by.x='egos',by.y='sharers')
    dn = merge(dn,retws[,c('sharers','time')],by.x='followees',by.y='sharers')
    
    dn = dn[which(dn$time.x>dn$time.y),]
    
    if (nrow(dn)>0){
      dn = plyr::ddply(dn,.(egos),mutate,n=length(followees),mint=max(time.y))
      dn = dn[which(dn$time.y==dn$mint),]
      dn = dn[,c('egos','followees','time.x','n')]
      colnames(dn) = c('sharers','seed_user','time','n')
      
      rest = retws[which(!retws$sharers %in% dn$sharers),c('sharers','seed_user','time')]
      rest$n=1
      
      tres = rbind(dn,rest)
      tres = tres[order(tres$time),]
    } else {
      tres = retws[,c('sharers','seed_user','time')]
      tres$n=1
    }
    
  } else {
    tres = retws[,c('sharers','seed_user','time')]
    tres$n=1
  }
  return (tres) # n indicate the number of exposures
}
```

## Example

To reconstruct the diffusion cascade of Figure 2.
```{r}
diffnet = reconstruct(retws=retws,followings=followings)
diffnet
```

To plot the diffusion cascade by using the igraph package. This figure is similar to Figure 2B.

```{r,warning=F}
library(igraph)
g = graph.data.frame(diffnet[,c('seed_user','sharers')],directed = T)
V(g)$color = "white"
V(g)$label.color = "black"
V(g)$size = 20
plot(g,edge.arrow.size=0.5,layout=layout_as_tree) 
```
