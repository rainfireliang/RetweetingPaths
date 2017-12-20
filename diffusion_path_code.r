# To reconstruct the diffusion paths needs two datasets:
# 1. all retweets from an origanl tweet, which can be obtained from Twitter RT API
# time order 1:5 from oldest to most recent
retws = data.frame(sharers=c(1:5),seed_user=rep(6,5),time=c(1:5))

# 2. the following relationships among the retweeters, which can be ontained from API
followings = data.frame(egos=c(1,2,3,3,5,5),followees=c(6,1,1,2,2,4))

# 3. reconstruct
# define the function
recontruct = function(retws,followings){
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

# test: return a dataframe with 4 colums: sharers, shared from, time, number of exposures
diffnet = recontruct(retws,followings)

library(igraph)
g = graph.data.frame(diffnet[,c('seed_user','sharers')],directed = T)
plot(g,edge.arrow.size=0.3,layout=layout_as_tree) # this figure is similar to Fig2
