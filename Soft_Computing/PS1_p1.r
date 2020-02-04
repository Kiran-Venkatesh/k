library(igraph)
#c1<-graph(edges=c('A','T','A','Z','Z','O','O','S','S','A','T','L','L','M','M','D','D','C','C','P','C','R','R','S','S','F','F','B','B','P','P','R','B','G','B','U','U','H','H','E','H','V','V','I','I','N'),directed=F)
#c1<-graph(edges=c('A','B','A','C','B','D','D','C','D','E','E','G','G','H','H','I','I','J','J','K','J','F','C','L','C','F','F','K','K','M','L','M'),directed=F)
#c1<-graph(edges=c('A','B','A','C','B','D','C','L','C','F','D','C','D','E','E','G','F','K','F','J','G','H','H','I','I','J','J','K','K','M','L','M'),directed=F)
c1<-graph(edges=c('A','B','A','C','B','D','B','E','C','F','C','G'),directed=F)
V(c1)$color<-'orange'
V(c1)['A']$color<-'red'
V(c1)['G']$color<-'green'
print(plot(c1))
E(c1)$weight <- 1
graph.strength(c1)
start<-'A'
end<-'G'
#BFS
goal<-function(n){
  if(n==end){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
depth<-0
bfs<-function(){
  f<-0
  OPEN<-list()
  OPEN<-c(OPEN,start)
  CLOSED<-list()
  while(length(OPEN)){
    
   
    first<-OPEN[[1]]
    if(goal(first)){
      f<-1
      CLOSED<-c(CLOSED,first)
      print('goal')
      break
    }
    OPEN<-OPEN[-which(OPEN==first)]
    CLOSED<-c(CLOSED,first)
    if(first!=start){
      V(c1)[first]$color<-'pink'
      
      plot(c1)
      
    }
    expanded<-adjacent_vertices(c1,first)    #EXpanded 'n'
    print(expanded)
    check<-split(expanded[[1]], rep(1:length(expanded[[1]])))
    print('Check')
    print(check)
    for(i in check){
      if(length(CLOSED)>0 && length(OPEN)>0)
      {
        if(any(as.character(names(i))==(CLOSED))||any(as.character(names(i))==(OPEN))){
          next
        }
        else{
          print(names(i))
          OPEN<-c(OPEN,as.character(names(i)))
          print('line1')
        
        }
      }
      else if(length(CLOSED)==0){
        if(any(as.character(names(i))==(OPEN))){
          next
        }
        else{
          print(names(i))
          OPEN<-c(OPEN,as.character(names(i)))
          print('line2')
          
        }
        
      }
      else if(length(OPEN)==0){
        if(any(as.character(names(i))==(CLOSED))){
          next
        }
        else{
          print(names(i))
          OPEN<-c(OPEN,as.character(names(i)))
          print('line3')
          
          
        }
        
      }
      else{
        print(names(i))
        OPEN<-c(OPEN,as.character(names(i)))
        print('line4')
        
        
      }
    }
  }
  if(f){
    return(CLOSED)
  }
  else{
    return('no-path')
  }
  
}
dfs<-function(c1,end){
  V(c1)[start]$color<-'red'
  V(c1)[end]$color<-'green'
  f<-0
  OPEN<-list()
  OPEN<-c(OPEN,start)
  CLOSED<-list()
  goal
  while(length(OPEN)){
    first<-OPEN[[1]]
    if(goal(first)){
      f<-1
      CLOSED<-c(CLOSED,first)
      print('goal')
      break
    }
    else if(first==end){
      f<-1
      CLOSED<-c(CLOSED,first)
      print('goal')
      break
      
    }
    OPEN<-OPEN[-which(OPEN==first)]
    CLOSED<-c(CLOSED,first)
    if(first!=start){
      V(c1)[first]$color<-'pink'
      plot(c1)
      
    }
    expanded<-adjacent_vertices(c1,first)    #EXpanded 'n'
    check<-split(expanded[[1]], rep(1:length(expanded[[1]])))
    check<-rev(check)
    #print('expand')
    #print(expanded)
    for(i in check){
      if(length(CLOSED)>0 && length(OPEN)>0)
      {
        if(any(as.character(names(i))==(CLOSED))||any(as.character(names(i))==(OPEN))){
          next
        }
        else{
          #print(names(i))
          OPEN<-c(as.character(names(i)),OPEN)
          #print('line1')
          
        }
      }
      else if(length(CLOSED)==0){
        if(any(as.character(names(i))==(OPEN))){
          next
        }
        else{
          #print(names(i))
          OPEN<-c(as.character(names(i)),OPEN)
          #print('line2')
          
        }
        
      }
      else if(length(OPEN)==0){
        if(any(as.character(names(i))==(CLOSED))){
          next
        }
        else{
         # print(names(i))
          OPEN<-c(as.character(names(i)),OPEN)
         # print('line3')
          
          
        }
        
      }
      else{
       # print(names(i))
        OPEN<-c(as.character(names(i)),OPEN)
       # print('line4')
        
        
      }
    }
  }
  if(f){
    return(CLOSED)
  }
  else{
    return("no-path")
  }
  
}

bfspath<-bfs()
dfspath<-dfs(c1,end)
edgesdfs<-c()
for(i in seq(1, length(dfspath), by=1)){
  if(i+1<=length(dfspath)){
    
  edgesdfs<-c(edgesdfs,dfspath[[i]])
  edgesdfs<-c(edgesdfs,dfspath[[i+1]])
  }
}
dfs1<-graph(edges=edgesdfs,directed=F)

common_edges<-c1%s%dfs1
  

ids<-function(){
  f<-0
  pj<-0
  uy<-'Y'
  #ed<-c('A','B','A','C','B','D','D','C','D','E','E','G','G','H','H','I','I','J','J','K','J','F','C','L','C','F','F','K','K','M','L','M')
  
  ed<-c('A','B','A','C','B','D','B','E','C','F','C','G','D','T')
  summ<-0
  last<-end
  idss<-c(start)
  for(lpo in c(2:4)){
    print(lpo)
    pj<-0
    if(lpo==0){
      c1<-graph(edges=ed[1],directed=F)
      last<-1
    }
    else{
      summ<-summ+2**(lpo)
      if(summ>length(ed)){
        summ=length(ed)
      }
      c1<-graph(edges=ed[1:summ],directed=F)
      
      
      plot(c1)
      uy<-readline(prompt="Enter (y/N): ")
      
      last<-summ
    }
    #print(dfs(c1))
    ry<-dfs(c1,ed[last])
    #print(ry)
    idss<-c(idss,ry)
    
  }
  
  return(idss)
}
  
  

