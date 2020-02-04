library(igraph)
start<-'A'
end<-'R'
goallist<-read.table(text = "
V1 weights
A 240
B 186
C 182
D 163
E 170
F 150
G 165
H 139
I 120
J 130
K 122
L 104
M 100
N 77
O 72
P 65
Q 65
R 0
                     ",header = T)
edgelist <- read.table(text = "
V1 v2 weights
                       A B 73
                       A C 64
                       A D 89
                       A E 104
                       E J 40
                       J N 53
                       J G 35
                       G Q 113
                       J Q 80
                       K H 35
                       H L 36
                       L I 28
                       I M 20
                       M O 50
                       L P 63
                       O P 41
                       O R 72
                       P R 65
                       Q R 65
                       B K 83
                       C I 64
                       D N 89
                       I F 31
                       F N 84",header=T)


c1<-graph_from_data_frame(edgelist)
rownames(goallist)<-goallist$V1

goal<-function(n){
  if(n==end){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}
depth<-0
hcap<-c()
bfs<-function(){
  f<-0
  
  OPEN<-list()
  OPEN<-c(OPEN,start)
  CLOSED<-list()
  while(length(OPEN)){
    
    
    first<-OPEN[[1]]
    hcap<-c(goallist[first,])
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
    check<-split(expanded[[1]], rep(1:length(expanded[[1]])))
    for(i in check){
      if(length(CLOSED)>0 && length(OPEN)>0)
      {
        if(any(as.character(names(i))==(CLOSED))||any(as.character(names(i))==(OPEN))){
          next
        }
        else{
          print(names(i))
          OPEN<-c(OPEN,as.character(names(i)))
          hcap<-c(hcap,goallist[as.character(names(i)),])
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
          hcap<-c(hcap,goallist[as.character(names(i)),])
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
          hcap<-c(hcap,goallist[as.character(names(i)),])
          print('line3')
          
          
        }
        
      }
      else{
        print(names(i))
        OPEN<-c(OPEN,as.character(names(i)))
        hcap<-c(hcap,goallist[as.character(names(i)),])
        
        print('line4')
        
        
      }
    }
  }
  print(OPEN)
  if(f){
    return(CLOSED)
  }
  else{
    return('no-path')
  }
  
}
