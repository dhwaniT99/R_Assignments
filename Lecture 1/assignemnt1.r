
kw_data=read.csv("/Users/dhwani/Desktop/Data/kw_data.csv")
data=kw_data
colnames(data)=c("title","k1","k2","k3","k4","k5","k6","k7","k8","k9","k10","k11","k12")
data=data[-1,]
kw=data[,-1]
corpus=c(kw$k1,kw$k2,kw$k3,kw$k4,kw$k5,kw$k6,kw$k7,kw$k8,kw$k9,kw$k10,kw$k11,kw$k12)
print(length(corpus))
corpus=corpus[corpus!=""]
corpus=tolower(corpus)
corpus=unique(corpus)

kw=read.csv("data.csv")
adj=matrix(0,nrow=248,ncol=248)

for (text in kw$conc)
 
{
  for (row in 1:248)
  {
    for (col in row:248)
    {
      
      if(str_contains(text,corpus[row]) & str_contains(text,corpus[col]))
      {
        print("match")
        adj[row,col]=(adj[row,col])+1
      }
    }
  }
}
adj=rbind(adj,corpus)
adj=cbind(adj,corpus)
print(adj)