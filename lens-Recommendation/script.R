
library(data.table)
movies<- readLines("data/movies.dat")
df<-strsplit(movies,split = "::")

reviews <- read.delim2('data/movies.dat', header=FALSE, col.names=c('user.id','item.id','rating','timestamp'))

reviews[[1]][1]

#df[[1]][1]#1
#df[[1]][2]#"Toy Story (1995)"
#df[[1]][3]#"Animation|Children's|Comedy"

 
total<-length(df)
#total<-3
dt<-data.table()
dt <- data.table(sno = character(total), name = character(total), category=character(total))
for(i in 1:total){
  for(j in 1:3){
     if(j==1)
     dt$sno[i]<-df[[i]][j]
     if(j==2)
       dt$name[i]<-df[[i]][j]
     if(j==3)
       dt$category[i]<-df[[i]][j]
  }
}
 #print(dt[90]$name)#"Animation|Children's|Comedy"


#dt[[1]][1]#1
#dt$sno[1]#1

#dt[[2]][1]# "Toy Story (1995)"
#dt$name[1]#"Toy Story (1995)"

#dt[[3]][1]#"Animation|Children's|Comedy"
#dt$category[1]#"Animation|Children's|Comedy"



library("recommenderlab")
colSums(is.na(dt))
dfsno<- as.data.frame(dt$sno,stringsAsFactors=FALSE)
dfname<- as.data.frame(dt$name,stringsAsFactors=FALSE)
dfcategory<- as.data.frame(dt$category,stringsAsFactors=FALSE)

dtfinal<-data.frame(total)

dtfinal[1:1]
cbind  (dtfinal,dfsno)

dtfinal[1:3]

#traceback(RS.matrix <-  as(finaldt, "realRatingMatrix"))
 
RS.matrix <-  as(finaldt, "realRatingMatrix")
RS.model <- Recommender(RR.matrix, method="UBCF")


Recommended.items <- predict(RS.model,  RR.matrix["name",], n=total)
identical(as(RS.matrix, "matrix"),m)
recommenderRegistry$get_entries(dataType = "realRatingMatrix")







# 

# getval<-function(){
#   for(i in 1:total){
#     val <-''
#     for(j in 1:3){
#       tmp<-paste(val,df[[i]][j])
#       val<-paste(tmp,',')
#     }
#     val<-substr(val,1,nchar(val)-1)
#     print (val)
#   }
# }
