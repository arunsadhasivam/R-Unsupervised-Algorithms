
library(data.table)
movies<- readLines("data/movies.dat")
df<-strsplit(movies,split = "::")


df[[1]][1]#1
df[[1]][2]#"Toy Story (1995)"
df[[1]][3]#"Animation|Children's|Comedy"

 
total<-length(df)
total<-3
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
print (val)
print(dt[1]$category)#"Animation|Children's|Comedy"

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
