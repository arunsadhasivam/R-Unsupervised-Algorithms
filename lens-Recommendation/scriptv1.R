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



dtfinal <-data.table(as.vector(dt))


dtfinal[1:3,2,with=FALSE]#not a vector
#1:        Toy Story (1995)
#2:          Jumanji (1995)
#3: Grumpier Old Men (1995)
dtfinal[1:3,2]#not a vector
#> dtfinal[1:3,2]#not a vector
#[1] 2




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
