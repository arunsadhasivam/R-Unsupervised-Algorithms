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


#converting 2 dimension data to single dimension 
#by converting to vector
#and adding as column to data.table.

vcsno<-as.vector(dt$sno)
vcname<-as.vector(dt$name)
vccat<-as.vector(dt$category)

vccat [1:3]
#> vccat [1:3]
#[1] "Animation|Children's|Comedy"  "Adventure|Children's|Fantasy"
#[3] "Comedy|Romance"              

vectordt<- as.vector(dt)
vectordt[1:3]
#sno                    name                     category
#1:   1        Toy Story (1995)  Animation|Children's|Comedy
#2:   2          Jumanji (1995) Adventure|Children's|Fantasy
#3:   3 Grumpier Old Men (1995)               Comedy|Romance
vectordt[1:3,2]
#[1] 1
vectordt[1:3,333]
#[1] 333

dtspecifynovec<-as.vector(dt)
dtspecifynovec[1:3,2,with=FALSE] #with =false says not a vector.

#name
#1:        Toy Story (1995)
#2:          Jumanji (1995)
#3: Grumpier Old Men (1995)

#data.table also work but since need to specify with= false otherwise it will return 2
#hence create a data.frame and append each vector 
#so that simply giving rows,column can retrieve results for selected row , column.
      
#e.g

dtspecifynovec[1:3,2] 
#[1] 2



#it just returns position only if column field in syntax is given
#i.e if [1:3,1] is given

dtfinal<- data.table(sno = vcsno, name=vcname,category=vccat)

# converted to single dimension just convert each column to
# separate vector elements and then add to dataframe
# if even adding to data.table wont work.

#dtfinal[1:3,1]
#1

dtfinal$name[1:5]
#[1] "Toy Story (1995)"                  
#[2] "Jumanji (1995)"                    
#[3] "Grumpier Old Men (1995)"           
#[4] "Waiting to Exhale (1995)"          
#[5] "Father of the Bride Part II (1995)"


dtfinal[1:3,2]
#[1]2
#since it consider as one row value only not as separater columns
#it just returns index position regardless of column or row
#if row,column syntax is given.
dtfinal[,2]
#[1]2

dtfinal[,333]
#[1]333

#data.table also work but need to says not a vector 
dtfinal1 <-data.table(dtfinal)
dtfinal1[1:3,2]
#[1] 2

#> colnames(dtfinal1)
#[1] "sno"      "name"     "category"
dtfinal1[,c("sno")]
#"sno"

dtfinal1[1:3,2,with=FALSE] #with =false says not a vector.

#name
#1:        Toy Story (1995)
#2:          Jumanji (1995)
#3: Grumpier Old Men (1995)



#to act as separater columns
dffinal <-data.frame(dtfinal)
dffinal[1:3,2]
# since it is internal vector it is showing in character. 
#[1] "Toy Story (1995)"        "Jumanji (1995)"         
#[3] "Grumpier Old Men (1995)"

dffinal[1:3,]
#sno                    name                     category
#1   1        Toy Story (1995)  Animation|Children's|Comedy
#2   2          Jumanji (1995) Adventure|Children's|Fantasy
#3   3 Grumpier Old Men (1995)               Comedy|Romance
dffinal[1,3]
#returns all
#"Animation|Children's|Comedy"
#..........
#..........
#[3881] "Drama"                                          
#[3882] "Drama"                                          
#[3883] "Drama|Thriller"


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
