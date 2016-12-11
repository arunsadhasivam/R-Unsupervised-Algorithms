
library(data.table)
users<- readLines("data/users.dat")
movies<- readLines("data/movies.dat")
ratings<- readLines("data/ratings.dat")


library("recommenderlab")
library(reshape2)

usersdt<-formatUsersdf(users)
usersdtfinal <-data.table(as.vector(usersdt))
usersdtfinal[1:5,2,With=FALSE]


moviesdt<- formatMoviesdf(movies)
moviesdtfinal <-data.table(as.vector(moviesdt))
moviesdtfinal[1:3,2,with=FALSE]


ratingsdt<-formatRatingsdf(ratings)
ratingsdtfinal <-data.table(as.vector(ratingsdt))
ratingsdtfinal[1:3,2,with=FALSE]
#dtfinal[1:3,2]


g<-acast(dtfinal, dtfinal$sno ~ dtfinal$category)
RR.matrix <- as(dtfinal, "matrix")
RS.model <- Recommender(RR.matrix, method="UBCF")
g<-acast(tr,  ~ )
r_b <- binarize(RR.matrix, minRating=1)
recom <- predict(rec, r[1:nrow(r)], type="ratings")







formatMoviesdf<-function(movies){
  df<-strsplit(movies,split = "::")
  
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
  return (dt)
}



formatUsersdf<-function(users){
  df<-strsplit(users,split = "::")
  total<-length(df)
  #total<-3
  dt<-data.table()
  dt <- data.table(UserID = character(total),
                   Gender = character(total),
                   Age=character(total),
                   Occupation = character(total),
                   Zipcode = character(total))
  
  for(i in 1:total){
    for(j in 1:5){
      if(j==1)
        dt$UserID[i]<-df[[i]][j]
      if(j==2)
        dt$Gender[i]<-df[[i]][j]
      if(j==3)
        dt$Age[i]<-df[[i]][j]
      if(j==4)
        dt$Occupation[i]<-df[[i]][j]
      if(j==5)
        dt$Zipcode[i]<-df[[i]][j]
      
    }
  }
  return (dt)
}



formatRatingsdf<-function(ratings){
  df<-strsplit(ratings,split = "::")
  
  total<-length(df)
  #total<-3
  dt<-data.table()
  dt <- data.table(UserID = character(total),
                   MovieID = character(total), 
                   Rating = character(total), 
                   Timestamp=character(total))
  for(i in 1:total){
    for(j in 1:4){
      if(j==1)
        dt$UserID[i]<-df[[i]][j]
      if(j==2)
        dt$MovieID[i]<-df[[i]][j]
      if(j==3)
        dt$Rating[i]<-df[[i]][j]
      if(j==4)
        dt$Timestamp[i]<-df[[i]][j]
    }
  }
  return (dt)
}

