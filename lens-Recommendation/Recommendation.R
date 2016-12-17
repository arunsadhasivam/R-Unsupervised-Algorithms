library(data.table)
users<- readLines("data/users.dat")
movies<- readLines("data/movies.dat")
ratings<- readLines("data/ratings.dat")


library("recommenderlab")
library(reshape2)



formatMoviesdf<-function(movies){
  df<-strsplit(movies,split = "::")
  
  total<-length(df)
  #total<-3
  dt<-data.table()
  dt <- data.table(MovieID = character(total), Title = character(total), Genres=character(total))
  for(i in 1:total){
    for(j in 1:3){
      if(j==1)
        dt$MovieID[i]<-df[[i]][j]
      if(j==2)
        dt$Title[i]<-df[[i]][j]
      if(j==3)
        dt$Genres[i]<-df[[i]][j]
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


usersdt<-formatUsersdf(users)

usersdtfinal <-data.table(as.vector(usersdt))
#usersdtfinal[1:3,3,With=FALSE]
#usersdtfinal1<-load(file="usersdtfinal.RData")
#save(usersdtfinal,file="usersdtfinal.RData")

moviesdt<- formatMoviesdf(movies)
moviesdtfinal <-data.table(as.vector(moviesdt))
#moviesdtfinal[1:3,2,with=FALSE]



ratingsdt<-formatRatingsdf(ratings[1:1000])
ratingsdtfinal <-data.table(as.vector(ratingsdt))
#ratingsdtfinal[1:3,2,with=FALSE]

nrow(ratingsdtfinal)
library(plyr)

userratingsfinal<-join(ratingsdtfinal,usersdtfinal ,by='UserID',
                type = "left" , match = "first")

#write.csv(userratingsfinal,file = "usermoviesratingfinal.csv")

usermoviesratingfinal<-join(userratingsfinal,moviesdtfinal,
                            by='MovieID',
                       type = "left" , match = "first")

# class(usermoviesratingfinal)
# names(usermoviesratingfinal)
#get the user rating of only age group 25 and occupation 17:  "technician/engineer"

usermoviesratingfinal<-usermoviesratingfinal[usermoviesratingfinal$Age==25]
usermoviesratingfinal<-usermoviesratingfinal[usermoviesratingfinal$Occupation==17]
#write.csv(usermoviesratingfinal,file = "usermoviesratingfinal.csv")

length(usermoviesratingfinal)
library(reshape2)

ratingmat <- dcast(usermoviesratingfinal, 
                   Title+ MovieID~Rating, 
                   value.var = "Rating", na.rm=FALSE)
ratingmat <- as(ratingmat, "realRatingMatrix")
rating_normalize <- normalize(ratingmat)
recommender_model <- Recommender(rating_normalize, 
  method = "UBCF", param=list(method="Cosine",nn=30))
colnames( recommender_model)
predRes <- predict(recommender_model, 
                 ratingmat, n=10,
                 type="topNList") #Obtain top 10 recommendations for 1st user in dataset

predRes_list <- as(predRes, "list") #convert recommenderlab object to readable list
pred_result <- matrix(0,10)
for (i in 1:10){
  result <- pred_result[[1]][i]
  pred_result[i]<-result
  print(result)
}
