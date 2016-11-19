TMDBdata <- function(x,apikey){
    tmdbURL1 <- "https://api.themoviedb.org/3/find/tt"
    tmdbURL2 <- "?api_key="
    tmdbURL3 <- "&external_source=imdb_id"
    #finaltmdbURL <- paste(tmdbURL1,x[1,31],tmdbURL2,apikey,tmdbURL3, sep = "")
    #pull <- fromJSON(finaltmdbURL)
    #filter_to_film <- as.data.frame(pull[[1]])
    #filter_to_film[,2] <- toString(filter_to_film[,2])
    #filter_to_film[,5] <- toString(filter_to_film[,5])
    #filter_to_film[,6] <- toString(filter_to_film[,6])
    #filter_to_film[,7] <- toString(filter_to_film[,7])
    #filter_to_film[,8] <- toString(filter_to_film[,8])
    #filter_to_film[,9] <- toString(filter_to_film[,9])
    #filter_to_film[,11] <- toString(filter_to_film[,11])
    #remove_redundancy <- filter_to_film[1,]
    #returnframe <- as.data.frame(remove_redundancy)
    #NArow <- remove_redundancy
    #NArow[,1:14] <- NA
    returnframe <- data.frame(adult = logical(),
                              backdrop_path = character(),
                              genre_ids = numeric(),
                              id = numeric(),
                              original_language = character(),
                              original_title = character(),
                              overview = character(),
                              release_date = character(),
                              poster_path = character(),
                              popularity = character(),
                              title = character(),
                              video = logical(),
                              vote_average = numeric(),
                              vote_count = numeric())
    NArow <- returnframe
    NArow[1,1:14] <- NA
    for(i in 1:nrow(x)){
        print(paste("This is step ",i))
        finaltmdbURL <- paste(tmdbURL1,x[i,31],tmdbURL2,apikey,tmdbURL3, sep = "")
        pull <- fromJSON(finaltmdbURL, nullValue=NA)
        filter_to_film <- as.data.frame(pull$movie_results)
        
        remove_redundancy <- filter_to_film[1,]
        if(nrow(filter_to_film) !=0 ){
            remove_redundancy[,2] <- toString(remove_redundancy[,2])
            remove_redundancy[,5] <- toString(remove_redundancy[,5])
            remove_redundancy[,6] <- toString(remove_redundancy[,6])
            remove_redundancy[,7] <- toString(remove_redundancy[,7])
            remove_redundancy[,8] <- toString(remove_redundancy[,8])
            remove_redundancy[,9] <- toString(remove_redundancy[,9])
            remove_redundancy[,11] <- toString(remove_redundancy[,11])
            
            returnframe <- rbind(returnframe,remove_redundancy)
            #print(remove_redundancy)
        }else if(nrow(filter_to_film) == 0){
            returnframe[i,] <- NArow[1,]
        }
        #print(returnframe[i,1:14])
        Sys.sleep(1)
        print(str(returnframe))
        print(paste(((i/nrow(x))*100),"% completed."))
    }
    returnframe[,c(2,5:9,11)] <- as.factor(returnframe[,c(2,5:9,11)])
    return(returnframe)
}