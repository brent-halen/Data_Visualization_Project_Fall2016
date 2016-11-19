TMDBdata <- function(x,apikey){
    tmdbURL1 <- "https://api.themoviedb.org/3/find/tt"
    tmdbURL2 <- "?api_key="
    tmdbURL3 <- "&external_source=imdb_id"
    URLlist <- c()
    Jsonlist <- c()
    for(i in 1:nrow(x)){
        finaltmdbURL <- paste(tmdbURL1,x[i,31],tmdbURL2,apikey,tmdbURL3, sep = "")
        append(URLlist,finaltmdbURL)
    }
    jsonl <- lapply(URLlist,fromJSON, nullValue = NA)
    jsonc <- toJSON(jsonl)
    write(jsonc, file = "TMDB.json")
}