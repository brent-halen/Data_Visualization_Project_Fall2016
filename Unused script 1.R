tmdbURL1 <- "https://api.themoviedb.org/3/find/tt"
tmdbURL2 <- "?api_key="
tmdbURL3 <- "&external_source=imdb_id"
finaltmdbURL <- paste(tmdbURL1,movie_metadata_widvec[1660,31],tmdbURL2,apikey,tmdbURL3, sep = "")
pull <- fromJSON(finaltmdbURL,nullValue = NA)
filter_to_film <- as.data.frame(pull[[1]])
remove_redundancy <- filter_to_film[1,]
remove_redundancy[,2] <- toString(remove_redundancy[,2])
remove_redundancy[,5] <- toString(remove_redundancy[,5])
remove_redundancy[,6] <- toString(remove_redundancy[,6])
remove_redundancy[,7] <- toString(remove_redundancy[,7])
remove_redundancy[,8] <- toString(remove_redundancy[,8])
remove_redundancy[,9] <- toString(remove_redundancy[,9])
remove_redundancy[,11] <- toString(remove_redundancy[,11])
#remove_redundancy <- filter_to_film[1,]