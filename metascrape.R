library(dplyr)
Metadatascrape <- function(x){
    y <- vector()
    for(i in 1:length(x)){
        
        y[i] <- sub("\\/\\?ref_=fn_tt_tt_1","",toString(x[i]))
        y[i] <- sub("http://www.imdb.com/title/tt","",toString(y[i]))
    }
    return(y)
}

Metadatascrape2 <- function(x){
    y <- vector()
    for(i in 1:length(x)){
        
    }
    return(y)
}

FindmovieIds <- function(x,y){
    z <- x
    newcolnum <- (ncol(x)+1)
    z[,newcolnum] <- ""
    step <- 1
    for(i in 1:nrow(x)){
        counter <- 1
        while(counter <= nrow(y)){
            if(x[i,29] == y[counter,2]){
                print(y[counter,1])
                z[i,newcolnum] <- y[counter,1]
                
            }
            counter <- counter+1
            
        }
        output <- z[i,newcolnum]
        print(output)
        percentcomplete <- (step/nrow(x))*100
        percentremaining <- 100 - percentcomplete
        printstatement <- paste("Working, on step ", step, " out of ", nrow(x),". ", percentcomplete, "% completed. ", percentremaining,"% until completion. ")
        print(printstatement)
        step <- step+1
    }
    namevector <- names(x)
    namevector <- append(namevector,names(y[,1]))
    namevector <- append(namevector,"movieId")
    names(z) <- namevector
    return(z)
}

MakeNAs <- function(x){
    returnframe <- x
    for(i in 1:nrow(x)){
        counter <- 1
        while(counter <= ncol(x)){
            if(as.character((x[i,counter]))==""||is.na(x[i,counter])==TRUE){
                returnframe[1,counter] <- NA
            } else {
                print("normal")
            }
            print(paste("Scanning row ", i, " of column ", counter))
            counter <- counter+1
        }
    }
    return(returnframe)
}



findNAs_onecol <- function(x){
    index.NA <- c()
    for(i in 1:length(x)){
        if(is.na(x[i])==TRUE){}
        append(index.NA,i)
        
    }
    return(index.NA)
}

GetRatings <- function(x,y){
    ratingdataframe <- data.frame()
    returndataframe <- x
    for(i in 1:nrow(x)){
        counter <- 1
        while(counter <= nrow(y)){
            
        }
    }
}



GetOMDBdata <- function(x){
    OMDBurl1 <- "http://www.omdbapi.com/?i=tt"
    OMDBurl2 <- "&plot=short&r=json&tomatoes=true"
    finalomdbURL <- paste(OMDBurl1,x[1,31],OMDBurl2, sep = "")
    pull <- fromJSON(finalomdbURL,nullValue=NA)
    dataframe <- as.data.frame(t(pull),stringsAsFactors = FALSE)
    returnframe <- dataframe
    
    for(i in 2:nrow(x)){
        finalomdbURL <- paste(OMDBurl1,x[i,31],OMDBurl2, sep = "")
        pull <- fromJSON(finalomdbURL,nullValue=NA)
        dataframe <- as.data.frame(t(pull),stringsAsFactors=FALSE)
        returnframe <- bind_rows(returnframe,dataframe)
        print(str(returnframe))
        print(paste("step ",nrow(returnframe)))
        print(paste(((i/nrow(x))*100),"% completed."))
    }
    return(returnframe)
}

GetOMDBdata2 <- function(x){
    OMDBurl1 <- "http://www.omdbapi.com/?i=tt"
    OMDBurl2 <- "&plot=short&r=json&tomatoes=true"
    IMDBid <- toString(x[1,2])
    while(nchar(IMDBid)<7){
        IMDBid <- paste("0",IMDBid,sep="")
    }
    finalomdbURL <- paste(OMDBurl1,IMDBid,OMDBurl2, sep = "")
    pull <- fromJSON(finalomdbURL,nullValue=NA)
    dataframe <- as.data.frame(t(pull),stringsAsFactors = FALSE)
    returnframe <- dataframe
    pausecounter <- 1
    for(i in 2:nrow(x)){
        IMDBid <- toString(x[i,2])
        while(nchar(IMDBid)<7){
            IMDBid <- paste("0",IMDBid,sep="")
        }
        finalomdbURL <- paste(OMDBurl1,IMDBid,OMDBurl2, sep = "")
        pull <- fromJSON(finalomdbURL,nullValue=NA)
        dataframe <- as.data.frame(t(pull),stringsAsFactors=FALSE)
        returnframe <- bind_rows(returnframe,dataframe)
        #print(str(returnframe))
        pausecounter <- pausecounter + 1
        if(pausecounter >= 1500){
            print("pausing")
            Sys.sleep(15)
            pausecounter <- 1
        }
        print(paste("step ",nrow(returnframe)))
        print(paste(((i/nrow(x))*100),"% completed."))
    }
    return(returnframe)
}

Get_average <- function(x){
    returnframe <- data.frame(AVGTomatometer = numeric(), Date = as.POSIXct(character()))
    print("Step 1 complete")
    unique_dates <- unique(x$Date)
    print("Step 2 complete")
    
    for(i in 1:length(unique_dates)){
        counter <- 1
        nument <- 0
        Sum <- 0
        Avg <- 0
        while(counter <= nrow(x)){
            if(x[counter,2] == unique_dates[i]){
                Sum <- Sum + x[counter,1]
                nument <- nument + 1
            }
        #print(paste((counter/nrow(x)*100),"% complete with step 3"))
        counter <- counter + 1
        }
        Avg <- Sum/nument
        returnframe[i,1] <- Avg
        returnframe[i,2] <- unique_dates[i]
        print(paste("Returnframe has ", nrow(returnframe), " rows."))
        step <- i
        print(paste((step/length(unique_dates)*100),"% completed."))
        
        
    }
    return(returnframe)
}

MovingAverageframe <- function(x){
    returnframe <- x
    for(i in 2:ncol(x)){
        tmp.zoo <- zoo(x[,i],x$Date)
        m.av <- rollmean(temp.zoo, 3, fill = list(NA, NULL, NA))
        print(ncol(returnframe))
        newcolnum <- ncol(returnframe) + 1
        returnframe[,newcolnum] <- m.av
        print(ncol(returnframe))
        name <- paste(names(x[,i]), ".av",sep="")
        
        names(returnframe[,newcolnum]) <- name
    }
    return(returnframe)
}

checkhigherthan100 <- function(x){
    returnvec <- c()
    for(i in 1:nrow(x)){
        if(as.numeric(x[i,21]) > 100){
            append(returnvec,x$tomatoURL)
        }
    }
    return(returnvec)
}

index.Problem <- c()
for(i in 1:nrow(OMDBdata2)){
    
    print(is.na(as.numeric(OMDBdata2[i,21])))
    if(is.na(as.numeric(OMDBdata2[i,21]))==TRUE){
        append(index.Problem,i)
    }
}
    
        