# This is a sample of functions that were built to generate the results that are presented in the report

## Connect to collention
col_conn <- function(collection){
  
  con <- mongo(collection = collection,
               url = "mongodb://m001-student:m001-mongodb-basics@cluster0-shard-00-00-9lxgj.mongodb.net:27017/my_project?ssl=true&authSource=admin")
  return(con)
}

## save image as pdf
save_pdf <- function(dir,file,width = 6.8, height = 4.2){
  pdf(paste0(substr(dir,1,nchar(dir)-4),"fig/",file,".pdf"), width = width, height = height) 
}

## Functions to clean text
clean_links <- function(x){
  
  clean_text <- strsplit(gsub('\"', '',iconv(gsub("\r","",gsub("\t","",gsub("\n"," ",gsub('"','',as.character(x))))), 'utf-8', 'latin1')),"http")[[1]][1]
  
  if(is.na(clean_text)==TRUE){
    clean_text<-strsplit(gsub('\"', '',gsub("\r","",gsub("\t","",gsub("\n"," ",gsub('"','',as.character(x)))))),"http")[[1]][1]
  }
  
  return(clean_text)
}

removeMostPunctuation <- function (x, preserve_intra_word_dashes = TRUE) {
  rmpunct <- function(x) {
    x <- gsub("#", "\002", x)
    x <- gsub("[[:punct:]]+", "", x)
    gsub("\002", "#", x, fixed = TRUE)
  }
  if (preserve_intra_word_dashes) { 
    x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
    x <- rmpunct(x)
    gsub("\001", "-", x, fixed = TRUE)
  } else {
    rmpunct(x)
  }
}

removeCharn <- function(x,n=2){
  unlist(strsplit(x," "))[-which(nchar(unlist(strsplit(x," ")))<n)]
  return(toString(x))
}

## Function that creates the matrix of terms
Build_dtm <- function(docs, minW = 3, maxW = 30, tokenizer = "words", minB = 10, maxB = Inf){
  
  dtm <- TermDocumentMatrix(docs, 
                            control = list(wordLengths=c(minW, maxW),
                                           tokenize = tokenizer,
                                           removePunctuation = FALSE,
                                           bounds = list (global = c(minB, maxB)),
                                           weighting = function(x)weightTfIdf(x, normalize = FALSE)))
  return(dtm)
}

## This function draws the worldclouds
draw_clouds <- function(nwords = 40, d_bots, d_humans, period){
  
  nbot <- min(nwords, length(d_bots$word))
  nhuman <- min(nwords, length(d_humans$word)) 
  
  par(mfrow=c(1,2),mar=rep(0, 4))
  
  wordcloud(words = d_bots$word[1:nbot], freq = d_bots$freq[1:nbot], scale = c(2,0.45),
            min.freq = 0, max.words=Inf, random.order=FALSE, rot.per=0.35, font = 1,
            colors=brewer.pal(8, "Dark2"), main="Title")
  
  wordcloud(words = d_humans$word[1:nhuman], freq = d_humans$freq[1:nhuman], scale = c(2,0.45),
            min.freq = 0, max.words=Inf, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"), main="Title")
  
  mytitle <- paste0("Period: ", period)
  mtext(mytitle, side = 3, line=-1.2, outer = TRUE, cex=1.6)
}

## This function generates the three criteria for deciding the number of clusters in k-means
choose_k <- function(dataframe, query, type, ngap, dir, period, k_max = 9){
  
  # Elbow method
  fviz_nbclust(dataframe, kmeans, method = "wss", k.max = k_max) +
    labs(subtitle = "Elbow method") + #, title = "") +
    geom_vline(xintercept = 4, linetype = 2, col ="slateblue") +
    geom_point()
  
  ggsave(path = paste0(substr(dir,1,nchar(dir)-4),"fig"), 
         file = paste0("elbow_",type,"_",query,"_",period,".pdf"), width = 6.8, height = 4.2)
  print("Elbow graph has been saved")
  
  # Silhouette method
  fviz_nbclust(dataframe, kmeans, method = "silhouette",k.max = k_max)+
    labs(subtitle = "Silhouette method") +
    geom_point()
  
  ggsave(path = paste0(substr(dir,1,nchar(dir)-4),"fig"), 
         file = paste0("sil_",type,"_",query,"_",period,".pdf"), width = 6.8, height = 4.2)
  print("Silhouette graph has been saved")  
  
  # Gap statistic
  fviz_nbclust(dataframe, k.max = k_max,
               kmeans,  iter.max=20, nstart = 20,  method = "gap_stat", nboot = ngap)+
    labs(subtitle = "Gap statistic method")
  ggsave(path = paste0(substr(dir,1,nchar(dir)-4),"fig"), 
         file = paste0("gap_",ngap,"_",type,"_",query,"_",period,".pdf"), width = 6.8, height = 4.2)
  print("Gap graph has been saved")
}
