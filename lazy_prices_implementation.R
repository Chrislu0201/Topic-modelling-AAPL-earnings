# Code for obtaining annual reports and computing similarity measures
# Used for: https://craha.wordpress.com/2018/05/21/an-implementation-of-lazy-prices/
library(edgarWebR)
library(dplyr)
#library(parallel) - I used mclapply instead of lapply below to speed things up when running more stocks
library(text2vec)
library(stringr)

#stock_list <- c("MMM", "AXP", "AAPL", "CAT")
stock_list <- c("AAPL")
#this may take a while, many html requests.
filings_list <- lapply(stock_list, function(x) {
  #Search for company's filings- count param supports intervals of 20
  filings <- company_filings(x, type = "10-", count = 20)
  #isolate 10K - 10Q
  filings <- filings[filings$type == "10-K" | filings$type == "10-Q",]
  #get filing information of each item
  filing_infos <- purrr::map_df(filings$href, filing_information)
  #join for some info
  filings <- bind_cols(filings, filing_infos)
  #look at each report in search results for company
  filings_reports <- lapply(filings$href, function(x) {
    docs <- filing_documents(x)
    doc <- docs[docs$description == 'Complete submission text file', ]
    #this will prevent an error if the stack gets too big when parsing the report and fails
    parsed_docs <- tryCatch({parse_submission(doc$href)}, error = function(e) {return(NULL)})
    if(is.null(parsed_docs)) {return(-1)}
    doc <- parse_filing(parsed_docs[which(stringr::str_detect(parsed_docs$TYPE, "10-")),'TEXT'])
    #MDA
    MDA <- doc[grepl("management", doc$item.name, ignore.case = TRUE), ]
    #RF
    RF <- doc[grepl("risk factor", doc$item.name, ignore.case = TRUE), ]
    #Legal
    LEGAL <- doc[grepl("legal", doc$item.name, ignore.case = TRUE), ]
    #QnQ
    QNQ <- doc[grepl("Quant", doc$item.name, ignore.case = TRUE), ]
    #Control
    CONTROL <- doc[grepl("Control", doc$item.name, ignore.case = TRUE), ]
    #other
    OTHER <- doc[grepl("Other", doc$item.name, ignore.case = TRUE), ]
    #strip "Tables" or highly numeric paragraphs
    MDA <- MDA[(stringr::str_count(MDA$text, pattern = '[0-9]')/nchar(MDA$text)) < .10,1]
    RF <- RF[(stringr::str_count(RF$text, pattern = '[0-9]')/nchar(RF$text)) < .10,1]
    LEGAL <- LEGAL[(stringr::str_count(LEGAL$text, pattern = '[0-9]')/nchar(LEGAL$text)) < .10,1]
    QNQ <- QNQ[(stringr::str_count(QNQ$text, pattern = '[0-9]')/nchar(QNQ$text)) < .10,1]
    CONTROL <- CONTROL[(stringr::str_count(CONTROL$text, pattern = '[0-9]')/nchar(CONTROL$text)) < .10,1]
    OTHER <- OTHER[(stringr::str_count(OTHER$text, pattern = '[0-9]')/nchar(OTHER$text)) < .10,1]
    
    sections <- list(MDA, RF, LEGAL, QNQ, CONTROL, OTHER)
    names(sections) <- c("MDA", "RF", "LEGAL", "QNQ", "CONTROL", "OTHER")
    
    return(sections)
  })
  
  names(filings_reports) <- filings$period_date
  
  return(filings_reports)
})

names(filings_list) <- stock_list

#Remove any companies that failed
filings_list <- filings_list[lapply(filings_list, length) > 1]

--------------------
#Export resulting data frame to .csv file
library(data.table)
library(stringr)

prep_fun <- function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}

ftemp<-rbindlist(filings_list, fill = TRUE)
ftemp2<-data.frame(ftemp)
f_df<-data.frame(matrix(rep(0, nrow(ftemp2)*ncol(ftemp2)), nrow = nrow(ftemp2)))
for (i in 1:nrow(ftemp2)) {
  for (j in 1:ncol(ftemp2)){
    temp<-str_remove_all(paste(ftemp2[i,j]),"[()\"\\\\]")
    temp<-str_remove(temp,"[c]")
    f_df[i,j]<-prep_fun(temp)
  }
}
sec<-c("MDA", "RF", "LEGAL", "QNQ", "CONTROL", "OTHER")
filings_df<-t(f_df)
names(filings_df)<-sec
#names(filings_df)<-c("Section",names(ftemp))
write.csv(filings_df,"/Users/LTY/Desktop/2020Summer/LazyPrice/LazyPriceData.csv", row.names = FALSE)
--------------------


similarities <- lapply(filings_list[1:4], function(x){
  n_reports <- nrow(summary(x))
  get_sims <- c('MDA', 'RF', 'LEGAL', 'QNQ', 'CONTROL', 'OTHER')
  all_sims <- list()
  for (i in 1:(n_reports-5)){
    this_period <- x[[i]]
    last_year <- x[[(i+4)]]
    sims <- lapply(get_sims, function(d){
      #Get text
      texta <- prep_fun(paste(this_period[d], collapse = ' '))
      textb <- prep_fun(paste(last_year[d], collapse = ' '))
      #tokenize
      it1 <- itoken(texta, progressbar = FALSE)
      it2 <- itoken(textb, progressbar = FALSE)
      it <- itoken(c(texta, textb), progressbar = FALSE)
      #build vocab
      v <- create_vocabulary(it)
      vectorizer <- vocab_vectorizer(v)
      #D-TF vectors
      dtm1 <- create_dtm(it1, vectorizer)
      dtm2 <-  create_dtm(it2, vectorizer)
      #Get the sims
      jaccard <- as.numeric(sim2(dtm1, dtm2, method = "jaccard", norm = "none"))
      cosine <- as.numeric(sim2(dtm1, dtm2, method = "cosine",  norm = "l2"))
      sim_vec <- c(jaccard, cosine)
      names(sim_vec) <- c("jaccard", "cosine")
      return(sim_vec)
    })
    names(sims) <- get_sims
    sims <- do.call('rbind', sims)
    all_sims[[i]] <- sims
  }
  names(all_sims) <- names(x)[1:length(all_sims)]
  return(all_sims)
})

names(similarities) <- names(filings_list)

#Jaccard Similarity
jaccard_sims <- lapply(similarities, function(x) {
  #x is a stock, list of dates
  jaccard_stock <- lapply(x, function(d) {
    jc <- d[,1]
    return(jc)
  })
  names(jaccard_stock) <- names(x)
  jaccard_stock <- do.call("rbind", jaccard_stock)
  return(jaccard_stock)
})
#add columns of the stocks and the dates to the similarity matrix
label_col <- unlist(lapply( names(similarities), function(x){return(rep(x, as.numeric(summary(jaccard_sims)[x,][1])/6))}))
jaccard_sims <- do.call("rbind", jaccard_sims)
jaccard_sims <- cbind(label_col, jaccard_sims, lubridate::year(rownames(jaccard_sims)),lubridate::month(rownames(jaccard_sims)),lubridate::day(rownames(jaccard_sims)) )
rownames(jaccard_sims) <- NULL
jaccard_sims <- as.data.frame(jaccard_sims, row.names = NULL)
colnames(jaccard_sims)[(length(jaccard_sims)-2):length(jaccard_sims)] <- c('y','m','d')

cosine_sims <- lapply(similarities, function(x) {
  #x is a stock, list of dates
  cos_stock <- lapply(x, function(d) {
    cs <- d[,2]
    return(cs)
  })
  names(cos_stock) <- names(x)
  cos_stock <- do.call("rbind", cos_stock)
  return(cos_stock)
})
#add columns of the stocks and the dates to the similarity matrix
cosine_sims <- do.call("rbind", cosine_sims)
cosine_sims <- cbind(label_col, cosine_sims, lubridate::year(rownames(cosine_sims)),lubridate::month(rownames(cosine_sims)),lubridate::day(rownames(cosine_sims)) )
rownames(cosine_sims) <- NULL
cosine_sims <- as.data.frame(cosine_sims, row.names = NULL)
colnames(cosine_sims)[(length(cosine_sims)-2):length(cosine_sims)] <- c('y','m','d')

#now you have similarity matricies of 10K-10Q items for you to rank, join to returns, and build what you need
#If you'd like my code for that, please reach out
