text_mining <- function(input, language, language_code){
  
  main_input<- input
  
  #Load the data as a corpus
  textid <-as.character(main_input$BLOCK_ID_UNIQUE)
  textdoc <- VCorpus(VectorSource(main_input$BLOCK_TEXT))
  
  #Removing punctuation
  textdoc <- tm_map(textdoc,content_transformer(replacePunctuation))
  
  #Clean data
  textdoc <- tm_map(textdoc, toSpace, "/")
  textdoc <- tm_map(textdoc, toSpace, "@")
  textdoc <- tm_map(textdoc, toSpace, "\\|")
  
  #Removing "acentuação"
  textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = "ã|á|à|â|ä|å", replacement = "a")
  textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = "é|ê|è", replacement = "e")
  textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = "í|ì|î|ï", replacement = "i")
  textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = "ó|ò|õ|ô", replacement = "o")
  textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = "ú|ù|ü", replacement = "u")
  textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = "ç", replacement = "c")
  
  textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = "«|»|€|£|´|‘|’|“|”|º|ª", replacement = "")
  textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = "/|@|\\||\u2028", replacement = " ")
  
  # Convert the text to lower case
  textdoc <- tm_map(textdoc, content_transformer(tolower))
  
  # Remove numbers
  textdoc <- tm_map(textdoc, removeNumbers)
  
  #textdoc[[1819]]$content
  
  ###########
  #Needles
  ###########
  
  inputs <- list.files(paste0(wd, "/01_Inputs"), pattern = paste0("tm_",procedure,"_needle_input_", language_code), full.names = TRUE)
  
  project_needles <- read.csv(file = inputs, head = TRUE, sep=";",na.strings=c("","NA"))
  
  term_list <- as.data.frame(as.character(unique(project_needles$term)))
  
  colnames(term_list)[1] <- "term"
  
  term_list$term <- tolower(term_list$term)
  
  term_list <- term_list[!is.na(term_list$term),]
  
  neelde_matrix <- as.matrix(unique(term_list))
  
  for (i in 1:length(term_list)){
    needle_list <- as.matrix(na.omit(project_needles[tolower(project_needles$term) == term_list[i], 1]))
    needle_list <- tolower(needle_list)
    
    list_of_needles <- " | "
    
    for (j in 1:length(needle_list))
      list_of_needles <- paste0(list_of_needles, " | ", needle_list[j])
    needle_list <- paste0(gsub(" \\| \\|", "", gsub("  ", " ", list_of_needles)), " ")
    
    textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = needle_list, replacement = paste0(" ", as.character(term_list[i]), " "))
  }
  
  rm(list_of_needles, project_needles, needle_list, term_list, i, j, inputs)
  
  #textdoc[[1819]]$content
  
  ###########
  #N-grams
  ###########
  
  inputs <- list.files(paste0(wd, "/01_Inputs"), pattern = paste0("tm_",procedure,"_ngrams_input_", language_code), full.names = TRUE)
  
  project_ngrams <- read.csv(file = inputs, head = FALSE, sep=";",na.strings=c("","NA"))
  
  colnames(project_ngrams)[1] <- "tobe"
  
  project_ngrams$tobe <- tolower(project_ngrams$tobe)
  
  project_ngrams$was <- gsub("_", " ", project_ngrams$tobe)
  
  for(k in 1:nrow(project_ngrams)){
    textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = project_ngrams$was[k], replacement = project_ngrams$tobe[k])
  }
  
  #textdoc[[1819]]$content
  
  ###########
  #Negation
  ###########
  inputs <- list.files(paste0(wd, "/01_Inputs"), pattern = paste0("tm_",procedure,"_negation_input_", language_code), full.names = TRUE)
  
  project_negation <- read.csv(file = inputs, head = FALSE, sep=";",na.strings=c("","NA"))
  
  str_negation <- function(x,negation) {
    str_split <- unlist(strsplit(x=x, split=" "))
    is_negative <- grepl(negation,str_split,ignore.case=T)
    negate_me <- append(FALSE,is_negative)[1:length(str_split)]
    str_split[negate_me==T]<- paste0(negation,"_",str_split[negate_me==T])
    paste(str_split,collapse=" ")
  }
  
  for (i in 1:length(textdoc)) {
    print(i)
    for(k in 1:nrow(project_negation)){
      textdoc[[i]]$content <- str_negation(textdoc[[i]]$content,project_negation$V1[k])
    }
  } 
  
  #textdoc[[537]]$content
  
  
  ###########
  #Stopwords
  ###########
  
  # Remove common stopwords - default dictionary
  textdoc <- tm_map(textdoc, removeWords, stopwords(language))
  
  # Remove project stopwords
  
  # Input project stopwords
  inputs <- list.files(paste0(wd, "/01_Inputs"), pattern = paste0("tm_",procedure,"_stopwords_input_", language_code), full.names = TRUE)
  
  project_stopwords <- read.csv(file = inputs, head = FALSE)
  
  # Remove project stopwords
  project_stopwords$V1 <- tolower(project_stopwords$V1)
  
  textdoc <- tm_map(textdoc, removeWords, project_stopwords$V1)
  
  # Eliminate extra white spaces
  textdoc <- tm_map(textdoc, stripWhitespace)
  
  # Text stemming - which reduces words to their root form
  textdoc <- tm_map(textdoc, stemDocument)
  
  #textdoc[[537]]$content
  
  ###############
  #Dictionary
  ###############
  
  #Dictionary
  inputs <- list.files(paste0(wd, "/01_Inputs"), pattern = paste0("tm_",procedure,"_dictionary_input_",language_code), full.names = TRUE)
  
  project_dictionary <- read.csv(file = inputs, head = TRUE, sep=";",na.strings=c("","NA"))
  
  synonym_list <- as.data.frame(as.character(unique(project_dictionary$synonym)))
  
  colnames(synonym_list)[1] <- "synonym"
  
  synonym_list$synonym <- tolower(synonym_list$synonym)
  
  synonym_list <- synonym_list[!is.na(synonym_list$synonym),]
  
  for (i in 1:length(synonym_list)){
    term_list <- as.matrix(na.omit(project_dictionary[tolower(project_dictionary$synonym) == synonym_list[i], 1]))
    term_list <- tolower(term_list)
    
    list_of_terms <- " | "
    
    for (j in 1:length(term_list))
      list_of_terms <- paste0(list_of_terms, " | ", term_list[j])
    term_list <- paste0(gsub(" \\| \\|", "", gsub("  ", " ", list_of_terms)), " ")
    
    textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = term_list, replacement = paste0(" ", as.character(synonym_list[i]), " "))
  }
  
  rm(list_of_terms)
  
  
  ###############
  #Similar words
  ###############
  
  inputs <- list.files(paste0(wd, "/01_Inputs"), pattern = paste0("tm_",procedure,"_similar_input_", language_code), full.names = TRUE)
  
  similar <- read.csv(file = inputs, head = TRUE, sep=";",na.strings=c("","NA"))
  
  similar$termuniformed <- tolower(similar$termuniformed)
  
  term_uniformed_list <- as.data.frame(as.character(unique(similar$termuniformed)))
  
  colnames(term_uniformed_list)[1] <- "termuniformed"
  
  term_uniformed_list$termuniformed <- tolower(term_uniformed_list$termuniformed)
  
  term_uniformed_list <- term_uniformed_list[!is.na(term_uniformed_list$termuniformed),]
  
  for (i in 1:length(term_uniformed_list)){
    term_list <- as.matrix(na.omit(similar[similar$termuniformed == term_uniformed_list[i], 1]))
    term_list <- tolower(term_list)
    
    list_of_terms <- " | "
    
    for (j in 1:length(term_list))
      list_of_terms <- paste0(list_of_terms, " | ", term_list[j])
    term_list <- paste0(gsub(" \\| \\|", "", gsub("  ", " " ,list_of_terms)), " ")
    
    textdoc <- tm_map(textdoc, content_transformer(gsub), pattern = term_list, replacement = paste0(" ",term_uniformed_list[i], " "))
  }
  
  rm(list_of_terms, term_list, similar, term_uniformed_list, i, j, inputs)
  
  #Metadata
  #Colocar na metadata do VCorpus a COD_WEEK identificada
  
  for (i in 1:nrow(main_input)){
    #textdoc
    meta(textdoc[[i]])$name <- main_input$name[i]
    #meta(textdoc[[i]])$id <- main_input$BLOCK_ID[i]
    meta(textdoc[[i]])$id <- main_input[main_input$BLOCK_ID_UNIQUE == main_input$BLOCK_ID_UNIQUE[i],]$BLOCK_ID_UNIQUE
    meta(textdoc[[i]])$language <- main_input[main_input$BLOCK_ID_UNIQUE == main_input$BLOCK_ID_UNIQUE[i],]$BLOCK_LANGUAGE
    
  }
  rm(i)
  textdoc[[1]]$content
  textdoc[[1]]$meta$id
  textdoc[[1]]$meta$language
  textdoc[[1]]$meta$block_id_unique
  
  memory.limit(size=9800000)
  #Count words
  # Build a term-document matrix
  #textdoc_dtm <- TermDocumentMatrix(textdoc)
  textdoc_dtm <- DocumentTermMatrix(textdoc)
  
  #dtm_m <- removeSparseTerms(textdoc_dtm, 0.9999)
  dtm_m <- as.matrix(textdoc_dtm)
  
  dtm_df <- as.data.frame(as.table(dtm_m))
  #dtm_m_df <- melt(dtm_m,c('Docs'))
  
  textdoc_df <- na.omit(dtm_df)
  
  textdoc_df <- textdoc_df[textdoc_df$Freq > 0,]
  
  textdoc_df$Docs <- as.character(textdoc_df$Docs)
  textdoc_df$Terms <- as.character(textdoc_df$Terms)
  
  #listOfDataframe <- list(textdoc_df,dtm_df)
  return(textdoc_df)
}
