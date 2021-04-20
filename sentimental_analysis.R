sentimental_analysis <- function(input, language, language_code){
  
  ###########
  #Libraries
  ###########
  
  #install_github("sillasgonzaga/OpLexicon")
  #library(OpLexicon)
  library(lexiconPT)
  library(remotes)
  library(tidyverse)
  
  ########
  #Inputs
  ########
  
  lexicon <- unique(sentiLex_lem_PT02 %>% select(term, polarity))
  lexicon <- lexicon %>% group_by(term) %>% summarise(polarity = max(polarity))
  #lex_dups <- lexicon %>% group_by(term) %>% summarise(n = n())
  
  inputs <- list.files(paste0(wd, "/01_Inputs"), pattern = paste0("tm_",procedure,"_negation_input_", language_code), full.names = TRUE)
  
  project_negation <- read.csv(file = inputs, head = FALSE, sep=";",na.strings=c("","NA"))
  
  project_negation <- as.character(project_negation$V1)
  
  
  ###############
  #Get Sentiment
  ###############
  textdoc_df <- input
  
  textdoc_sa <- textdoc_df
  
  textdoc_sa_aux <- textdoc_sa %>% separate(Terms, c("Terms_1", "Terms_2"), "_",extra = "merge",fill = "left")
  
  textdoc_sa_aux <- textdoc_sa_aux %>% left_join(lexicon, by = c("Terms_2" = "term"))
  
  textdoc_sa_aux$negation <- ifelse(textdoc_sa_aux$Terms_1 %in% project_negation, 1,0)
  
  textdoc_sa <- textdoc_sa %>% cbind((textdoc_sa_aux %>% select(negation,polarity)))
  
  textdoc_sa$polarity_adjusted <-ifelse(textdoc_sa$negation == 1, textdoc_sa$polarity * textdoc_sa$negation * -1, textdoc_sa$polarity)  
  
  #test <- textdoc_sa_aux_2 %>% group_by(Docs,Terms_1,Terms_2,polarity) %>% summarise(n = n()) %>% select(-polarity)
  #test1 <- textdoc_sa_aux %>% group_by(Docs,Terms_1,Terms_2) %>% summarise(n = n())
  #tdups<- setdiff(test,test1)
  
  return(textdoc_sa)
}
