#Clear GLobal Environment
rm(list=ls())

##################
#Global variables
##################
language_code <- "PT"
language <- "portuguese"
procedure <- "NPS"

###########
#Libraries
###########

#Data manipulation
library(dplyr)
library(stringr)
library(data.table)
library(DescTools)

#Text Mining
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")


###############
#Set directory
###############

wd <- "//tapnet.tap.pt/bi/Dev/DW/ANALYTICS/R Projects/20210315_Text_Mining/"

setwd(wd)


###########
#Functions
###########

# Set R functions path ----------------------------------------------------

R_functions_path <- paste0(wd, "/../R_Functions")
file.sources <- list.files(R_functions_path)
sapply(paste0(R_functions_path, "/", file.sources), source, encoding = "UTF-8",.GlobalEnv)
rm(R_functions_path, file.sources)

########
#Inputs
########

#Aux variables - SQL
sql_where <- " WHERE 1=1"

#Operational costs
sql_input <- paste0("SELECT * FROM [DW_ANALYTICS].[INPUT].[TM_BLOCK_SOURCE]")

main_input <- DBI_load_dwh_dataset('PRD', sql_input)

#SQL Handle
dbhandle <- odbcDriverConnect(paste0('driver={SQL Server}; 
                                     server=CN_SQL_P01_RPT\\I01; 
                                     database=DW_ANALYTICS; 
                                     trusted_connection=true; 
                                     charset=latin1; 
                                     use_unicode=True'))


main_input <- sqlQuery(dbhandle, sql_input, stringsAsFactors=FALSE)

main_input <- main_input %>% filter(BLOCK_LANGUAGE == language_code)

main_input <- head(main_input, 15)
# main_input <- main_input %>% filter(BLOCK_ID == "12698515")

odbcClose(dbhandle)

#############
#Functions
#############
add_initial_and_final_space <- function(x) return(paste0(" ", x, " "))

replacePunctuation <- function(x) gsub("[[:punct:]]+", " ", x)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))


##########################################
# TEXT MINING
##########################################
sapply(paste0(wd,"03_Functions/text_mining.R"), source, encoding = "UTF-8",.GlobalEnv)
text_mining_output <- text_mining(main_input,language,language_code)


##########################################
# SENTIMENTAL ANALYSIS
##########################################
sapply(paste0(wd,"03_Functions/sentimental_analysis.R"), source, encoding = "UTF-8",.GlobalEnv)

sentimental_analysis_output <- sentimental_analysis(text_mining_output, language, language_code)


##########################################
# OUTPUT to SQL
##########################################
sentimental_analysis_output$BLOCK_NAME <- procedure
sentimental_analysis_output <- rename(sentimental_analysis_output,BLOCK_ID_UNIQUE = Docs)
sentimental_analysis_output <- rename(sentimental_analysis_output,TERMS = Terms)
sentimental_analysis_output <- rename(sentimental_analysis_output,FREQUENCY = Freq)
sentimental_analysis_output <- rename(sentimental_analysis_output,NEGATION = negation)
sentimental_analysis_output <- rename(sentimental_analysis_output,SENTIMENT = polarity_adjusted)

sentimental_analysis_output <- sentimental_analysis_output %>% select(BLOCK_NAME,BLOCK_ID_UNIQUE,TERMS,NEGATION,FREQUENCY,SENTIMENT)

DBI_execute_dwh_query('PRD', 'TRUNCATE TABLE OUTPUT.TM_ANALYSIS')
DBI_save_dwh_dataset('PRD', 'OUTPUT.TM_ANALYSIS', sentimental_analysis_output)


##########################################
# CORRELATIONS
##########################################
#Sort by descearing value of frequency


dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)

#Display the top 10 most frequent words
head(dtm_d, 10)


#Plot the most frequent words
barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")

# #generate word cloud
# set.seed(1234)
# wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
#           max.words=100, random.order=FALSE, rot.per=0.40, 
#           colors=brewer.pal(8, "Dark2"))


#Find associations 
findAssocs(textdoc_dtm, terms = c("covid","tap","voo"), corlimit = 0.1)

