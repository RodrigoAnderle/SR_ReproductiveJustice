
# SUMMARY

##packages
require(tidyverse)
require(bibliometrix)

## Upload data
#data <- readRDS("data/data.rds")
data <- readRDS("data/data_co-citacoes.rds")
  
## Analyzing results
data$DB[is.na(data$DB)] <- "ISI"
results <- biblioAnalysis(data)
summary_results <- summary(results)


plot(x = results, k = 10, pause = FALSE)

## Most cited references
CR <- citations(data, field = "article", sep = ";")
length(CR$Source) # 41073
cbind(CR$Cited[1:20]) 

## Country Scientific Collaboration

M <- metaTagExtraction(data, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, 
                           analysis = "collaboration", 
                           network = "countries", sep = ";")
net=networkPlot(NetMatrix, 
                n = dim(NetMatrix)[1], 
                Title = "Country Collaboration", 
                type = "fruchterman", size=TRUE, 
                remove.multiple=FALSE,labelsize=0.7)


##Key-words
NetMatrix <- biblioNetwork(M, 
                           analysis = "co-occurrences", 
                           network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 50,
                Title = "Keyword Co-occurrences", 
                type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
NetMatrix <- biblioNetwork(M, 
                           analysis = "co-occurrences", 
                           network = "author_keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 50,
                Title = "Keyword Co-occurrences", 
                type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

## Conceptual Structure
CS <- conceptualStructure(M,field="ID", method="CA", minDegree=2, #clust=8, 
                          stemming=FALSE, labelsize=10)

CS <- conceptualStructure(M,field="DE", method="CA", minDegree=6,
                          stemming=FALSE, labelsize=10)

CS <- conceptualStructure(M,field="DE", method="MCA", minDegree=6,
                          stemming=FALSE, labelsize=10)
CS <- conceptualStructure(M,field="DE", method="MDS", minDegree=6,
                          stemming=FALSE, labelsize=10)

CS <- conceptualStructure(M,field="TI", method="CA", minDegree=4,
                          stemming=FALSE, labelsize=10)

CS <- conceptualStructure(M,field="AB", method="CA", minDegree=10,
                          stemming=FALSE, labelsize=10)


## Historical Network
histResults <- histNetwork(M)
net <- histPlot(histResults,n = 200 ,size = 10, labelsize=5)


## Cocitation
NetMatrix <- biblioNetwork(data, 
                           analysis = "co-citation", 
                           network = "references", sep = ".  ")
net=networkPlot(NetMatrix)
