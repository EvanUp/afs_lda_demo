## Generate a corpus
library(tidyverse)
rawdocs <- c('Evan does not believe in caramel',
             'caramel is the worst food on the planet',
             'caramel is not a food',
             'Evan believes in chocolate',
             'chocolate believes in Evan',
             'If I one wish, caramel would no longer exist',
             'je suis tellement fatigué')
docs <- strsplit(rawdocs, split=' ', perl=T) # generate a list of documents

# original code written by Andrew Brooks- check out his fantastic blog:
# http://brooksandrew.github.io/simpleblog/articles/latent-dirichlet-allocation-under-the-hood/

## PARAMETERS
K <- 2 # number of topics
alpha <- 1 
eta <- .001
iterations <- 3

vocab <- unique(unlist(docs))

## Replace words in documents with wordIDs
for(i in 1:length(docs)) docs[[i]] <- match(docs[[i]], vocab)

## 1. Randomly assign topics to words in each doc.  2. Generate word-topic count matrix.
wt <- matrix(0, K, length(vocab)) 
ta <- sapply(docs, function(x) rep(0, length(x))) 
for(d in 1:length(docs)){ 
  for(w in 1:length(docs[[d]])){
    ta[[d]][w] <- sample(1:K, 1) 
    ti <- ta[[d]][w] 
    wi <- docs[[d]][w] 
    wt[ti,wi] <- wt[ti,wi]+1    
  }
}


dt <- matrix(0, length(docs), K)
for(d in 1:length(docs)){
  for(t in 1:K){ 
    dt[d,t] <- sum(ta[[d]]==t)  
  }
}

tibble(Topic_1 = dt[,1],
       Topic_2 = dt[,2])

for (i in seq_along(docs)){
  print(docs[[i]])
}

for (i in seq_along(ta)){
  print(ta[[i]])
}

iterations = 10000

for(i in 1:iterations){ # for each pass through the corpus
  for(d in 1:length(docs)){ # for each document
    for(w in 1:length(docs[[d]])){ # for each token 
      
      t0 <- ta[[d]][w] # initial topic assignment to token w
      wid <- docs[[d]][w] # wordID of token w
      
      dt[d,t0] <- dt[d,t0]-1 # we don't want to include token w in our document-topic count matrix when sampling for token w
      wt[t0,wid] <- wt[t0,wid]-1 # we don't want to include token w in our word-topic count matrix when sampling for token w
      
      ## UPDATE TOPIC ASSIGNMENT FOR EACH WORD -- COLLAPSED GIBBS SAMPLING MAGIC.  Where the magic happens.
      denom_a <- sum(dt[d,]) + K * alpha # number of tokens in document + number topics * alpha
      denom_b <- rowSums(wt) + length(vocab) * eta # number of tokens in each topic + # of words in vocab * eta
      p_z <- (wt[,wid] + eta) / denom_b * (dt[d,] + alpha) / denom_a # calculating probability word belongs to each topic
      t1 <- sample(1:K, 1, prob=p_z/sum(p_z)) # draw topic for word n from multinomial using probabilities calculated above
      
      ta[[d]][w] <- t1 # update topic assignment list with newly sampled topic for token w.
      dt[d,t1] <- dt[d,t1]+1 # re-increment document-topic matrix with new topic assignment for token w.
      wt[t1,wid] <- wt[t1,wid]+1 #re-increment word-topic matrix with new topic assignment for token w.
      
      if(t0!=t1) print(paste0('iter:', i, ' doc:', d, ' token:' ,w, ' topic:',t0,'=>',t1)) # examine when topic assignments change
    }
  }
}


phi <- (wt + eta) / (rowSums(wt+eta)) # topic probabilities per word
colnames(phi) <- vocab
print(phi)
options(scipen=999)
test <- t(phi)

sort(test[,2], decreasing = TRUE)
