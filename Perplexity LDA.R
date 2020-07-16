# find best number of topics in LDA

folds <- 5
splitfolds <- sample(1:folds, 23, replace = TRUE)
candidate_k <- c(2:15) # candidates for how many topics

# we parallelize by the different number of topics.  A processor is allocated a value
# of k, and does the cross-validation serially.  This is because it is assumed there
# are more candidate values of k than there are cross-validation folds, hence it
# will be more efficient to parallelise

SEED = 123

Sys.time()
system.time({
  results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
    k <- candidate_k[j]
    results_1k <- matrix(0, nrow = folds, ncol = 2)
    colnames(results_1k) <- c("k", "perplexity")
    for(i in 1:folds){
      train_set <- dtm[splitfolds != i , ]
      valid_set <- dtm[splitfolds == i, ]
      fitted <- LDA(dtm, k = k, control = list(seed = SEED))
      results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
    }
    return(results_1k)
  }
})
Sys.time()

# Now lets graph the results and find the optimal number of k.

results_df <- as_tibble(results)

ggplot(results_df, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("5-fold cross-validation of EVEN topic modeling",
          "(ie five different models fit for each candidate number of topics)") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
