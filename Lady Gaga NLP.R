########## Exploring Latent Topics in Lady Gaga's Work ##############

if(!require(tidyverse))install.packages("tidyverse")
if(!require(tm))install.packages("tm")
if(!require(gridExtra))install.packages("gridExtra") #viewing multiple plots together
if(!require(tidytext))install.packages("tidytext") #text mining
if(!require(wordcloud2))install.packages("wordcloud2") # wordclouds
if(!require(genius))install.packages("genius")
if(!require(ggthemes))install.packages("ggthemes")
if(!require(data.table))install.packages("data.table")
if(!require(qdapRegex))install.packages("qdapRegex")



# Example with 2 different artists and albums
artist_albums <- tribble(
  ~artist, ~album, ~album_num,
  "Lady Gaga", "The Fame", 1,
  "Lady Gaga", "The Fame Monster", 2,
  "Lady Gaga", "Born This Way", 3,
  "Lady Gaga", "Artpop", 4,
  "Tony Bennett & Lady Gaga", "Cheek to Cheek", 5,
  "Lady Gaga", "Joanne", 6,
  "Lady Gaga", "Chromatica", 7,
)

# grab all the songs
music_cache = c()
for (i in seq_along(artist_albums$artist)){
  music_cache[[i]] <- genius_album(artist = artist_albums$artist[[i]], 
                                   album = artist_albums$album[[i]])
}

# unnest them and bind them together
gaga_genius <- rbindlist(music_cache, fill=TRUE)
gaga_genius$artist <- "Lady Gaga"

head(gaga_genius)
n_distinct(gaga_genius$track_title)

gaga_genius2 <- gaga_genius

gaga_genius2$track_title[grep("^Telephone", gaga_genius2$track_title)] <- "Telephone"
gaga_genius2$track_title[grep("^Just Dance", gaga_genius2$track_title)] <- "Just Dance"
gaga_genius2$track_title[grep("^Yo? and I", gaga_genius2$track_title)] <- "You and I"

top_10_songs <- tribble(
  ~track_title, ~top_10_song,
  "Just Dance", 1,
  "Poker Face", 1,
  "Born This Way", 1,
  "Bad Romance", 1,
  "Telephone", 1,
  "The Edge of Glory", 1,
  "Applause", 1,
  "Million Reasons", 1,
  "Alejandro", 1,
  "LoveGame", 1,
  "Paparazzi", 1,
  "Yoü and I", 1,
  "Dope", 1,
  "Judas", 1
)

# Verify all songs will join
top_10_songs %>% anti_join(gaga_genius2)

# left join in top 10 song dummy
gaga_genius3 <- gaga_genius2 %>% left_join(top_10_songs)
# change the rest to 0's
gaga_genius3$top_10_song[is.na(gaga_genius3$top_10_song)] <- 0

fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
gaga_genius3$lyric <- sapply(gaga_genius3$lyric, fix.contractions)

# get rid of special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
gaga_genius3$lyric <- sapply(gaga_genius3$lyric, removeSpecialChars)

# convert everything to lower case
gaga_genius3$lyric <- sapply(gaga_genius3$lyric, tolower)

write_csv(gaga_genius3, "../data/gaga.csv", na="")


# Start Here!! -----------------------------------------------------------

gaga_genius3 <- read_csv('../data/gaga.csv')

# How many words are in each song?
full_word_count <- gaga_genius3 %>%
  unnest_tokens(word, lyric) %>%
  group_by(track_title, top_10_song) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))

head(full_word_count)

# create a histogram of wordcount per song
full_word_count %>% ggplot() +
  geom_histogram(aes(x= num_words), fill ="magenta4")+
  ylab("Song Count")+
  ylab("Word Count per Song")

# filtering words.
gaga_words_filtered <- gaga_genius3 %>%
  unnest_tokens(word, lyric) %>%
  #anti_join(stop_words) %>%
  filter(nchar(word) > 0)

# what are stop words?
#head(stop_words)
#a <- stop_words

# Now lets find out what the most frequently used words are!
gaga_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill="magenta4") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Lady Gaga Lyrics") +
  coord_flip()

# Lets make a quick wordcloud of the top 300 words
gaga_words_counts <- gaga_words_filtered %>%
  count(word, sort = TRUE)

wordcloud2(gaga_words_counts[1:300, ], size = .5)

# What about word length?
gaga_word_lengths <- gaga_genius3 %>%
  unnest_tokens(word, lyric) %>%
  distinct() %>%
  mutate(word_length = nchar(word)) %>% 
  na.omit(word_length) %>%
  group_by(word_length) %>% 
  summarize(count= n())


# preprocessing ---------------------------------------------------------------------

df <- read_csv('../data/gaga.csv')
df <- df %>%
  group_by(track_title) %>%
  summarise(text=paste(lyric,collapse=' ')) %>% 
  ungroup() %>% 
  filter(text != "NA") # drop chromatica I, II, III :(

# clean and cast to dtm
dtm <- df %>% 
  unnest_tokens(word, text) %>%
  group_by(track_title, word) %>% 
  anti_join(stop_words) %>%
  filter(nchar(word) > 3) %>% 
  count() %>% 
  ungroup() %>% 
  cast_dtm(document = track_title, term = word, value = n)

# dtm objects contain the names `i` and `j` which are the indices for where entries are in the sparse
# matrix. If dm$i does not contain a particular row index `p`, then row `p` is empty.
ui = unique(dtm$i)
dtm <- dtm[ui,]



# Run LDA -----------------------------------------------------------------

k = 3
## First LDA run
system.time({
  vem_model_1 <- LDA(dtm, k = k, control = list(seed = 650))
})

# visualizing functions ---------------------------------------------------
gamma_gen <- function(x){
  # create a matrix with gamma values, documents, and topic #
  lda_gamma <- tidy(x, documents = documents, matrix = "gamma")
  
  # spread it out into a matrix
  gamma_init <- lda_gamma %>% 
    mutate(topic = paste('Gamma Topic ', topic)) %>% 
    spread(key = topic, value = gamma)
  
  # Generate blank topic columns
  for (i in 1:k){
    gamma_init[paste0('Topic_', i)] <- 0 
  }
  
  # now lets create binaries for whether or not each
  # account falls into each category (gamma > 0.5.
  for (col in 2:(k+1)){
    for (row in seq_along(gamma_init$document)){
      if(gamma_init[[col]][[row]] > 0.5){
        gamma_init[[col+k]][[row]] <- 1
      }
    }
  }
  # proportion of authors falling into each topic
  #print(colSums(gamma_init[(k+2):ncol(gamma_init)])/nrow(gamma_init))
  
  return(gamma_init)
}


viz_terms <- function(x){
  # look at top 10 terms by topic for 5 topics
  x %>% 
    group_by(topic) %>% 
    top_n(15, beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta) %>% 
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
}

# Lets See what the topics are! -----------------------------------------------------------------

terms_1 <- tidy(vem_model_1, matrix = 'beta')
# visualize the model
viz_terms(terms_1)
# generate gamma matrix
gamma_1 <- gamma_gen(vem_model_1)

# LDAvis ---------------------------------------------------------------------


library(LDAvis)

## Examine Intertopic Distance and relevant terms
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

# launches graph in browser
serVis(topicmodels2LDAvis(vem_model_1))

