#' Get Title word frequency
#'
#' This function takes a subreddit and the number of posts we wish to scrape, and returns a data frame with the most common words in the title of the posts and their respective frequencies and a plot of the wordcloud.
#'
#' @param num_posts Number of reddit posts to be scraped
#' @param subreddit The subreddit to be scraped
#' @param num_words The number of words to be plotted in the wordcloud
#' @param colour_pallete The colour_pallete to be used in the wordcloud
#' @param num_colours The number of different colours to be used in the wordcloud
#'
#' @return A data frame with the 100 most common words and their frequencies and a plot of the wordcloud
#' @export
#'
#' @examples
#' num_posts = 10000;
#' subreddit = "rabbits"
#'
#' #Default plot
#' words_freq <- get_word_freq(num_posts, subreddit)
#'
#' #Custom plot
#' get_word_freq <- get_word_freq(num_posts, subreddit,
#'                                 num_words = 50,
#'                                 colour_pallete = "Set1",
#'                                 num_colours = 5)
#'
get_word_freq <- function(num_posts, subreddit, num_words = 100, colour_pallete = "Dark2", num_colours = 8){

  URL<-GET(glue("https://api.pushshift.io/reddit/search/submission/?subreddit={subreddit}&size=1"))

  if (http_error(URL)){
    stop(paste0(subreddit," cannot contain any symbols"))
  }

  jsonParsed <- content(URL,as="parsed")
  temp_json <- jsonParsed$data

  if (is_empty(temp_json)){
    stop(paste0(subreddit," is not a valid subreddit"))
  }


  first_post <- temp_json %>% list.select(created_utc,title) %>% unlist()
  begin_time <- first_post[1]

  titles = first_post[2]

  num_searches <- ceiling(num_posts/500)

  for (i in 1:num_searches){

    temp <- get_500(begin_time,subreddit)

    begin_time = temp[[1]]
    temp_title = temp[[2]]

    titles = c(titles,temp_title)

  }


  titles = titles[1:num_posts]

  titles <- as_tibble(titles)
  colnames(titles) <- "title"

  title_tokens <- titles %>% unnest_tokens(word, title)

  data(stop_words)

  title_tokens <- title_tokens %>%
    anti_join(stop_words)


  title_tokens <- title_tokens %>%
    filter(!grepl("[0-9]",word))


  top_words <- title_tokens %>%
    count(word, sort = TRUE) %>% .[1:num_words,]

  wordcloud(words = top_words$word, freq = top_words$n, min.freq = 1,
            max.words=num_words, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(num_colours, colour_pallete))

  return(top_words)

}
