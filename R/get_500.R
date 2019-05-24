#' get_500
#'
#' This function takes an epoch time and a subreddit name and returns the titles of the 500 posts to that subreddit before the time. It also returns the time (epoch) of the last post scraped
#'
#' @param time Epoch time for the scraping to begin
#' @param subreddit A valid subreddit name
#'
#' @return The titles of the 500 post previous to the input time of the subreddit.
#' @export
#'
#' @examples
#'subreddit <- "rabbits"
#'times <- 1558699200   #24/05/2019 12:00:00 pm in epoch time
#'
#'titles <- get_500(times,subreddit)
#'
get_500 <- function(time,subreddit){

  url<-glue("https://api.pushshift.io/reddit/search/submission/?subreddit={subreddit}&size=500&before={time}")

  URL <- GET(url)

  if (http_error(URL)){
    stop(paste0(subreddit," cannot contain any symbols"))
  }

  jsonParsed <- content(URL,as="parsed")
  reddit_json <- jsonParsed$data

  if (is_empty(reddit_json)){
    stop(paste0(subreddit," is not a valid subreddit"))
  }

  times <- reddit_json %>% list.select(created_utc) %>% unlist
  final_time = times[length(times)]

  titles <- reddit_json %>% list.select(title) %>% unlist

  return(list(final_time = final_time,title = titles))

}
