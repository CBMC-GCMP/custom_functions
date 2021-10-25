wordcloud_topics <- function(keyword = keyword, type, time = "all") {
          
          require(wordcloud2)
          
          if(type == "Queries") {
                    related_queries <- gtrendsR::gtrends(
                              keyword = keyword,
                              low_search_volume = TRUE,
                              time = time) %>% 
                              .$related_queries %>% 
                              filter(related_queries =="top") %>% 
                              mutate(subject = as.numeric(subject)) %>% 
                              select(word=value, freq=subject) %>% 
                              group_by(word) %>% 
                              summarise(freq = sum(freq)) %>% 
                              as.data.frame()
                    
                    rownames(related_queries) <- related_queries$word
                    
                    # Basic plot
                    
                    wordcloud2(data=related_queries,  shape = 'circle')
                    
          }else if(type == "Topics") {
                   
                    related_topics <- gtrendsR::gtrends(
                              keyword = keyword,
                              low_search_volume = TRUE,
                              time = time) %>% 
                              .$related_topics %>% 
                              filter(related_topics =="top") %>% 
                              mutate(subject = as.numeric(subject)) %>% 
                              select(word=value, freq=subject) %>% 
                              group_by(word) %>% 
                              summarise(freq = sum(freq)) %>% 
                              as.data.frame()
                    
                    rownames(related_topics) <- related_topics$word
                    
                    # Basic plot
                    
                    wordcloud2(data=related_topics, shape = 'circle', size = 3)
          }else { 
                    stop("You must choose either Topics or Queries", call. = FALSE)
                    
                    }
          
}
