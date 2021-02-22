
source("setup.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # For debugging
  output$text1 <- renderPrint({
    cat(paste("Session ID: ",Sys.getpid()," \n"))
    cat(paste("Text Value: ",input$extract," \n"))
    cat(paste("Select Value: ",input$n_topics," \n"))
  })
  
  output$gender_waffle <- renderPlot({
    
    # Prevents plot from trying to generate on empty data
    req(input$extract)
    
    ggplot(sentence_analysis(input$extract)) +
      geom_tile(aes(x = x, y = y, fill = predG),
                color = "white", size = 2) +
      coord_equal() +
      theme_minimal() +
      scale_fill_gradient(
        low = fColour,
        high = mColour,
        limits=c(0,1),
        na.value = "grey48") +
      guides(fill = guide_colourbar(title = "If the square representing the sentence isn't grey, the sentence is\npredominantly Female  \u2190  -  \u2192  predominantly Male",
                                    title.position = "top",
                                    title.hjust = 0.5,
                                    label = F,
                                    ticks = F,
                                    barwidth = 30)) +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 16),
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank())
    
  })
  
  output$gender_clouds <- renderPlot({
    
    req(input$extract)
    
    if(nrow(sentence_analysis(input$extract)) > 1) {
      wordsdf <- sentence_analysis(input$extract) %>%
        mutate(text = as.vector(text)) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        mutate(factorG = factor(ifelse(factorG == "NaN", "Neutral", as.character(factorG)),
                                levels = c("Female", "Neutral", "Male"))) %>%
        group_by(factorG) %>%
        count(word) %>%
        # To allow binding with new level
        mutate(factorG = as.character(factorG)) %>%
        filter(word != "") %>%
        arrange(desc(n))
      
      for(gender in c("Female", "Male", "Neutral")) {
        # want a cloud for each even if there are no words,
        # so create N/A input so that there is a cloud that just
        # says N/A if necessary
        if (!any(grepl(gender, wordsdf$factorG))) {
          wordsdf <- rbind.data.frame(wordsdf,
                                      c(factorG = gender,
                                        word = "N/A",
                                        n = 2))
        }
      }
      
      wordsdf$factorG <- factor(wordsdf$factorG,
                                levels = c("Female", "Neutral", "Male"))
      
      # Facet labels, to reduce confusion (e.g. "That's not a male word" for words used in "male" sentences)
      factorGLabs <- c("Predominantly Female\nSentences", 
                       "Neutral\nSentences", 
                       "Predominantly Male\nSentences")
      names(factorGLabs) <- c("Female", "Neutral", "Male")
      
      ggplot(wordsdf, aes(label = word, colour = factorG, size = as.numeric(n))) +
        geom_text_wordcloud_area(shape = "circle", rm_outside = T) +
        facet_wrap(~factorG,
                   labeller = labeller(factorG = factorGLabs)) +
        scale_radius(range = c(5, 20), limits = c(0, NA)) +
        scale_color_manual(breaks = c("Female", "Male", "Neutral"),
                           values=c(fColour, mColour, "grey48")) +
        theme_minimal() +
        theme(strip.text.x = element_text(size = 16),
              axis.text.y = element_text(size = 12))
      
    }
  })
  
  output$hover_info <- renderUI({
    
    hover <- input$plot_hover
    
    if(is.null(hover)){
      
      wellPanel(
        style = "position:relative",
        p(HTML("Hover the mouse over one of the squares to see the sentence it represents."))
      )
    } else {
      
      xMouse <- round(hover$x, 0)
      yMouse <- round(hover$y, 0)
      
      # filter() gave "can't slice a scalar" error
      textOut <- subset(sentence_analysis(input$extract), 
                        x == xMouse & y == yMouse)
      
      wellPanel(
        style = "position:relative",
        p(HTML(paste0("<b> Sentence #: </b>", textOut$id, "<br/>",
                      "<b> Text: </b>", textOut$text, "<br/>"))))
    }
    
  })
  
  output$sa_plot <- renderPlotly({
    
    req(input$extract)
    
    wordsdf <- sentence_analysis(input$extract) %>%
      mutate(text = as.vector(text)) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      mutate(factorG = factor(ifelse(factorG == "NaN", "Neutral", as.character(factorG)),
                              levels = c("Female", "Neutral", "Male"))) %>%
      group_by(factorG) %>%
      count(word) %>%
      # To allow binding with new level
      mutate(factorG = as.character(factorG)) %>%
      filter(word != "") %>%
      arrange(desc(n))
    
    for(gender in c("Female", "Male", "Neutral")) {
      # want a facet for each even if there are no words
      if (!any(grepl(gender, wordsdf$factorG))) {
        wordsdf <- rbind.data.frame(wordsdf,
                                    c(factorG = gender,
                                      word = "N/A",
                                      n = 2))
      }
    }
    
    wordsdf$factorG <- factor(wordsdf$factorG,
                              levels = c("Female", "Neutral", "Male"))
    
    wordsSentiments <- wordsdf %>%
      inner_join(ncr_tibble) %>%
      group_by(factorG) %>%
      add_count(sentiment, name = "sentiment_count") %>%
      ungroup() %>%
      # to order by most used sentiment across all genders
      arrange(desc(gender), desc(sentiment_count)) %>%
      mutate(factorS = factor(sentiment, levels = unique(sentiment))) %>%
      group_by(factorG, sentiment) %>%
      mutate(associated_words = paste(unique(word), collapse = ", ")) %>%
      select(factorG, factorS, sentiment, sentiment_count, associated_words) %>%
      unique()
    
    sa_static <- 
      ggplot(filter(wordsSentiments, !is.na(factorS))) +
      scale_fill_manual(breaks = c("Female", "Male", "Neutral"),
                        values=c(fColour, mColour, "grey48")) +
      geom_col(inherit.aes = F, data = wordsSentiments,
               aes(x = fct_rev(factorS), y = sentiment_count, 
                   fill = factorG,
                   text = paste0("Associated word(s):<br>", associated_words)), 
               show.legend = F) +
      scale_x_discrete() +
      coord_flip() +
      facet_grid(~factorG, drop = F) +
      labs(y = "Word count per emotion \n(note that some words count towards several emotions)",
           x = "") +
      theme_minimal() +
      theme(legend.position = "none") +
      theme(strip.text.x = element_text(size = 16),
            strip.text.y = element_text(size = 24))
    
    plotly::ggplotly(sa_static, tooltip = c("text"))
  })
  
  output$sa_progress <- renderPlot({
    
    req(input$extract)
    
    sentiment_summary <- sentence_analysis(input$extract) %>%
      mutate(text = as.vector(text)) %>%
      unnest_tokens(token = "ngrams", n = 2, bigram, text) %>%
      separate(bigram, c("previous_word", "word"), sep = " ") %>%
      anti_join(stop_words) %>%
      mutate(factorG = factor(ifelse(factorG == "NaN", "Neutral", as.character(factorG)),
                              levels = c("Female", "Neutral", "Male")),
             coef = ifelse(previous_word %in% c("not", "no", "never"),
                           -1,
                           1)) %>%
      inner_join(afinn_tibble) %>%
      mutate(sentiment_value = value * coef) %>%
      group_by(id) %>%
      summarise(sentence_sentiment = sum(sentiment_value))
    
    sentiment_sentences <- sentence_analysis(input$extract) %>%
      mutate(text = as.character(text)) %>%
      left_join(sentiment_summary, by = "id") %>%
      mutate(sentence_sentiment = ifelse(is.na(sentence_sentiment), 
                                         0,
                                         sentence_sentiment))
    
    ggplot(sentiment_sentences) +
      geom_col(aes(x = id, y = sentence_sentiment, 
                   fill = predG, colour = predG), 
               size = 1.2,
               width = 0.8) +
      scale_colour_gradient(
        low = fColour,
        high = mColour,
        limits=c(0,1),
        na.value = "grey48",
        guide = "none") +
      labs(y = "Positive/negative emotional score",
           x = "") +
      theme_minimal() +
      scale_fill_gradient(
        low = fColour,
        high = mColour,
        limits=c(0,1),
        na.value = "grey48") +
      xlim(c(0, max(sentiment_sentences$id))) +
      guides(fill = guide_colourbar(title = "If the bar representing the sentence isn't grey, the sentence is\npredominantly Female  \u2190  -  \u2192  Predominantly Male",
                                    title.position = "top",
                                    title.hjust = 0.5,
                                    title.vjust = 0.5,
                                    label = F,
                                    ticks = F,
                                    barwidth = 30)) +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 16),
            axis.text.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 16))
    
  })
  
  
  output$sa_hover <- renderUI({
    
    hover <- input$plot_hover
    
    if(is.null(hover)){
      
      wellPanel(
        style = "position:relative",
        p(HTML("Hover the mouse over one of the bars to see the sentence it represents."))
      )
    } else {
      
      xMouse <- round(hover$x, 0)
      
      # filter() gave "can't slice a scalar" error
      textOut <- subset(sentence_analysis(input$extract), 
                        id == xMouse)
      
      wellPanel(
        style = "position:relative",
        p(HTML(paste0("<b> Sentence #: </b>", textOut$id, "<br/>",
                      "<b> Text: </b>", textOut$text, "<br/>"))))
    }
    
  })
  
  
  output$topic_plot <- renderPlot({
    
    req(input$extract)
    
    wordsdf <- sentence_analysis(input$extract) %>%
      mutate(text = as.vector(text)) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) 
    
    doc_matrix <- wordsdf %>%
      group_by(word) %>%
      tally() %>%
      # cast_dtm() needs a document ID
      mutate(doc = rep("1", length(unique(word)))) %>%
      cast_dtm(doc, word, n)
    
    doc_lda <- topicmodels::LDA(doc_matrix,
                                k = ifelse(input$n_topics != "NA",
                                           as.numeric(as.character(input$n_topics)),
                                           # setting arbitrarily, they can change it 
                                           # according to what comes out
                                           3),
                                control = list(seed = 1234)) 
    
    doc_lda_tibble <- tidytext::tidy(doc_lda) %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      rename(word = term)
    
    doc_lda_tibble %>% inner_join(wordsdf) %>%
      group_by(word, topic) %>%
      summarise(meanPredG = mean(predG, na.rm = T),
                meanBeta = mean(beta, na.rm = T)) %>%
      mutate(topic_label = paste0("Topic ", topic)) %>%
      ggplot(aes(label = word, colour = meanPredG, 
                 size = meanBeta)) +
      geom_text_wordcloud_area(shape = "circle", 
                               rm_outside = F,
                               show.legend = T) +
      facet_wrap(~topic_label,
                 ncol = 2) +
      scale_radius(range = c(5, 20), limits = c(0, NA)) +
      scale_colour_gradient(
        low = fColour,
        high = mColour,
        limits = c(0,1),
        na.value = "grey48") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_text(size = 16),
            strip.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 12)) +
      guides(colour = guide_colourbar(title = 
                                        "If a word is not grey, the sentences in which it featured were\npredominantly Female  \u2190    -    \u2192  predominantly Male",
                                      title.position = "top",
                                      title.vjust = 0.5, 
                                      title.hjust = 0.5,
                                      label = F,
                                      ticks = F,
                                      barwidth = 30),
             size = F)
    
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste0("VisualisingMFRep_", Sys.Date(),".html"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "genderText.Rmd")
      file.copy("genderText.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      source("setup.R")
      
      params <- list(extract = input$extract,
                     n_topics = input$n_topics)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
})

