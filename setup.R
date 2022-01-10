# Aux functions and library set up for the app

# Load packages
library(tidyverse)
library(magrittr)
library(ggwordcloud)
library(tm)
library(tidytext)
library(ggpol)
library(ggnewscale)
library(reshape2)
library(textdata)
library(gt)

# Set up for text analysis
m_list <- c(tolower(read_table("S2-2_mod.txt", col_names = F)[["X1"]]),
            # adding pronouns, titles and family 
            "he", "him", "his", "guys?", "m[ae]n", "boys?", "gentlem[ae]n","male","mr",
            "father","son","da", "dad","daddy","husband", "uncle", "nephew", "brother-in-law",
            "brother", "stepbrother", "granpa", "gramps", "grandad", "granda", 
            "grandfather", "grandson", "mister")
m_string <- paste0("\\b", paste0(m_list, collapse = "\\b|\\b"), "\\b")


f_list <- c(tolower(read_table("S2-1_mod.txt", col_names = F)[["X1"]]),
            # adding pronouns, titles and family 
            "she","hers?","girls?","wom[ae]n", "lady","ladies","female","mrs", "maid",
            "mother","daughter","mum","ma", "m[ou]mmy","wife", "aunt", "auntie", "niece", 
            "sister", "sister-in-law", "stepsister", "granny", "grandma", "gran", 
            "grandmother", "granddaughter", "ms", "maam", "madam", "mam")
f_string <- paste0("\\b", paste0(f_list, collapse = "\\b|\\b"), "\\b")

fColour <- "#e03d60"
mColour <- "#1d5e8c"

fGender <- function(vector){
  vector[vector > 0.5] <- "Male"
  vector[vector < 0.5] <- "Female"
  vector[vector == 0.5] <- "Neutral"
  # if previous transformations occur, the class changes to character
  vector[vector == "NaN" | is.nan(vector)] <- "Neutral"
  return(vector)
}

sentence_analysis <- function(x){
  textdf <- corpus::text_split(x) %>%
    mutate(sumM = 
             stringi::stri_count_regex(tolower(text), 
                                       m_string),
           sumF = 
             stringi::stri_count_regex(tolower(text), 
                                       f_string),
           predG = sumM / (sumM + sumF),
           id = 1:length(text),
           sentNum = paste0("Sentence #", id, ":"),
           denom = ceiling(sqrt(length(text))),
           y = -ceiling(id / denom),
           x = ifelse(id %% denom == 0,
                      denom,
                      id %% denom),
           factorG = as.factor(fGender(predG)))
  
  return(textdf)
}

## Getting the lexicons into Shiny - the requirement for a manual
# response to "will you cite these resources" makes it crash
# I am citing them, so this is a reasonable workaround

## This only needs to run once in order to create the RData file
# afinn_tibble <- textdata::lexicon_afinn(manual_download = TRUE)  
# ncr_tibble <- textdata::lexicon_nrc(manual_download = TRUE)
# 
# save(list = c("afinn_tibble", "ncr_tibble"), 
#      file = "sentiment_tibbles.RData")

load("sentiment_tibbles.RData")

