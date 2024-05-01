
library(haven)
library(tidyverse)
# setwd('')

data <- read_sav("L_AlgoSoc_wave1_1.0p.sav")



#### Ability to distinguish AI from real #########

#TAKEAWAY: Journalists more trusted

# Define a function to create histograms for each variable
create_histograms <- function(data, var_names, var_labels) {
  # Calculate number of rows and columns for the grid layout
  n <- length(var_names)
  ncol <- 2  # Number of columns in the grid
  nrow <- ceiling(n / ncol)  # Number of rows in the grid
  
  # Create an empty list to store individual plot objects and mean values
  plots <- vector("list", length = n)
  mean_values <- numeric(length = n)
  
  # Loop through each variable name and create a histogram
  for (i in seq_along(var_names)) {
    var <- var_names[i]
    label <- var_labels[i]
    
    # Calculate the mean of the variable
    var_mean <- mean(data[[var]], na.rm = TRUE)
    
    # Store the mean value
    mean_values[i] <- var_mean
    
    # Create a ggplot histogram
    p <- ggplot(data, aes_string(x = var)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      labs(title = paste("", label),
           x = label,
           y = "Frequency") +
      geom_vline(xintercept = var_mean, color = "red", linetype = "dashed", size = 1) 
    
    # Store the plot in the list
    plots[[i]] <- p
  }
  
  # Sort the plots based on mean values
  sorted_plots <- plots[order(mean_values, decreasing = TRUE)]
  
  # Arrange sorted plots in a grid layout
  gridExtra::grid.arrange(grobs = sorted_plots, ncol = ncol)
}

# Specify the variable names and corresponding labels
var_names <- paste0("CONF", 1:8)
  var_labels <- c(
  "People from the general population",
  "Members of Parliament",
  "Judges",
  "Journalists",
  "Police officers",
  "Doctors",
  "Friends/family",
  "Yourself"
)

#function to create histograms for each variable
create_histograms(data, var_names, var_labels)





########## Exposure to AI Content #######


#TAKEAWAY: More exposure to photos than text
mean(data$EXPO1,na.rm = T)
mean(data$EXPO2,na.rm = T)

# Specify the variable names and corresponding labels for EXPO1-EXPO3
expo_var_names <- c("EXPO1", "EXPO2", "EXPO3")
expo_var_labels <- c(
  "Texts made by AI",
  "Photos or images made by AI",
  "Videos made by AI"
)

# Call the function to create histograms for EXPO1-EXPO3 variables
create_histograms(data, expo_var_names, expo_var_labels)




####### Knowledge of AI Terms ##########


# Specify the variable names and corresponding labels for KAI1-KAI5
kai_var_names <- c("KAI1", "KAI2", "KAI3", "KAI4", "KAI5")
kai_var_labels <- c(
  "Generative AI",
  "ChatGPT",
  "Deepfakes",
  "Algorithms",
  "Chatbots"
)

# Call the function to create histograms for KAI1-KAI5 variables
create_histograms(data, kai_var_names, kai_var_labels)


###### Importance of AI #########

#TAKEAWAY: very low concern

# Specify the variable names and corresponding labels for IS1-IS8
is_var_names <- c("IS1", "IS2", "IS3", "IS4", "IS5", "IS6", "IS7", "IS8")
is_var_labels <- c(
  "Computers or AI replacing human decision makers",
  "Healthcare and elderly care",
  "Crime and safety",
  "Immigration and integration",
  "Economy and income",
  "Social system, welfare state",
  "Education, innovation, culture",
  "Housing construction and costs"
)

# Call the function to create histograms for IS1-IS8 variables
create_histograms(data, is_var_names, is_var_labels)


##### Whether they can determine social benefits etc ############


mean(data$TADS1, na.rm=T)
mean(data$TADS2, na.rm=T)
mean(data$TADS3, na.rm=T)


###### Risky, Useful, Fairer ###########
#Media system - 3.62
mean(data$ADSR1, na.rm = T) #Useful 3.08
mean(data$ADSR2, na.rm = T) #Risky - 4.87
mean(data$ADSR3, na.rm = T) #Fairer - 2.91

mean(((data$ADSR1+data$ADSR3)/2), na.rm = T) #Useful and fairer 2.99


#legal system - 3.69
mean(data$ADSRt2_1, na.rm = T) #Useful 3,09
mean(data$ADSRt2_2, na.rm = T) #Risky 5.02
mean(data$ADSRt2_3, na.rm = T) #Fairer 2.97

mean(((data$ADSRt2_1+data$ADSRt2_3)/2), na.rm = T) #WUseful and Fairer 3.03


#health - 3.81
mean(data$ADSRt3_1, na.rm = T) #Useful 3,51
mean(data$ADSRt3_2, na.rm = T) #Risky 4.82
mean(data$ADSRt3_3, na.rm = T) #Fairer 3.10
mean(((data$ADSRt3_1+data$ADSRt3_2+data$ADSRt3_3)/3), na.rm = T)




######## Values - respect for user privacy most important ###########


# Define a function to create histograms for each variable
create_histograms_ordered <- function(data, var_names, var_labels) {
  # Calculate the mean of each variable
  var_means <- sapply(var_names, function(var) mean(data[[var]], na.rm = TRUE))
  
  # Sort variable names based on mean values
  sorted_var_names <- var_names[order(var_means)]
  
  # Calculate number of rows and columns for the grid layout
  n <- length(sorted_var_names)
  ncol <- 2  # Number of columns in the grid
  nrow <- ceiling(n / ncol)  # Number of rows in the grid
  
  # Create an empty list to store individual plot objects
  plots <- vector("list", length = n)
  
  # Loop through each variable name and create a histogram
  for (i in seq_along(sorted_var_names)) {
    var <- sorted_var_names[i]
    label <- var_labels[var_names == var]
    var_mean <- var_means[var_names == var]
    
    # Create a ggplot histogram
    p <- ggplot(data, aes_string(x = var)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      labs(title = paste("Histogram of", label),
           x = label,
           y = "Frequency") +
      geom_vline(xintercept = var_mean, color = "red", linetype = "dashed", size = 1) #+
     #annotate("text", x = var_mean, y = -Inf, label = paste("Mean =", round(var_mean, 2)),
               #vjust = 2, color = "red", size = 4)  # Adjust vjust for vertical positioning
    
    # Store the plot in the list
    plots[[i]] <- p
  }
  
  # Arrange plots in a grid layout
  gridExtra::grid.arrange(grobs = plots, ncol = ncol)
}

# Specify the variable names and corresponding labels for ADSV1-ADSV8
adsv_var_names <- c("ADSV1", "ADSV2", "ADSV3", "ADSV4", "ADSV5", "ADSV6", "ADSV7", "ADSV8")
adsv_var_labels <- c(
  "Respect for user privacy",
  "Ease of use",
  "Politically neutral",
  "Freedom to choose information",
  "Non-discriminatory",
  "Accessibility",
  "Transparency about decision making",
  "Human oversight"
)

data %>% 
  select(ADSVR_1) %>% 
  mutate(ADSVR_1 = sjmisc::to_label(ADSVR_1)) %>% 
  count(ADSVR_1, sort = T)

#PLOT HERE
create_histograms_ordered(data, adsv_var_names, adsv_var_labels)


#BARPLOT
# Calculate the mean values for each variable
var_means <- sapply(adsv_var_names, function(var) mean(data[[var]], na.rm = TRUE))

# Create a data frame for plotting
plot_data <- data.frame(variable = adsv_var_labels, mean_score = var_means)

# Sort the data frame by mean scores
plot_data <- plot_data[order(plot_data$mean_score), ]

# Create a bar plot
ggplot(plot_data, aes(x = variable, y = mean_score)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = round(mean_score, 2)), vjust = -0.5, color = "black", size = 4) +
  labs(title = "Average Scores for Variables",
       x = "Variables",
       y = "Average Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



####### Open ended answers ###########

head(data$ADSD1)


library(tm)
library(wordcloud2)

# Preprocess the text data
corpus <- Corpus(VectorSource(data$ADSD1))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("dutch"))
corpus <- tm_map(corpus, stripWhitespace)

# Convert the corpus to a document-term matrix
dtm <- DocumentTermMatrix(corpus)

# Convert the document-term matrix to a matrix
m <- as.matrix(dtm)

# Calculate word frequencies
word_freq <- colSums(m)

# Create a data frame with word frequencies
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

# Sort the data frame by word frequency in descending order
word_freq_df <- word_freq_df[order(-word_freq_df$freq), ]

# Generate the word cloud
wordcloud2(word_freq_df, size = 1.5)



### Sentiment

library(textdata)
library(sentimentr)

# Perform sentiment analysis
sentiment_scores <- sentiment(data$ADSD1)

# Check for missing or empty values in the original text column
missing_values <- is.na(data$ADSD1) | data$ADSD1 == ""
if (any(missing_values)) {
  cat("There are missing or empty values in the original text column.\n")
  # Remove rows with missing or empty values from the sentiment scores data frame
  sentiment_scores <- sentiment_scores[!missing_values, ]
}

# Subset the sentiment scores data frame to match the length of the original text column
sentiment_scores <- sentiment_scores[1:length(data$ADSD1), ]

# Keep the original text column alongside the sentiment scores
sentiment_scores <- cbind(data$ADSD1, sentiment_scores)

# Join sentiment scores back to the original dataset
data_with_sentiment <- cbind(data$ADSD1, sentiment_scores)




