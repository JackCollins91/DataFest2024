

#  ________          __        ___________             __   
#  \______ \ _____ _/  |______ \_   _____/___   ______/  |_ 
#   |    |  \\__  \\   __\__  \ |    __)/ __ \ /  ___|   __\
#   |    `   \/ __ \|  |  / __ \|     \\  ___/ \___ \ |  |  
#  /_______  (____  /__| (____  |___  / \___  >____  >|__|  
#          \/     \/          \/    \/      \/     \/       

library(dplyr)

#################################
#                               #
# Get started with ASA DataFest #
#                               #
#################################

# In this R script, we demonstrate how to:
# 1) Access the ASA DataFest 2024 dataset.
# 2) Read the data into R data frames.
# 3) Conduct basic analysis.

#################################
#                               #
#   Step 1: Access the data     #
#                               #
#################################

# To access the ASA dataset, use this link: 
# https://bwsyncandshare.kit.edu/s/ErACwGqLAJ9iyxS
# Read the documentation carefully to understand the contents of the dataset.

#################################
#                               #
#   Step 2: Read the data       #
#                               #
#################################

data_directory <- '2024-ASADataFest-Data-and-Documentation/2024-ASADataFest-Data-and-Documentation/'
checkpoints <- read.csv(paste0(data_directory, 'Data Files/checkpoints.csv'))
items <- read.csv(paste0(data_directory, 'Data Files/items.csv'))
media_views <- read.csv(paste0(data_directory, 'Data Files/media_views.csv'))
page_views <- read.csv(paste0(data_directory, 'Data Files/page_views.csv'))
responses <- read.csv(paste0(data_directory, 'Data Files/responses.csv'))

#################################
#                               #
#   Step 3: Example Analysis    #
#                               #
#################################

# Question: Are responses more often correct when the student has retried more 
# pages in that chapter?

# Derive each individual student's portion of correct item responses per chapter.
responses <- responses %>%
  mutate(score = points_earned / points_possible)

average_scores <- responses %>%
  group_by(student_id, chapter) %>%
  summarise(average_score = mean(score, na.rm = TRUE))

# Derive each student's number of 'try again' clicks per chapter.
average_try_again_clicks <- page_views %>%
  group_by(student_id, chapter) %>%
  summarise(average_retry_clicks = mean(tried_again_clicks, na.rm = TRUE))

# Join the two tables together using 'student_id' and 'chapter' as a 
# composite key.
results <- merge(
  average_scores, 
  average_try_again_clicks,
  by = c("student_id", "chapter"),
  all.x = TRUE
)

# Visualize the results by scatter-plotting the average scores with the average 
# number of retries.
plot(results$average_retry_clicks, results$average_score,
     xlab = "Average Retry Clicks",
     ylab = "Average Score",
     main = "Scatter Plot of Average Retry Clicks vs Average Score"
)
lm_model <- lm(average_score ~ average_retry_clicks, data = results)
abline(lm_model, col = "red")

#################################
#                               #
#  Step 4: Enjoy DataFest 2024! #
#                               #
#################################
