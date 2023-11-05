# Load required libraries
library(dplyr)
library(ggplot2)
library(cowplot)
library(mongolite)

db_name <- "spotifydb"
collection_name <- "sample"
mongo_uri <- "mongodb://localhost:27017/?readPreference=primary&appname=MongoDB%20Compass&ssl=false"

# Connect to the MongoDB database
mongo_conn <- mongo(collection = collection_name, url = mongo_uri, db = db_name)

data <- mongo_conn$find('{}')

# Summary statistics and structure of the dataset
str(data)
summary(data)


#db cleaning
data$released_year <- as.numeric(data$released_year)
data$streams <- as.numeric(data$streams)


# Sort the dataset by streams in descending order and select the top 10 songs
top_songs <- data %>%
  arrange(desc(streams)) %>%
  head(10)

# Create a stacked bar plot for the top 10 songs
stacked_bar_plot <- ggplot(top_songs, aes(x = track_name, y = streams, fill = artist_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Songs of the Year (Stacked Bar Plot)", x = "Song", y = "Streams") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the stacked bar plot
print(stacked_bar_plot)

# Time of the year when most songs were released
release_counts <- data %>%
  mutate(month = format(release_date, "%B")) %>%
  group_by(month) %>%
  summarise(song_count = n()) %>%
  arrange(desc(song_count))


# Data visualization
# Plot top 10 songs
ggplot(top_songs, aes(x = reorder(track_name, streams), y = streams)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Played Songs")


# Plot song releases by month
ggplot(release_counts, aes(x = month, y = song_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Song Releases by Month")

# Select only numerical columns for correlation heatmap
numeric_data <- data %>%
  select(bpm, danceability, valence, energy, acousticness, instrumentalness, liveness, speechiness)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data)

# Create a heatmap using ggplot2
ggplot(data = as.data.frame(as.table(correlation_matrix)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap")



# Scatter Plot: Relationship between valence and danceability
scatter_plot <- ggplot(data, aes(x = valence, y = danceability)) +
  geom_point() +
  labs(title = "Scatter Plot of Valence vs. Danceability", x = "Valence (%)", y = "Danceability (%)")

# Violin Plot: Distribution of streams by playlist
violin_plot <- ggplot(data, aes(x = in_spotify_playlists, y = streams)) +
  geom_violin(fill = "blue") +
  labs(title = "Distribution of Streams by Spotify Playlists", x = "In Spotify Playlists", y = "Streams")

# Stacked Bar Plot: Composition of songs by key
stacked_bar_plot <- data %>%
  group_by(key) %>%
  summarise(song_count = n()) %>%
  ggplot(aes(x = as.factor(key), y = song_count, fill = as.factor(key)))+
  geom_bar(stat = "identity") +
  labs(title = "Composition of Songs by Key", x = "Key", y = "Song Count")

# Create a 2x4 grid of the plots using cowplot
plot_grid(scatter_plot)
plot_grid(stacked_bar_plot)
plot_grid(violin_plot)

histogram_energy <- ggplot(data, aes(x = energy)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Distribution of Energy Levels", x = "Energy (%)") +
  scale_x_continuous(labels = scales::percent_format(scale = 1))

# Display the histogram
print(histogram_energy)

# Prepare the data for the pie chart
key_counts <- data %>%
  group_by(key) %>%
  summarise(count = n())

# Create a data frame for the pie chart
key_data <- data.frame(Key = key_counts$key, Count = key_counts$count)

# Create the pie chart
pie_chart_key <- ggplot(key_data, aes(x = "", y = Count, fill = Key)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Songs by Key")

# Display the pie chart
print(pie_chart_key)
