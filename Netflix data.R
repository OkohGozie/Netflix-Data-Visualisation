
#  Netflix: Top 10 Most Watched Genres – R + ggplot2 (Only)

# Install packages 
install.packages("tidyverse")
install.packages("RColorBrewer")

# Load libraries
library(tidyverse)
library(RColorBrewer)

# Load data set
df <- read_csv("Netflix_shows_movies_clean.csv")
cat("Dataset loaded:", nrow(df), "titles\n")

#  Extract & Count Genres from 'listed_in'

top_genres <- df %>%
  separate_rows(listed_in, sep = ",\\s*") %>%   # Split by comma + optional space
  mutate(listed_in = str_trim(listed_in)) %>%   # Clean white space
  count(listed_in, name = "count") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10) %>%
  mutate(listed_in = fct_inorder(listed_in))    # Keep order in plot

# Print results
cat("\nTOP 10 MOST WATCHED GENRES:\n")
print(top_genres)


# Horizontal Bar Plot (ggplot2)
p <- ggplot(top_genres, aes(x = count, y = listed_in, fill = count)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = count), 
            hjust = -0.3, 
            size = 4.5, 
            color = "white", 
            fontface = "bold") +
  scale_fill_gradientn(colors = brewer.pal(10, "Viridis")) +
  labs(
    title = "Top 10 Most Watched Genres on Netflix",
    x = "Number of Titles",
    y = "Genre"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.major.y = element_blank()
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)))

#Show plot
print(p)

# Save plot
ggsave("Netflix_Top10_Genres_R.png", 
       plot = p, 
       width = 12, height = 6, 
       dpi = 300, 
       bg = "white")

cat("\nPlot saved: Netflix_Top10_Genres_R.png\n")
