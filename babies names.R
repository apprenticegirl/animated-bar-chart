require(gganimate)
require(dplyr)
require(RColorBrewer)

#Very important, you must install Image Magick and Gifski package##

#Now, let's upload the numbers
df <- read_csv("~/newborns.csv")

#First, we must create the dataframe with the data we need
babies <- df %>%
  group_by (year) %>%
  mutate(Rank=dense_rank(desc(total))) #we have to include a rank by year so we can order data in descending order


babies$num_label<-as.character(babies$total) #Included this variable as a character because I had some issues with the labels

#We must create a color palette with enough colors, so each name has their own
#Define the number of colors you want
nb.cols <- 25
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

#Create static graphs
p <- ggplot(babies, aes(Rank)) + 
  geom_tile(aes(y=total/2, height=total, width=0.9, fill=name), alpha=0.5, colour="grey50") + 
  geom_text(aes(y = 0.1, label = name), vjust = 1, hjust = 1.2, size =8) + 
  geom_text(aes(y=total, label = num_label), hjust= -0.1, size=8)+
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  scale_fill_manual(values = mycolors)  +
  guides(color = FALSE, fill = FALSE)  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        plot.margin = margin(0.5, 3, 0.5, 5, "cm"),
        plot.caption = element_text(hjust=0),
        plot.title = element_text(size = 28, face = "bold"))

#Let's animate the graph
animated_graph <- p + transition_states(year, transition_length = 2, state_length = 1) + ease_aes('cubic-in-out') +
  view_follow(fixed_y = TRUE) +
  view_follow(fixed_x = TRUE) +
  labs(title = 'Birth year: {as.integer(closest_state)}')

#Finally, let's turn it into a gif
animate(animated_graph, duration = 30, fps=25, width = 1200, height = 628)
anim_save("babies names.gif")


