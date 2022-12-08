# Set working directory to an appropriate one on your computer


#Load the library and functions needed
source("Libraries.r")
source("Cleaning.r")


# Save the raw penguin data in a folder called "data_raw" and load the data 
write.csv(penguins_raw, "data_raw/penguins_raw.csv")
penguins_raw <- read.csv("data_raw/penguins_raw.csv")


# Clean the raw penguin data and save it in a folder called "data_clean"
penguins_now_clean <- cleaning(penguins_raw)
write.csv(penguins_now_clean, "data_clean/penguins_clean.csv")


# Subset flipper lengths and body mass data, and remove missing data
remove_empty_flipperlength_bodymass <- function(data_clean){
  data_clean %>%
    filter(!is.na(flipper_length_mm), !is.na(body_mass_g)) %>%
    select(species, flipper_length_mm, body_mass_g)
}

penguins_flippers_bodymass <- remove_empty_flipperlength_bodymass(penguins_now_clean)

head(penguins_flippers_bodymass)


# Create a linear regression model between body mass and flipper length
linear_model <- lm(body_mass_g ~ flipper_length_mm, penguins_flippers_bodymass)

summary(linear_model)

anova(linear_model)


# Plot a scatterplot of flipper length against body mass, with the linear model overlaid
flipper_bodymass_scatterplot <- ggplot(data = penguins_flippers_bodymass,
                                       aes(x = body_mass_g, 
                                           y = flipper_length_mm)) +
  geom_point(size = 1, position = "jitter",
             aes(colour = species, shape = species)) +
  geom_smooth(method = "lm", colour = "black") +
  theme_bw() +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) + 
  labs(title = "Penguin Body Mass VS Flipper Length", subtitle = "Body Mass and Flipper Lengths for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Flipper length (mm)", y = "Body mass (g)", colour = "Penguin Species", shape = "Penguin Species") +
  scale_x_continuous(breaks = seq(2500, 6500, by = 500)) +
  scale_y_continuous(breaks = seq(170, 250, by = 10))

flipper_bodymass_scatterplot


# Save the scatterplot into a png file, and put it in a folder called "figures"
agg_png("figures/flipper_bodymasss_scatterplot.png", 
        width = 20, height = 20, units = "cm", res = 600, scaling = 1)
flipper_bodymass_scatterplot
dev.off()




