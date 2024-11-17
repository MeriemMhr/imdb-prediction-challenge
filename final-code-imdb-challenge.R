################################################################################
################### Import data ################################################
################################################################################

imdb_data=read.csv("/Users/macbookpro/Desktop/lxc/git/IMDB_Prediction_Game/IMDB_data.csv")
# imdb_df <- read.csv("/Users/macbookpro/Desktop/lxc/git/IMDB_Prediction_Game/IMDB_data.csv")
# IMDB=read.csv("/Users/macbookpro/Desktop/lxc/git/IMDB_Prediction_Game/IMDB_data.csv")
imdb_df = imdb_data
IMDB = imdb_df

IMDB3=read.csv("/Users/macbookpro/Desktop/lxc/git/IMDB_Prediction_Game/test_data_IMDB_Fall_2023.csv")

attach(IMDB3)
attach(imdb_data) 
attach(IMDB)
# View(imdb_data)

################################################################################
############################### Install ########################################
################################################################################
install.packages("GGally")
install.packages("caTools")
install.packages("boot")
install.packages("dplyr")
install.packages("psych")
install.packages("lmtest")
install.packages("plm")

###################################################################
##################### Loading the Library #########################
###################################################################
library(ggplot2)
library(tidyr)
library(corrplot)
library("car")
library(dplyr)
library(car)
library(gridExtra)
library(ggcorrplot)
library(GGally)
library(tidyverse)
library(stargazer)
require(lmtest)
require(plm)
require(psych)
require(caTools)
require(splines)
require(methods)
library(boot)


###################################################################
########### Step 1 : Descriptive Analysis of Data: ################
###################################################################

###################################################################
##################       Summary statistics    ####################
###################################################################

summary(imdb_score)
summary(movie_budget)
summary(release_year)
summary(duration)
summary(aspect_ratio)
summary(nb_news_articles)
summary(actor1_star_meter)
summary(actor2_star_meter)
summary(actor3_star_meter)
summary(nb_faces)
summary(movie_meter_IMDBpro)

###################################################################
##################          Boxplots           ####################
###################################################################

boxplot(imdb_score)
boxplot(movie_budget)
boxplot(release_year)
boxplot(release_month)
boxplot(release_day)
boxplot(duration)
boxplot(aspect_ratio)
boxplot(nb_news_articles)
boxplot(nb_faces)
boxplot(movie_meter_IMDBpro)

###################################################################
##################          Histogram           ###################
###################################################################

hist(imdb_score)
hist(movie_budget)
hist(release_year)
# hist(release_month)
hist(release_day)
hist(duration)
hist(aspect_ratio)
hist(nb_news_articles)
hist(nb_faces)
hist(movie_meter_IMDBpro)
# Histogram for 'imdb_score'
ggplot(imdb_data, aes(x=imdb_score)) + 
  geom_histogram(binwidth=0.5, fill="blue", color="black", alpha=0.7) +
  labs(title="Distribution of IMDb Scores", x="IMDb Score", y="Count")

# Histogram for 'movie_budget'
# Transforming movie_budget to 'Million' scale
imdb_data$movie_budget_M=imdb_data$movie_budget / 1e6

# Histogram plot for movie_budget in 'Million' scale
ggplot(imdb_data, aes(x=movie_budget_M)) + 
  geom_histogram(binwidth=10, fill="skyblue", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Budget (in Million scale)", x="Movie Budget (in thousands)", y="Count") +
  theme_minimal()

# Histogram for 'duration'
# Transforming duration to 'Hour' scale
imdb_data$duration_hour=imdb_data$duration / 60

# Histogram plot for duration in 'Hour' scale
ggplot(imdb_data, aes(x=duration_hour)) + 
  geom_histogram(binwidth=0.5, fill="lightgreen", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Duration (in Hour scale)", x="Movie Duration (in hours)", y="Count") +
  theme_minimal()

# Histogram for 'nb_news_articles'
ggplot(imdb_data, aes(x=nb_news_articles)) + 
  geom_histogram(binwidth=10, fill="coral", color="black", alpha=0.7) +
  labs(title="Histogram of Number of News Articles", x="Number of Articles", y="Count") +
  theme_minimal()

# Histogram plot for nb_faces
ggplot(imdb_data, aes(x=nb_faces)) + 
  geom_histogram(binwidth=1, fill="lightcoral", color="black", alpha=0.7) +
  labs(title="Histogram of Number of Faces in Movie Posters", x="Number of Faces", y="Count") +
  theme_minimal()

# Histogram plot for release_year
ggplot(imdb_data, aes(x=release_year)) + 
  geom_histogram(binwidth=1, fill="red", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Release Years", x="Release Year", y="Count") +
  theme_minimal()

# Preparing the data
imdb_data$movie_budget_M=imdb_data$movie_budget / 1e6
imdb_data$duration_hour=imdb_data$duration / 60

# Creating individual plots
p1=ggplot(imdb_data, aes(x=imdb_score)) + 
  geom_histogram(binwidth=0.5, fill="blue", color="black", alpha=0.7) +
  labs(title="Distribution of IMDb Scores", x="IMDb Score", y="Count")

p2=ggplot(imdb_data, aes(x=movie_budget_M)) + 
  geom_histogram(binwidth=10, fill="skyblue", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Budget (in Million scale)", x="Movie Budget (in millions)", y="Count")

p3=ggplot(imdb_data, aes(x=duration_hour)) + 
  geom_histogram(binwidth=0.5, fill="lightgreen", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Duration (in Hour scale)", x="Movie Duration (in hours)", y="Count")

p4=ggplot(imdb_data, aes(x=nb_news_articles)) + 
  geom_histogram(binwidth=10, fill="coral", color="black", alpha=0.7) +
  labs(title="Histogram of Number of News Articles", x="Number of Articles", y="Count")

p5=ggplot(imdb_data, aes(x=nb_faces)) + 
  geom_histogram(binwidth=1, fill="lightcoral", color="black", alpha=0.7) +
  labs(title="Histogram of Number of Faces in Movie Posters", x="Number of Faces", y="Count")

p6=ggplot(imdb_data, aes(x=release_year)) + 
  geom_histogram(binwidth=1, fill="red", color="black", alpha=0.7) +
  labs(title="Histogram of Movie Release Years", x="Release Year", y="Count")

# Arranging the plots in a 2x3 grid
grid.arrange(p1, p2, p3, p4, p5, p6, nrow=2)


###################################################################
#######    Visualizing relationships between variables    #########
###################################################################

plot(imdb_score,movie_budget)
plot(imdb_score,release_year)
plot(imdb_score,release_day) # show no good
plot(imdb_score,duration)
plot(imdb_score,aspect_ratio)
plot(imdb_score,nb_news_articles)
plot(imdb_score,nb_faces)
plot(imdb_score,movie_meter_IMDBpro)

###################################################################
##################        Correlation matrix         ##############
###################################################################


numeric_vars = imdb_data[, sapply(imdb_data, is.numeric)]
correlations = cor(numeric_vars, use="pairwise.complete.obs")
corrplot(correlations, method="color")

# List of numeric variables
numeric_vars <- c('movie_id', 'imdb_score', 'movie_budget', 'release_day', 'release_year',
                  'duration', 'aspect_ratio', 'nb_news_articles', 'actor1_star_meter',
                  'actor2_star_meter', 'actor3_star_meter', 'nb_faces', 'action',
                  'adventure', 'scifi', 'thriller', 'musical', 'romance', 'western',
                  'sport', 'horror', 'drama', 'war', 'animation', 'crime',
                  'movie_meter_IMDBpro')

# Calculate correlation matrix
corr_matrix <- cor(imdb_df[, numeric_vars], use = "complete.obs")

# Plot heatmap of the correlation matrix
ggcorrplot(corr_matrix, hc.order = TRUE, 
           type = "full", lab = FALSE, 
           outline.col = "white", 
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726")) + 
  ggtitle('Correlation Matrix of Numeric Variables')

###################################################################
##################        Scatterplot matrix         ##############
###################################################################

selected_vars = c("imdb_score", "movie_budget", "release_year", "duration", "aspect_ratio", "nb_news_articles")
ggpairs(imdb_data[selected_vars])

# Transform actor1_star_meter and movie_meter_IMDBpro like the following:
imdb_df$inverse_actor_ranking = 1/(imdb_df$actor1_star_meter)
imdb_df$inverse_movie_ranking = 1/(imdb_df$movie_meter_IMDBpro)

# Scatter plot for imdb_score vs release_year
p1 <- ggplot(imdb_df, aes(x=release_year, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Release Year", x="Release Year", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs duration
p2 <- ggplot(imdb_df, aes(x=duration, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Duration", x="Duration", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs movie_budget
p3 <- ggplot(imdb_df, aes(x=movie_budget, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Movie Budget", x="Movie Budget", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs movie_meter
p4 <- ggplot(imdb_df, aes(x=imdb_df$inverse_movie_ranking, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Inverse of Movie Meter", x="Movie Meter", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs nb_faces
p5 <- ggplot(imdb_df, aes(x=nb_faces, y=imdb_score)) +
  geom_point(alpha=0.5) +
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(title="IMDb Score vs Number of Faces", x="Number of Faces", y="IMDB Score") +
  theme_minimal() +
  theme(legend.position="none")

# Scatter plot for imdb_score vs inverse of actor 1 meter
p6 <- ggplot(imdb_df, aes(x = imdb_df$inverse_actor_ranking, y = imdb_score)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "IMDb Score vs Inverse of Actor 1 Meter", x = "Actor 1 Meter") + 
  theme_minimal() + 
  theme(legend.position = "none")

# Scatter plot for imdb_score vs number of news articles
p7 <- ggplot(imdb_df, aes(x = imdb_df$nb_news_articles, y = imdb_score)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "IMDb Score vs Number of News Articles", x = "Number of News Articles") + 
  theme_minimal() + 
  theme(legend.position = "none")

# Arrange the plots in a grid
grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol=4)


###################################################################
##################        Density plot               ##############
###################################################################

ggplot(imdb_data, aes(x=imdb_score)) + geom_density(fill="blue")

###################################################################
############   Bar plot for categorical variables         #########
###################################################################

ggplot(imdb_data, aes(x=release_month)) + geom_bar(fill="blue")
ggplot(imdb_data, aes(x=release_year)) + geom_bar(fill="green")
ggplot(imdb_data, aes(x=release_day)) + geom_bar(fill="salmon")
ggplot(imdb_data, aes(x=language)) + geom_bar(fill="yellow")

###################################################################
#### Top 10 Production Companies by Number of Movies   ############
###################################################################
# Bar chart for 'production_company'
top_companies=head(sort(table(imdb_data$production_company), decreasing=TRUE), 10)
barplot(top_companies, las=2, col=rainbow(length(top_companies)), main="Top 10 Production Companies by Number of Movies", ylab="Number of Movies")


correlations=cor(imdb_data[,c('imdb_score', 'movie_budget', 'duration', 'release_year', 'movie_meter_IMDBpro')])
corrplot(correlations, method="circle")


###################################################################
# Step 2: Dummy variables for categorical data
###################################################################

#Adding a dummy variable in the regression (Color):

imdb_data$language=as.factor(imdb_data$language)
imdb_data$colour_film=as.factor(imdb_data$colour_film)
attach(imdb_data)


# Two-category dummy variable (colour_film) 
# Explore the levels and counts for 'colour_film'
cat("\nLevels for colour_film:\n")
print(levels(colour_film))
cat("\nTable for colour_film:\n")
print(table(colour_film))
plot(imdb_score, colour_film, col=ifelse(colour_film=="Color", "forestgreen", "salmon"))

lm_color=lm(imdb_score ~colour_film)
summary(lm_color)
plot(imdb_score, colour_film, col=ifelse(colour_film=="Color", "forestgreen", "salmon"))

plot(imdb_score, colour_film,
     col = ifelse(colour_film == "Color", "orange", "black"),
     pch = 21, 
     bg = ifelse(colour_film == "Color", "orange", "black"),
     xlab = "IMDb Score",  
     ylab = "Colour Film" 
)
legend("topright",
       legend = levels(colour_film),
       fill = c("black", "orange"), 
       pch = 21,
       title = "Film Color" 
)


###################################################################
# Step 3: Exploring the four key issues of a linear regression model
###################################################################

###################################################################
## Significance in Multiple Linear Regression   ###################
###################################################################
reg1=lm(imdb_score~movie_budget+release_year+duration+aspect_ratio+nb_news_articles+nb_faces+movie_meter_IMDBpro)
summary(reg1)

# RESULT:        Multiple R-squared:  0.2482,	Adjusted R-squared:  0.2455 
# RESULT:        F-statistic: 90.66 on 7 and 1922 DF,  p-value: < 2.2e-16

residualPlots(reg1)


# Create the HTML table using stargazer with custom title and variable labels
stargazer(reg1, 
          type="html", 
          out="regression_output.html",
          title="Pearson Residuals of Numerical Variables",
          dep.var.labels=c("IMDb Score"),
          covariate.labels=c("Movie budget", "Release year", "Duration", "Aspect ratio", "Number of articles", "Number of faces", "Movie meter IMDb pro", "Intercept"))


################################################################

# Remove variables with p-value < 0.1 :  movie_budget, duration, nb_news_articles, movie_meter_IMDBpro

reg2=lm(imdb_score~release_year+aspect_ratio+nb_faces)
residualPlots(reg2)
stargazer(reg2, 
          type="html", 
          out="regression_output.html",
          title="Pearson Residuals of Numerical Variables",
          dep.var.labels=c("IMDb Score"),
         covariate.labels=c("Release year","Aspect ratio", "Number of faces", "Intercept"))

# Remove release_year (p-value < 0.1)

reg3=lm(imdb_score~aspect_ratio+nb_faces)
residualPlots(reg3)

# Remove aspect_ratio (p-value < 0.1)

reg4=lm(imdb_score~nb_faces)
residualPlots(reg4)

# still p-value <0.1 : no linear relationship!

################################################################
## the variables we used for our final model   #################
################################################################

# Transform your dataset
imdb_data=imdb_data %>%
  mutate(
    inv_movie_meter = 1 / movie_meter_IMDBpro
  )

# Fit the linear model with the specified variables
reg1=lm(imdb_score ~ release_year + duration + nb_news_articles + nb_faces + action + adventure + thriller + musical + romance + sport + horror + drama + war + animation + crime + inv_movie_meter, data = imdb_data)

# Summary of the model
summary(reg1)

# Plot residuals for the model, but only for selected numeric variables
residualPlots(reg1, terms = ~ release_year + duration + nb_news_articles + nb_faces + inv_movie_meter)

################################################################
# Step 4: Exploring polynomial regression model
################################################################
################################################################################
# Step 4-1 : Polynomial regression of degree 2 to 6 - between each two variables
################################################################################
# Remove non-numeric columns
numeric_cols = sapply(imdb_data, is.numeric)
numeric_data = imdb_data[, numeric_cols]

# Remove 'imdb_score' from the columns to be regressed upon, as it's the response variable.
numeric_data$imdb_score = NULL

# Loop through each column and fit polynomial regression models
for (col_name in names(numeric_data)) {
  cat(paste("\n\nRegression for:", col_name, "\n"))
  
  for (degree in 2:6) {
    formula_str = paste("imdb_score ~ poly(", col_name, ",", degree, ", raw=TRUE)")
    model = lm(as.formula(formula_str), data=imdb_data)
    cat(paste("\nDegree:", degree, "\n"))
    print(summary(model))
  }
}
################################################################
##################  R-square matrix  ###########################
################################################################

# Remove non-numeric columns for simplicity. If you want to include factors, you'd need to handle them separately.
numeric_cols = sapply(imdb_data, is.numeric)
numeric_data = imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols = c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime")

# Drop the specified columns
numeric_data = numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Remove 'imdb_score' from the columns to be regressed upon, as it's the response variable.
numeric_data$imdb_score = NULL

# Initialize a matrix to store R-squared values
r_squared_matrix = matrix(0, nrow=length(names(numeric_data)), ncol=5,
                           dimnames=list(names(numeric_data), 2:6))

# Loop through each column and fit polynomial regression models
for (col_name in names(numeric_data)) {
  
  for (degree in 2:6) {
    formula_str = paste("imdb_score ~ poly(", col_name, ",", degree, ", raw=TRUE)")
    model = lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the matrix
    r_squared_matrix[col_name, as.character(degree)] = summary(model)$r.squared
  }
}

# Convert matrix to a long-format data frame for easy sorting and grouping
r_squared_df = as.data.frame(as.table(r_squared_matrix))
colnames(r_squared_df) = c("Variable", "Degree", "R_Squared")

# Group by variable and select the highest R-squared for each variable
best_r_squared_per_variable = r_squared_df %>%
  group_by(Variable) %>%
  top_n(1, wt=R_Squared)

# Print the sorted dataframe
print(best_r_squared_per_variable)

#########################################################
# Add the inv_movie_meter variable to imdb_data
imdb_data$inv_movie_meter=ifelse(imdb_data$movie_meter_IMDBpro != 0, 1 / imdb_data$movie_meter_IMDBpro, NA)

# convert categorical variables to factors
categorical_vars=c("action", "adventure", "thriller", "musical", "romance", 
                      "sport", "horror", "drama", "war", "animation", "crime")
imdb_data[categorical_vars] = lapply(imdb_data[categorical_vars], factor)

# Define numeric and categorical columns to include
include_cols=c("release_year", "duration", "nb_news_articles", "nb_faces", "inv_movie_meter")
categorical_vars=c("action", "adventure", "thriller", "musical", "romance", 
                      "sport", "horror", "drama", "war", "animation", "crime")

# Keep only the selected numeric columns
numeric_data=imdb_data[include_cols]

# Loop through each column and fit polynomial regression models
r_squared_matrix=matrix(0, nrow=length(include_cols), ncol=5, 
                           dimnames=list(include_cols, 2:6))

for (col_name in include_cols) {
  for (degree in 2:6) {
    # Construct formula with polynomial terms for the numeric variable and linear terms for categorical
    formula_terms=paste("poly(", col_name, ",", degree, ", raw=TRUE)", collapse = " + ")
    formula_str=paste("imdb_score ~", formula_terms, " + ", paste(categorical_vars, collapse = " + "))
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the matrix
    r_squared_matrix[col_name, as.character(degree)] <- summary(model)$r.squared
  }
}

# Convert matrix to a long-format data frame for easy sorting and grouping
r_squared_df=as.data.frame(as.table(r_squared_matrix))
colnames(r_squared_df)= c("Variable", "Degree", "R_Squared")

# Group by variable and select the highest R-squared for each variable
best_r_squared_per_variable=r_squared_df %>%
  group_by(Variable) %>%
  top_n(1, wt=R_Squared)

# Print the sorted dataframe
print(best_r_squared_per_variable)
 
################################################################################
# Step 4-2 : Polynomial regression of degree 2 to 6 - between each three variables
################################################################################
# Remove non-numeric columns and the specified columns
numeric_cols = sapply(imdb_data, is.numeric)
numeric_data = imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols = c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data = numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 3 variables
combinations = combn(names(numeric_data), 3)

# Initialize a data frame to store R-squared values
r_squared_df = data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo = combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms = sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, ")")
    })
    poly_terms = paste(terms, collapse = " + ")
    formula_str = paste("imdb_score ~", poly_terms)
    
    model = lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df = rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Degree=degree, 
                                                   R_Squared=summary(model)$r.squared))
  }
}

# Sort by R-squared in descending order
sorted_r_squared_df = r_squared_df %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(sorted_r_squared_df)

# Remove non-numeric columns and the specified columns
numeric_cols = sapply(imdb_data, is.numeric)
numeric_data = imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols = c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 3 variables
combinations=combn(names(numeric_data), 3)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df <- rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Degree=degree, 
                                                   R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
best_r_squared_per_combo=r_squared_df %>%
  group_by(Variable1, Variable2, Variable3) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)

################################################################################
# Step 4-3 : Polynomial regression of degree 2 to 6 - between each four variables
################################################################################

# Remove non-numeric columns and the specified columns
numeric_cols=sapply(imdb_data, is.numeric)
numeric_data=imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols=c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 4 variables
combinations=combn(names(numeric_data), 4)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Variable4=character(0), 
                           Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, ")*I(", combo[4], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df=rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Variable4=combo[4], 
                                                   Degree=degree, R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
best_r_squared_per_combo=r_squared_df %>%
  group_by(Variable1, Variable2, Variable3, Variable4) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)

################################################################################
# Step 4-4 : Polynomial regression of degree 2 to 6 - between each five variables
################################################################################


# Remove non-numeric columns and the specified columns
numeric_cols=sapply(imdb_data, is.numeric)
numeric_data=imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols=c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 5 variables
combinations=combn(names(numeric_data), 5)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Variable4=character(0), 
                           Variable5=character(0), Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, 
             ")*I(", combo[4], "^", d, ")*I(", combo[5], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df=rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Variable4=combo[4], 
                                                   Variable5=combo[5], Degree=degree, 
                                                   R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
best_r_squared_per_combo <- r_squared_df %>%
  group_by(Variable1, Variable2, Variable3, Variable4, Variable5) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)

################################################################################
# Step 4-5 : Polynomial regression of degree 2 to 6 - between each six variables
################################################################################


# Remove non-numeric columns and the specified columns
numeric_cols=sapply(imdb_data, is.numeric)
numeric_data=imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols=c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 6 variables
combinations=combn(names(numeric_data), 6)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Variable4=character(0), 
                           Variable5=character(0), Variable6=character(0), 
                           Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, 
             ")*I(", combo[4], "^", d, ")*I(", combo[5], "^", d, ")*I(", combo[6], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df=rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Variable4=combo[4], 
                                                   Variable5=combo[5], Variable6=combo[6], 
                                                   Degree=degree, R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
best_r_squared_per_combo=r_squared_df %>%
  group_by(Variable1, Variable2, Variable3, Variable4, Variable5, Variable6) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)

################################################################################
# Step 4-6 : Polynomial regression of degree 2 to 6 - between each seven variables
################################################################################

# Remove non-numeric columns and the specified columns
numeric_cols=sapply(imdb_data, is.numeric)
numeric_data=imdb_data[, numeric_cols]

# List of columns to exclude
exclude_cols=c("action", "adventure", "scifi", "thriller", "musical", "romance", 
                  "western", "sport", "horror", "drama", "war", "animation", "crime", 
                  "movie_id", "imdb_score") # Excluding movie_id and imdb_score

# Drop the specified columns
numeric_data=numeric_data[, !(names(numeric_data) %in% exclude_cols)]

# Generate all possible combinations of 7 variables
combinations=combn(names(numeric_data), 7)

# Initialize a data frame to store R-squared values
r_squared_df=data.frame(Variable1=character(0), Variable2=character(0), 
                           Variable3=character(0), Variable4=character(0), 
                           Variable5=character(0), Variable6=character(0),
                           Variable7=character(0), Degree=integer(0), R_Squared=numeric(0))

# Loop through each combination and fit polynomial regression models
for (i in 1:ncol(combinations)) {
  combo=combinations[, i]
  
  for (degree in 2:6) {
    # Construct polynomial formula
    terms=sapply(1:degree, function(d) {
      paste0("I(", combo[1], "^", d, ")*I(", combo[2], "^", d, ")*I(", combo[3], "^", d, 
             ")*I(", combo[4], "^", d, ")*I(", combo[5], "^", d, ")*I(", combo[6], "^", d, 
             ")*I(", combo[7], "^", d, ")")
    })
    poly_terms=paste(terms, collapse = " + ")
    formula_str=paste("imdb_score ~", poly_terms)
    
    model=lm(as.formula(formula_str), data=imdb_data)
    
    # Store the R-squared value in the data frame
    r_squared_df=rbind(r_squared_df, data.frame(Variable1=combo[1], Variable2=combo[2], 
                                                   Variable3=combo[3], Variable4=combo[4], 
                                                   Variable5=combo[5], Variable6=combo[6], 
                                                   Variable7=combo[7], Degree=degree, 
                                                   R_Squared=summary(model)$r.squared))
  }
}

# Group by the variable combination and keep only the row with the highest R-squared for each combination
best_r_squared_per_combo=r_squared_df %>%
  group_by(Variable1, Variable2, Variable3, Variable4, Variable5, Variable6, Variable7) %>%
  top_n(1, wt=R_Squared) %>%
  ungroup() %>%
  arrange(desc(R_Squared))

# Print the sorted dataframe
print(best_r_squared_per_combo)


######################################################################################
####################################### Model Selection ##############################
######################################################################################

# Lets start simple by finding a regression between imdb_score and movie_buget and duration
reg1=lm(imdb_score~movie_budget)
summary(reg1)

reg2=lm(imdb_score~movie_budget+duration)
summary(reg2)

#Let's make the plot more complicated and add categorical variables

reg3=lm(imdb_score~movie_budget+duration+
          release_day+release_month+release_year+
          action+adventure+scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime)
summary(reg3)

# removing release time
reg4=lm(imdb_score~movie_budget+duration+
          action+adventure+scifi+thriller+musical+romance+western+sport+horror+drama+war+animation+crime)
summary(reg4)

residualPlots(reg4)

ncvTest(reg4)

################################################################################
####################################### Director dummies #######################
################################################################################

# Create the table
imdb5 = IMDB 
view(imdb5)
director_movie_count <- table(imdb5$director)

# Convert the table to a data frame
director_movie_count_df <- as.data.frame(director_movie_count)

colnames(director_movie_count_df) <- c("director", "frequency")
# Calculate the quartiles
sorted_director_movie_count <- sort(director_movie_count_df$frequency)
quartile1 <- sorted_director_movie_count[ceiling(length(sorted_director_movie_count) * 0.5)]
quartile2 <- sorted_director_movie_count[ceiling(length(sorted_director_movie_count) * 0.8)]
quartile3 <- sorted_director_movie_count[ceiling(length(sorted_director_movie_count) * 0.95)]

# Define the director quartiles
director_quartiles <- c(0, quartile1, quartile2, quartile3, max(director_movie_count))

# Create dummy variables for directors
imdb5$director_dummy <- ifelse(imdb5$director %in% director_movie_count_df$name[0:quartile1], 1, 0)
imdb5$director_dummy <- ifelse(imdb5$director %in% director_movie_count_df$name[(quartile1 + 1):(quartile2)], 2, imdb5$director_dummy)
imdb5$director_dummy <- ifelse(imdb5$director %in% director_movie_count_df$name[(quartile2 + 1):(quartile3)], 3, imdb5$director_dummy)
imdb5$director_dummy <- ifelse(imdb5$director %in% director_movie_count_df$name[(quartile3 + 1):(max(director_movie_count))], 4, imdb5$director_dummy)
imdb5$director_dummy <- ifelse(imdb5$director_dummy == 0, "other", imdb5$director_dummy)

################################################################################
################################ Ranking issue #################################
################################################################################
# We will inverse the actor1_star_meter, actor2_star_meter, actor3_star_meter, movie_meter_IMDBpro
# as they are rankings and the lower the number the higher the rank. Thus dividing 1 by the rank will inverse that as the higher
# the rank the higher the number.

IMDB = IMDB %>%
  mutate(
    inv_actor1 = 1 / actor1_star_meter,
    inv_actor2 = 1 / actor2_star_meter,
    inv_actor3 = 1 / actor3_star_meter,
    inv_movie_meter = 1 / movie_meter_IMDBpro
  )

################################################################################
################################## Model Selection #############################
################################################################################

# Clearly we have unlinear relation here

reg5 = lm(imdb_score ~movie_budget + duration+
            inv_actor1 + inv_actor2 + inv_actor3 + inv_movie_meter +
            action + adventure + scifi + thriller + musical + romance + western + sport + horror + drama + war + animation + crime, 
          data = IMDB)

summary(reg5)


# We can remove inv_actor2 and inv_actor3 as hey are insignificant
reg6 = lm(imdb_score ~movie_budget + duration+
            inv_actor1 + inv_movie_meter +
            action + adventure + scifi + thriller + musical + romance + western + sport + horror + drama + war + animation + crime, 
          data = IMDB)

summary(reg6)

qqPlot(reg6, envelope=list(style="none"))

outlierTest(reg6)

# We can remove the outliers to improve the model
IMDB2=IMDB[-c(32, 316,1806,989,1581,192,261),]

reg7 = lm(imdb_score ~movie_budget + duration+
            inv_actor1 + inv_movie_meter +
            action + adventure + scifi + thriller + musical + romance + western + sport + horror + drama + war + animation + crime, 
          data = IMDB2)

summary(reg7)

# This slightly improved the model but not enough
reg8 = lm(imdb_score ~movie_budget + duration+
            inv_actor1 + inv_movie_meter + nb_news_articles + nb_faces +
            action + adventure + scifi + thriller + musical + romance + western + sport + horror + drama + war + animation + crime, 
          data = IMDB2)

summary(reg8)

# lets check for reg8 outliers and if its reasonable to remove them

qqPlot(reg8, envelope=list(style="none"))

# Now lets check for multicollinearity between the numerical variables

quantvars=IMDB2[, c(4, 5, 9, 15, 9, 18,20,22, 40)]
corr_matrix=cor(quantvars)
round(corr_matrix, 2)

# Clearly no multicollinearity, let's try vif test

vif(reg8)

# Same thing no multicollinearity

# Now lets see how many unique directors we have

unique_directors_count = IMDB2 %>% 
  select(director) %>% 
  distinct() %>% 
  n_distinct()

unique_directors_count

# Out of 1923 rows we have 1114 unique directors which can over fit the data greatly.

reg9 = lm(imdb_score ~movie_budget + duration+
            inv_actor1 + inv_movie_meter + nb_news_articles + nb_faces +
            action + adventure + scifi + thriller + musical + romance + western + sport + horror + drama + war + animation + crime, 
          data = IMDB2)

summary(reg9)

################################################################################
############ Inverse the IMDB Test Data ########################################
################################################################################

IMDB3 = IMDB3 %>%
  mutate(
    inv_actor1 = 1 / actor1_star_meter,
    inv_actor2 = 1 / actor2_star_meter,
    inv_actor3 = 1 / actor3_star_meter,
    inv_movie_meter = 1 / movie_meter_IMDBpro
  )
# Check if data types match

str(IMDB2)
str(IMDB3)

# Make them match
IMDB3$imdb_score = as.numeric(IMDB3$imdb_score)

# Convert movie_budget in IMDB2 to numeric
IMDB3$movie_budget = as.numeric(gsub(",", "", IMDB3$movie_budget))

# Confirm that other columns have the correct data types
colnames(IMDB3) = colnames(IMDB)

# Reorder the columns to match the order in IMDB
IMDB3 = IMDB3[, colnames(IMDB)]

# Check the structure of IMDB2 to ensure it matches IMDB
str(IMDB3)

quantiles = quantile(IMDB2$release_year, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

reg10 = glm(imdb_score ~ bs(release_year, knots=c(quantiles), degree=1) + duration +
              inv_movie_meter + nb_news_articles + nb_faces + inv_actor1, 
            data = IMDB2)

summary(reg10)

mse=cv.glm(IMDB2, reg10, K=10)$delta[1]
mse

predicted_scores = predict(reg10, newdata = IMDB3)
predicted_scores

# lets test to see after 1995
imdb4 <- IMDB2[IMDB2$release_year >= 1995, ]

quantiles = quantile(imdb4$release_year, probs = c(0.2,0.4,0.6,0.8))

reg11 = glm(imdb_score ~ bs(release_year, knots=c(quantiles), degree=1) + duration + movie_budget +
              inv_movie_meter + nb_news_articles + nb_faces + inv_actor1, 
            data = imdb4)

summary(reg11)

mse=cv.glm(imdb4, reg11, K=10)$delta[1]
mse

predicted_scores = predict(reg11, newdata = IMDB3)
predicted_scores

# we will remove inv_actor1 due to its insignificance in the model
reg12 = glm(imdb_score ~ bs(release_year, knots=c(quantiles), degree=1) + duration + movie_budget +
              inv_movie_meter + nb_news_articles + nb_faces, 
            data = imdb4)

summary(reg12)

mse=cv.glm(imdb4, reg12, K=10)$delta[1]
mse

predicted_scores = predict(reg12, newdata = IMDB3)
predicted_scores

################################################################################
############################ TEST polynomial degree ############################
################################################################################

#Now lets test for different polynomial degree. we will remove movie budget as the results make more sense without it


msematrix = data.frame(a = integer(), b = integer(), d = integer(),e = integer(),f = integer(), mse = numeric())

for (a in 1:3){
  for (b in 1:3){
    for (d in 1:3){
      for (e in 1:3){
        for (f in 1:3){
          reg13 = glm(imdb_score ~ poly(release_year, degree=a) + poly(duration, b) +
                        poly(inv_movie_meter,d) + poly(nb_news_articles, e) + poly(nb_faces,f), 
                      data = IMDB2)
          mse = cv.glm(IMDB2, reg13, K=30)$delta[1]
          
          # Store results in the data frame
          msematrix = rbind(msematrix, data.frame(a = a, b = b, d = d, e=e, f=f, mse = mse))
        }
      }
    }
  }
}

min_mse_row = msematrix[which.min(msematrix$mse),]
min_mse_row$mse
min_mse_row$a
min_mse_row$b
min_mse_row$d
min_mse_row$e
min_mse_row$f

#plugging in the results for the above we get
reg14 = glm(imdb_score ~ poly(release_year,2) + poly(duration, 2) +
              poly(inv_movie_meter,3) + nb_news_articles + nb_faces, 
            data = IMDB2)

mse=cv.glm(imdb4, reg14, K=10)$delta[1]
mse

summary(reg14)

predicted_scores = predict(reg14, newdata = IMDB3)
predicted_scores

#since it doesn make sense to have a score above 10 we will thus keep with this models our final one.

#We spliced the imdb inverse movie meter and addied the relevant genres again to reduce the mse

quantiles = quantile(IMDB2$release_year, probs = c(0.2,0.4,0.6,0.8))

quantiles1 = quantile(IMDB2$inv_movie_meter, probs = c(0.2,0.4,0.6,0.8))

reg15= glm(formula = imdb_score ~ bs(release_year, knots = c(quantiles),degree = 1) + 
             duration + bs(inv_movie_meter, knots = c(quantiles1), degree = 1) 
           + nb_news_articles + nb_faces+
             action + adventure + thriller + musical + romance + sport + horror + drama + war + animation + crime,
           data = IMDB2)

summary(reg15)

mse=cv.glm(IMDB2, reg15, K=10)$delta[1]
mse

predicted_scores = predict(reg15, newdata = IMDB3)

# Create a data frame with movie names and predicted scores
predicted_scores_df = data.frame(Movie_Name = IMDB3$movie_title, Predicted_Score = predicted_scores)

# Print the data frame with movie names and predicted scores
print(predicted_scores_df)

# Testing the predictive power on our sample

sample=sample.split(IMDB2$imdb_score, SplitRatio=0.5)
train_set=subset(IMDB2, sample==TRUE)
test_set=subset(IMDB2, sample==FALSE)

reg16= lm(formula = imdb_score ~ bs(release_year, knots = c(quantiles),degree = 1) + 
            duration + bs(inv_movie_meter, knots = c(quantiles1), degree = 1) 
          + nb_news_articles + nb_faces+
            action + adventure + thriller + musical + romance + sport + horror + drama + war + animation + crime, 
          data = train_set)

actual=test_set$imdb_score
prediction=predict(reg16, test_set)
squared_error=(actual-prediction)^2
MSE=mean(squared_error)
MSE
#############################################################################
##########################  FINAL Model #####################################
#############################################################################
# Test for heteroskedasticity and changing it to linear

reg17= lm(formula = imdb_score ~ bs(release_year, knots = c(quantiles),degree = 1) + 
            duration + bs(inv_movie_meter, knots = c(quantiles1), degree = 1) 
          + nb_news_articles + nb_faces+
            action + adventure + thriller + musical + romance + sport + horror + drama + war + animation + crime,
          data = IMDB2)

summary(reg17)

ncvTest(reg17)

coeftest(reg17, vcov=vcovHC(reg17, type="HC1"))

predicted_scores = predict(reg17, newdata = IMDB3)

# Create a data frame with movie names and predicted scores
predicted_scores_df = data.frame(Movie_Name = IMDB3$movie_title, Predicted_Score = predicted_scores)

# Print the data frame with movie names and predicted scores
print(predicted_scores_df)

# prediction of the score are very similar

#############################################################################
########################## STARGAZER TABLE FOR FINAL REGRESSION #############
#############################################################################

# Regression
reg17= lm(formula = imdb_score ~ bs(release_year, knots = c(quantiles),degree = 1) + 
            duration + bs(inv_movie_meter, knots = c(quantiles1), degree = 1) 
          + nb_news_articles + nb_faces+
            action + adventure + thriller + musical + romance + sport + horror + drama + war + animation + crime,
          data = IMDB2)

# Load stargazer
library(stargazer)

stargazer(predicted_scores, title = "Predicted Movie Scores", type = "text",
          digits = 2)

# Stargazer
stargazer(reg17, type = "html")
summary(reg17)


