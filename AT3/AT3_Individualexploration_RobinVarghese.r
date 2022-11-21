#Loading libraries

library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(broom)
library(rstanarm)
library(kableExtra)
library(bayesplot)
library(bayestestR)
library(brms)
library(tidyverse)

#Loading the data
life_15 <- read.csv ("C:/Users/robin/Downloads/archive (14)/2015.csv")
life_16 <- read.csv ("C:/Users/robin/Downloads/archive (14)/2016.csv")
life_17 <- read.csv ("C:/Users/robin/Downloads/archive (14)/2017.csv")
life_18 <- read.csv ("C:/Users/robin/Downloads/archive (14)/2018.csv")
life_19 <- read.csv ("C:/Users/robin/Downloads/archive (14)/2019.csv")
head(life_19)

#Rename dataset column' names - changing year 2018 datasets comparing base year 2017 dataset
life_18=plyr::rename(life_18, replace = c( "Country.or.region"="Country", 
                                           "Overall.rank"="Happiness.Rank" ,
                                           "GDP.per.capita"="Economy..GDP.per.Capita.",
                                           "Healthy.life.expectancy"="Health..Life.Expectancy.",
                                           "Freedom.to.make.life.choices"="Freedom",
                                           "Perceptions.of.corruption"="Trust..Government.Corruption.",
                                           "Social.support"="Family",
                                           "Score"="Happiness.Score"))
colnames(life_18)

#Renaming year 2019 datasets column names with base year 2017 dataset
life_19=plyr::rename(life_19, replace = c( "Country.or.region"="Country", 
                                           "Overall.rank"="Happiness.Rank" ,
                                           "GDP.per.capita"="Economy..GDP.per.Capita.",
                                           "Healthy.life.expectancy"="Health..Life.Expectancy.",
                                           "Freedom.to.make.life.choices"="Freedom",
                                           "Perceptions.of.corruption"="Trust..Government.Corruption.",
                                           "Social.support"="Family",
                                           "Score"="Happiness.Score"))
colnames(life_19)

#Renaming year 2015 datasets column names with base year 2017 datasets
life_15=plyr::rename(life_15, replace = c( "Happiness Rank" = "Happiness.Rank", 
                                           "Happiness Score" = "Happiness.Score",
                                           "Economy (GDP per Capita)" = "Economy..GDP.per.Capita.",
                                           "Health (Life Expectancy)" = "Health..Life.Expectancy.",
                                           "Trust (Government Corruption)" = "Trust..Government.Corruption.",
                                           "Dystopia Residual"="Dystopia.Residual"))
colnames(life_15)

#Renaming year 2016 datasets column names with base year 2017 datasets
life_16=plyr::rename(life_16, replace = c( "Happiness Rank" = "Happiness.Rank", 
                                           "Happiness Score" = "Happiness.Score",
                                           "Economy (GDP per Capita)" = "Economy..GDP.per.Capita.",
                                           "Health (Life Expectancy)" = "Health..Life.Expectancy.",
                                           "Trust (Government Corruption)"  = "Trust..Government.Corruption.",
                                           "Dystopia Residual"="Dystopia.Residual"))
colnames(life_16)

#Insert year column at first position (index 0)
life_15<-cbind(Year=2015,life_15)
life_16<-cbind(Year=2016,life_16)
life_17<-cbind(Year=2017,life_17)
life_18<-cbind(Year=2018,life_18)
life_19<-cbind(Year=2019,life_19)

#Change column type for emerging dataset
life_18$Trust..Government.Corruption. = as.numeric(life_18$Trust..Government.Corruption.)

str(life_18)

#Merge data from 2015-2019
life_15_16<-dplyr::bind_rows(life_15,life_16)
life_15_16_17<-dplyr::bind_rows(life_15_16,life_17)
life_18_19<-dplyr::bind_rows(life_18,life_19)
life_df<-dplyr::bind_rows(life_18_19,life_15_16_17)
head(life_df)

#Change Happiness.Rank data type
life_df$Happiness.Rank  = as.numeric(life_df$Happiness.Rank )

str(life_df)

#Remove unnessesary columns
life_df = subset(life_df, select = -c(Lower.Confidence.Interval,Upper.Confidence.Interval,Dystopia.Residual,Standard.Error,Whisker.high,Whisker.low))

colSums(is.na(life_df))

#Removing countries which doesnt have five year data/which are not present in other dataset
df3<-life_df[!(life_df$Country=="Djibouti" | life_df$Country=="Egypt"|life_df$Country=="Gambia"
               |life_df$Country=="Hong Kong"|life_df$Country=="Hong Kong S.A.R., China"|life_df$Country=="Iran"
               |life_df$Country=="Ivory Coast"|life_df$Country=="Kyrgyzstan"|life_df$Country=="Laos"|life_df$Country=="Lesotho"
               |life_df$Country=="Macedonia"|life_df$Country=="North Cyprus"|life_df$Country=="North Macedonia"
               |life_df$Country=="Northern Cyprus"|life_df$Country=="Oman"|life_df$Country=="Palestinian Territories"|life_df$Country=="Puerto Rico"
               |life_df$Country=="Russia"|life_df$Country=="Slovakia"|life_df$Country=="Somaliland region"
               |life_df$Country=="Somaliland Region"|life_df$Country=="South Korea"|life_df$Country=="Suriname"
               |life_df$Country=="Swaziland"|life_df$Country=="Syria"|life_df$Country=="Somalia"
               |life_df$Country=="Taiwan"|life_df$Country=="Taiwan Province of China"|life_df$Country=="Turkey"
               |life_df$Country=="Venezuela"|life_df$Country=="Yemen"|life_df$Country=="Angola"|life_df$Country=="Belize"),]


#Loading the old dataset
life_df1 <- read.csv("C:/Users/robin/Documents/STDS/Assignment AT2/Practice Analysis/LifeExpectancy_merged_R.csv")
head(life_df1, 5)

#Removing columns not to be included
life_df1<- select(life_df1,-c(4,6:39))

#Filtering data (Year 2015 and above)
life_df2<-(life_df1 %>% filter(Year > 2014))

#Removing countries which doesnt have five year data/which are not present in other dataset
df2<-life_df2[!(life_df2$Country.Name=="Antigua and Barbuda" | life_df2$Country.Name=="Barbados"
                |life_df2$Country.Name=="Cote d'Ivoire"|life_df2$Country.Name=="Djibouti"|life_df2$Country.Name=="Equatorial Guinea"
                |life_df2$Country.Name=="Eritrea"|life_df2$Country.Name=="Eswatini"|life_df2$Country.Name=="European Union"|life_df2$Country.Name=="Fiji"|life_df2$Country.Name=="Greenland"
                |life_df2$Country.Name=="Grenada"|life_df2$Country.Name=="Guam"|life_df2$Country.Name=="Guinea-Bissau"|life_df2$Country.Name=="Guyana"
                |life_df2$Country.Name=="Kiribati"|life_df2$Country.Name=="Lesotho"|life_df2$Country.Name=="Maldives"|life_df2$Country.Name=="Middle East & North Africa"
                |life_df2$Country.Name=="North America"|life_df2$Country.Name=="North Macedonia"|life_df2$Country.Name=="Oman"
                |life_df2$Country.Name=="Papua New Guinea"|life_df2$Country.Name=="Puerto Rico"|life_df2$Country.Name=="Papua New Guinea"|life_df2$Country.Name=="Samoa"
                |life_df2$Country.Name=="Sao Tome and Principe"|life_df2$Country.Name=="Seychelles"|life_df2$Country.Name=="Solomon Islands"
                |life_df2$Country.Name=="South Asia"|life_df2$Country.Name=="Sub-Saharan Africa"|life_df2$Country.Name=="Tonga"
                |life_df2$Country.Name=="Vanuatu"|life_df2$Country.Name=="World"|life_df2$Country.Name=="Somalia"
                |life_df2$Country.Name=="Suriname"|life_df2$Country.Name=="Angola"|life_df2$Country.Name=="Belize"),]

#Renaming country column name
df2=plyr::rename(df2, replace = c( "Country.Name"="Country"))

#Merging both datasets
df_final<- merge(df2, df3,by = c('Country','Year') , all=TRUE)

#Remove unnessesary columns
df_final = subset(df_final, select = -c(Happiness.Rank,Region,Health..Life.Expectancy.))

colSums(is.na(df_final))

#Impute with mean or median values for numerical columns
df_final$Trust..Government.Corruption.[is.na(df_final$Trust..Government.Corruption.)] <- median(df_final$Trust..Government.Corruption., na.rm = T)
df_final$Life.Expectancy[is.na(df_final$Life.Expectancy)] <- mean(df_final$Life.Expectancy, na.rm = T)
df_final$Happiness.Score[is.na(df_final$Happiness.Score)] <- median(df_final$Happiness.Score, na.rm = T)
df_final$Population[is.na(df_final$Population)] <- median(df_final$Population, na.rm = T)
df_final$Family[is.na(df_final$Family)] <- median(df_final$Family, na.rm = T)
df_final$Generosity[is.na(df_final$Generosity)] <- median(df_final$Generosity, na.rm = T)
df_final$Economy..GDP.per.Capita.[is.na(df_final$Economy..GDP.per.Capita.)] <- median(df_final$Economy..GDP.per.Capita., na.rm = T)
df_final$Freedom[is.na(df_final$Freedom)] <- median(df_final$Freedom, na.rm = T)

colSums(is.na(df_final))

#Plotting correlation plots for life expectancy dataset
Corrdata <- df_final[,1:10] %>% 
  select_if(is.numeric)

#Plotting the correlation among all parameter
ggcorr(Corrdata, 
       method = c("pairwise","pearson"),
       label = T, 
       label_size = 4,
       label_round = 2,
       hjust = 1,
       size = 3, 
       color = "royalblue",
       layout.exp = 5,
       low = "green3", 
       mid = "gray95", 
       high = "darkorange",
       name = "Correlation")

#Removing unwanted columns for modelling
df_final1 = subset(df_final, select = -c(Country,Year))

#correlation between happiness score and life expectancy
ggplot(data = df_final1, aes(x = Happiness.Score, y = Life.Expectancy)) + 
  geom_point(color="blue", alpha=0.35, size=2) +
  geom_smooth(method=lm, color="red")

#Modelling
life_data<-df_final1
# Regression Modelling using stan_glm
model1<- stan_glm(Life.Expectancy ~ Happiness.Score, data = life_data)

# summary of the model
summary(model1)

# extracting simulations from the model 
sims <- as.matrix(model1)
# Appearance
head(sims)

# Plotting all of posterior distributions
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 95% intervals")
mcmc_areas(sims, prob = 0.9) + plot_title

# area plot for happiness parameter
mcmc_areas(sims,
           pars = c("Happiness.Score"),
           prob = 0.9) + plot_title

# 90% credible intervals
# using rstanarm function and model object
posterior_interval(model1, pars = "Happiness.Score", prob=.9)
# calculated directly from simulations from posterior distribution
quantile(sims[,2], probs = c(.025,.975))  

# Creating a data-frame of posterior samples 
model <- model1%>% 
  as_tibble() %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)
head(model)

# aesthetic controllers
n_draws <- 500
alpha_level <- .15
color_draw <- "grey60"
color_mean <-  "#3366FF"

# make the plot
ggplot(life_data) + 
aes(x = Happiness.Score, y = Life.Expectancy ) + 
coord_cartesian(ylim = c(50, 100)) +
geom_abline(
    aes(intercept = intercept, slope = Happiness.Score), 
    data = sample_n(model, n_draws), 
    color = color_draw, 
    alpha = alpha_level) + geom_abline(intercept = mean(model$intercept), 
    slope = mean(model$Happiness.Score), 
    size = 1, 
    color = color_mean) +
  geom_point() + 
  labs(x = 'Average Happiness Score', 
       y = 'Life.Expectany' , 
       title = 'Visualization of Regression Lines From the Posterior Distribution')

# posterior predictive check 
pp_check(model1, "stat")

#creating new data for predictions, average happiness score = 5
new <- data.frame(Happiness.Score = 5)

#simply using the single point summary of the posterior distributions
# for the model coefficients (those displayed in the model summary above)
y_point_est <- predict(model1, newdata = new)
# same prediction "by hand"
# we use the means from our simulation matrix because 
# extracting coefficients from the model object gives us the 
# coefficient medians (and the predict function above uses the means)
y_point_est_2 <- mean(sims[,1]) + mean(sims[,2])*new


y_linpred <- posterior_linpred(model1, newdata = new)
# compute it "by hand"
# we use the sims matrix we defined above 
# sims <- as.matrix(model1)
y_linpred_2 <- sims[,1] + (sims[,2]*5) 

# predictive distribution for a new observation using posterior_predict
set.seed(1)
y_post_pred <- posterior_predict(model1, newdata = new)
# calculate it "by hand"
n_sims <- nrow(sims)
sigma <- sims[,3]
set.seed(1)
y_post_pred_2 <- as.numeric(sims[,1] + sims[,2]*5) + rnorm(n_sims, 0, sigma)

# creating a dataframe containing the values from the posterior distributions & linear predictions 
# of happiness score 5.
post_dists <- as.data.frame(rbind(y_linpred, y_post_pred)) %>% 
  setNames(c('prediction'))
post_dists$pred_type <- c(rep('posterior_linpred', 4000),
                          rep('posterior_predict', 4000))
y_point_est_df = as.data.frame(y_point_est)
pal <- c('yellow', 'blue','lightgreen', 'purple', 'gold')
ggplot(data = post_dists, aes(x = prediction, fill = pred_type)) + 
  geom_histogram(alpha = .75, position="identity") + 
  geom_point(data = y_point_est_df,
             aes(x = y_point_est,
                 y = 100,
                 fill = 'Linear Point Estimate'),
             color =  pal[2],
             size = 4,
             # alpha = .75,
             show.legend = F) +
  scale_fill_manual(name = "Prediction Method",
                    values = c(pal[c(2,3,5)]),
                    labels = c(bquote(paste("Linear Point Estimate ", italic("(predict)"))),
                               bquote(paste("Linear Prediction With Uncertainty " , italic("(posterior_linpred)"))),
                               bquote(paste("Posterior Predictive Distribution ",  italic("(posterior_predict)"))))) +
# set the plot labels and title
  labs(color='life expectancy vs happiness score',
       x = "Predicted Life Expectancy", 
       y = "Count", 
       title = 'Uncertainty in Posterior Prediction Methods')   +
  theme_bw()

