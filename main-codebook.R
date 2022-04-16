
# libraries ---------------------------------------------------------------

library(tidyverse)
library(usethis)
library(janitor)
library(psych)
library(effsize)
library(Rcmdr)
library(sjPlot)
library(stargazer)
library(car)
library(beepr)

# datasets ----------------------------------------------------------------

nhanes 

# set up ------------------------------------------------------------------


## customizing R -----------------------------------------------------------

Tools > Global Options > Appearance
Tools > Keyboard Shortcuts Help
Tools > Modify Keyboard Shortcuts

options key helps you make a large cursor or modify multiple lines at once


## R Markdown --------------------------------------------------------------

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
echo = TRUE    - do you want to show R code in the report?
include = TRUE - do you want to show plots in the report?
message = TRUE - do you want to show warning messages in the report?

  
## github ------------------------------------------------------------------

library(usethis)
use_git()
use_github()

# data wrangling ----------------------------------------------------------

## importing --------------------------------------------------------------

faketucky <- read_csv("data/faketucky.csv", 
                      na = "999",
                      col_types = list(enrolled_in_college = col_character(), 
                                       free_and_reduced_lunch = col_character(),
                                       male = col_character(),
                                       received_high_school_diploma = col_character()))

nhanes <- read_csv("data/nhanes.csv") %>%
  clean_names()


## basics ------------------------------------------------------------------

attach(datasetname)
rm(objectname or datasetname) 

class(variablename)

x <- as.factor(x)

c is for concatenate
concatenate Links things together (e.g., creating a vector)
Particularly useful if entering data manually, or combining or changing data

## learning the data -------------------------------------------------------

all(complete.cases(DATASET))
#to see if all records have complete data
#to remove records with missing data
newDATASET <- na.omit(DATASET)

glimpse()
nrow() # number of rows
ncol() # number of columns
dim() # rows, columns
skim(dataset)
names(dataset)

## select ------------------------------------------------------------------

#selecting data columns
newdf <- select(dataset, 
                column_name
                column_name2)

newdf <- select(dataset, c(column_1:column10))

newdf <- data %>% 
  select(-x) %>% 
  select(contains('state')) %>% 
  filter(state_code == CO)

#deleting data columns
newdf <- select(dataset, -column_name)

ALTERNATIVELY, 

nhanes %>%
  select(-education)
nhanes %>%
  select(-(health_gen:smoke_now))

dataset %>% 
  select(starts_with(""))
dataset %>% 
  select(ends_with(""))

starts_with(): Starts with a prefix 
ends_with(): Ends with a suffix 
contains(): Contains a literal string 
num_range(): Matches a numerical range like x01, x02, x03 
one_of(): Matches variable names in a character vector 
everything( ): Matches all variables 
last_col(): Select last variable, possibly with an offset 
matches(): Matches a regular expression a sequence of symbols/characters expressing a string/pattern to be searched for within text

hotels %>% 
  select(adults, children, babies) %>%
  arrange(desc(babies))


## filter ------------------------------------------------------------------

hotels %>% 
  filter(adults == 0 children >= 1) %>% 
  select(adults, babies, children)

hotels %>% 
  filter(adults == 0 children >= 1 babies >= 1) %>%
  select(adults, babies, children)

hotels %>% 
  distinct(market_segment) %>% 
  arrange(market_segment)

hotels %>% 
  distinct(hotel, market_segment) %>% 
  arrange(hotel, market_segment)

nhanes %>% 
  filter(marital_status == "Divorced" | marital_status == "Separated") %>% 
  select(marital_status)

nhanes %>% 
  filter(marital_status %in% c("Divorced", "Separated", "Widowed")) %>%
  select(marital_status)

nhanes %>% 
  filter(education == "College Grad") %>% 
  filter(marital_status %in% c("Divorced", "Separated", "Widowed")) %>%
  select(education, marital_status)

# dropping NAs
nhanes %>% 
  filter(phys_active_days >= 5) %>% 
  filter(!is.na(days_phys_hlth_bad)) %>% 
  select(phys_active_days, days_phys_hlth_bad)
nhanes %>% 
  filter(education == "College Grad") %>% 
  filter(marital_status %in% c("Divorced", "Separated", "Widowed")) %>%
  select(education, marital_status)

### subsetting ---------------------------------------------------------------

mean(age[gender==“male” & COHORT==4])
mean(PCL_SCORE[gender=="Male"], na.rm = TRUE)

## across ------------------------------------------------------------------

starwars %>% 
  mutate(across(all_of(c("name", "hair_color")), ~ as_factor(.x)))

starwars %>% 
  mutate(across(hair_color:species, ~ as_factor(.x)))

starwars %>% 
  mutate(across(where(is.character), ~ as_factor(.x)))

## tables ------------------------------------------------------------------

table(variablename)  #frequency table for factors
table(gender)/N   # N represents total sample size


## rename ------------------------------------------------------------------

                  "new" = "old"
starwars %>% rename(sex2="sex",gender2="gender")

#starwars %>% rename_with(.fn=function(x){paste0(x,"_baseline")})


## mutate ------------------------------------------------------------------

mutate(starwars,BMI=mass/height)

mutate(starwars,BMI=mass/height,
       blonde_indicator=if_else(hair_color=="blond" & eye_color=="blue","Blondie","Bald"))
mutate(starwars,BMI=mass/height,
       blonde_indicator=case_when(
         hair_color == "blond" & eye_color == "blue" ~ "Blondie/Bluie",
         eye_color == "blue" ~ "Bluie"))

nhanes %>%
  mutate(height = round(height, digits = 0)) %>% 
  select(height)

### computing variables using base R ----------------------------------------

dataset$new_variable <- as.numeric(dataset$variable)
dataset$new_variable <- as.numeric(as.factor(dataset$variable))

describe(dataset$newvariable)

complete$total_cost <- complelte$degree_num*complete$in_state_tuition

#### sum rows ----------------------------------------------------------------

bfi <- bfi %>% 
  mutate(ext_sum = select(., EXT1:EXT10) %>% 
           rowSums(na.rm = T))

##assign to this dataset from this dataset we are creating this new variable from these columns and summing these rows 


#### if_else -----------------------------------------------------------------

COVID_data <- COVID_data %>% 
  mutate(New_Case_Range = if_else (NewCases| > 3500, "35004", "0-3500"))


## pivot_ ------------------------------------------------------------------


### from REDCap -------------------------------------------------------------

new_df <- pivot_wider(df,
                      id_cols = mrn,
                      names_from = redcap_event_name,
                      values_from = c(variable, variable))

## merging data ------------------------------------------------------------

dataset <- read.csv(dataset path) %>% 
  rename(
    new_variable_name = old_variable_name)

merged <- full_join(dataset1, dataset2, by = c("variable"))
#.x or .y means it came from either dataset1 or dataset2 this can be changed by using suffix=c("x","y")
merged <- full_join(dataset1, dataset2, by = c("variable", "variable2")) #merging on two variables

#other options to merge are left_join, right_join, inner_join, anti_join

left_join(): all rows from x 
right_join(): all rows from y 
full_join(): all rows from both x andy 
semi_join(): all rows from x where there are matching values in y, keeping just columns from x inner_join(): all rows from x where there are matching values in y, return all combination of multiple matches in the case of multiple matches 
anti_join(): return all rows from x where there are not matching values in y, never duplicate rows of x

#https://datascience4psych.github.io/DataScience4Psych/working-with-multiple-data-frames.html


## summarize ---------------------------------------------------------------

nhanes %>% 
  summarize(mean_hours_sleep = mean(sleep_hrs_night, na.rm = TRUE))

nhanes %>% 
  summarize(mean_hours_sleep = mean(sleep_hrs_night, na.rm = TRUE),
            number_of_responses = n())


## group_by ----------------------------------------------------------------

nhanes %>% 
  group_by(gender) %>% 
  summarize(mean_hours_sleep = mean(sleep_hrs_night, na.rm = TRUE))

nhanes %>% 
  group_by(gender, work) %>% 
  summarize(mean_hours_sleep = mean(sleep_hrs_night, na.rm = TRUE),
            number_of_observations = n())


## count -------------------------------------------------------------------

nhanes %>% 
  count(education, phys_active)


## arrange -----------------------------------------------------------------

nhanes %>% 
  count(education) %>% 
  arrange(desc(n)) 


# data visualization ------------------------------------------------------


## ggplot ------------------------------------------------------------------


### plots -------------------------------------------------------------------


#### dot & density -------------------------------------------------------

ggplot(datset, 
       aes(variable)) +
  geom_dotplot()

ggplot(datset, 
       aes(variable)) +
  geom_density()

#### scatter -------------------------------------------------------------

plot(x, y) #Both variables should be continuous

ggplot(data = nhanes,
       mapping = aes(x = weight,
                     y = height)) +
  geom_point()


#### histogram ---------------------------------------------------------------

ggplot(data = nhanes, 
       mapping = aes(x = weight)) +
  geom_histogram()

#adjusting bin sizes
ggplot(data = nhanes, 
       mapping = aes(x = weight)) +
  geom_histogram(bins = 50) 

ggplot(datset, 
       aes(variable)) +
  geom_histogram(binwidth = 1000) +
  theme_minimal() +
  labs(x = "New X Axis Label",
       title = "Adds a Title Above the Plot",
       y = "") +
  theme(plot.title = element_text(hjust = 0.5)) # this line centers the title 


#### bar  ---------------------------------------------------------------

# Use the v1 approach to make a bar chart that shows a count of the number of people who say they smoke. Include NA responses.

ggplot(data = nhanes, 
       mapping = aes(x = smoke_now)) +
  geom_bar() 

# Create a new data frame called `sleep_by_gender` that shows the average amount of sleep per night that males and females report getting. Drop any NA (or NaN) responses from this data frame.

sleep_by_gender <- nhanes %>% 
  group_by(gender) %>% 
  summarize(avg_sleep = mean(sleep_hrs_night, na.rm = TRUE)) 

ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep)) +
  geom_bar(stat = "identity") 

# Make the same graph as above, but use `geom_col` instead of `geom_bar`.

ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep)) +
  geom_col()


#### box  ---------------------------------------------------------------

boxplot(continuousvariable ~ categoricalvariable)


### color & fill -------------------------------------------------------------

ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep,
                     fill = gender)) +
  geom_col()

ggplot(data = nhanes,
       mapping = aes(x = weight,
                     y = height,
                     color = phys_active)) +
  geom_point()


### scales ------------------------------------------------------------------

#### color ------------------------------------------------------------------

ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep,
                     fill = gender)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") 

#### axes ------------------------------------------------------------------

change the y-axis so it goes from 0 to 8 
ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep,
                     fill = gender)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 8))

#add breaks in the y-axis

ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep,
                     fill = gender)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, 8),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8))

### text & labels ------------------------------------------------------------------

ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep,
                     fill = gender))
geom_col() +
  geom_text(aes(label = round(avg_sleep, 1)),
            vjust = 1.5,
            color = "white") +
  scale_fill_brewer(palette = "Dark2",
                    na.value = "blue") +
  scale_y_continuous(limits = c(0, 8),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8))

# geom_label instead of geom_text

ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep,
                     fill = gender)) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = round(avg_sleep, 1)),
             vjust = -1.1,
             show.legend = FALSE,
             color = "white") +
  scale_fill_brewer(palette = "Dark2",
                    na.value = "blue") +
  scale_y_continuous(limits = c(0, 8),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8))

# plot labels

ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep,
                     fill = gender)) +
  geom_col(show.legend = FALSE)+
  geom_text(aes(label = round(avg_sleep, 1)),
            vjust = 1.5,
            color = "white") +
  scale_fill_brewer(palette = "Dark2",
                    na.value = "blue") +
  scale_y_continuous(limits = c(0, 8),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  labs(title = "Women sleep slightly more than men on average",
       y = "Hours of sleep per night",
       x = "")

### themes ------------------------------------------------------------------

#### hrbrthemes ------------------------------------------------------------------

install.packages("devtools")
devtools::install_github("hrbrmstr/hrbrthemes")
library(hrbrthemes)

ggplot(data = sleep_by_gender, 
       mapping = aes(x = gender, 
                     y = avg_sleep,
                     fill = gender)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(avg_sleep, 1)),
            vjust = 1.5,
            color = "white") +
  scale_fill_brewer(palette = "Dark2",
                    na.value = "blue") +
  scale_y_continuous(limits = c(0, 8),
                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8)) +
  labs(title = "Women sleep slightly more than men on average",
       y = "Hours of sleep per night",
       x = "") +
  theme_ipsum()

### facet ------------------------------------------------------------------

facet_grid(): 2d grid rows cols use for no split 
facet_grid: Subsets the data according to two separate variables.

facet_wrap(): 1d ribbon wrapped according to number of rows and columns specified or available plotting area
facet_wrap: Lays out the plots for each subset sequentially. 


ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) +
  geom_point() + 
  facet_wrap(~ species, ncol = 2)

ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + 
  geom_point() +
  facet_grid(sex ~ species)

##

ggplot(data = sleep_by_gender_by_age, 
       aes(x = age_decade,
           y = avg_sleep,
           fill = gender)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(avg_sleep, 1)),
            vjust = 1.5,
            color = "white") +
  scale_y_continuous(limits = c(0, 8),
                     breaks = seq(0, 8, by = 1)) +
  scale_fill_brewer(palette = "Dark2") +
  facet_wrap(~gender) +
  theme_ipsum() +
  labs(title = "Sleep by gender and age",
       y = "Hours of sleep per night",
       x = "")

### save ------------------------------------------------------------------

ggsave("Plots/fig1.png")

ggsave(filename = "plots/my-sleep-plot.png",
       height = 5,
       width = 8,
       unit = "in")

### filtered data ------------------------------------------------------------------

filtered <- dataset %>% 
  filter(variable1 != variable2) %>% 
  ggplot (aes(x =
                y =)) +
  geom_point(aes(shape = variable,
                 color = variable)) +
  geom_smooth() +
  labs(title = "" ,
       x = "" ,
       y = "" ,
       subtitle = ) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none")

ggplot(data=reduced_iop, mapping = aes(x=age, y=PCL…)) + 
  geom_point(mapping = aes(shape = gender)) + 
  geom_smooth(data=filter(reduced_IOP, mst2=="no"))
                          
# inferential stats ------------------------------------------------------------

## correlations ------------------------------------------------------------

cor.test(PCL_SCORE_POST, PHQ_SCORE_POST)
##measures how strong of a relationship (the fewer exceptions we have to this overall trend)       between the two things you are measuring -1 -- 1 

Pearson Correlations
Assumptions
Parametric Test
Interval/Ration scoring for both variables
Normal distribution of both variables
Lack of influential outliers/skew
Linear relationship between variables

#checking assumptions for Pearson Correlations  
hist(PCL_SCORE_POST)
#good
hist(PHQ_SCORE_POST)
#good
hist(Pain2)
#failed - not an interval/ratio scale 
#Likert scale is textbook ordinal sale
hist(IND_CPT_COUNT)
#failed - skew

plot(IND_CPT_COUNT ~ PHQ_SCORE_POST)


Non-Parametric Correlations

cor.test(xvariable, yvariable, method="spearman")
#Spearman's rho 
#Ordinal scale variables

cor.test(xvariable, yvariable, method="kendall")
#Kendall's tau 
#Outliers, small samples, lots of 'ties'

#with "ties" - explained
#Ranking people from the lowest to the highest in these variables
#If you have an individual with the same PHQ scores, they would have the exact same rank
#The fact that there's a tie means that there's two people who had the exact same value for that variable and Spearman's correlation has can be seen as suspect in those occasions
#Can't rank you PHQ score as higher or lower than mine

So the p-value is...
The p-value is NOT the probability that our result happened by chance
We have no way of knowing how liekly our result happened by chance
The p-value is the probability of results like the one we obtained actually happening by mathematical chance

What p really means
Our best estimate as to how often a result like this would happen by "chance"

Significant = there is a difference that we do not think occurred just by chance
This does not mean that there necessarily is a REALLY BIG DIFFERENCE or RELATIONSHIP

What is next?
  Don't treat p-values as binary entities
    But remember sample size plays a role in p-value size too
      Don't resort to terms like "more significant"
Don't interpret individual studies without reference to other existing findings
    Meta-analysis
  Don't report (or publish) only the "significant" findings
Be transparent about how many tests were conducted
P-hacking
Avoid causal language in observational studies
Impact 
Cause
Affect
Influence
Embrace uncertainty

## sjPlot ------------------------------------------------------------

library(psych)
describe(dataset)

library(sjPlot)
#this require sjPlot
dataset %>% 
  select(column_name:column_name10) %>% 
  tab_corr(na.deletion = "pairwise", corr.method = "pearson")

tab_corr(tuition_pipe, na.deletion = "pairwise", corr.method="pearson", file = 
           "Figures/corr_table.html")


## t.test ------------------------------------------------------------

Comparing Groups
Simple Two-Group Comparison
X = IV (Categorical Groups)
Grouping variable (e.g. men vs. women; MST vs. non-MST)
Y = DV (Continuous Outcome)
Outcome

t.test(PCL_SCORE~gender, var.eq = TRUE)
##t is an absolutely value
##there's about a 36% chance that a difference like the one we are seeing here could arise just by  chance

#Assumptions We Are Making (T-Test)
#Independent Groups
#Normally Distributed Scores
#Parametric Test
#Homogeneity of Variance
#Across Two Groups
#Outcome variable is interval or ratio scaling
#Some rule of thumbs ... are at least 15 possible scores possible?

What if my Comparison is Pre-Post?
  Paired T-Test (i.e. Independent T-Test, Matched T-Test)
Wilcoxon Signed-Rank Test
For more ordinal variables

t.test(PCL_SCORE, PCL_SCORE_POST, paired = TRUE)
wilcox.test(AUDITC_SCORE, AUDITC_SCORE_POST, paired = TRUE)

Homogeneity of variance (amount of variability is the same between measures) is rarely upheld longitudinally
At the beginning of treatment, every one pretty much has high scores
But after treatment, you have folks who improve and folks who do not
If other timepoints exist (>2), other problems usually exist to using ANOVA-based tests
E.g. Homogeneity of Correlation
~ANOVA is when there are more than two means to compare~
  
  Effect Size

Difference is significant, which means... We found a difference between groups that was more than by chance
And does not mean...how big that difference is

How large is the difference?
  Effect Size

First Step: Is there a difference? 
  Second Step: How big is that difference?
  
  2 Primary Ways to Measure Effect
A difference metric
Standardized difference between means
E.g. Cohens d
          Gives you the effect size in SD units 
          How many standard deviation units apart are these two means? (My group means are this far apart)
    Proportion of the difference in Y that vary systematically with X
      Could be accounted for by X
        E.g. R^2

#### effect size ------------------------------------------------------------

library(effsize)
cohen.d(`PCL5 Score_Intake`, PCL1WEEK, na.rm = TRUE, paired = TRUE, hedges = TRUE)

Cohen's d
Difference between means / pooled SD
library(effsize)
Could use Soper's online calculator for between-group variant (type in mean, type in SD)
      Within-subject variant can be a bit more complex
      Standards
        .3 .5 .8 (?)
          Large Effects ... (e.g. Large reductions in PCL or PHQ score)

R^2
    SSM / SST
      We will want for regression here
Not standardly used for t-test
Or just square R for correlation
Standards
.1 .8 .15 (?)

#### Mann-Whitney U Test ------------------------------------------------------------

Other Popular Options for Two Group Comparisons

#Ordinal Y Variable
#The populations of interests are not equal

#Still making assumptions
#Less Restrictive
#Independent Groups
#Shape of Distributions

#wilcox.test(Y~X)

wilcox.test(AUDITC_SCORE~gender)

#Mean Ranked - if you take all of the AUDITC scores of all the participants, instead of giving them their actual AUDITC score, you rank them (lowest AUDITC = 1...so on) 
#So you lose the original form of measurement and instead you just rank each person's AUDITC score
#And then you measure the rank 

#### Welch's t-test ------------------------------------------------------------

#does not assume equal variances
#default

#t.test(DV~IV)
t.test(PHQ_SCORE~gender)

#help(t.test)

## chi^2 ------------------------------------------------------------

Chi^2
Comparing groups (2+) in a DV that is also categorical
Does the frequency of Y differ across X groupings?
  Looking for the relationship between two categorical variables
(e.g. MST v. non-MST & Deployment)

Did men and women differ in frequency in MST?
  Is there a relationship between sex and MST?
  
  #Create Table
  #table <- table(X,Y)
  
  MSTsextable <- table(MSTYN, gender)
View(MSTsextable)

#Run Chi^2 on table
#chitest <- chisq.test(table)

chi2result <- chisq.test(MSTsextable)
chi2result

#OR
#chitest <- chisq.test(table(y,x))
#chitest

MST911 <- table(MSTYN, Post911)
MST911

chiMST911 <- chisq.test(MST911)
chiMST911

#Bar Plot
#easy was to show the differences in frequencies
#barplot(tablename, beside = T, legend = T)
barplot(MSTsextable, beside = T, legend = T, xlab = "gender", ylab = "frequency")
barplot(MST911, beside = T, legend = T, xlab = "Post-911")

Chi^2 Assumptions
Expected counts in each cell exceed 5
Chiname$expected
fisher.test(tablename) #Fisher's Exact Test
Independent Groups and mutually exclusive categories
Underlying (theoretical) smooth probably distribution (~"continuity")
N<40? Expected counts <10?
  Yates Continuity Correction
chisq.test(tablename, correction = T)

Effect Size for Chi^2
Cramers v
    Correlation between X and Y
      .3 .7
      .1 .3 .5

## Rcmdr ------------------------------------------------------------
      
library(Rcmdr)
      
## linear regression ------------------------------------------------------------
  
Components of a regression model
X
(e.g. age)
Y
(e.g. depression)
(Bi-Variate Correlations)
Slope
Intercept 
Our prediction (e.g. for depression)

#model <- lm (Y ~ X1)

The Presence of Other Variables
Controlling for other variables
Necessary?
  Examining predictors in the presence (while adjusting for) other potential predictors
R^2 increase
Change in meaning of slope and intercept

regression_1 <- lm(what_you_want_to_predict ~ predictor_variable + control, data = dataset)
summary(regression_1)

### linear reg tables ------------------------------------------------------------

#using kable
install.packages("kable")
library(kableExtra)
kable(regression_1) #a regression table

#using stargazer
install.packages("stargazer")
library(stargazer)
stargazer(regression_1) #produces as an html output 

#using sjPlot
library(sjPlot)
tab_model(regression_1)

Results indicate that Number of years in degree (beta = 'r reg1$coefficients$degree_num')

## multiple linear ------------------------------------------------------------

AUDITCagePTSDdepModel <- lm(data = reduced_IOP, PCL_SCORE ~ PHQ_SCORE + age + AUDITC_SCORE)
summary(AUDITCagePTSDdepModel)

Factors to Include
Determining which Xs belong in the model
Problems
Multicollinearity 
Correlations between predictor variable
How does each contribute to the DV prediction?
  Changes to significance problems

Checking regression assumptions
Multicollinearity: Get VIFs (from "car" package)

tidy(lm.a) #coefficient-level output
glance(lm.a) #model-level output
anova(lm.a) #ANOVA decomposition

### multicolinearity ------------------------------------------------------------

#Variance Inflation Factors
#Multicolinearity can artificially increases the amount of variability in our prediction
#Estimates for how inflated the amount of error variability seems to be getting as a result of multicolinearity 

install.packages("car")
library(car)

#vif(modelname)
vif(PTSDgadGENDER)

#Exactly 1 would mean that there was no multicolinearity, no inflation at all
#What this number shows you how much inflation and how much error variability exists
#If VIF is < 2 or 3, then you should be good

cor.test(PHQ_SCORE, GAD_SCORE)

# funs ------------------------------------------------------------

## beepr ------------------------------------------------------------

library(beepr)
beep(sound=2)

      