#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function for easier library loading
suppressPackageStartupMessages(
  libraries("dplyr",
            "pastecs", # for stat.desc()
            "car", # for LeveneTest()
            "data.table", # for set.names()
            "report",  # report ANOVA output
            "multcomp", # enables Tukey tests
            "rstatix",
            "ggpubr",
            "WRSS", # robust statistical methods
            "AICcmodavg",
            "emmeans", # for emmeans test
            "ggstatsplot",
            "nortest", # various normality test
            "stats", # for ks.test()
            "BayesFactor", # for Bayesian ANOVA
            "WRS2",
            "asbio", # for additional post hoc functionality
            "gplots", # for plotmeans()
            "coin", # for independence_test()
            "rcompanion" # for pairwisePermutationTest()
    ))

###############################
###############################
#### Set Working Directory ####
###############################
###############################

setwd("C:/Final Data Sets/Trinity College PhD 2021/GTD Data")

#####################
#####################
#### Obtain Data ####
#####################
#####################

GTD <- read.csv("globalterrorismdb_0919dist.csv")

########################
########################
#### Filtering Data ####
########################
########################

################################
# Filter Doubt Terrorism == No #
################################

# This removes all non - terrorism as terrorism incidents are coded yes
# yes means it is doubted violence is terrorism

GTDDT <- GTD %>% dplyr::filter(doubtterr == 0)

###########################
# Filter Specificity == 1 #
###########################

# This removes all rows where the geographic coordinates have not been verified
# This is important because province and city variables are used in the modeling, so it is necessary to know exactly where each attack occurred.

GTDDTS <- GTDDT %>% dplyr::filter(specificity == 1)

##################
# Rename Columns #
##################

library(data.table) # for set.names function

setnames(GTDDTS, old = c("iyear", "imonth", "provstate", "city", "attacktype1_txt", "gname", "targtype1_txt", "weaptype1_txt", "nkill", "nwound"), 
         new = c("Year", "Month", "Province", "City", "Attack", "Group", "Target", "Weapon", "Dead", "Wounded"))

#########################
# Select Relevant Columns
#########################

GTDDTS_FCS <- dplyr::select(GTDDTS, Year, Province, City, multiple, success, Attack, Target, Group, Weapon, Dead)

write.csv(GTDDTS_FCS, file = "GTDDTS.csv", row.names = F)
GTDDTS <- read.csv("GTDDTS.csv")
dim(GTDDTS)

#############################
# Normality Based Filtering #
#############################

# The shapiro_test() test for normality, requires a sample size to be between 3 and 5000. There is the ad.test() to use with a different sample size. 
# However, this only is suitable for one variable. Therefore, to reduce the sample size so it is between these parameters (3-5000 rows), ISIS as the highest named group, is chosen as the sample. The FG attack count is indicated in the table below

Groups <- GTDDTS %>%
  count(Group, sort = T)
Groups <- as.data.frame(Groups) # convert Groups object into data frame
Groups <- rename(Groups, Count = n) # rename n column as Count
head(Groups, n = 10) # select and show first ten groups with highest count

colSums(is.na(GTDDTS)) # count the NA's in each column

FG_NNA <- na.omit(GTDDTS) # remove NA's
colSums(is.na(FG_NNA))

FG_NNA <- write.csv(FG_NNA, file = "FG_NNA.csv", row.names = F)
FG_NNA <- read.csv("FG_NNA.csv")

glimpse(FG_NNA)

# Filter ISIS

ISIS <- FG_NNA %>% dplyr::filter(Group == "Islamic State of Iraq and the Levant (ISIL)")

###################
# Variable Counts #
###################

t <- ISIS %>%
  count(Attack, sort = T)
View(t)

t <- ISIS %>%
  count(Province, sort = T)
View(t)

t <- ISIS %>%
  count(City, sort = T)
View(t)

t <- ISIS %>%
  count(Target, sort = T)
View(t)

t <- ISIS %>%
  count(Group, sort = T)
View(t)

t <- ISIS %>%
  count(Weapon, sort = T)
View(t)

#############################
# Recategorise Attack Types #
#############################

Attacks <- ISIS %>%
  count(Attack, sort = T)
View(Attacks)
Attacks <- as.data.frame(Attacks) # convert Groups object into data frame
Attacks <- rename(Attacks, Count = n) # rename n column as Count
Attacks

# Hostage Taking (Barricade Incident), with 17, Facility/Infrastructure Attack with 7 and Hijacking with 3, have very low counts, 
# so have been collectively grouped as Other Attack. Unknown Attack has also been grouped with Other Attack. 
# Create new variable Attack_type using mutate() and case_when()

ISIS <- ISIS %>%
  mutate(Attack_Type = case_when(
    Attack == 'Unknown' |
    Attack == 'Assassination' |  
    Attack == 'Hostage Taking (Barricade Incident)' | 
    Attack == 'Facility/Infrastructure Attack' |
    Attack == 'Hijacking' ~ 'Other Attack',
    Attack == 'Bombing/Explosion' ~ 'Bomb',
    Attack == 'Armed Assault' ~ 'Armed Assault',
    Attack == 'Hostage Taking (Kidnapping)' ~ 'Hostage Kidnap'))

ISIS$Attack_Type <- as.factor(ISIS$Attack_Type) # convert variable to factor

# Show the levels of the Attack Type variable:

unique(ISIS$Attack_Type)

glimpse(ISIS)

#################
# Visualization #
#################

# The below plot indicates the dead count means of each attack type by ISIS

plotmeans(formula = Dead ~ Attack_Type, # Count by Attack Type
          data = ISIS, # the data frame
          xlab = "Attack", # x-axis label
          ylab = "Mean", # y-axis label
          n.label = FALSE # don't display sample size
              )

# The question is, are the differences in mean scores in the above plot down to chance or 'real?'
# Assassination and Infrastructure Attack have the lowest means
# The black circle for each attack type is the actual mean for that attack type.
# The blue bar is the range of mean values for each attack type

#####################
#####################
# Check assumptions # 
#####################
#####################

##########################
##########################
# Check for independence #
##########################
##########################

ISIS <- select(ISIS, Attack_Type, Dead)
dim(ISIS)

# fit regression model
model <- lm(Dead ~ Attack_Type, ISIS)

# perform Durbin-Watson test
set.seed(123)
durbinWatsonTest(model)

#  The Durbin Watson test reports a test statistic, with a value from 0 to 4, where the rule of thumb is:

# 2 is no autocorrelation.
# 0 to <2 is positive autocorrelation.
# >2 to 4 is negative autocorrelation.

# From the output we can see that the test statistic is 1.973246 and the corresponding p-value is 0.204. The test statistic is between 0 and 2, 
# which means there is positive autocorrelation. But, it is very close to two, so is very slight Since this p-value is bigger than 0.05, we can uphold the null hypothesis and conclude that the residuals in this regression model are not autocorrelated and therefore are independent.

############
# Outliers #
############

# Outliers can be easily identified using box plot methods, implemented in the R function identify_outliers() [rstatix package].

ISIS %>% 
  group_by(Attack_Type) %>%
  identify_outliers(Dead)

# There are some extreme outliers, so this assumption has been violated. It's possible to keep the outliers in the data and perform a robust ANOVA test using the WRS2 package using the t1way() function, as follows:

t1way(Dead ~ Attack_Type, ISIS)

# The p value is zero, so there are significant differences in the means

########################
# Normality assumption #
########################

# The normality assumption can be checked by using one of the following two approaches:

# Analyzing the ANOVA model residuals to check the normality for all groups together. This approach is easier and it's very handy when you have many groups 
# or if there are few data points per group.
# Check normality for each group separately. This approach might be used when you have only a few groups and many data points per group.

# Check normality assumption by analyzing the model residuals. QQ plot and Shapiro-Wilk test of normality are used. 
# QQ plot draws the correlation between a given data and the normal distribution.

# Build the linear model
model  <- lm(Dead ~ Attack_Type, ISIS)

# Create a QQ plot of residuals

ggqqplot(residuals(model))

# The sample residuals follow a normal theoretical distribution for much of the sample data distribution, 
# but it appears to diverge from the theoretical line towards the end of the data set.

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

# It is significant, with a p-value of  2.04e-81, so the assumption of normality has been violated

# Check normality assumption by groups. Computing Shapiro-Wilk test for each Attack variable level. 
# If the data is normally distributed, the p-value should be greater than 0.05. The output below indicates that hostage kidnap and other attack have particularly significant distributions that dont follow normal distribution

ISIS %>% 
  group_by(Attack_Type) %>%
  shapiro_test(Dead)

# Other Normality Tests

# Anderson-Darling normality test

ad.test(ISIS$Dead)

# Cramer-von Mises normality test

cvm.test(ISIS$Dead)

# Lilliefors (Kolmogorov-Smirnov) normality test

lillie.test(ISIS$Dead)

# Pearson chi-square normality test

pearson.test(ISIS$Dead)

# Shapiro-Francia normality test

sf.test(ISIS$Dead) # Error in sf.test(ISIS$Dead) : sample size must be between 5 and 5000

# All attack types are significant apart from hijacking, which means they are not normally distributed

ggqqplot(ISIS, "Dead", facet.by = "Attack_Type")

# Therefore, with normality assumption violated, it is advised to use the kruskal test, which is the non-parametric alternative to one-way ANOVA test, as follows:

kruskal.test(Dead ~ Attack_Type, ISIS)

# As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the attack types

####################################
# Homogeneity of variance assumption
####################################

# The residuals versus fits plot can be used to check the homogeneity of variances.
plot(model, 1)
plot(model)

### Levene's test for homogeneity of variance ###
# It's also possible to use the Levene's test to check the homogeneity of variances:
TR <- ISIS %>% levene_test(Dead ~ Attack_Type)
levene_test(ISIS, Dead ~ Attack)

# From the output above, we can see that the p-value is 1.11e-21, which is significant. This means that, there is significant difference between variances across attack types. Therefore, we cant assume the homogeneity of variances in the different attack types.

### Bartlett test for homogeneity of variance ###

bartlett.test(Dead ~ Attack_Type, ISIS) 

#############################################################
# What to do when homogeneity of variance has been violated #
#############################################################

# The Welch one-way test is an alternative to the standard one-way ANOVA in the situation where the homogeneity of variance can't be assumed (i.e., Levene test is significant).
# In this case, the Games-Howell post hoc test or pairwise t-tests (with no assumption of equal variances) can be used to compare all possible combinations of group differences.

# Welch One way ANOVA test
res.aov2 <- ISIS %>% welch_anova_test(Dead ~ Attack_Type)
res.aov2

# The p-value is significant, so the population variances are not similar

# Alternatively, oneway.test() can be used

oneway.test(Dead ~ Attack_Type, ISIS)

# The p-value is significant, so the population variances are not similar

##################################################################
# Indicate which variable pairs have the strongest significances #
##################################################################

# Pairwise comparisons (Games-Howell)

pwc2 <- ISIS %>% games_howell_test(Dead ~ Attack_Type)
pwc2

# The estimate is the difference in means between the two groups. 

# Computation #

res.aov <- ISIS %>% anova_test(Dead ~ Attack_Type)
res.aov

################
################
# Post-hoc tests
################
################

# A significant one-way ANOVA is generally followed up by Tukey post-hoc tests to perform multiple pairwise comparisons between groups. 
# This tells us that the mean value between each group is not equal. However, it doesn't tell us which groups differ from each other. 
# To determine this, it is neccesary to use Tukey HSD statistical test.

# Pairwise comparisons
pwc <- ISIS %>% tukey_hsd(Dead ~ Attack_Type)
pwc

TukeyHSD(res.aov2, conf.level = 0.99)

############################################
# Additional Pairwise Post Hoc Comparisons #
############################################

# #require(asbio)
lsdCI(ISIS$Dead, ISIS$Attack)

# Next is a simple bonferroni adjustment.
#require(asbio)
bonfCI(ISIS$Dead, ISIS$Attack)

tukeyCI(ISIS$Dead, ISIS$Attack)

#require(asbio)
scheffeCI(ISIS$Dead, ISIS$Attack)

## the asbio package permits implementation of the Dunnett test
# with specification of the ANOVA model and the level of the control group
#require(asbio)
dunnettCI(ISIS$Dead, ISIS$Attack, control="control")

#################################
# Robust Methods and Resampling #
#################################

################
# Robust ANOVA #
################

# For the t1way() below, the heteroscedastic assumption is not required.

t1way(formula = Dead ~ Attack_Type, data = ISIS)

# The lincon() function below is the post hoc test to the t1way() above

lincon(formula = Dead ~ Attack_Type, data = ISIS)

med1way(formula = Dead ~ Attack_Type, data = ISIS, iter = 1000)
?t1way()

###########################################
# Resampling Methods: Permutation Testing #
###########################################

# library(coin)
independence_test(formula = Dead ~ Attack_Type, data = ISIS)

# It is significant

# library(rcompanion)
ppairs <- pairwisePermutationTest(formula = Dead ~ Attack_Type, 
                                  data = ISIS,
                                  method = "fdr")
ppairs

# All pairs are significant

####################################
# Resampling Methods: Bootstraping #
####################################

t1waybt(Dead ~ Attack_Type, 
        tr = .2, 
        nboot = 800, 
        data = ISIS)

# It is significant

# It is possible to employ another WRS2 function, mcppb20, to do pairwise post hoc tests comparing pairs of groups within the bootstrapping and trimming framework

mcppb20(Dead ~ Attack_Type,
        tr = .2,
        nboot = 800, 
        data = ISIS)

########################
# Bayesian One Way ANOVA
########################

models <- anovaBF( 
  formula = Dead ~ Attack_Type, ISIS
)

models

models/max(models)

