#################
#################
#### Project ####
#################
#################

# This provides a one way ANOVA analysis of Islamic State Terrorism
# It uses a categorical variable named attack type and a continuous fatal casualties variable to model the ANOVA

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

# This removes all non - terrorism as terrorism incidents are coded one
# One means it is doubted violence is terrorism

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

setnames(GTDDTS, old = c("iyear", "imonth", "provstate", "city",    "attacktype1_txt", "gname", "targtype1_txt", "weaptype1_txt", "nkill", "nwound"), new = c("Year", "Month", "Province", "City", "Attack", "Group", "Target", "Weapon", "Dead", "Wounded"))

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

# The shapiro_test() test for normality, requires a sample size to be between 3 and 5000. There is the ad.test() to use with a different sample size. However, this only is suitable for one variable. Therefore, to reduce the sample size so it is between these parameters (3-5000 rows), ISIS as the highest named group, is chosen as the sample. The FG attack count is indicated in the table below

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

##########################################################
# Example how to use ifelse nested within a dplyr function
##########################################################

# Group

GF <- mutate(FG_NNA, 
             Groups = ifelse(Group %in% "Islamic State of Iraq and the Levant (ISIL)", "ISIS",
                      ifelse(Group %in% "Shining Path (SL)", "ShiningPath",
                      ifelse(Group %in% "Taliban", "Taliban",
                      ifelse(Group %in% "Al-Shabaab", "Al-Shabaab", 
                      ifelse(Group %in% "Irish Republican Army (IRA)", "IRA",                         "OtherGroup"))))))
str(GF)
t <- GF %>%
  count(Groups, sort = T)
View(t)
unique(GF$Groups)

#############################
# Recategorise Attack Types #
#############################

Attacks <- ISIS %>%
  count(Attack, sort = T)
View(Attacks)
Attacks <- as.data.frame(Attacks) # convert Groups object into data frame
Attacks <- rename(Attacks, Count = n) # rename n column as Count
Attacks

# Hostage Taking (Barricade Incident), with 17, Facility/Infrastructure Attack with 7 and Hijacking with 3, have very low counts, so have been collectively grouped as Other Attack. Unknown Attack has also been grouped with Other Attack. 
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

ISIS$Attack_Type <- as.factor(ISIS$Attack_Type)

# Show the levels of the Attack Type variable:

unique(ISIS$Attack_Type)

glimpse(ISIS)

ISIS <- select(ISIS, Attack_Type, Dead)

######################
# Summary statistics #
######################

# Compute some summary statistics (count, mean and sd) of the variable Dead organized by Attack Type:

ISIS %>%
  group_by(Attack_Type) %>%
  get_summary_stats(Dead, type = "mean_sd")

by(ISIS$Dead, list(ISIS$Attack_Type), stat.desc)
boxplot(ISIS$Dead ~ ISIS$Attack)

#################
# Visualization #
#################

# Create a box plot of Attack type by dead:
  
# IS Dead Per Attack Type Box Plot

ggplot(ISIS, aes(x = Attack_Type, 
                   y = Dead, 
                   fill = Attack_Type)) +
  geom_boxplot() +
  scale_fill_discrete(name = "Attack Type") +
  ggtitle("IS Dead Per Attack Type Box Plot") +
  #geom_jitter(shape = 15,
  #color = "steelblue",
  #position = position_jitter(0.21)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Attack Type")

# IS Dead Per Attack Type Jitter Plot

ggplot(ISIS, aes(x = Attack_Type, 
                   y = Dead, 
                   fill = Attack_Type)) +
  geom_jitter() +
  scale_fill_discrete(name = "Attack Type") +
  ggtitle("IS Dead Per Attack Type Box Plot") +
  #geom_jitter(shape = 15,
  #color = "steelblue",
  #position = position_jitter(0.21)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Attack Type")

ggplot(ISIS, aes(x = Attack_Type, 
                   y = Dead, 
                   fill = Attack_Type)) +
  geom_boxplot() +
  scale_fill_discrete(name = "Attack Type") +
  ggtitle("IS Dead Per Attack Type Box Plot") +
  #geom_jitter(shape = 15,
  #color = "steelblue",
  #position = position_jitter(0.21)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Attack Type") +
  scale_y_continuous(limits = c(0,40)) 

gd <- ISIS %>% 
  group_by(Attack_Type) %>% 
  summarise(Dead = mean(Dead)) 

ggplot(gd, aes(x = Attack_Type, 
               y = Dead, 
               fill = Attack_Type)) +
  geom_line(group = "Attack_Type") +
  ggtitle("IS Dead Per Attack Type Means") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Attack Type")

ggboxplot(ISIS, 
          x = "Attack_Type", 
          y = "Dead", 
          xlab = "Attack Type")

plotmeans(formula = Dead ~ Attack_Type, # Count by Attack Type
          data = ISIS, # the data frame
          xlab = "Attack", # x-axis label
          ylab = "Mean", # y-axis label
          n.label = FALSE # don't display sample size
              )
?plotmeans
# The question is, are the differences in mean scores in the above plot down to chance or 'real?'
# Assassination and Infrastructure Attack have the lowest means
# The black circle for each attack type is the actual mean for that attack type.
# The blue bar is the range of mean values for each attack type

#################
# What is ANOVA #
#################

# ANOVA is a statistical test for estimating how a quantitative dependent variable changes according to the levels of one or more categorical independent variables. ANOVA tests whether there is a difference in means of the groups at each level of the independent variable.

# Anova Assumptions
# The ANOVA test makes the following assumptions about the data:

# Independence of the observations. Each subject should belong to only one group. There is no relationship between the observations in each group. Having repeated measures for the same participants is not allowed.
# No significant outliers in any cell of the design
# Normality. the data for each design cell should be approximately normally distributed.
# Homogeneity of variances. The variance of the outcome variable should be equal in every cell of the design.

# Note that, if the above assumptions are not met there is a non-parametric alternative (Kruskal-Wallis test) to the one-way ANOVA.

# Unfortunately, there are no non-parametric alternatives to the two-way and the three-way ANOVA. Thus, in the situation where the assumptions are not met, you could consider running the two-way/three-way ANOVA on the transformed and non-transformed data to see if there are any meaningful differences.

# If both tests lead you to the same conclusions, you might not choose to transform the outcome variable and carry on with the two-way/three-way ANOVA on the original data.

# It's also possible to perform robust ANOVA test using the WRS2 R package.

# No matter your choice, you should report what you did in your results.

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

# From the output we can see that the test statistic is 1.973246 and the corresponding p-value is 0.204. The test statistic is between 0 and 2, which means there is positive autocorrelation. But, it is very close to two, so is very slight Since this p-value is bigger than 0.05, we can uphold the null hypothesis and conclude that the residuals in this regression model are not autocorrelated and therefore are independent.

# Now 

stat.test <- pairwise.t.test(ISIS$Dead, 
                             ISIS$Attack_Type,
                             p.adjust.method = "none")

stat.test <- ISIS %>% 
  t_test(Dead ~ Attack_Type, paired = F) %>% # must use paired = false as not all arguments in the x and y axis are the same length
  add_significance()
stat.test

# Create the plot
myplot <- ggboxplot(ISIS, x = "Attack_Type", y = "Dead", add = "point")
# Add statistical test p-values
stat.test <- stat.test %>% add_xy_position(x = "Attack_Type")
myplot + stat_pvalue_manual(stat.test, label = "p.adj.signif")

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

# Analyzing the ANOVA model residuals to check the normality for all groups together. This approach is easier and it's very handy when you have many groups or if there are few data points per group.
# Check normality for each group separately. This approach might be used when you have only a few groups and many data points per group.

# Check normality assumption by analyzing the model residuals. QQ plot and Shapiro-Wilk test of normality are used. QQ plot draws the correlation between a given data and the normal distribution.

# Build the linear model
model  <- lm(Dead ~ Attack_Type, ISIS)

# Create a QQ plot of residuals

ggqqplot(residuals(model))

# The sample residuals follow a normal theoretical distribution for much of the sample data distribution, but it appears to diverge from the theoretical line towards the end of the data set.

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model))

# It is significant, with a p-value of  2.04e-81, so the assumption of normality has been violated

# Check normality assumption by groups. Computing Shapiro-Wilk test for each Attack variable level. If the data is normally distributed, the p-value should be greater than 0.05. The output below indicates that hostage kidnap and other attack have particularly significant distributions that dont follow normal distribution

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
?levene_test
# From the output above, we can see that the p-value is 1.11e-21, which is significant. This means that, there is significant difference between variances across attack types. Therefore, we cant assume the homogeneity of variances in the different attack types.

### Bartlett test for homogeneity of variance ###

bartlett.test(Dead ~ Attack_Type, ISIS) 

# From the output above, we can see that the p-value is 2.2e-16, which is significant. This means that, there is significant difference between variances across attack types. Therefore, we cant assume the homogeneity of variances in the different attack types.

# In a situation where the homogeneity of variance assumption is not met, it is possible to compute the Welch one-way ANOVA test using the function welch_anova_test() from the rstatix package. This test does not require the assumption of equal variances.

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

########################################
# Visualization: box plots with p-values
########################################

pwc2 <- pwc2 %>% add_xy_position(x = "Attack_Type", 
                                 step.increase = 1)
ggboxplot(ISIS, 
          x = "Attack_Type", 
          y = "Dead") +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov2, detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )

# Computation #

res.aov <- ISIS %>% anova_test(Dead ~ Attack_Type)
res.aov

# In the table above, the column ges corresponds to the generalized eta squared (effect size). It measures the proportion of the variability in the outcome variable (here Dead count) that can be explained in terms of the predictor (here, attack type). An effect size of 0.04 (4%) means that 4% of the change in the dead count can be accounted for the attack type.

# From the above ANOVA table, it can be seen that there are significant differences between attack types (p = 6.24e-23), which are highlighted with "*", F(4, 2677) = 28.144, p = 6.24e-23, eta2[g] = 0.0, where
# F indicates that we are comparing to an F-distribution (F-test); (2, 27) indicates the degrees of freedom in the numerator (DFn) and the denominator (DFd), respectively; 4.85 indicates the obtained F-statistic value
# p specifies the p-value
# ges is the generalized effect size (amount of variability due to the factor)

################
################
# Post-hoc tests
################
################

# A significant one-way ANOVA is generally followed up by Tukey post-hoc tests to perform multiple pairwise comparisons between groups. This tells us that the mean value between each group is not equal. However, it doesn't tell us which groups differ from each other. To determine this, it is neccesary to use Tukey HSD statistical test.

# Pairwise comparisons
pwc <- ISIS %>% tukey_hsd(Dead ~ Attack_Type)
pwc

TukeyHSD(res.aov2, conf.level = 0.99)

# The output contains the following columns:

# estimate: estimate of the difference between means of the two groups
# conf.low, conf.high: the lower and the upper end point of the confidence interval at 95% (default)
# p.adj: p-value after adjustment for the multiple comparisons.
# The significant differences in dead count are between the groups that have star symbols in the p.adjusted significance column

##########################################
# # Visualization: box plots with p-values
##########################################

pwc <- pwc %>% add_xy_position(x = "Attack")
ggboxplot(ISIS, x = "Attack", y = "Dead") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

############################################
# Additional Pairwise Post Hoc Comparisons #
############################################

# using the asbio package for Post Hoc pairwise comparisons

# First, the so-called Fisher's Protected LSD test is obtained. Recall that when there are three groups or categorical levels, simulation work has shown that performing the LSD
# tests following a significant omnibus F test does afford protection from error rate inflation. But when the design has more than three groups, the LSD test# CANNOT be recommended.

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

#############################################################################
# When no assumptions have been violated, I can follow a normal one way Anova, as follows #
############

my.anova <- aov(Dead ~ Attack, ISIS)
my.anova
summary(my.anova)
contrasts(ISIS$Attack) # indicates which Attack variable level is used as the intercept reference, which in this case, it is armed assault

#  R doesn't use the names "between-group" and "within-group". Instead, it tries to assign more meaningful names: in our particular example, the between groups variance corresponds to the effect that the drug has on the outcome variable; and the within groups variance is corresponds to the "leftover" variability, so it calls that the residuals

summary(my.anova)

# The p-value is significant, so the differences are real

summary.lm(my.anova)

# Results Interpretation #

# The intercept is the baseline or control group - armed assault in this instance. 
# The intercept estimate is the mean for armed assault
# The remainder of the values in the estimate column are the difference in means between the baseline armed assault and each individual attack type. So, for instance, the mean of assassination attack deaths is 3.2365 lower than armed assault mean dead, as indicated by the minus symbol. All other means are higher than armed assault, as they are all positive.
# The error is the differences in standard error between each pair of means
# Unarmed Assault is the only non significant difference in mean with armed assault. All others are significant
# The F-statistic is 169.3 on 4 and 50642 DF,  p-value: < 2.2e-16, which is a lot higher than it would be if it was by chance.

summary.aov(my.anova)

# There is a difference in means, but which attack type(s) are different? 
# The one-way ANOVA test does not inform which group has a different mean. Instead, you can perform a Tukey test with the function TukeyHSD().

Tukey <- TukeyHSD(my.anova) # HSD is Honestly Significant Difference
plot(Tukey)


# The p adj column indicates whether each pair of mean difference is significant or not in terms of dead values. In other words, this column informs which pairs of mean differences are driving the fact that the Attack variable has significant differences in mean with dead values.

# Homogeneity of variance #

bartlett.test(Dead ~ Attack_Type, GTDAD)
# Significant result, therefore variances cannot be assumed to be equal
plot(my.anova)

outlierTest(my.anova)

# There are ten rows as outliers

# Effect Size # 

