if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
devtools::install_github("hadley/ggplot2")
devtools::install_github("hadley/dplyr")
devtools::install_github("hadley/tidyr")
devtools::install_github("tidyverse/glue")
devtools::install_github("tidyverse/readxl")

library(tableone)
library(readxl)
library(dplyr) 
library(ggplot2)
library(ggpubr)
library(tidyr)
library(glue)
library(rstatix)
library(lemon)

## ============================== General settings and loading data ==============================
# Define a theme to be used in figures
my.theme 		<- theme_light()
my.theme 		<- my.theme + theme(panel.border=element_blank(), 
axis.line 		<- element_line(), 
axis.ticks 		<- element_line(colour='black'),
axis.title.y 	<- element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
text 			<- element_text(size=20, family="serif"))

# Load data in a wide format and convert to something that may be used for estimations of ANOVA and for plotting purposes
wdir 				<- getwd()
df 					<- read_excel(paste(wdir, "data-guideXT.xlsx", sep=""), sheet = "table_all")
df 					<- df %>% drop_na(updrs_GXT)
df 					<- as.data.frame(df)
df 					<- df[df$include==1,]
df_wide 			<- df %>% select(pseud, updrs_off, updrs_GXT, updrs_MT) %>% gather(condition, UPDRS_score, updrs_off, updrs_GXT, updrs_MT)
df_wide$pseud 		<- as.factor(df_wide$pseud)
df_wide$condition 	<- as.factor(df_wide$condition)
df$gender 			<- recode(as.factor(df$gender), "1" = "female", "2" = "male", .default = NA_character_)

# Create stats, which are necessary for plotting later
df.summary <- df_wide %>% group_by(condition) %>% summarise(sd = sd(UPDRS_score), updrs_mn = mean(UPDRS_score))
df.summary

## ============================== Demographics ==============================
listVars<- c("age", "gender", "ledd", "disease_duration")
catVars <- c("gender") # categorial variables
table1 	<- CreateTableOne(vars = listVars, data = df, factorVars = catVars)
print(table1, showAllLevels = TRUE)

## ============================== Visualisation for UPDRS changes ==============================
p1 <- ggplot(df_wide, aes(x=condition, y=UPDRS_score)) +
geom_jitter(position = position_jitter(width=0.1, height=0),
color = "darkgray") + 
scale_x_discrete(labels= c("GuideXT", "Clinical \ntesting", "DBS-OFF")) +
geom_line(aes(x=condition, y=updrs_mn, group = 1), data = df.summary, linetype="twodash", size=1) +
geom_errorbar(
aes(x=condition, y=updrs_mn, ymin = updrs_mn-sd, ymax = updrs_mn+sd),
data = df.summary, width = 0.1,  size=1) +
geom_point(aes(x=condition, y=updrs_mn), data = df.summary, size=2) +
# coord_capped_cart(left='none', bottom=brackets_horizontal(direction='up')) +
ylim(0, 75) + labs(y = "Score at Unified Parkinson's Disease \nRating Scale (UPDRS)", x = "") 
my.theme

# Statistics on changes of UPDRS due to DBS settings (conditions)
df_wide %>% group_by(condition) %>% identify_outliers(UPDRS_score) # Outliers
df_wide %>% group_by(condition) %>% shapiro_test(UPDRS_score) # Normality
ggqqplot(df_wide, "UPDRS_score", facet.by = "condition")
res.aov <- anova_test(data = df_wide, dv = UPDRS_score, wid = pseud, within = condition)
get_anova_table(res.aov)

# pairwise comparisons
pwc <- df_wide %>% pairwise_t_test(
UPDRS_score ~ condition, paired = TRUE,
p.adjust.method = "bonferroni")
pwc

# Add statistics on visualisation
pwc <- pwc %>% add_xy_position(x = "condition")
p1 + 
stat_pvalue_manual(pwc, tip.length = 0.005, y.position= c(60, 65, 70), 
family="serif", bracket.size=.5) +
labs(subtitle = get_test_label(res.aov, detailed = TRUE), caption = get_pwc_label(pwc)
)
