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
library(gridExtra)
library(Hmisc)
library(ggthemes)

## ============================== General settings and loading data ==============================
# Define a theme to be used in figures
my.theme <- theme_light()
my.theme <- my.theme + theme(panel.border=element_blank(), 
                             axis.line = element_line(), 
                             axis.ticks = element_line(colour='black'),
							               axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
							               text=element_text(size=20, family="serif"))

# Load data in a wide format and convert to something that may be used for estimations of ANOVA and for plotting purposes
wdir 	        <- getwd()
df 		        <- read_excel(paste(wdir, "data-guideXT.xlsx", sep=""), sheet = "table_all")
df 		        <- df %>% drop_na(updrs_GXT)
df 		        <- as.data.frame(df)
df 		        <- df[df$include==1,]
df_angle      <- df %>% select(pseud, DICE_L, DICE_R) %>% gather(side, "DC", DICE_L, DICE_R)
df_angle$side <- as.factor(df_angle$side)

# Required function
data_summ <- function(x) {
   m <- mean(x)
   mdn <- median(x)
   ymin <- m-sd(x)
   ymax <- m+sd(x)
   return(c(y=m,ymin=ymin,ymax=ymax))
}

## ============================== Visualisation for VTA overlap ==============================
gap = .005
bp_size = 2.25

# Start plotting data
p5 <- ggplot(data=df_angle, aes(x=side, y=DC, fill=side)) + 
	            ylim(0,1) +
	            geom_jitter(aes(color=side), position=position_jitter(0.05), size=1.25) + 
	            scale_color_manual(values = c("#6A7F99", "#B34439")) + 
	            geom_rangeframe(y=rep(c(0, 1), 29)) +
	            scale_x_discrete(labels = c('Left STN', 'Right STN')) +
	            ylab("Overlap between VTA estimated via GuideXT\U2122 \nsoftware and Lead-DBS toolbox") +
	            xlab("") +
	            theme_minimal() + 
	            theme(legend.position = "none", text=element_text(size=20, family="serif")) + 
	
	# Start plotting q1-3 of the boxplot
	geom_segment(aes(x = 1, y = min(DC[side=="DICE_L"]), 
						xend = 1, yend = -gap+median(DC[side=="DICE_L"])),  
		size=.25, color="black", linetype=1) +
	geom_segment(aes(x = 1, y = gap+median(DC[side=="DICE_L"]), 
						xend = 1, yend = max(DC[side=="DICE_L"])),  
		size=.25, color="black", linetype=1) +

	# Start plotting q1-3 of the boxplot
	geom_segment(aes(x = 1, y = quantile(DC[side=="DICE_L"])[2], 
						xend = 1, yend = -gap+median(DC[side=="DICE_L"])),  
		size=bp_size, color="black", linetype=1) +
	geom_segment(aes(x = 1, y = gap+median(DC[side=="DICE_L"]), 
						xend = 1, yend = quantile(DC[side=="DICE_L"])[4]),  
		size=bp_size, color="black", linetype=1) +


# Start plotting q1-3 of the boxplot
	geom_segment(aes(x = 2, y = min(DC[side=="DICE_L"]), 
						xend = 2, yend = -gap+median(DC[side=="DICE_R"])),  
		size=.25, color="black", linetype=1) +
	geom_segment(aes(x = 2, y = gap+median(DC[side=="DICE_R"]), 
						xend = 2, yend = max(DC[side=="DICE_R"])),  
		size=.25, color="black", linetype=1) +

	# Start plotting q1-3 of the boxplot
	geom_segment(aes(x = 2, y = quantile(DC[side=="DICE_R"])[2], 
						xend = 2, yend = -gap+median(DC[side=="DICE_R"])),  
		size=bp_size, color="black", linetype=1) +
	geom_segment(aes(x = 2, y = gap+median(DC[side=="DICE_R"]), 
						xend = 2, yend = quantile(DC[side=="DICE_R"])[4]),  
		size=bp_size, color="black", linetype=1)
		

## ============================== Visualisation relationship DICE vs. clinical outcome ==============================
# Load data in a wide format and convert to something that may be used for estimations of ANOVA and for plotting purposes
wdir 	          <- getwd()
df 		          <- read_excel(paste(wdir, "data-guideXT.xlsx", sep=""), sheet = "table_all")
df 		          <- df %>% drop_na(updrs_GXT)
df 		          <- as.data.frame(df)
df 		          <- df[df$include==1,]
df$updrs_diffL  <- df$updrs_MTL - df$updrs_GXTL
df$updrs_diffR  <- df$updrs_MTR - df$updrs_GXTR

df_angle            <- 	df %>% select(pseud, DICE_L, DICE_R) %>% gather(side, "DC", DICE_L, DICE_R)
df_angle2           <- 	df %>% select(pseud, updrs_diffL, updrs_diffR) %>% gather(UPDRS_side, "UPDRS", updrs_diffL, updrs_diffR)
df_angle$UPDRSdiff  <- df_angle2$UPDRS
df_angle$side       <- as.factor(df_angle$side)


p6 <- ggplot(data=df_angle, aes(x=abs(UPDRSdiff), y=DC)) +
      geom_point(shape=1) +    # Use hollow circles
      geom_smooth(method=lm,   # Add linear regression line
                se=FALSE)    # Don't add shaded confidence region
p6
# ANNOTATION: p6 is not used in the manuscript vut only described!!


