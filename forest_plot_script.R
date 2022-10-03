

library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)
library(cowplot)



############################################################################
# Gather data from stata output files:
############################################################################
# Pooled_results.txt, Interaction_results.txt, Interaction_results_N.txt, 
# Interaction_results_age.txt
############################################################################


overall.df <- read.table("Pooled_results.txt", header= TRUE)

overall.df$Variable <- 0
overall.df$Variable <- "overall"
overall.df$Strata <- 0
overall.df$Strata <- 1
overall.df2 <- overall.df[,c(9,10,1,2,3,4,6,7)]


strata.df <- read.table("Interaction_results.txt", header= TRUE)
cases.df <- read.table("Interaction_results_N.txt", header= TRUE)

strata.df$Cases <- 0
strata.df$Cases <- cases.df$Cases
strata.df$Participants <- 0
strata.df$Participants <- cases.df$Participants
strata.df2 <- strata.df[,c(1,2,3,4,5,6,8,9)]

age.df <- read.table("Interaction_results_age.txt", header= TRUE)
age.df$Variable <- 0
age.df$Variable <- "attained_age_cat"
age.df <- age.df[-1,]
age.df$Strata <- 1:3
age.df2 <- age.df[,c(10,1:5, 7:8)]

base.df <- data.frame("Variable"= rep(c("overall","attained_age_cat","bmi_r_initial_cat", "mena_cat", 
"ever_birth_cat", "age_fb_cat", "parity_cat_qc", "fam_bc_r",  "oc_ever_cat","smoke_ever_r_impute", "ethnic_cat"),1))
base.df$Strata <- 0
base.df$Strata <- max(strata.df$Strata)+1
base.df$Cases <- 0
base.df$Participants <- 0
base.df$log.HR <- 0
base.df$se.log.HR <- 0
base.df$log.HR.bmi <- 0
base.df$se.log.HR.bmi <- 0
base.df2 <- base.df[,c(1,2,3,4,5,6,7,8)]

# combine results into single data frame

results.df <- rbind(overall.df2, age.df2, strata.df2, base.df2)


##############################################################################
# Create Hazard ratio variables and 95% C.I., with and without BMI adjustment.
##############################################################################


results.df$HR <- 0
results.df$HR.low <- 0
results.df$HR.high <- 0
results.df$HR.bmi <- 0
results.df$HR.low.bmi <- 0
results.df$HR.high.bmi  <- 0

results.df$HR <- exp(0.8*results.df$log.HR)
results.df$HR.low <- exp(0.8*(results.df$log.HR-1.96*results.df$se.log.HR))
results.df$HR.high <- exp(0.8*(results.df$log.HR+1.96*results.df$se.log.HR))

results.df$HR.bmi <- exp(0.8*results.df$log.HR.bmi)
results.df$HR.low.bmi <- exp(0.8*(results.df$log.HR.bmi-1.96*results.df$se.log.HR.bmi))
results.df$HR.high.bmi <- exp(0.8*(results.df$log.HR.bmi+1.96*results.df$se.log.HR.bmi))

names(results.df)
#results.df <- rbind(results.df, base.df)
#results.df <- results.df[order(results.df$Strata),]

summary(as.factor(results.df$Variable))
results.df$New.Variable <- 0
results.df$New.Variable[results.df$Variable == "overall"] <- 1
results.df$New.Variable[results.df$Variable == "attained_age_cat"] <- 2
results.df$New.Variable[results.df$Variable == "bmi_r_initial_cat"] <- 3
results.df$New.Variable[results.df$Variable == "mena_cat"] <- 4
results.df$New.Variable[results.df$Variable == "ever_birth_cat"] <- 5
results.df$New.Variable[results.df$Variable == "age_fb_cat"] <- 6
results.df$New.Variable[results.df$Variable == "parity_cat_qc"] <- 7
results.df$New.Variable[results.df$Variable == "fam_bc_r"] <- 8
results.df$New.Variable[results.df$Variable == "oc_ever_cat"] <- 9
results.df$New.Variable[results.df$Variable == "smoke_ever_r_impute"] <- 10
results.df$New.Variable[results.df$Variable == "ethnic_cat"] <- 11
summary(as.factor(results.df$New.Variable))


############################################################################
# Round Hazard ratios to 2 decimal places, combine C.I. into character string.
############################################################################



results.df$HR.round <- 0
results.df$HR.low.round <- 0
results.df$HR.high.round <- 0
results.df$HR.bmi.round <- 0
results.df$HR.low.bmi.round <- 0
results.df$HR.high.bmi.round <- 0


results.df$HR.round <- format(round(results.df$HR,2), nsmall = 2 )
results.df$HR.low.round <- format(round(results.df$HR.low,2), nsmall = 2 )
results.df$HR.high.round <- format(round(results.df$HR.high,2), nsmall = 2, trim = TRUE )
results.df$HR.bmi.round <- format(round(results.df$HR.bmi,2), nsmall = 2 )
results.df$HR.low.bmi.round <- format(round(results.df$HR.low.bmi,2), nsmall = 2 )
results.df$HR.high.bmi.round <- format(round(results.df$HR.high.bmi,2), nsmall = 2)

results.df$HR[results.df$Strata == max(results.df$Strata)] <- NA  
results.df$HR.low[results.df$Strata == max(results.df$Strata)] <- NA 
results.df$HR.high[results.df$Strata == max(results.df$Strata)] <- NA 

results.df$HR.bmi[results.df$Strata == max(results.df$Strata)] <- NA  
results.df$HR.low.bmi[results.df$Strata == max(results.df$Strata)] <- NA 
results.df$HR.high.bmi[results.df$Strata == max(results.df$Strata)] <- NA 

results.df$HR.string <- 0
results.df$HR.bmi.string <- 0
results.df$HR.string <- paste0(as.character(results.df$HR.round), " (", as.character(results.df$HR.low.round),
"-", as.character(results.df$HR.high.round), ")" )
results.df$HR.string <- paste0(as.character(results.df$HR.round), " (", as.character(results.df$HR.low.round),
"-", as.character(results.df$HR.high.round), ")" )
results.df$HR.bmi.string <- paste0(as.character(results.df$HR.bmi.round), " (", as.character(results.df$HR.low.bmi.round),
"-", as.character(results.df$HR.high.bmi.round), ")" )
results.df$HR.string[is.na(results.df$HR)] <- ""
results.df$HR.bmi.string[is.na(results.df$HR)] <- ""

results.df$N.string <- 0
results.df$N.string <-prettyNum(results.df$Cases ,big.mark=",",scientific=FALSE)
results.df$N.string[is.na(results.df$HR)] <- ""
results.df$N.string2 <- 0
results.df$N.string2 <-prettyNum(results.df$Participants ,big.mark=",",scientific=FALSE)
results.df$N.string2[is.na(results.df$HR)] <- ""


results.df$HR.low <- pmax(results.df$HR.low, 0.3) 
results.df$HR.high <- pmin(results.df$HR.high, 1.7) 
results.df$HR.low.bmi <- pmax(results.df$HR.low.bmi, 0.3) 
results.df$HR.high.bmi <- pmin(results.df$HR.high.bmi, 1.7) 

results.df <- results.df[order(results.df$Strata),]
results.df <- results.df[order(results.df$New.Variable),]



###################################################################
# Prepare data for text that accompanies forest plot.
###################################################################
# forest_plot1_strata_content.csv
###################################################################


content.df <- read.csv("forest_plot1_strata_content.csv")
content.df$Content <- as.character(content.df$Content)
M <- max(results.df$Strata)
space.logic <- results.df$Strata != M & results.df$Variable != "overall"
new.vec <- paste0("       ",as.character(content.df$Content[space.logic]))
length(content.df$Content[space.logic])
length(new.vec)
I.vec <- 1:length(content.df$Content)
I.vec <- I.vec[space.logic]
for( i in I.vec){
content.df$Content[i] <- paste0("       ",as.character(content.df$Content[i]))
}

content.vec <- c(as.character(content.df$Content), results.df$N.string, results.df$N.string2)
content.vec[40] <- ""


##################################################################
# Prepare data frames for each part of forest plot:
# table1.df, forest1.df, forest2.df, table2.df, table3.df
##################################################################

N <- length(results.df$HR)

table1.df <- data.frame("group" = rep(0,3*N), "position" = rep(0,3*N), "content" = rep(0,3*N))
group.vec <- c( paste0("X","0",1:9),paste0("X",10:length(results.df$HR)))
table1.df$group <- rep(group.vec, 3)
table1.df$group <- factor(table1.df$group , levels = rev(group.vec))
table1.df$position <- rep(c(1,30,40), each= N)
table1.df$content <- content.vec 

forest1.df <- data.frame("group" = rep(0,N), "HR" =rep(0,N), "HR.low"= rep(0,N), "HR.high"= rep(0,N) )
forest1.df$group <- group.vec
forest1.df$group <- factor(forest1.df$group , levels = rev(group.vec))
forest1.df$HR <- 0
forest1.df$HR <- results.df$HR
forest1.df$HR.low <- results.df$HR.low
forest1.df$HR.high <- results.df$HR.high
forest1.df$shape.code <- 0
forest1.df$shape.code <- as.factor(forest1.df$shape.code)
forest1.df$size.code <- 0
forest1.df$size.code <- as.factor(forest1.df$size.code)

forest2.df <- data.frame("group" = rep(0,N), "HR" =rep(0,N), "HR.low"= rep(0,N), "HR.high"= rep(0,N) )
forest2.df$group <- group.vec
forest2.df$group <- factor(forest2.df$group , levels = rev(group.vec))
forest2.df$HR <- 0
forest2.df$HR <- results.df$HR.bmi
forest2.df$HR.low <- results.df$HR.low.bmi
forest2.df$HR.high <- results.df$HR.high.bmi
forest2.df$shape.code <- 0
forest2.df$shape.code <- as.factor(forest2.df$shape.code)
forest2.df$size.code <- 0
forest2.df$size.code <- as.factor(forest2.df$size.code)


table2.df <- data.frame("group" = rep(0,N), "position" = rep(0,N),"content"= rep(0,N) )
table2.df$group <- group.vec
table2.df$group <- factor(table2.df$group , levels = rev(group.vec))
table2.df$position <- 0
table2.df$position <- 1
table2.df$content <- 0
table2.df$content <- results.df$HR.string

table3.df <- data.frame("group" = rep(0,N), "position" = rep(0,N),"content"= rep(0,N) )
table3.df$group <- group.vec
table3.df$group <- factor(table3.df$group  , levels = rev(group.vec))
table3.df$position <- 0
table3.df$position <- 1
table3.df$content <- 0
table3.df$content <- results.df$HR.bmi.string


#####################################################################
# Set parameters that define proportions and margins for the plots
#####################################################################

text.size <- 3.4
table1.width <- 48
table2.start <- 0
table2.width <- 100

table2.df$position <- table2.start
table3.df$position <- table2.start

margin.zero <- c(0,0,0,0)
margin.vec <- c(0.2, 0, 0.2, 0)


######################################################
######################################################
# Create individual plots to later combine.
#####################################################
######################################################

######################################################
# Table1 plot
######################################################


table1.plot <- ggplot(data = table1.df, aes(x = group, y = position, label = content))+
theme_classic() +
theme(  panel.grid.major = element_blank(), 
          legend.position = "none",
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          panel.border = element_blank(), 
          axis.text.x = element_text(colour="white"),
          axis.text.y = element_blank(), 
          axis.ticks = element_line(colour="white"),
	   line = element_blank(),
	   plot.margin = unit(margin.vec, "cm"))+
 	   geom_text(size = text.size, hjust=0, vjust=0.5) +
          labs(x="", y = NULL)+ylim(c(0,table1.width ))+
	   scale_x_discrete(position = "top") +
coord_flip()


######################################################
# 	Forest1 plot
######################################################


forest1.plot <- ggplot(data = forest1.df, aes(x = group, y = HR, ymin = HR.low, ymax = HR.high, size = size.code, shape = shape.code))+
theme_classic() +
theme(strip.text = element_text(size = 9)  ,
        axis.line.y = element_blank(),
	 axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x=  element_text(size = 10),
	 axis.ticks = element_blank(),
	 legend.position = "none",
	 plot.margin = unit(margin.vec, "cm")) +
geom_hline(yintercept = c(0.2, 0.6, 1.0, 1.4, 1.8),
	 linetype = "solid",
	 col = c("gray85", "gray85","gray45", "gray85","gray85"),size = 0.5)+
geom_linerange(size = 0.8, col = "gray45")+
geom_point(stroke = 2, col = "gray45")+
scale_shape_manual(values = c(19))+
scale_size_manual(values = c(0.25))+
scale_x_discrete(name= NULL, position = "top") +
scale_y_continuous(name = "Hazard Ratio", breaks =  c(0.2, 0.6, 1.0, 1.4, 1.8), limits = c(0.1, 1.9))+
coord_flip()


######################################################
# 	Forest2 plot
######################################################


forest2.plot <- ggplot(data = forest2.df, aes(x = group, y = HR, ymin = HR.low, ymax = HR.high, size = size.code, shape = shape.code))+
theme_classic() +
theme(strip.text = element_text(size = 9)  ,
        axis.line.y = element_blank(),
	 axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.text.x=  element_text(size = 10),
	 axis.ticks = element_blank(),
	 legend.position = "none",
	 plot.margin = unit(margin.vec, "cm")) +
geom_hline(yintercept = c(0.2, 0.6, 1.0, 1.4, 1.8),
	 linetype = "solid",
	 col = c("gray85", "gray85","gray45", "gray85","gray85"),size = 0.5)+
geom_linerange(size = 0.8, col = "gray45")+
geom_point(stroke = 2, col = "gray45")+
scale_shape_manual(values = c(19))+
scale_size_manual(values = c(0.25))+
scale_x_discrete(name= NULL, position = "top") +
scale_y_continuous(name = "Hazard Ratio", breaks =  c(0.2, 0.6, 1.0, 1.4, 1.8), limits = c(0.1, 1.9))+
coord_flip()


######################################################
# Table2 plot
######################################################


table2.plot <- ggplot(data = table2.df, aes(x = group, y = position, label = content))+
theme_void() +
theme(panel.grid.major = element_blank(), 
          legend.position = "none",
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          panel.border = element_blank(), 
	   axis.line.x  = element_blank(), 
          axis.text.y = element_text(colour="white"),
          axis.text.x = element_blank(), 
          axis.ticks = element_line(colour="white"),
	   line = element_blank(),
	   plot.margin = unit(margin.vec , "cm"))+
	   geom_text(size = text.size, hjust=0, vjust=0.5)+
          labs(x=NULL,y="")+ylim(c(0,200))+
	   scale_x_discrete(position = "top") +
coord_flip()


######################################################
# Table3 plot
######################################################


table3.plot <- ggplot(data = table3.df, aes(x = group, y = position, label = content))+
theme_classic() +
theme(panel.grid.major = element_blank(), 
          legend.position = "none",
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          panel.border = element_blank(), 
	   axis.line.x  = element_blank(), 
          axis.text.y = element_text(colour="white"),
          axis.text.x = element_blank(), 
          axis.ticks = element_line(colour="white"),
	   line = element_blank(),
	   plot.margin = unit(margin.vec , "cm"))+
	   geom_text(size = text.size, hjust=0, vjust=0.5)+
          labs(x=NULL,y="")+ylim(c(0,200))+
	   scale_x_discrete(position = "top") +
coord_flip()



#################################################################
# Combine plots using Cowplot, and create pdf of forest plot
##################################################################



# define relative width proportions of each contributing figure.

a <- 0.34
b1 <- 0.16
b2 <- 0.17
c <- (1-a-b1-b2)/2

# create pdf

pdf(file = "Forest_plot.pdf", width = 10, height =8.5)
plot_grid(table1.plot , forest1.plot, table2.plot, forest2.plot, table3.plot, align = "h", ncol = 5, rel_widths=c(a,c,b1,c,b2))
dev.off()





