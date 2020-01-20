startup_dataset <- read.csv(file.choose())

head(startup_dataset)

summary(startup_dataset)

library("ggplot2")

summary(startup_dataset$category_list)

startup_dataset1 <- NULL

startup_dataset1 <- startup_dataset[startup_dataset$category_list == "Education",  ]
startup_dataset1 <- startup_dataset[startup_dataset$category_list == "Entertainment" |startup_dataset$category_list == "Finance" |startup_dataset$category_list == "Health" |startup_dataset$category_list == "Manufacturing" |startup_dataset$category_list == "Marketing" |startup_dataset$category_list == "Mobile" |startup_dataset$category_list == "Services" |startup_dataset$category_list == "Software" |startup_dataset$category_list == "Technology", ]


basic_plot <- ggplot(data = startup_dataset1, aes(x = startup_dataset1$category_list))
sector_wise_companies <- basic_plot + geom_bar(aes(fill = startup_dataset1$category_list))
sector_wise_companies_final <- sector_wise_companies + xlab("Startup Sectors") + ylab("Number of Companies") + ggtitle("Number of companies by sectors")
sector_wise_companies_final <- sector_wise_companies_final +   theme(axis.title.x = element_text(colour = "Dark Green", size = 30),
                                                                     axis.title.y = element_text(colour = "Dark Green", size = 30),
                                                                     axis.text.x = element_text(size = 20),
                                                                     axis.text.y = element_text(size = 10),
                                                                     legend.title = element_text(size = 30),
                                                                     legend.text = element_text(size = 20),
                                                                     legend.justification = c(1,1),
                                                                     plot.title = element_text(colour = "DarkBlue", size = 40, family = "Courier", hjust = 0.5)
)

sector_wise_companies_final$labels$fill <- "Sectors"
sector_wise_companies_final

acad_dataset <- read.csv(file.choose(), na.strings = "")
head(acad_dataset)
summary(acad_dataset)
acad_dataset$Positions
#------------------------------------------------------
acad_dataset
colnames(acad_dataset) <- c("Positions", "X", "XII", "UnderGrad", "PostGrad", "PhD")
plot_academics <- function(i)
{
  new_df <- data.frame(acad_dataset[i, ])  
  new_df <- t(new_df)
  new_df <- new_df[-1, ]
  new_df <- new_df[complete.cases(new_df)]
  new_df <- data.frame(new_df)
  colnames(new_df) <- "info"
  res <- ggplot(new_df, aes(x = rownames(new_df), y = new_df$info)) + geom_col(width = 0.3, fill = "Sky Blue")
  res <- res + xlab("Standards") + ylab("CGPA") + ggtitle(acad_dataset[i,1])
  res <- res + theme(axis.title.x = element_text(colour = "DarkBlue", size = 30),
                    axis.title.y = element_text(colour = "DarkBlue", size = 30),
                    axis.text.x = element_text(size = 20),
                    axis.text.y = element_text(size = 10),
                    legend.title = element_text(size = 30),
                    legend.text = element_text(size = 20),
                    legend.justification = c(1,1),
                    plot.title = element_text(colour = "DarkBlue", size = 40, family = "Courier", hjust = 0.5)
  )

res
}

plot_academics(1)
plot_academics(2)
plot_academics(3)
plot_academics(4)
plot_academics(5)
plot_academics(6)
plot_academics(7)

install.packages("splitstackshape")
library(splitstackshape)
?cSplit
startup_dataset2 <- cSplit(startup_dataset, "Investors", "|", "long", drop = TRUE)
head(startup_dataset2)
startup_dataset2 <- cSplit(startup_dataset2, "Investors", ":", "wide", drop = TRUE)
head(startup_dataset2)

?group_by

library(dplyr)
investor_data1 <- group_by(startup_dataset2, Investors_1) %>% summarise(investor_data1 = sum(Investors_2))

colnames(investor_data1) <- c("CompanyName", "AmountInvested")
typeof(investor_data1$AmountInvested)

investor_data2 <- investor_data1[order(-investor_data1$AmountInvested), ]
investor_data2
top_investors <- investor_data2[1: 20, ]

top_investors_plot <- ggplot(data = top_investors, aes(x = top_investors$CompanyName, y = top_investors$AmountInvested)) + geom_bar(stat = "identity", fill = "SkyBlue") + coord_flip()
top_investors_plot <- top_investors_plot + xlab("Company Name") + ylab("Amount Invested(in $)") + ggtitle("Top investors")
top_investors_plot <- top_investors_plot + theme(axis.title.x = element_text(colour = "Red", size = 30),
                                               axis.title.y = element_text(colour = "Red", size = 30),
                                               axis.text.x = element_text(size = 20),
                                               axis.text.y = element_text(size = 10),
                                               legend.title = element_text(size = 30),
                                               legend.text = element_text(size = 20),
                                               legend.justification = c(1,1),
                                               plot.title = element_text(colour = "Red", size = 40, family = "Courier", hjust = 0.5)
)

top_investors_plot

#--------------------------------Ranking algorithm-------------------------------------
college_rank_dataset <- read.csv(file.choose()) 
head(college_rank_dataset)
nrow(college_rank_dataset)
college_rank_dataset1 <- college_rank_dataset[college_rank_dataset$year == 2015, ]
nrow(college_rank_dataset1)
college_rank_dataset1$score
mentor_dataset <- read.csv(file.choose())
summary(mentor_dataset)
str(mentor_dataset)
mentor_dataset$University <- as.character(mentor_dataset$University)
mentor_dataset$Names <- as.character(mentor_dataset$Names)

mentor_grade <- function(name)
{
  college_name <- mentor_dataset[mentor_dataset$Names == name, 2]
  ranking_score <- college_rank_dataset1[college_rank_dataset1$institution == college_name, 13]
  experience <- mentor_dataset[mentor_dataset$University == college_name, 3]
  grade <- 0.5*ranking_score + 1.67*experience
  return (grade)
}

resultant <- c(mentor_grade("Ervin Muric"),mentor_grade("Alexis Cuffolo"),mentor_grade("Maxime Jadot"),mentor_grade("Romain Pinte"),mentor_grade("Bastien Gallaire"),mentor_grade("Lucas Sotteau"),mentor_grade("Bertrand Billi"),mentor_grade("Tom Herenger"),mentor_grade("Gillian Benoy"),mentor_grade("Tom Cocqu"))
mentor_dataset$points <- resultant
mentor_dataset


ranked_mentor_dataset <- mentor_dataset[order(-mentor_dataset$points), ]
ranked_mentor_dataset
