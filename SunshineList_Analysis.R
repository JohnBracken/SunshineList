#Analyze the salary data from the Ontario 2017 Sunshine List.
#Salaries of some government employees making over $100k per year
#are publicly disclosed.

#List of necessary libraries for data analysis.
library(dplyr)
library(plyr)
library(ggplot2)
library(caret)

#Ensure that salaries are listed to the full cent.
options(digits=10)

#Set working directory.
directory <- "C:/Users/310084562/My Documents/SunshineList_2017"
setwd(directory)

#Read in the sunshine list data and format each column accordingly.
sunshine_data <- read.csv("Ontario_SunshineList_2017.csv", header = TRUE)
sunshine_data$Last.Name <- as.character(sunshine_data$Last.Name)
sunshine_data$First.Name <- as.character(sunshine_data$First.Name)

#Convert monetary values to full numeric values, taking into account
#the dollar signs and commas.
sunshine_data$Salary.Paid <- as.character(sunshine_data$Salary.Paid)
sunshine_data$Salary.Paid <- gsub('[$,]','', sunshine_data$Salary.Paid)
sunshine_data$Salary.Paid <- as.numeric(sunshine_data$Salary.Paid)


sunshine_data$Taxable.Benefits <- as.character(sunshine_data$Taxable.Benefits)
sunshine_data$Taxable.Benefits <- gsub('[$,]','', sunshine_data$Taxable.Benefits)
sunshine_data$Taxable.Benefits <- as.numeric(sunshine_data$Taxable.Benefits)

#Make the year (2017) a factor variable instead of an integer.
sunshine_data$Calendar.Year <- as.factor(sunshine_data$Calendar.Year)

#As an example, filter the data to show only entries for employees
#making over $300,000 per year.
sub_data <- sunshine_data %>% filter(sunshine_data$Salary.Paid > 300000)


#Aggregate the salary data by specific government sectors, to assess
#which sectors are spending the most money on salaries.  Aggregate
#by the total amount each sector pays on employee salaries.
sector_data <- aggregate(sunshine_data$Salary.Paid~sunshine_data$Sector, 
                             sunshine_data,sum)
colnames(sector_data) <-c("Sector", "Total Cash Salaries($Cdn millions)")
sector_data <- arrange(sector_data,desc(`Total Cash Salaries($Cdn millions)`))
sector_data$`Total Cash Salaries($Cdn millions)` <- sector_data$`Total Cash Salaries($Cdn millions)`/1e6 


#Plot the top ten sectors that spent the most money on salaries in 2017, only accounting for disclosed
#employees making $100k per year or more.
plot_sectors <- ggplot(sector_data[1:10,], aes(reorder(Sector,-`Total Cash Salaries($Cdn millions)`),
                                        `Total Cash Salaries($Cdn millions)`)) +
  geom_bar(stat="identity", col = "black", fill = 'seagreen2') + 
  labs(title = "Total Cash for Salaries by Sector",x = "Sector",
       y= "Total Cash Salaries Paid ($Cdn millions)") + 
  theme(text = element_text(size=14),axis.text.x = element_text(angle = 60, hjust = 1))

print(plot_sectors)


#Now aggregate the salary data by the public employer (ie. branch of the government, university etc.)
employer_data <- aggregate(sunshine_data$Salary.Paid~sunshine_data$Employer, 
                         sunshine_data,sum)
colnames(employer_data) <-c("Employer", "Total Cash Salaries($Cdn millions)")
employer_data <- arrange(employer_data,desc(`Total Cash Salaries($Cdn millions)`))
employer_data$`Total Cash Salaries($Cdn millions)` <- employer_data$`Total Cash Salaries($Cdn millions)`/1e6 

#Shorten the names of some of the employers to make a cleaner plot.
levels(employer_data$Employer)[380] <- "Community Safety and Correctional Services"
levels(employer_data$Employer)[59] <- "Attorney General"

#Plot the top 15 public employers in Ontario in terms of total salary money spent in 2017.
plot_employers <- ggplot(employer_data[1:15,], aes(reorder(Employer,-`Total Cash Salaries($Cdn millions)`),
                                               `Total Cash Salaries($Cdn millions)`)) +
  geom_bar(stat="identity", col = "black", fill = 'sienna2') + 
  labs(title = "Total Cash for Salaries by Employer",x = "Employer",
       y= "Total Cash Salaries Paid ($Cdn millions)") + 
  theme(text = element_text(size=14),axis.text.x = element_text(angle = 60, hjust = 1))

print(plot_employers)




#Now aggregate the salary data by profession to determine which jobs are receiving the most
#public money, due to a large number of positions, a high salary per position, or combination
#of the two.
profession_data <- aggregate(sunshine_data$Salary.Paid~sunshine_data$Job.Title, 
                           sunshine_data,sum)
colnames(profession_data) <- c("Profession", "Total Cash Salaries($Cdn millions)")
profession_data <- arrange(profession_data,desc(`Total Cash Salaries($Cdn millions)`))
profession_data$`Total Cash Salaries($Cdn millions)` <- profession_data$`Total Cash Salaries($Cdn millions)`/1e6 
levels(profession_data$Profession)[12885] <- "Law Enforcement Officer"

#Plot the top 1o public professions to see which jobs salaries are getting the most public provincial cash. 
plot_profession <- ggplot(profession_data[1:10,], aes(reorder(Profession,-`Total Cash Salaries($Cdn millions)`),
                                                   `Total Cash Salaries($Cdn millions)`)) +
  geom_bar(stat="identity", col = "black", fill = 'hotpink1') + 
  labs(title = "Total Cash for Salaries by Profession",x = "Profession",
       y= "Total Cash Salaries Paid ($Cdn millions)") + 
  theme(text = element_text(size=14),axis.text.x = element_text(angle = 60, hjust = 1))

print(plot_profession)



#Do a linear regression model to see if there is relationship
#between salary paid and taxable benefits.  The results of the fit
#show that the correlation between salary and taxable benefits is
#very weak, possibly even non-existent.
fit_money <- lm(Salary.Paid~Taxable.Benefits,data=sunshine_data)
summary(fit_money)

#Plot the regression model fit line with the data.
plot(sunshine_data$Taxable.Benefits, sunshine_data$Salary.Paid,
     xlim = c(0, 1.2e5),
     ylim = c(0,2e6),
     main = "Salary Paid vs. Taxable Benefits ($Cdn)",
     xlab = "Taxable Benefits ($Cdn)",
     ylab = "Salary Paid ($Cdn)",bg = "orange",
     col = "black", cex = 1.1, pch = 21, frame = FALSE)
abline(fit_money, lwd =2)


#Create a plot of residuals for the fit.  The residuals are large
#So the agreement between the model and many data points is not
#great.
residuals <- resid(fit_money)
plot(sunshine_data$Taxable.Benefits, residuals, ylab="Residuals",
     xlab = "Taxable Benefits", main="Sunshine List Salaries 2017")
abline(0,0)









