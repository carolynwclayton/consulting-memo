### Consulting II Homework 1
### Carolyn Clayton

## Inputting the data

Alfuzosin <- c(1, 1, 2, 2, 3, 1, 0, 3, 3, 2, 5, 2, 1, 3, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 1, 5, 1, 2, 3, 1, 2, 5, 1, 5, 3, 3, 3, 2, 5, 2, 3, 4, 3, 4, 5, 3, 4, 3, 2, 3, 5, 2, 2, 3)
AfterAlfuzosin <- c(10, 7, 4, 4, 3, 5, 6, 7, 7, 3, 5, 2, 4, 4, 6, 3, 6, 2, 5, 3, 5, 2, 4, 2, 3, 6, 4, 5, 4, 7, 4, 3, 3, 4, 2, 4, 4, 3)
Drug <- vector()
i <- 1
repeat{ if(i <= length(Alfuzosin))
  {Drug[i] = 0}
    else if(i <= length(Alfuzosin) + length(AfterAlfuzosin))
  {Drug[i] = 1}
  i <- i+1
  if(i > length(Alfuzosin) + length(AfterAlfuzosin)) break()
}
Drug <- factor(Drug, 
               levels = c(0, 1), 
               labels = c("Alfuzosin", "After Alfuzosin"))
Urination <- c(Alfuzosin, AfterAlfuzosin)
UrinationData <- data.frame(Urination, Drug)

## Analysis

# Descriptives

library(ggplot2)
p <- ggplot(UrinationData, aes(x = Drug, y = Urination, fill = Drug)) +
  geom_boxplot(alpha = 0.5) + 
  stat_summary(fun.y = mean, geom = "point", shape = 5) +
  guides(fill = FALSE) +
  ggtitle("Boxplot of Nightly Urinations by Drug Status")
ggsave("C://Users//Carolyn//Dropbox//ASchool//! Consulting II//Homework 1//homework 1 boxplot.png")
dev.off() 

sink("C://Users//Carolyn//Dropbox//ASchool//! Consulting II//Homework 1//output.txt")

library(psych)
describeBy(UrinationData$Urination, group = Drug)

# Tests

var.test(UrinationData$Urination ~ UrinationData$Drug)
t.test(UrinationData$Urination ~ UrinationData$Drug, alternative = "two.sided", paired = F, conf.level = 0.95, var.equal = FALSE)

sink()