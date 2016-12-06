library(tidyverse)
library(apaTables)

bfi_data <- psych::bfi

## CREATE ANALYTIC DATA

# create categorical variables...
categorical_variables <- select(bfi_data, gender)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)

# create sets of scale items...
age <- select(bfi_data, age)
education <- select(bfi_data, education)
agreeableness <- select(bfi_data, A1, A2, A3, A4, A5)
extraversion <- select(bfi_data, E1, E2, E3, E4, E5)
neuroticism <- select(bfi_data, N1, N2, N3, N4, N5)

# flip any reverse-key items...
agreeableness <- mutate(agreeableness, A1=7-A1)
extraversion <- mutate(extraversion, E1=7-E1)
extraversion <- mutate(extraversion, E2=7-E2)

# obtain scale scores...
agreeableness <- psych::alpha(as.data.frame(agreeableness), check.keys = FALSE)$scores
extraversion <- psych::alpha(as.data.frame(extraversion), check.keys = FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuroticism), check.keys = FALSE)$scores

# combine everything into analytic_data...
analytic_data <- cbind(agreeableness, extraversion, neuroticism, categorical_variables, education, age)
write_csv(analytic_data, path = "analytic_data.csv")


## CONDUCT ANALYSES

# 1 - apa cor table excluding gender...
analytic_data_no_gender <- analytic_data %>% select(-gender)
apa.cor.table(analytic_data_no_gender, filename = "Table1.doc", table.number = 1)

# 2 - apa cor table for men over 40...
analytic_data_men_over_40 <- filter(analytic_data, gender=="Male" & age > 40)
apa.cor.table(analytic_data_men_over_40, filename = "Table2.doc", table.number = 2)

# 3 - create a scatterplot of relation b/w agreeableness & extraversion for men over 40...
my.plot <- qplot(agreeableness, extraversion, data = analytic_data_men_over_40)
my.plot <- my.plot + theme_classic(14)
my.plot <- my.plot + theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'), axis.line.y = element_line(colour = 'black', size=0.5, linetype = 'solid'))
my.plot <- my.plot + labs(title="", x="Agreeableness", y="Extraversion")
print(my.plot)
ggsave("Figure1.pdf", plot=my.plot, width = 6, height = 6)

# Based on our data, there appears to be a positive correlation between agreeableness and extraversion for men over the age of 40, r = .48, 95% CI [.33, .61].  