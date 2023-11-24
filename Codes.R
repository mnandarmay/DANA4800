heart <- read.csv("heart_disease_uci.csv", header = T)

heart <- heart[,c("age", "sex", "dataset", "cp", "chol", "thalch", "exang")]
heart$sex <- as.factor(heart$sex)
heart$dataset <- as.factor(heart$dataset)
heart$cp <- as.factor(heart$cp)
heart$exang <- as.factor(heart$exang)

library(dplyr)

# Combine Cleveland and VA Long Beach into USA
heart <- heart %>%
  mutate(dataset = case_when(
    dataset %in% c("Cleveland", "VA Long Beach") ~ "USA",
    TRUE ~ dataset
  ))

head(heart)

# Q1: Describe the distribution of chest pain types (cp) in patients with potential heart disease.
cp_freq <- table(heart$cp)
cp_rel_freq <- cp_freq/sum(cp_freq) * 100

#barplot(cp_rel_freq, ylim = c(0, 60), xlab = "Chest Pain Types", ylab = "Percentages",
#        main = "Frequency of Chest Pain Types in People with Potential Heart Disease")

# Order by descending
categories <- c("Asymptomatic", "Non-angina", "Atypical angina", "Typical angina")
values <- c(53.91304, 22.17391, 18.91304, 5.00000)

# Create a bar plot
barplot(values, names.arg = categories, col = "skyblue", ylim = c(0, 60),
        main = "Chest Pain Types Distribution",
        xlab = "Chest Pain Types", ylab = "Percentage")

# Q2: Describe the distribution of cholesterol level (chol) in patients with potential heart disease.
heart$chol[is.na(heart$chol)] <- mean(heart$chol, na.rm = T) # mean replace missing values

hist(heart$chol, xlab = "Choloesterol Levels (mg/dL)", main = "Distribution of Cholesterol Levels", col = "plum")
boxplot(heart$chol, horizontal = T, xlab = "Cholesterol Levels (mg/dL)",
        col = "plum", main = "Cholesterol Levels Distribution")

summary(heart$chol)

# Q3: Is there a correlation between age and maximum heart rate (thalch) in patients?
heart$thalch[is.na(heart$thalch)] <- mean(heart$thalch, na.rm = T) # mean replace missing values

# Scatterplot and correlation analysis
plot(heart$age, heart$thalch, ylim = c(25, 250), xlim = c(25, 80),
     xlab = "Age", ylab = "Max Heart Rate", main = "Max Heart Rate by Age")
cor(heart$thalch, heart$age)

# regression analysis
age_thalch_ls <- lm(heart$thalch ~ heart$age)
summary(age_thalch_ls)

# Q4: Is there a relationship between cholesterol levels (chol) and geographic regions (origin)?
boxplot(heart$chol ~ heart$dataset, xlab = "Geographical Region", ylab = "Cholesterol Levels (mg/dL)",
        col = "mediumpurple2", main = "Cholesterol Distribution in Different Geographical Regions")

# Q5: Are sex and presence of exercise induced angina (exang) independent of each other?
angina_freq <- table(heart$sex, heart$exang) # two-way table
angina_freq

barplot(angina_freq, legend.text = c("Female", "Male"), col = c("pink", "lightblue"),
        args.legend = list(x = 'topright'), main = "Sex vs. Exercise Induced Angina",
        ylab = "Count", xlab = "Exercise Induced Angina", beside = T)

chisq.test(angina_freq) #X2 analysis
