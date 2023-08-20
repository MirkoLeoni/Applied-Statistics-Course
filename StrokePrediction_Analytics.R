# Libraries and Data
library(tidyverse)
library(tidymodels)
library(caret)
library(data.table)
library(ggplot2)
library(wesanderson)
library(ggridges)
library(ggpubr)
library(kableExtra)
library(corrplot)
library(modelsummary)

# Seed
set.seed(1234)

setwd("C:/Users/Mirko/Desktop/HealthCare_Project")

df_raw = read_csv("healthcare-dataset-stroke-data.csv")

## DF MANAGEMENT ----

# Helper functions
make_NA = function(x){ifelse(x == "N/A", NA, x)}
work_type_fix = function(x){
  
  x = ifelse(x == "children", 1, x)
  x = ifelse(x == "Govt_job", 2, x)
  x = ifelse(x == "Never_worked", 3, x)
  x = ifelse(x == "Private", 4, x)
  x = ifelse(x == "Self-employed", 5, x)
  
  x
}
smoking_status_fix = function(x){
  
  x = ifelse(x == "never smoked", 1, x)
  x = ifelse(x == "formerly smoked", 2, x)
  x = ifelse(x == "smokes", 3, x)
  x = ifelse(x == "Unknown", 4, x)
  
  x
}

# Cleaning ----
df = df_raw %>% 
  select(-id) %>% 
  filter(gender != "Other") %>% 
  
  # Consistent capitalization among features
  rename(Stroke = stroke) %>%
  rename(Gender = gender) %>%
  rename(Hypertension = hypertension) %>%
  rename(Heart_disease = heart_disease) %>%
  rename(Ever_married = ever_married) %>%
  rename(Work_type = work_type) %>%
  rename(Smoking_status = smoking_status) %>%
  rename(Age = age) %>%
  rename(Avg_glucose_level = avg_glucose_level) %>%
  rename(BMI = bmi) %>%
  
  # Factorize categorical vars with labels
  mutate(Stroke = factor(Stroke,
                         levels = c(0,1),
                         labels = c("No", "Yes"))) %>%
  mutate(Gender = ifelse(Gender == "Female", 0, 1)) %>%
  mutate(Gender = factor(Gender,
                         levels = c(0,1),
                         labels = c("Female", "Male"))) %>%
  mutate(Hypertension = factor(Hypertension,
                               levels = c(0,1),
                               labels = c("No", "Yes"))) %>%
  mutate(Heart_disease = factor(Heart_disease,
                                levels = c(0,1),
                                labels = c("No", "Yes"))) %>%
  mutate(Ever_married = ifelse(Ever_married == "No", 0, 1)) %>%
  mutate(Ever_married = factor(Ever_married,
                               levels = c(0,1),
                               labels = c("No", "Yes"))) %>%
  mutate(Work_type = work_type_fix(Work_type)) %>%
  mutate(Work_type = factor(Work_type,
                            levels = c(1,2,3,4,5),
                            labels = c("Children",
                                       "Government",
                                       "Never Worked",
                                       "Private",
                                       "Self-employed"))) %>%
  mutate(Residence_type = ifelse(Residence_type == "Rural", 0, 1)) %>%
  mutate(Residence_type = factor(Residence_type,
                                 levels = c(0,1),
                                 labels = c("Rural", "Urban"))) %>%
  mutate(Smoking_status = smoking_status_fix(Smoking_status)) %>%
  mutate(Smoking_status = factor(Smoking_status,
                                 levels = c(1,2,3,4),
                                 labels = c("Never Smoked",
                                            "Former Smoker",
                                            "Smokes",
                                            "Unknown"))) %>%
  mutate(BMI =  make_NA(BMI)) %>%
  mutate(BMI = as.numeric(BMI))

# Splits and folds ----
train_ind = as.vector(createDataPartition(y = df$Stroke, times = 1, p = 0.8)) 

df_train = df#[train_ind[[1]], ]
df_test = df[-train_ind[[1]], ]

numfolds = 10
folds = createFolds(df_train$Stroke, k = numfolds)


##EDA ----
# Categorical feature table ----
df_train_categorical = df_train %>% select(c(Gender,
                                              Hypertension,
                                              Heart_disease,
                                              Ever_married,
                                              Work_type,
                                              Residence_type,
                                              Smoking_status))

datasummary_skim(data = df_train_categorical %>%
                   rename(`Heart Disease` = Heart_disease) %>%
                   rename(`Ever Married` = Ever_married) %>%
                   rename(`Work Type` = Work_type) %>%
                   rename(`Residence Type` = Residence_type) %>%
                   rename(`Smoking Status` = Smoking_status),
                 type = "categorical") %>%
  kable_classic()

# Numeric Feature table ----
n <- function(x){length(na.omit(x))}
emptycol = function(x){" "} 

plt_list <- df_train %>% select(c(Age,
                                  Avg_glucose_level,
                                  BMI))

plt_list <- lapply(plt_list, na.omit)
plt_list <- lapply(plt_list, scale)


datasummary(Age + 
              `Average Glucose Level` +
              BMI ~ 
              n + 
              Mean*Arguments(na.rm = TRUE) + 
              Median*Arguments(na.rm = TRUE) + 
              SD*Arguments(na.rm = TRUE) +
              Heading("Boxplot") * emptycol +
              Heading("Histogram") * emptycol, 
            data = df_train %>%
              rename(`Average Glucose Level` = Avg_glucose_level), 
            title = "Characteristics") %>%
  column_spec(column = 6, image = spec_boxplot(plt_list)) %>%
  column_spec(column = 7, image = spec_hist(plt_list)) %>%
  kable_classic()

# Numeric Feature Table by Stroke Status
datasummary((Age + 
               `Average Glucose Level` +
               BMI)*Stroke ~ 
              n + 
              Mean*Arguments(na.rm = TRUE) + 
              Median*Arguments(na.rm = TRUE) + 
              SD*Arguments(na.rm = TRUE),
            data = df_train %>%
              rename(`Average Glucose Level` = Avg_glucose_level), 
            title = "Characteristics by Stroke Status") %>%
  kable_classic()

# Stroke distribution ----

Stroke_plt <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Stroke, fill = Stroke), position = "dodge") +
  theme_minimal() + 
  scale_fill_manual(values = c(wes_palette("Moonrise1")[2], wes_palette("Moonrise1")[5])) +
  theme(plot.title = element_text(size = 15, face = "bold")) +
  labs(y = "", title = "Stroke Distribution")

Stroke_plt

# Numerical variables distributions ----

# Age
Age_plt1 <- df_train %>%
  ggplot() + 
  geom_histogram(data = df_train[df_train$Gender == "Male",], 
                 aes(x = Age), 
                 binwidth = , 
                 fill = wes_palette("Darjeeling1")[5], color = "black") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 15, face = "bold")) +
  labs(y = "Count", x = "Age (years)", title = "Age Distribution (male)")

Age_plt2 <- df_train %>%
  ggplot() +
  geom_density_ridges_gradient(aes(x = Age, y = Stroke, fill = Stroke), scale = 2, rel_min_height = 0.001) +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[3], wes_palette("Darjeeling1")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20))+
  labs(x = "Age (years)", y = "Stroke" , title = "")

Age_plt3 <- df_train %>% 
  ggplot() +
  geom_boxplot(aes(x = Stroke, y = Age, fill = Stroke)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[3], wes_palette("Darjeeling1")[5])) +
  labs(x = "Stroke", y = "Age (years)", title = "")

ggarrange(Age_plt3, Age_plt2,
          ncol = 2,
          nrow = 1)

# Average glucose level 
Avg_glucose_level_plt1 <- df_train %>%
  ggplot() + 
  geom_histogram(data = df_train, 
                 aes(x = Avg_glucose_level), 
                 binwidth = , 
                 fill = wes_palette("IsleofDogs1")[1], color = "gray") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 15, face = "bold")) +
  labs(y = "Count", x = "Average Glucose Level", title = "Average Glucose Level Distribution")

Avg_glucose_level_plt2 <- df_train %>%
  ggplot() +
  geom_density_ridges_gradient(aes(x = Avg_glucose_level, y = Stroke, fill = Stroke), scale = 2, rel_min_height = 0.001) +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("Darjeeling1")[4])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20))+
  labs(x = "", y = "Stroke" , title = "")

Avg_glucose_level_plt3 <- df_train %>% 
  ggplot() +
  geom_boxplot(aes(x = Stroke, y = Avg_glucose_level, fill = Stroke)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("Darjeeling1")[4])) +
  labs(x = "Stroke", y = "Average Glucose Level", title = "")

ggarrange( Avg_glucose_level_plt2, Avg_glucose_level_plt3,
          ncol = 1,
          nrow = 2)

# BMI
BMI_plt1 <- df_train %>%
  ggplot() + 
  geom_histogram(data = df_train, 
                 aes(x = BMI), 
                 binwidth = , 
                 fill = wes_palette("IsleofDogs1")[1], color = "gray") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 15, face = "bold")) +
  labs(y = "Count", x = "BMI", title = "BMI Distribution")

BMI_plt2 <- df_train %>%
  ggplot() +
  geom_density_ridges_gradient(aes(x = BMI, y = Stroke, fill = Stroke), scale = 2, rel_min_height = 0.001) +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[3], wes_palette("Darjeeling1")[5])) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20))+
  labs(x = "", y = "Stroke" , title = "")

BMI_plt3 <- df_train %>% 
  ggplot() +
  geom_boxplot(aes(x = Stroke, y = BMI, fill = Stroke)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20))+
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[3], wes_palette("Darjeeling1")[5])) +
  labs(x = "Stroke", y = "BMI", title = "")

ggarrange( BMI_plt2, BMI_plt3,
          ncol = 1,
          nrow = 2)

# Categorical features plots ----

# Gender
Gender_plt1 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Gender, fill = Gender), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[3], wes_palette("Darjeeling1")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Gender", title = "Gender Distribution")


Gender_plt2 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Gender, fill = Stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion", x = "Gender", title = "Gender Distribution by Stroke Status")

# Hypertension
Hypertension_plt1 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Hypertension, fill = Hypertension), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[2], wes_palette("GrandBudapest2")[3])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Hypertension", title = "Hypertension Distribution")



Hypertension_plt2 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Hypertension, fill = Stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",  x = "Hypertension", title = "Hypertension Distribution by Stroke Status")


# Heart Disease
Heart_disease_plt1 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Heart_disease, fill = Heart_disease), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[2], wes_palette("GrandBudapest2")[3])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Heart Dsiease", title = "Heart Disease Distribution")


Heart_disease_plt2 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Heart_disease, fill = Stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",  x = "Heart Disease", title = "Heart Disease Distribution by Stroke Status")


# Ever married
Ever_married_plt1 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Ever_married, fill = Ever_married), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("GrandBudapest2")[2], wes_palette("GrandBudapest2")[3])) +
  theme_minimal() + 
  theme(plot.title = element_text( face = "bold"),
        legend.position = "None") +
  labs(y = "Count", x = "Ever Married", title = "Ever Married Distribution")

Ever_married_plt2 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Ever_married, fill = Stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("FantasticFox1")[4], wes_palette("FantasticFox1")[5])) +
  theme_transparent() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Proportion",  x = "Married", title = "Marital status Distribution by Stroke Status")+
  coord_flip()


# Work Type: collapse into two cateogies: Child/never worked and all others
Work_type_plt1 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Work_type, fill = Work_type), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[1], 
                               wes_palette("Darjeeling1")[2],
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[4],
                               wes_palette("Darjeeling1")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Count", x = "Work Type", title = "Work Type Distribution")

Work_type_plt2 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Work_type, fill = Stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], wes_palette("Darjeeling1")[1])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion", x = "Work Type", title = "Work Type Distribution by Stroke Status")


# Residence type
Residence_type_plt1 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Residence_type, fill = Residence_type), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[3], wes_palette("Darjeeling1")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20))  +
  labs(y = "Count", x = "Residence Type", title = "Residence Type Distribution")+
  coord_flip()

Residence_type_plt2 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Residence_type, fill = Stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",  x = "Residence Type", title = "Residence Type Distribution by Stroke Status")





# Smoking Status
Smoking_status_plt1 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Smoking_status, fill = Smoking_status), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], 
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[1],
                               wes_palette("Darjeeling1")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Count", x = "Smoking Status", title = "Smoking Status Distribution")

Smoking_status_plt2 <- df_train %>%
  ggplot() +
  geom_bar(aes(x = Smoking_status, fill = Stroke), position = "fill") + 
  scale_fill_manual(values = c(wes_palette("Royal2")[2], wes_palette("Royal2")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold")) +
  labs(y = "Proportion",  x = "Smoking Status", title = "Smoking Status Distribution by Stroke Status")

auxxxx = df_train %>% filter(Stroke == "Yes")

df_train %>% filter(Stroke == "Yes", Smoking_status == "F") %>%
  ggplot() +
  geom_bar(aes(x = Smoking_status, fill = Smoking_status), position = "dodge") + 
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], 
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[1],
                               wes_palette("Darjeeling1")[5])) +
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Count", x = "Smoking Status", title = "Smoking Status Distribution")

df_smokers = df_train %>% filter(Smoking_status == "Smokes")
perc_smokers = nrow(df_smokers[df_smokers$Stroke == "Yes",])/nrow(df_smokers)

df_former_smokers = df_train %>% filter(Smoking_status == "Former Smoker")
perc_former_smokers=nrow(df_former_smokers[df_former_smokers$Stroke == "Yes",])/nrow(df_former_smokers)

df_no_smokers = df_train %>% filter(Smoking_status == "Never Smoked")
perc_no_smokers =nrow(df_no_smokers[df_no_smokers$Stroke == "Yes",])/nrow(df_no_smokers)

df_unknown = df_train %>% filter(Smoking_status == "Unknown")
perc_unknown = nrow(df_unknown[df_unknown$Stroke == "Yes",])/nrow(df_unknown)

perc = c(perc_unknown, perc_no_smokers, perc_former_smokers, perc_smokers)

incidence = data.frame(Smoking_status = factor(c("Unknown","Never Smoked","Former Smoker", "Smokes")), perc = perc) %>% arrange(perc)


ggplot(incidence, aes(x=reorder(Smoking_status, -perc), y=perc, fill = Smoking_status)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], 
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[1],
                               wes_palette("Darjeeling1")[5])) +
  geom_text(aes(label = paste(round(perc*100, digits = 2), "%")), vjust = -0.2, size = 5,
            position = position_dodge(0.9))+
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Percentage", x = "Smoking Status", title = "Stroke Distribution per Smoking Status")


ggarrange(Gender_plt1,
          Hypertension_plt1,
          Heart_disease_plt1,
          Ever_married_plt1,
          Work_type_plt1,
          Residence_type_plt1,
          Smoking_status_plt1,
          nrow  = 4,
          ncol = 2)


## UNBALANCED DATA ADJUSTMENTS ----

library(ROSE)

over_df_train = ovun.sample(Stroke ~., 
                            data = df_train,
                            method = "over",
                            N = nrow(df_train[df_train$Stroke == "No", ])*2)$data

under_df_train = ovun.sample(Stroke ~., 
                            data = df_train,
                            method = "under",
                            N = nrow(df_train[df_train$Stroke == "Yes", ])*2)$data

both_df_train = ovun.sample(Stroke ~., 
                            data = df_train,
                            method = "both",
                            seed = 123,
                            N = nrow(df_train))$data

ROSE_df_train = ROSE(Stroke ~., 
                            data = df_train,
                            seed = 123,
                            N = nrow(df_train))$data

table(over_df_train$Stroke)
table(under_df_train$Stroke)
table(both_df_train$Stroke)
table(ROSE_df_train$Stroke) 
summary(ROSE_df_train)#-> NEGATIVE AGE AND NEGATIVE AVG_GLUCOSE_LEV !!!!
summary(ROSE_df_train[ROSE_df_train$Work_type == "Children",]) #-> CHILDREN WT 31 YEARS?


ggplot() + 
  geom_histogram(data = filter(df_train, Smoking_status == "Former Smoker"), 
                 aes(x = Age), 
                 binwidth = , 
                 fill = wes_palette("Darjeeling1")[5], color = "white") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 15, face = "bold")) +
  labs(y = "Count", x = "Age", title = "Age Distribution (Unknown smoking status)")

pl02 = filter(df_train, Smoking_status == "Unknown") %>% 
  ggplot() +
  geom_boxplot(aes(x = Smoking_status, y = Age, fill = Smoking_status)) +
  theme_minimal() +
  scale_fill_manual(values =  wes_palette("Darjeeling1")[5]) +
  labs(x = "Stroke", y = "Age (years)", title = "")+
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 0), 
        axis.title = element_text(size = 20))



ggarrange(pl02, pl01, ncol=2, nrow=1)

aux001 = nrow(filter(df, Residence_type == "Urban", Stroke == "Yes"))/nrow(filter(df, Residence_type == "Urban"))
aux002 = nrow(filter(df, Residence_type == "Rural", Stroke == "Yes"))/nrow(filter(df, Residence_type == "Rural"))

incidence_restype = data.frame(Smoking_status = factor(c("Urban","Rural")), perc = c(aux001,aux002))

ggplot(incidence_restype, aes(x=Smoking_status, y=perc, fill = Smoking_status)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], 
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[1],
                               wes_palette("Darjeeling1")[5])) +
  geom_text(aes(label = paste(round(perc*100, digits = 2), "%")), vjust = -0.2, size = 5,
            position = position_dodge(0.9))+
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Percentage", x = "Residence Type", title = "Stroke Distribution per Residence Type")



aux01 = c( nrow(filter(df, Work_type == "Never Worked", Stroke == "Yes"))/nrow(filter(df, Work_type == "Never Worked")),
           nrow(filter(df, Work_type == "Government", Stroke == "Yes"))/nrow(filter(df, Work_type == "Government")),
           nrow(filter(df, Work_type == "Private", Stroke == "Yes"))/nrow(filter(df, Work_type == "Private")),
           nrow(filter(df, Work_type == "Self-employed", Stroke == "Yes"))/nrow(filter(df, Work_type == "Self-employed")) )

auxnames = c("Never Worked","Government", "Private","Self-employed")

df_work = data.frame(Smoking_status = auxnames, perc = aux01)

ggplot(df_work, aes(x=reorder(Smoking_status, -perc), y=perc, fill = Smoking_status)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[2], 
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[5])) +
  geom_text(aes(label = paste(round(perc*100, digits = 2), "%")), vjust = -0.2, size = 5,
            position = position_dodge(0.9))+
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Percentage", x = "Residence Type", title = "Stroke Distribution per Residence Type")



df_hdis = data.frame(Smoking_status = c("Yes", "No"), perc = c( nrow(filter(df, Heart_disease == "Yes", Stroke == "Yes"))/nrow(filter(df, Heart_disease == "Yes")),
                                                                nrow(filter(df, Heart_disease == "Yes", Stroke == "No"))/nrow(filter(df, Heart_disease == "No"))))

ggplot(df_hdis, aes(x=reorder(Smoking_status, -perc), y=perc, fill = Smoking_status)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[1], 
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[5])) +
  geom_text(aes(label = paste(round(perc*100, digits = 2), "%")), vjust = -0.2, size = 5,
            position = position_dodge(0.9))+
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Percentage", x = "Heart Disease", title = "Stroke Distribution per Heart Disease")



df_hiper = data.frame(Smoking_status = c("Yes", "No"), perc = c( nrow(filter(df, Hypertension == "Yes", Stroke == "Yes"))/nrow(filter(df, Hypertension == "Yes")),
                                                                nrow(filter(df, Hypertension == "Yes", Stroke == "No"))/nrow(filter(df, Hypertension == "No"))))

ggplot(df_hiper, aes(x=reorder(Smoking_status, -perc), y=perc, fill = Smoking_status)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[4], 
                               wes_palette("Darjeeling1")[5],
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[5])) +
  geom_text(aes(label = paste(round(perc*100, digits = 2), "%")), vjust = -0.2, size = 5,
            position = position_dodge(0.9))+
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Percentage", x = "Hypertension", title = "Stroke Distribution per Hypertension")

for(i in 1:nrow(df)){
  j = 0
  j = ifelse(df[i, "BMI"] >= 30, j+1, j+0)
  j = ifelse(df[i, "Hypertension"] == "Yes", j+1, j+0)
  j = ifelse(df[i, "Avg_glucose_level"] >= 150, j+1, j+0)
  df[i, "Risk_Factor"] = j
}
df$Risk_Factor = factor(df$Risk_Factor)

aux000 = table(df[df$Stroke == "Yes", "Risk_Factor"])/table(df$Risk_Factor)
df_risk_factor = data.frame(risk = names(aux000)[1:4], perc = c(aux000[[1]], aux000[[2]], aux000[[3]], aux000[[4]]))

ggplot(df_risk_factor, aes(x= risk, y=perc, fill = risk)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[4], 
                               wes_palette("Darjeeling1")[5],
                               wes_palette("Darjeeling1")[3],
                               wes_palette("Darjeeling1")[2])) +
  geom_text(aes(label = paste(round(perc*100, digits = 2), "%")), vjust = -0.2, size = 5,
            position = position_dodge(0.9))+
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold", size = 25),
        legend.position = "None", axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20)) +
  labs(y = "Stroke incidence", x = "Risk Factors", title = "Stroke Distribution per Risk Factors")

library(tree)
set.seed(1234)
df_tree = data.frame(Risk_Factor = df$Risk_Factor,
                     BMI = df$BMI,
                     GlucoseLevel = df$Avg_glucose_level,
                     Hypertension = df$Hypertension)
tree_risk = tree(Risk_Factor ~ Hypertension + BMI + GlucoseLevel, data = df_tree)

par(pty = "m")
plot(tree_risk); text(tree_risk, col = wes_palette("Darjeeling1")[1])


library(randomForest)


df_RF = na.omit(df)
rf_BMI = randomForest(BMI ~., data = df_RF)

plot(rf_BMI)

NAs = which(is.na(df$BMI) == T)

BMI_sub = predict(rf_BMI, newdata = df[NAs,])
num = unlist(lapply(df, is.numeric))





