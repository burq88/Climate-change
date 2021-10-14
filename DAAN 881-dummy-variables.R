library(readxl)
project_data <- read_excel("20210312_Project_Data_Draft.xlsx")
head(project_data)
colnames(project_data)

library(fastDummies)

#converting state and commodity into factors

project_data$State_factor <- as.numeric(factor(project_data$State))
project_data$Commodity_factor <- as.numeric(factor(project_data$Commodity))
head(project_data)
str(project_data)

#converting commodity into dummy variables

library(openxlsx)
project_data_1 <- fastDummies::dummy_cols(project_data, select_columns = "Commodity", remove_first_dummy = TRUE)
str(project_data_1)

write.xlsx(project_data_1, file = "20210323_Project_Data_w_dummies.xlsx", asTable = FALSE)
