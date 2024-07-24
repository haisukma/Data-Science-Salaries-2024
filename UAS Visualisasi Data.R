# Importing libraries
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(treemap)

# Import CSV File
Data_Salary <- read_csv('C:\\Users\\diaje\\Downloads\\Dataset salary 2024.csv')
# Looking at Column names
colnames(Data_Salary)
summary(Data_Salary)
# Inspect for Duplicates
Data_Duplicated <- Data_Salary[duplicated(Data_Salary), ]
# Drop Duplicates
Clean_DataSalary <- Data_Salary[!duplicated(Data_Salary), ]

# Find Missing Values
jumlah_missing <- Data_Salary %>%
  group_by(is.na(colnames)) %>%
  summarise(n = n()) %>%
  filter(is.na(colnames) == TRUE) %>%
  pull(n)

print(paste0("Jumlah missing value di kolom '", colnames(Data_Salary)[1:11], "': ", jumlah_missing))

# Plot Salary in USD by Employment Type
ggplot(Clean_DataSalary, aes(x = employment_type, y = sum(salary_in_usd))) +
  geom_col(fill = "orange") + 
  labs(title = "Salary in USD by Employment Type", 
       x = "Employment Type", # Label sumbu x
       y = "Sum Salary")+
  theme(
    plot.background = element_rect(fill = "lightblue"),  # Change background color
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# Plot Salary in USD by Company Location
data_summary <- Clean_DataSalary %>%
  group_by(company_location) %>%
  summarise(total_salary_usd = sum(salary_in_usd)) %>%
  arrange(desc(total_salary_usd)) %>%
  head(5)

ggplot(data_summary, aes(x = reorder(company_location, total_salary_usd), y = total_salary_usd)) +
  geom_col(fill = "orange") +
  labs(title = "Top 5 Salary in USD by Company Location",
       x = "Company Location",
       y = "Sum Salary") +
  coord_flip()+
  theme(
    plot.background = element_rect(fill = "lightblue"),  # Change background color
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# Unique Company
unique_company_location <- unique(Data_Salary$company_location)
print(unique_company_location)
# Unique Job Title
unique_job_title <- unique(Data_Salary$job_title)
print(unique_job_title)
# Unique Company Size
unique_company_size <- unique(Data_Salary$company_size)
print(unique_company_size)

Gaji <- Clean_DataSalary %>% 
  summarise(
    Gaji_RataRata = mean(salary_in_usd),
    Gaji_Minimum = min(salary_in_usd),
    Gaji_Maksimum = max(salary_in_usd)
  )

Gaji

avg_salary_by_job <- Clean_DataSalary %>%
  group_by(job_title) %>%
  summarise(avg_salary = mean(salary_in_usd)) %>%
  arrange(desc(avg_salary)) %>%
  head(10)

ggplot(avg_salary_by_job, aes(x = reorder(job_title, -avg_salary), y = avg_salary)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "10 Posisi dengan Gaji Rata-Rata Tertinggi", x = "Posisi", y = "Gaji Rata-Rata")+
  theme(
    plot.background = element_rect(fill = "lightblue"),  # Change background color
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# Plot Salary in USD by Company Size
ggplot(Data_Salary, aes(x = company_size, y = sum(salary_in_usd))) +
  geom_col(fill = "orange") + 
  labs(title = "Salary in USD by Company Size", 
       x = "Company Size", # Label sumbu x
       y = "Sum Salary")+
  theme(
    plot.background = element_rect(fill = "lightblue"),  # Change background color
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# Plot Salary by Experience Level
avg_salary_by_department <- Clean_DataSalary %>%
  group_by(experience_level) %>%
  summarise(avg_salary = mean(salary_in_usd)) %>%
  arrange(desc(avg_salary)) %>%
  head(10)

ggplot(avg_salary_by_department, aes(x = reorder(experience_level, -avg_salary), y = avg_salary)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Gaji berdasarkan pengalaman", x = "Level Pengalaman", y = "Gaji Rata-Rata")+
  theme(
    plot.background = element_rect(fill = "lightblue"),  # Change background color
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

avg_salary_by_year_experience <- Clean_DataSalary %>%
  group_by(work_year, experience_level) %>%
  summarise(mean_salary_in_usd = mean(salary_in_usd, na.rm = TRUE)) %>%
  ungroup()

ggplot(avg_salary_by_year_experience, aes(x = work_year, y = mean_salary_in_usd, color = experience_level, group = experience_level)) +
  geom_line() +
  geom_point() +
  labs(title = "Salary in USD by Experience Level Over Time", x = "Work Year", y = "Average Salary in USD") +
  theme(
    plot.background = element_rect(fill = "lightblue"),  # Change background color
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  )

# Plot Salary in USD by Employee Residence
data_treemap <- Data_Salary %>%
  select(employee_residence, salary_in_usd)

data_treemap_summary <- data_treemap %>%
  group_by(employee_residence) %>%
  summarise(total_salary = sum(salary_in_usd))

treemap(
  data_treemap_summary,
  index = "employee_residence",
  vSize = "total_salary", 
  vColor = ~employee_residence, 
  title = "Salary in USD by Employee Residence", 
  key.size = 0.2, 
  text.size = 0.5,
)