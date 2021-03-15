#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Association Rule Mining"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("sup",
                        "Support",
                        min = 0,
                        max = 1,
                        value = 0.01),
            sliderInput("con",
                        "Confidence",
                        min = 0,
                        max = 1,
                        value = 0.5),
            sliderInput("len",
                        "Minimum rule length",
                        min = 3,
                        max = 10,
                        value = 3),
            sliderInput("num",
                        "Number of top rules",
                        min = 5,
                        max = 10,
                        value = 5),
            radioButtons("attr", 
                         "Attrition", 
                         choices = c("Attrition=Yes", "Attrition=No"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("arules"),
           plotOutput("scatterplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$arules <- renderPrint({
        library(tidyverse)
        emp_atr <- read_csv("employee_attrition.csv", col_types = cols())
        
        emp_atr <- select(emp_atr, -c("EmployeeCount", "EmployeeNumber", "Over18", "StandardHours"))
        char_var <- sapply(emp_atr, is.character)
        
        emp_atr[,char_var] <- lapply(emp_atr[,char_var], as.factor)
        
        outlier_obs <- c(which(emp_atr$TotalWorkingYears == 114), which(emp_atr$YearsWithCurrManager == 219))
        emp_atr <- emp_atr[-outlier_obs, ]
        
        emp_atr$DistanceFromHome[is.na(emp_atr$DistanceFromHome)] <- mean(emp_atr$DistanceFromHome, na.rm = T)
        emp_atr$JobLevel[is.na(emp_atr$JobLevel)] <- mean(emp_atr$JobLevel, na.rm = T)
        emp_atr$PercentSalaryHike[is.na(emp_atr$PercentSalaryHike)] <- mean(emp_atr$PercentSalaryHike, na.rm = T)
        emp_atr$PerformanceRating[is.na(emp_atr$PerformanceRating)] <- mean(emp_atr$PerformanceRating, na.rm = T)
        emp_atr$RelationshipSatisfaction[is.na(emp_atr$RelationshipSatisfaction)] <- mean(emp_atr$RelationshipSatisfaction, na.rm = T)
        emp_atr$TotalWorkingYears[is.na(emp_atr$TotalWorkingYears)] <- mean(emp_atr$TotalWorkingYears, na.rm = T)
        emp_atr$YearsSinceLastPromotion[is.na(emp_atr$YearsSinceLastPromotion)] <- mean(emp_atr$YearsSinceLastPromotion, na.rm = T)
        
        emp_atr <- emp_atr[-c(which(is.na(emp_atr$Gender)), which(is.na(emp_atr$OverTime))),]
        
        emp_atr <- select(emp_atr, -c("Education", "DailyRate", "DistanceFromHome", "EnvironmentSatisfaction", "HourlyRate", 
                                      "JobInvolvement", "JobSatisfaction", "MonthlyRate", "NumCompaniesWorked",
                                      "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", 
                                      "TrainingTimesLastYear", "WorkLifeBalance"))
        library(arules)
        library(arulesViz)
        emp_atr$Age <- arules::discretize(emp_atr$Age, method = "fixed", 
                                          breaks = c(18, 30, 45, 60), labels = c("Below 30", "Between 30 and 45", "Above 45"))
        emp_atr$JobLevel <- arules::discretize(emp_atr$JobLevel, method = "interval",
                                               breaks = 5, labels = c("1", "2", "3", "4", "5"))
        emp_atr$MonthlyIncome <- arules::discretize(emp_atr$MonthlyIncome, method = "fixed",
                                                    breaks = c(0,5000, 10000, 15000, 20000), labels = c("Less than 5000", "Between 5000 and 10000",
                                                                                                        "Between 10000 and 15000", "Above 15000"))
        emp_atr$TotalWorkingYears <- arules::discretize(emp_atr$TotalWorkingYears, method = "interval",
                                                        breaks = 3, labels = c("Less", "Medium", "High"))
        emp_atr$YearsAtCompany <- arules::discretize(emp_atr$YearsAtCompany, method = "interval",
                                                     breaks = 3, labels = c("Less", "Medium", "High"))
        emp_atr$YearsInCurrentRole <- arules::discretize(emp_atr$YearsInCurrentRole, method = "interval",
                                                         breaks = 3, labels = c("Less", "Medium", "High"))

        fac_var <- sapply(emp_atr, is.factor)
        employee <- as(emp_atr[, fac_var], "transactions")
        
        rules <- apriori(data = employee, parameter = list(supp = input$sup, conf = input$con, minlen = input$len),
                          appearance = list(default = "lhs", rhs = input$attr),
                          control = list(verbose = F))
        
        subset_rules <- which(colSums(is.subset(rules, rules)) > 1)
        rules <- sort(rules[-subset_rules], by = "lift", descreasing = T)
        
        inspect(head(rules, input$num))
    })
    
    output$scatterplot <- renderPlot({
        library(tidyverse)
        emp_atr <- read_csv("employee_attrition.csv", col_types = cols())
        
        emp_atr <- select(emp_atr, -c("EmployeeCount", "EmployeeNumber", "Over18", "StandardHours"))
        char_var <- sapply(emp_atr, is.character)
        
        emp_atr[,char_var] <- lapply(emp_atr[,char_var], as.factor)
        
        outlier_obs <- c(which(emp_atr$TotalWorkingYears == 114), which(emp_atr$YearsWithCurrManager == 219))
        emp_atr <- emp_atr[-outlier_obs, ]
        
        emp_atr$DistanceFromHome[is.na(emp_atr$DistanceFromHome)] <- mean(emp_atr$DistanceFromHome, na.rm = T)
        emp_atr$JobLevel[is.na(emp_atr$JobLevel)] <- mean(emp_atr$JobLevel, na.rm = T)
        emp_atr$PercentSalaryHike[is.na(emp_atr$PercentSalaryHike)] <- mean(emp_atr$PercentSalaryHike, na.rm = T)
        emp_atr$PerformanceRating[is.na(emp_atr$PerformanceRating)] <- mean(emp_atr$PerformanceRating, na.rm = T)
        emp_atr$RelationshipSatisfaction[is.na(emp_atr$RelationshipSatisfaction)] <- mean(emp_atr$RelationshipSatisfaction, na.rm = T)
        emp_atr$TotalWorkingYears[is.na(emp_atr$TotalWorkingYears)] <- mean(emp_atr$TotalWorkingYears, na.rm = T)
        emp_atr$YearsSinceLastPromotion[is.na(emp_atr$YearsSinceLastPromotion)] <- mean(emp_atr$YearsSinceLastPromotion, na.rm = T)
        
        emp_atr <- emp_atr[-c(which(is.na(emp_atr$Gender)), which(is.na(emp_atr$OverTime))),]
        
        emp_atr <- select(emp_atr, -c("Education", "DailyRate", "DistanceFromHome", "EnvironmentSatisfaction", "HourlyRate", 
                                      "JobInvolvement", "JobSatisfaction", "MonthlyRate", "NumCompaniesWorked",
                                      "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction", "StockOptionLevel", 
                                      "TrainingTimesLastYear", "WorkLifeBalance"))
        library(arules)
        library(arulesViz)
        emp_atr$Age <- arules::discretize(emp_atr$Age, method = "fixed", 
                                          breaks = c(18, 30, 45, 60), labels = c("Below 30", "Between 30 and 45", "Above 45"))
        emp_atr$JobLevel <- arules::discretize(emp_atr$JobLevel, method = "interval",
                                               breaks = 5, labels = c("1", "2", "3", "4", "5"))
        emp_atr$MonthlyIncome <- arules::discretize(emp_atr$MonthlyIncome, method = "fixed",
                                                    breaks = c(0,5000, 10000, 15000, 20000), labels = c("Less than 5000", "Between 5000 and 10000",
                                                                                                        "Between 10000 and 15000", "Above 15000"))
        emp_atr$TotalWorkingYears <- arules::discretize(emp_atr$TotalWorkingYears, method = "interval",
                                                        breaks = 3, labels = c("Less", "Medium", "High"))
        emp_atr$YearsAtCompany <- arules::discretize(emp_atr$YearsAtCompany, method = "interval",
                                                     breaks = 3, labels = c("Less", "Medium", "High"))
        emp_atr$YearsInCurrentRole <- arules::discretize(emp_atr$YearsInCurrentRole, method = "interval",
                                                         breaks = 3, labels = c("Less", "Medium", "High"))
        
        fac_var <- sapply(emp_atr, is.factor)
        employee <- as(emp_atr[, fac_var], "transactions")
        
        rules <- apriori(data = employee, parameter = list(supp = input$sup, conf = input$con, minlen = input$len),
                         appearance = list(default = "lhs", rhs = input$attr),
                         control = list(verbose = F))
        
        subset_rules <- which(colSums(is.subset(rules, rules)) > 1)
        rules <- sort(rules[-subset_rules], by = "lift", descreasing = T)
        
        subrules <- head(rules, input$num)
        
        plot(subrules, method = "paracoord", control = list(reorder = TRUE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


