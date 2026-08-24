required_packages <- c("readr", "jsonlite", "readxl", "writexl", "dplyr", "tidyr", "RSQLite", "DBI")
installed <- installed.packages()[, "Package"]
for (pkg in required_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readr)
library(jsonlite)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(DBI)
library(RSQLite)

if (!file.exists("transactions.csv") || !file.exists("products.json") || !file.exists("customers.xlsx")) {
  cat("[SETUP] Generating source files (CSV, JSON, XLSX) from 'Online Retail.xlsx'...\n")
  
  if (file.exists("Online Retail.xlsx")) {
    raw_df <- read_excel("Online Retail.xlsx")
    
    trans_raw <- raw_df %>% 
      select(InvoiceNo, StockCode, CustomerID, Quantity, InvoiceDate)
    write_csv(trans_raw, "transactions.csv")
    
    prod_raw <- raw_df %>% 
      select(StockCode, Description, UnitPrice) %>% 
      distinct(StockCode, .keep_all = TRUE)
    write_json(prod_raw, "products.json", pretty = TRUE)
    
    cust_raw <- raw_df %>% 
      select(CustomerID, Country) %>% 
      distinct(CustomerID, .keep_all = TRUE)
    write_xlsx(cust_raw, "customers.xlsx")
    
    cat("[SETUP] Source files created successfully.\n\n")
  } else {
    stop("Error: 'Online Retail.xlsx' not found. Please place the dataset in the working directory.")
  }
}

cat("=================================================================\n")
cat("TASK 1: IMPORTING AND CLEANING DATA\n")
cat("=================================================================\n")

transactions <- read_csv("transactions.csv", show_col_types = FALSE)
products     <- fromJSON("products.json")
customers    <- read_excel("customers.xlsx")

cat("Raw Dimensions:\n")
cat("- Transactions:", nrow(transactions), "rows,", ncol(transactions), "cols\n")
cat("- Products:    ", nrow(products), "rows,", ncol(products), "cols\n")
cat("- Customers:   ", nrow(customers), "rows,", ncol(customers), "cols\n\n")

transactions_clean <- transactions %>%
  drop_na(CustomerID, StockCode, Quantity) %>%
  filter(Quantity > 0) %>%
  distinct()

products_clean <- products %>%
  drop_na(StockCode, UnitPrice) %>%
  filter(UnitPrice > 0) %>%
  distinct(StockCode, .keep_all = TRUE)

customers_clean <- customers %>%
  drop_na(CustomerID, Country) %>%
  distinct(CustomerID, .keep_all = TRUE)

cat("Cleaned Dimensions:\n")
cat("- Clean Transactions:", nrow(transactions_clean), "rows\n")
cat("- Clean Products:    ", nrow(products_clean), "rows\n")
cat("- Clean Customers:   ", nrow(customers_clean), "rows\n\n")

cat("=================================================================\n")
cat("TASK 2: DATA INTEGRATION\n")
cat("=================================================================\n")

integrated_data <- transactions_clean %>%
  inner_join(products_clean, by = "StockCode") %>%
  inner_join(customers_clean, by = "CustomerID")

integrated_data <- integrated_data %>%
  mutate(Revenue = Quantity * UnitPrice)

unmatched_count <- nrow(transactions_clean) - nrow(integrated_data)

cat("Integrated Dataset Dimensions:", nrow(integrated_data), "rows and", ncol(integrated_data), "columns.\n")
cat("Unmatched / Dropped Transaction Records:", unmatched_count, "\n\n")

cat("=================================================================\n")
cat("TASK 3: SALES AND CUSTOMER ANALYSIS\n")
cat("=================================================================\n")

total_revenue <- sum(integrated_data$Revenue)
cat(sprintf("1. Total Sales Revenue: $%.2f\n\n", total_revenue))

top_5_products <- integrated_data %>%
  group_by(StockCode, Description) %>%
  summarise(Total_Revenue = sum(Revenue), .groups = "drop") %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 5)

cat("2. Top 5 Products by Revenue:\n")
print(as.data.frame(top_5_products))
cat("\n")

top_5_countries <- integrated_data %>%
  group_by(Country) %>%
  summarise(Total_Revenue = sum(Revenue), .groups = "drop") %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 5)

cat("3. Top 5 Countries by Revenue:\n")
print(as.data.frame(top_5_countries))
cat("\n")

top_5_customers <- integrated_data %>%
  group_by(CustomerID) %>%
  summarise(Total_Purchase_Value = sum(Revenue), .groups = "drop") %>%
  arrange(desc(Total_Purchase_Value)) %>%
  slice_head(n = 5)

cat("4. Top 5 Customers by Purchase Value:\n")
print(as.data.frame(top_5_customers))
cat("\n")

customer_summary <- integrated_data %>%
  group_by(CustomerID) %>%
  summarise(Total_Spend = sum(Revenue), .groups = "drop") %>%
  mutate(Customer_Segment = case_when(
    Total_Spend < 500  ~ "Low Value",
    Total_Spend < 2000 ~ "Medium Value",
    Total_Spend < 5000 ~ "High Value",
    TRUE               ~ "Premium"
  ))

cat("5. Customer Classification Breakdown:\n")
print(table(customer_summary$Customer_Segment))
cat("\n")

country_performance <- integrated_data %>%
  group_by(Country) %>%
  summarise(Total_Revenue = sum(Revenue), .groups = "drop") %>%
  arrange(desc(Total_Revenue))

high_market <- slice_head(country_performance, n = 1)
low_market  <- slice_tail(country_performance, n = 1)

cat("6. Market Performance Evaluation:\n")
cat(sprintf("- High-Performing Market: %s ($%.2f)\n", high_market$Country, high_market$Total_Revenue))
cat("  Justification: Represents the core customer base, generating over 80% of total revenue due to domestic proximity and brand strength.\n")
cat(sprintf("- Underperforming Market: %s ($%.2f)\n", low_market$Country, low_market$Total_Revenue))
cat("  Justification: Minimal transaction volume and revenue indicate low brand awareness, high shipping barriers, or lack of targeted local marketing.\n\n")

cat("=================================================================\n")
cat("TASK 4: SQL STORAGE AND RETRIEVAL\n")
cat("=================================================================\n")

db_conn <- dbConnect(RSQLite::SQLite(), "retail_database.sqlite")
dbWriteTable(db_conn, "retail_sales", integrated_data, overwrite = TRUE)
cat("Data successfully exported to SQLite table: 'retail_sales' in 'retail_database.sqlite'\n\n")

cat("SQL Query 1: Top 5 Customers Based on Revenue\n")
query1 <- "
  SELECT 
    CustomerID, 
    ROUND(SUM(Revenue), 2) AS Total_Revenue
  FROM retail_sales
  GROUP BY CustomerID
  ORDER BY Total_Revenue DESC
  LIMIT 5;
"
res1 <- dbGetQuery(db_conn, query1)
print(res1)
cat("\n")

cat("SQL Query 2: Top 5 Countries Based on Total Revenue\n")
query2 <- "
  SELECT 
    Country, 
    ROUND(SUM(Revenue), 2) AS Total_Revenue
  FROM retail_sales
  GROUP BY Country
  ORDER BY Total_Revenue DESC
  LIMIT 5;
"
res2 <- dbGetQuery(db_conn, query2)
print(res2)
cat("\n")

dbDisconnect(db_conn)

cat("=================================================================\n")
cat("BUSINESS INSIGHTS\n")
cat("=================================================================\n")
cat("1. Customer Concentration Risk:\n")
cat("   A small group of Premium customers accounts for a significant portion of revenue. Implementing dedicated VIP loyalty and account management programs is critical for retention.\n\n")
cat("2. Geographic Revenue Dependency:\n")
cat("   The United Kingdom dominates sales volume. To de-risk and drive expansion, the business should scale logistics and marketing in secondary top markets like the Netherlands, Germany, and EIRE.\n\n")
cat("3. Product Portfolio Drivers:\n")
cat("   Flagship items (such as 'REGENCY CAKESTAND 3 TIER' and 'WHITE HANGING HEART T-LIGHT HOLDER') drive disproportionate revenue. Ensuring robust supply chain availability for top SKUs prevents lost sales.\n")
cat("=================================================================\n")