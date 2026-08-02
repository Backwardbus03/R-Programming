file_name <- "PRSA_Data_Aotizhongxin_20130301-20170228.csv"

air_quality <- tryCatch({
  read.csv(file_name)
}, error = function(e) {
  message("Error: The file is not found, cannot be opened, or the format is incorrect.")
  return(NULL)
})

if(!is.null(air_quality)) {
  print(head(air_quality))
  
  str(air_quality)
  
  cat("Number of rows:", nrow(air_quality), "\n")
  cat("Number of columns:", ncol(air_quality), "\n")
  
  has_missing <- any(is.na(air_quality))
  cat("Contains missing values:", has_missing, "\n")
  
  total_missing <- sum(is.na(air_quality))
  cat("Total missing values in dataset:", total_missing, "\n\n")
}

temperature <- c(28, 30, NA, 32)
cat("NA Example Output:", is.na(temperature), "\n")

missing_object <- NULL
cat("NULL Example Output:", is.null(missing_object), "\n")

undefined_value <- 0/0
cat("NaN Example Output:", is.nan(undefined_value), "\n\n")

missing_summary <- function(df, vars) {
  total_records <- nrow(df)
  summary_list <- list()
  
  for(v in vars) {
    if(v %in% names(df)) {
      miss_val <- sum(is.na(df[[v]]))
      miss_pct <- (miss_val / total_records) * 100
      
      if(miss_pct > 20) {
        warning(paste("Variable", v, "contains more than 20% missing values."))
      }
      
      summary_list[[v]] <- data.frame(Variable = v, 
                                      Total_Records = total_records, 
                                      Missing_Values = miss_val, 
                                      Missing_Percentage = round(miss_pct, 2))
    }
  }
  return(do.call(rbind, summary_list))
}

selected_vars <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
miss_summary_table <- missing_summary(air_quality, selected_vars)
print(miss_summary_table)
cat("\n")

air_quality$pollution_ratio <- air_quality$PM2.5 / air_quality$PM10

cat("NA count:", sum(is.na(air_quality$pollution_ratio)), "\n")
cat("NaN count:", sum(is.nan(air_quality$pollution_ratio)), "\n")
cat("Infinite count:", sum(is.infinite(air_quality$pollution_ratio)), "\n")

air_quality$pollution_ratio[is.nan(air_quality$pollution_ratio) | is.infinite(air_quality$pollution_ratio)] <- NA
cat("Replacement complete. New NA count in pollution_ratio:", sum(is.na(air_quality$pollution_ratio)), "\n\n")

missing_before <- sapply(air_quality[selected_vars], function(x) sum(is.na(x)))

numeric_variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM")

for(var in numeric_variables) {
  if(var %in% names(air_quality)) {
    missing_before_task5 <- sum(is.na(air_quality[[var]]))
    
    var_median <- median(air_quality[[var]], na.rm = TRUE)
    
    air_quality[[var]][is.na(air_quality[[var]])] <- var_median
    
    missing_after_task5 <- sum(is.na(air_quality[[var]]))
    
    cat("Variable:", var, "\n")
    cat("  Missing before:", missing_before_task5, "\n")
    cat("  Median used:", var_median, "\n")
    cat("  Missing after:", missing_after_task5, "\n\n")
  }
}

calculate_mode <- function(x) {
  unique_x <- unique(na.omit(x))
  unique_x[which.max(tabulate(match(na.omit(x), unique_x)))]
}

wd_missing_before <- sum(is.na(air_quality$wd))
wd_mode <- calculate_mode(air_quality$wd)
air_quality$wd[is.na(air_quality$wd)] <- wd_mode
wd_missing_after <- sum(is.na(air_quality$wd))

cat("Variable: wd\n")
cat("  Mode calculated:", wd_mode, "\n")
cat("  Missing before:", wd_missing_before, "\n")
cat("  Missing after:", wd_missing_after, "\n\n")

clean_variable <- function(dataset, variable_name) {
  result <- tryCatch({
    if(!(variable_name %in% names(dataset))) {
      stop("Error: The variable does not exist in the dataset.")
    }
    
    target_var <- dataset[[variable_name]]
    
    if(all(is.na(target_var))) {
      stop("Error: The variable contains only missing values.")
    }
    
    if(!is.numeric(target_var)) {
      stop("Error: A categorical variable was passed instead of a numerical variable.")
    }
    
    var_med <- median(target_var, na.rm = TRUE)
    if(is.na(var_med)) {
      stop("Error: The median cannot be calculated.")
    }
    
    target_var[is.na(target_var)] <- var_med
    message(paste("Successfully cleaned numerical variable:", variable_name))
    return(target_var)
    
  }, error = function(e) {
    message(e$message)
    return(NULL) 
  })
  
  return(result)
}

cat("Testing with nonexistent variable:\n")
test1 <- clean_variable(air_quality, "FAKE_VAR")

cat("Testing with categorical variable 'wd':\n")
test2 <- clean_variable(air_quality, "wd")

missing_after <- sapply(air_quality[selected_vars], function(x) sum(is.na(x)))
values_replaced <- missing_before - missing_after

comparison_table <- data.frame(
  Variable = selected_vars,
  Missing_Before = missing_before,
  Missing_After = missing_after,
  Values_Replaced = values_replaced
)
print(comparison_table)
cat("\n")

bar_data <- rbind(comparison_table$Missing_Before, comparison_table$Missing_After)
colnames(bar_data) <- comparison_table$Variable
rownames(bar_data) <- c("Before Cleaning", "After Cleaning")

png("missing_values_plot.png", width=800, height=600)
barplot(bar_data, 
        beside = TRUE, 
        col = c("coral", "lightgreen"),
        main = "Missing Values Before and After Data Cleaning",
        xlab = "Variables", 
        ylab = "Number of Missing Values",
        legend.text = TRUE,
        args.legend = list(x = "topright"))
dev.off()
cat("Visualization saved as 'missing_values_plot.png' in the working directory.\n\n")

write.csv(air_quality, "cleaned_air_quality_data.csv", row.names = FALSE)
cat("Data successfully exported to 'cleaned_air_quality_data.csv'.\n")