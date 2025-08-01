#include <Rcpp.h>
#include <unordered_set>
#include <string>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame colndistinct(DataFrame input_df) {
  // Get dimensions
  int num_cols = input_df.size();
  int num_rows = input_df.nrows();
  
  // Early return for empty data frame
  if (num_cols == 0) {
    return DataFrame::create();
  }
  
  // Get column names
  CharacterVector col_names = input_df.names();
  
  // Pre-allocate result vector with known size
  IntegerVector distinct_counts(num_cols);
  
  // Reusable containers for different data types to avoid repeated allocations
  std::unordered_set<std::string> string_set;
  std::unordered_set<int> int_set;
  std::unordered_set<double> double_set;
  std::unordered_set<bool> bool_set;
  
  // Process each column
  for (int col_idx = 0; col_idx < num_cols; col_idx++) {
    
    SEXP current_column = input_df[col_idx];
    int distinct_count = 0;
    
    // Handle different column types with type-specific containers for better performance
    switch (TYPEOF(current_column)) {
    
    case INTSXP: {
      // Integer column - use integer set directly
      int_set.clear();
      // Reserve capacity if beneficial (rough heuristic)
      if (num_rows > 100) {
        int_set.reserve(std::min(num_rows, 1000));
      }
      
      IntegerVector int_col = as<IntegerVector>(current_column);
      bool has_na = false;
      
      for (int row_idx = 0; row_idx < num_rows; row_idx++) {
        if (IntegerVector::is_na(int_col[row_idx])) {
          has_na = true;
        } else {
          int_set.insert(int_col[row_idx]);
        }
      }
      
      distinct_count = int_set.size() + (has_na ? 1 : 0);
      break;
    }
      
    case REALSXP: {
      // Numeric column - use double set directly
      double_set.clear();
      if (num_rows > 100) {
        double_set.reserve(std::min(num_rows, 1000));
      }
      
      NumericVector num_col = as<NumericVector>(current_column);
      bool has_na = false;
      
      for (int row_idx = 0; row_idx < num_rows; row_idx++) {
        if (NumericVector::is_na(num_col[row_idx])) {
          has_na = true;
        } else {
          double_set.insert(num_col[row_idx]);
        }
      }
      
      distinct_count = double_set.size() + (has_na ? 1 : 0);
      break;
    }
      
    case LGLSXP: {
      // Logical column - use boolean set (max 3 values: TRUE, FALSE, NA)
      bool_set.clear();
      CharacterVector logical_col = as<CharacterVector>(current_column);
      bool has_na = false;
      
      for (int row_idx = 0; row_idx < num_rows; row_idx++) {
        if (logical_col[row_idx] == NA_STRING) {
          has_na = true;
        } else {
          // Convert to boolean for efficient storage
          bool_set.insert(logical_col[row_idx] == "TRUE");
        }
      }
      
      distinct_count = bool_set.size() + (has_na ? 1 : 0);
      break;
    }
      
    case STRSXP: 
    default: {
      // Character column and other types - use string set
      string_set.clear();
      if (num_rows > 100) {
        string_set.reserve(std::min(num_rows, 1000));
      }
      
      CharacterVector char_col = as<CharacterVector>(current_column);
      bool has_na = false;
      
      for (int row_idx = 0; row_idx < num_rows; row_idx++) {
        if (char_col[row_idx] == NA_STRING) {
          has_na = true;
        } else {
          // Use direct string insertion to avoid extra conversions
          string_set.insert(std::string(char_col[row_idx]));
        }
      }
      
      distinct_count = string_set.size() + (has_na ? 1 : 0);
      break;
    }
    }
    
    distinct_counts[col_idx] = distinct_count;
  }
  
  // Efficiently construct result DataFrame using List
  List result_list(num_cols);
  CharacterVector result_names(num_cols);
  
  for (int col_idx = 0; col_idx < num_cols; col_idx++) {
    result_list[col_idx] = IntegerVector::create(distinct_counts[col_idx]);
    result_names[col_idx] = col_names[col_idx];
  }
  
  // Create DataFrame directly from List
  DataFrame result(result_list);
  result.names() = result_names;
  
  // Set tibble attributes
  CharacterVector class_attr = CharacterVector::create("tbl_df", "tbl", "data.frame");
  result.attr("class") = class_attr;
  result.attr("row.names") = IntegerVector::create(1);
  
  return result;
}
