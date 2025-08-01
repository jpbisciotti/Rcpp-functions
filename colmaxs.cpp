#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame colmaxs(DataFrame df) {
  const int ncols = df.size();
  List result_list(ncols);
  const CharacterVector col_names = df.names();
  
  for (int i = 0; i < ncols; i++) {
    SEXP col = df[i];
    
    switch(TYPEOF(col)) {
    case REALSXP: {
      const NumericVector& num_col = as<NumericVector>(col);
      const int n = num_col.size();
      
      if (n == 0) {
        result_list[i] = NumericVector::create(NA_REAL);
        break;
      }
      
      // Find first non-NA value for proper initialization
      double max_val = NA_REAL;
      for (int j = 0; j < n; j++) {
        if (!NumericVector::is_na(num_col[j])) {
          max_val = num_col[j];
          // Continue from next element to find actual maximum
          for (int k = j + 1; k < n; k++) {
            if (!NumericVector::is_na(num_col[k]) && num_col[k] > max_val) {
              max_val = num_col[k];
            }
          }
          break;
        }
      }
      result_list[i] = NumericVector::create(max_val);
      break;
    }
      
    case INTSXP: {
      const IntegerVector& int_col = as<IntegerVector>(col);
      const int n = int_col.size();
      
      if (n == 0) {
        result_list[i] = IntegerVector::create(NA_INTEGER);
        break;
      }
      
      int max_val = NA_INTEGER;
      for (int j = 0; j < n; j++) {
        if (!IntegerVector::is_na(int_col[j])) {
          max_val = int_col[j];
          for (int k = j + 1; k < n; k++) {
            if (!IntegerVector::is_na(int_col[k]) && int_col[k] > max_val) {
              max_val = int_col[k];
            }
          }
          break;
        }
      }
      result_list[i] = IntegerVector::create(max_val);
      break;
    }
      
    case LGLSXP: {
      const LogicalVector& log_col = as<LogicalVector>(col);
      const int n = log_col.size();
      
      if (n == 0) {
        result_list[i] = LogicalVector::create(NA_LOGICAL);
        break;
      }
      
      // Optimized logical comparison: TRUE > FALSE, so early exit possible
      int max_val = NA_LOGICAL;
      for (int j = 0; j < n; j++) {
        if (!LogicalVector::is_na(log_col[j])) {
          max_val = log_col[j];
          if (max_val == TRUE) {
            break; // TRUE is maximum possible logical value
          }
          // Continue looking for TRUE
          for (int k = j + 1; k < n; k++) {
            if (!LogicalVector::is_na(log_col[k]) && log_col[k] > max_val) {
              max_val = log_col[k];
              if (max_val == TRUE) break; // Early exit optimization
            }
          }
          break;
        }
      }
      result_list[i] = LogicalVector::create(max_val);
      break;
    }
      
    case STRSXP: {
      const CharacterVector& char_col = as<CharacterVector>(col);
      const int n = char_col.size();
      
      if (n == 0) {
        result_list[i] = CharacterVector::create(NA_STRING);
        break;
      }
      
      String max_val = NA_STRING;
      for (int j = 0; j < n; j++) {
        if (!CharacterVector::is_na(char_col[j])) {
          max_val = String(char_col[j]);  // Explicit conversion
          for (int k = j + 1; k < n; k++) {
            if (!CharacterVector::is_na(char_col[k])) {
              String current_val = String(char_col[k]);  // Explicit conversion
              if (current_val > max_val) {
                max_val = current_val;
              }
            }
          }
          break;
        }
      }
      result_list[i] = CharacterVector::create(max_val);
      break;
    }
      
    default: {
      // Handle factors and other types by converting to character
      const CharacterVector char_col = as<CharacterVector>(col);
      const int n = char_col.size();
      
      if (n == 0) {
        result_list[i] = CharacterVector::create(NA_STRING);
        break;
      }
      
      String max_val = NA_STRING;
      for (int j = 0; j < n; j++) {
        if (!CharacterVector::is_na(char_col[j])) {
          max_val = String(char_col[j]);  // Explicit conversion
          for (int k = j + 1; k < n; k++) {
            if (!CharacterVector::is_na(char_col[k])) {
              String current_val = String(char_col[k]);  // Explicit conversion
              if (current_val > max_val) {
                max_val = current_val;
              }
            }
          }
          break;
        }
      }
      result_list[i] = CharacterVector::create(max_val);
      break;
    }
    }
  }
  
  // Preserve column names and set attributes
  result_list.names() = col_names;
  DataFrame result = DataFrame(result_list);
  
  // Set tibble attributes efficiently
  result.attr("class") = CharacterVector::create("tbl_df", "tbl", "data.frame");
  result.attr("row.names") = IntegerVector::create(1);
  
  return result;
}
