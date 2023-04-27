dummyVars.undo = function(df, col_prefix) {
  if (!endsWith(col_prefix, '_')) {
    # If col_prefix doesn't end with a period, include one, but save the
    # "pretty name" as the one without a period
    pretty_col_prefix = col_prefix
    col_prefix = paste0(col_prefix, '_')
  } else {
    # Otherwise, strip the period for the pretty column name
    pretty_col_prefix = substr(col_prefix, 1, nchar(col_prefix)-1)
  }
  
  # Get all columns with that encoding prefix
  cols = names(df)[names(df) %>% startsWith(col_prefix)]
  
  # Find the rows where all values are zero. If this isn't the case
  # with your data there's no worry, it won't hurt anything.
  base_level.idx = rowSums(df[cols]) == 0
  
  # Set the column value to a base value of zero
  df[base_level.idx, pretty_col_prefix] = 0
  
  # Go through the remaining columns and find where the maximum value (1) occurs
  df[!base_level.idx, pretty_col_prefix] = cols[apply(df[!base_level.idx, cols], 1, which.max)] %>%
    strsplit('\\_') %>%
    sapply(tail, 1) 
  
  # Drop the encoded columns
  df[cols] = NULL
  
  return(df)  
}

df_d_1_s_1_r <- dummyVars.undo(df_d_1_s_1, 'uf')
colnames(df_d_1_s_1_r)
tail(df_d_1_s_1_r$uf)
df_z <- df_d_1_s_1_r[!(df_d_1_s_1_r$uf=="0"),]
dim(df_z)
head(df_z$uf)
unique(df_z$uf)
count(df_1, uf)
count(df_z, uf)
