
resident_data %>%
  filter(name == "Adam Streicher") %>%
  select(access_code) %>%
  na.omit() 

NYqoWH 

devtools::document()

#workign through observation data:
labels <- parse_ip_obs_labels(ass_dict)


always_include_cols <- c("Date", "ip_obs_type", "Level", "Evaluator")
unwanted_cols <- c(
  "assess_a_resident_timestamp", "ass_date", "clin_context", "ip_obs_type",
  "slu_gim_att", "att_sign", "assess_a_resident_complete", "week", 
  "year", "name", "eval_type", "weekyr", "start_year", 
  "academic_year_start", "record_id"
)
filtered_data <- resident_data %>%
  filter(name == "Adam Streicher", ip_obs_type == "Written H&P")

non_empty_cols <- colSums(!is.na(filtered_data)) > 0

cols_in_data <- intersect(always_include_cols, names(filtered_data))
non_empty_cols[cols_in_data] <- TRUE
final_data <- filtered_data[, non_empty_cols, drop = FALSE]
final_data <- final_data[, setdiff(names(final_data), unwanted_cols), drop = FALSE]

rename_map <- setNames(ass_dict$field_label, ass_dict$field_name)

# Now rename columns in final_data *only* if they exist in rename_map.
final_data <- dplyr::rename_with(
  final_data,
  ~ ifelse(.x %in% names(rename_map), rename_map[.x], .x)
)

colnames(final_data)

prep_obs_table(
  data          = ass_dat,
  dict          = ass_dict,
  resident      = "Adam Streicher", 
  selected_label= "Written H&P"
)

