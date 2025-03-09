
resident_data %>%
  filter(name == "Adam Streicher") %>%
  select(access_code) %>%
  na.omit() 


progress_data <- compute_eval_progress(resident_data, 'Cale Martin', count_res_assessments)

display_progress(progress_data)


y<- resident_data %>%
  filter(name == "beta test", redcap_repeat_instrument == "Faculty Evaluation")

ggplot(plot_data, aes(x = reorder(Level, -Count), y = Count, fill = rotation)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(title = "Faculty Evaluations Completed by Level", x = "Level", y = "Count") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none")


generate_survey_link <- function(record_id, instrument, url, token) {
  formData <- list(
    "token" = token,
    "content" = 'surveyLink',
    "record" = record_id,
    "instrument" = instrument
  )
  
  response <- httr::POST(url, body = formData, encode = "form")
  link <- httr::content(response)
  return(link)
}

create_ccc_table(resident_data, "Adam Streicher")



# Suppose these are the four possible evaluation types:
cc_types <- c("1st quarter - inbasket coverage", "2nd quarter - summative evaluation", "3rd quarter - documentation", "4th quarter - summative evaluation")

# 'df' should be your data filtered by name. For example:
df <- resident_data %>% filter(name == "Adam Streicher")  
# (In your app, you'd use resident_info() to filter)

# Create a summary table ensuring all Level and cc_eval_type combinations exist.
eval_summary <- df %>%
  # Select the fields of interest.
  select(Level, cc_eval_type, Evaluator) %>%
  # Complete the grid of all Levels and evaluation types.
  complete(Level, cc_eval_type = cc_types, fill = list(evaluator = NA)) %>%
  group_by(Level, cc_eval_type)

%>%
  summarize(
    # If every evaluator is NA for this combo, then it's "Not Done"; else "Done"
    status = ifelse(all(is.na(cc_eval_type)), "Not Done", "Done"),
    # If done, collapse the unique evaluator names into a single string; otherwise blank.
    evaluator = ifelse(status == "Done", paste(unique(evaluator[!is.na(evaluator)]), collapse = ", "), ""),
    .groups = "drop"
  )

# Now create the tile plot:
ggplot(eval_summary, aes(x = cc_eval_type, y = Level, fill = status)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = evaluator), size = 4, color = "black") +
  scale_fill_manual(values = c("Done" = "lightgreen", "Not Done" = "lightcoral")) +
  labs(title = "Evaluation Status by Type and Level",
       x = "Evaluation Type", y = "Level") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12))








