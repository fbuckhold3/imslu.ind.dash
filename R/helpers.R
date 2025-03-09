#' Develop resident assessment data:

count_res_assessments <- function(data, resident_name) {
  eval_labels <- c("Performance during the rotation" = "Summative",
                   "Quarterly" = "Continuity Clinic",
                   "An observation of an activity" = "Observation")
  plot_data <- data %>%
    filter(name == resident_name) %>%
    group_by(Level, eval_type, Rotation) %>%
    summarize(Count = n(), .groups = 'drop') %>%
    na.omit() %>%
    mutate(eval_type = recode(eval_type, !!!eval_labels),  # Rename eval types
           Rotation = str_replace(Rotation, "\\(.*\\)", ""))  # Remove parentheses content
  
  if (nrow(plot_data) == 0) {
    return(NULL)
  }
  else return(plot_data)
}

count_fac_eval <- function(data, resident_name) {
  plot_data <- data %>%
    filter(name == resident_name, redcap_repeat_instrument == "Faculty Evaluation") %>%
    group_by(rot) %>%
    summarize(Count = n(), .groups = 'drop') %>%
    na.omit()
  
  if (nrow(plot_data) == 0) {
    return(NULL)
  }
  else return(plot_data)
}

  
compute_eval_progress <- function(data, resident_name, count_function ) {
  plot_data <- count_function(data, resident_name)  # 🔹 Use standardized function
  
  if (is.null(plot_data)) {
    return(NULL)
  }
  
  plot_data %>%
    group_by(Level) %>%
    summarize(Total = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Progress = pmin((Total / 25) * 100, 100))  # Cap at 100%
}

#' Summary plot of evaluations completed by level:
generate_tot_eval_plot <- function(data, resident_name) {
  
  # Corrected eval_type labels
  plot_data <- count_res_assessments(data, resident_name)  # 🔹 Use standardized function
  
  if (is.null(plot_data) || !"Level" %in% colnames(plot_data) || all(is.na(plot_data$Level))) {
    message("No valid `Level` data found! Displaying default plot.")
    
    # 🔹 Return a default placeholder plot
    return(
      ggplot(data.frame(eval_type = c("Summative", "Observation", "Continuity Clinic"), 
                        Count = c(0, 0, 0)), aes(x = eval_type, y = Count, fill = eval_type)) +
        geom_bar(stat = "identity", width = 0.6) +
        scale_fill_manual(values = c("Summative" = "#E69F00", 
                                     "Observation" = "#56B4E9", 
                                     "Continuity Clinic" = "#009E73")) +
        labs(title = "No Evaluations Yet", x = "Evaluation Type", y = "Number of Evaluations") +
        theme_minimal()
    )
  }
  
  # Proceed with normal stacked bar chart if `Level` exists
  ggplot(plot_data, aes(x = eval_type, y = Count, fill = Rotation)) +
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    scale_fill_brewer(palette = "Set2", limits = unique(plot_data$Rotation)[1:8]) +
    theme_minimal() +
    labs(title = "Evaluations Completed by Level",
         x = "Evaluation Type",
         y = "Number of Evaluations",
         fill = "Rotation") +
    coord_flip() +
    facet_wrap(~Level, ncol = 1)  # Only facets if Level exists
}

#' Progress bar for total completed:
compute_eval_progress <- function(data, resident_name) {
  data %>%
    filter(name == resident_name) %>%
    group_by(Level) %>%
    summarize(Total = sum(Count, na.rm = TRUE), .groups = 'drop') %>%
    mutate(Progress = pmin((Total / 25) * 100, 100))  # Cap at 100%
}

### Display of 

# Helper function to display progress (with fallback message if no data)
display_progress <- function(progress_data) {
  if (is.null(progress_data) || nrow(progress_data) == 0) {
    return(div("No evaluations completed yet.", style = "text-align: center; font-size: 16px;"))
  }
  tagList(
    lapply(1:nrow(progress_data), function(i) {
      level <- progress_data$Level[i]
      total <- progress_data$Total[i]
      prog <- progress_data$Progress[i]
      
      div(
        style = "margin-bottom: 15px;",
        h5(level, style = "margin-bottom: 5px;"),
        div(
          class = "progress",
          div(
            class = "progress-bar",
            role = "progressbar",
            style = paste0("width: ", prog, "%;"),
            `aria-valuenow` = prog,
            `aria-valuemin` = "0",
            `aria-valuemax` = "100",
            paste0(prog, "%")
          )
        ),
        p(paste("Evaluations Completed:", total),
          style = "margin-top: 5px; font-size: 90%;")
      )
    })
  )
}
