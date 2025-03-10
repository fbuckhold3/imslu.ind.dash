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


#' Code form imres but not uploaded yet. DELETE when done:
#'-------------------------------------------------------------------------
#'
# Plus delta function for plus delta mod:
#' @export
#' @title plus delta function
#' @description Get data of plus-delta
#' @param id eval data, resident is resident nam
#' @return datatable of plus-delta
generate_p_d <- function(data, resident) {
  data %>%
    filter(name == resident) %>%
    select(Date, Rotation, Level, cc_res_does_well, res_to_improve, min_giv_feedback, Evaluator) %>%
    dplyr::rename(Plus = cc_res_does_well, Delta = res_to_improve, Feedback = min_giv_feedback) %>%
    filter(!(is.na(Rotation) & is.na(Plus) & is.na(Delta))) 
}


#' Create Continuity Clinic (CC) Evaluation Completion Table
#'
#' Creates a reactable table displaying the completion status of evaluations 
#' organized by level and evaluation type, filtered by a specific name.
#' Always includes the four standard evaluation types across all quarters.
#'
#' @param data A dataframe containing columns: name, Level, cc_eval_type, and Evaluator
#' @param name The name to filter the data by
#'
#' @return A reactable table object
#' @export
#'
#' @importFrom reactable reactable colDef colGroup reactableTheme
#' @importFrom htmltools div
#' @importFrom dplyr filter arrange
create_cc_table <- function(data, name) {
  # Ensure required packages are available
  if (!requireNamespace("reactable", quietly = TRUE)) {
    stop("Package 'reactable' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("Package 'htmltools' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # Define the standard evaluation types exactly as they appear in the data
  standard_eval_types <- c(
    "1st quarter - inbasket coverage",
    "2nd quarter - summative evaluation",
    "3rd quarter - documentation",
    "4th quarter - summative evaluation"
  )
  
  # Define display names for the columns
  display_names <- c(
    "Q1 - Inbasket", 
    "Q2 - Summative", 
    "Q3 - Documentation", 
    "Q4 - Summative"
  )
  
  # Filter data by name and remove NAs in cc_eval_type
  filtered_data <- data %>% 
    dplyr::filter(name == !!name, !is.na(cc_eval_type)) %>%
    dplyr::arrange(Level)
  
  # Create table data with all levels
  result_df <- data.frame(Level = unique(filtered_data$Level), stringsAsFactors = FALSE)
  
  # If no levels found, create a placeholder row
  if (nrow(result_df) == 0) {
    result_df <- data.frame(Level = "No data available", stringsAsFactors = FALSE)
  }
  
  # Add columns for each standard evaluation type
  for (i in 1:length(standard_eval_types)) {
    et <- standard_eval_types[i]
    display_name <- display_names[i]
    
    # Initialize with FALSE (incomplete) for all levels
    result_df[[paste0(display_name, " Status")]] <- FALSE
    result_df[[paste0(display_name, " Evaluator")]] <- NA
    
    # Fill in data for each level if available
    for (j in 1:nrow(result_df)) {
      level_val <- result_df$Level[j]
      
      # Skip if placeholder row
      if (level_val == "No data available") next
      
      # Find matching records
      matching_rows <- filtered_data %>% 
        dplyr::filter(Level == level_val, cc_eval_type == et)
      
      if (nrow(matching_rows) > 0) {
        # Mark as complete and add evaluator name
        result_df[[paste0(display_name, " Status")]][j] <- TRUE
        result_df[[paste0(display_name, " Evaluator")]][j] <- matching_rows$Evaluator[1]
      }
    }
  }
  
  # Theme for the table
  custom_theme <- reactable::reactableTheme(
    borderColor = "#dfe2e5",
    stripedColor = "#f6f8fa",
    highlightColor = "#f0f5ff",
    cellPadding = "10px 12px",
    style = list(
      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif",
      fontSize = "14px"
    ),
    headerStyle = list(
      backgroundColor = "#f1f3f5",
      color = "#212529",
      fontWeight = 600,
      borderBottom = "2px solid #dee2e6"
    )
  )
  
  # Define columns for reactable
  cols <- list(
    Level = reactable::colDef(
      name = "Level",
      minWidth = 120,
      cell = function(value) {
        htmltools::div(
          style = "font-weight: 600; color: #1a73e8;",
          value
        )
      }
    )
  )
  
  # Add status and evaluator columns for each evaluation type
  column_groups <- list()
  for (i in 1:length(display_names)) {
    display_name <- display_names[i]
    
    # Create column names
    status_col <- paste0(display_name, " Status")
    eval_col <- paste0(display_name, " Evaluator")
    
    # Status column with checkmark or O
    cols[[status_col]] <- reactable::colDef(
      name = display_name,
      cell = function(value) {
        if (isTRUE(value)) {
          htmltools::div(
            style = "display: flex; justify-content: center; align-items: center;",
            htmltools::div(
              style = "background-color: #e6f4ea; color: #137333; border-radius: 50%; width: 30px; height: 30px; display: flex; justify-content: center; align-items: center; font-size: 18px;",
              "✓"
            )
          )
        } else {
          htmltools::div(
            style = "display: flex; justify-content: center; align-items: center;",
            htmltools::div(
              style = "background-color: #fce8e6; color: #c5221f; border-radius: 50%; width: 30px; height: 30px; display: flex; justify-content: center; align-items: center; font-weight: bold; font-size: 18px;",
              "O"
            )
          )
        }
      },
      width = 120,
      align = "center"
    )
    
    # Evaluator column
    cols[[eval_col]] <- reactable::colDef(
      name = "Evaluator",
      cell = function(value) {
        if (is.na(value)) {
          htmltools::div(style = "color: #999; font-style: italic;", "Not assigned")
        } else {
          htmltools::div(style = "font-weight: 500;", value)
        }
      },
      width = 130
    )
    
    # Add to column groups
    column_groups[[length(column_groups) + 1]] <- reactable::colGroup(
      name = display_name,
      columns = c(status_col, eval_col)
    )
  }
  
  # Create the reactable with improved styling
  reactable::reactable(
    result_df,
    columns = cols,
    bordered = TRUE,
    highlight = TRUE,
    striped = TRUE,
    theme = custom_theme,
    columnGroups = column_groups
  )
}



#' Create a styled DataTable for CC evaluation data
#' @param data The processed data to display
#' @param caption Table caption/title
#' @return A styled DT::datatable
#' @export
create_styled_dt <- function(data, caption = NULL) {
  if (nrow(data) == 0) {
    return(DT::datatable(
      data.frame(Message = paste0("No ", tolower(caption), " data available")),
      options = list(dom = 't'),
      caption = caption,
      rownames = FALSE
    ))
  }
  
  DT::datatable(
    data,
    options = list(
      pageLength = 5,
      dom = 'ftp',
      scrollX = TRUE,
      columnDefs = list(list(
        targets = "_all",
        render = DT::JS(
          "function(data, type, row) {
            if (data === null || data === '') {
              return '<span style=\"color: #999; font-style: italic;\">Not provided</span>';
            }
            return data;
          }"
        )
      ))
    ),
    caption = caption,
    rownames = FALSE,
    class = 'cell-border stripe hover'
  ) %>%
    DT::formatStyle(
      columns = names(data),
      backgroundColor = '#f8f9fa',
      borderColor = '#dfe2e5'
    )
}







process_summative_data <- function(data, resident_name, level) {
  cat("Function called with resident:", resident_name, "and level:", level, "\n")
  
  cc_names <- cc_names <- c(
    `Intern Presentation` = 'cc_intern_pc1_1', 
    Differential = 'cc_intern_pc3_1', 
    `Health Promotion` = 'cc_intern_pc5_1', 
    `Chronic Management` = 'cc_intern_pc5_2', 
    `Minimize unfamiliar terms (I)` = 'cc_intern_ics1_2', 
    `Shared Decision-Making (I)` = 'cc_intern_ics1_1', 
    Respect = 'cc_intern_prof1', 
    `Takes feedback (I)` = 'cc_intern_pbl2_1', 
    `Acknowledge errors` = 'cc_intern_pbl2_2', 
    `Presentation PGY2` = 'cc_pgy2_pc1_1', 
    Documentation = 'cc_pgy2_ics3_1', 
    `Reflection on practice` = 'cc_pgy2_pbl2_2', 
    `Care coordination (2)` = 'cc_pgy2_sbp2_1', 
    `Use Evidence (2)` = 'cc_pgy2_pbl1', 
    `Shared Decision-Making (2)` = 'cc_pgy2_ics1_1', 
    `Teamwork (2)` = 'cc_pgy2_ics2_2', 
    `Takes feedback (2)` = 'cc_pgy_pbl2_1', 
    `Minimize unfamiliar terms (2)` = 'cc_pgy2_ics_1_2', 
    `Presentation PGY3` = 'cc_pgy3_pc1_1', 
    `Teamwork (3)` = 'cc_pgy3_ics2_2', 
    `Shared Decision-Making (3)` = 'cc_pgy3_ics1_1', 
    `Minimize unfamiliar terms (3)` = 'cc_pgy3_ics1_2', 
    `Care coordination (3)` = 'cc_pgy3_sbp2_1', 
    `Use Evidence (3)` = 'cc_pgy3_pbl1', 
    `Takes feedback (3)` = 'cc_pgy3_pbl2_1', 
    `Acknowledge errors (3)` = 'cc_pgy3_pbl2_2'
  )
  
  tryCatch({
    # Get all summative evaluations for this resident
    summative_evals <- data %>%
      filter(name == resident_name,
             !is.na(cc_eval_type),
             cc_eval_type %in% c("2nd quarter - summative evaluation", "4th quarter - summative evaluation"))
    
    cat("Found", nrow(summative_evals), "summative evaluations\n")
    
    # Instead of filtering by Level, which isn't working correctly,
    # let's try to determine the level based on the variable names in each record
    if (level == "Intern") {
      # Check for Intern-specific variables being non-NA
      filtered_data <- summative_evals %>%
        filter(!is.na(cc_intern_pc1_1) | !is.na(cc_intern_pc3_1) | !is.na(cc_intern_pc5_1) |
                 !is.na(cc_intern_ics1_2) | !is.na(cc_intern_ics1_1) | !is.na(cc_intern_prof1))
    } else if (level == "PGY2") {
      # Check for PGY2-specific variables being non-NA
      filtered_data <- summative_evals %>%
        filter(!is.na(cc_pgy2_pc1_1) | !is.na(cc_pgy2_ics3_1) | !is.na(cc_pgy2_pbl2_2) |
                 !is.na(cc_pgy2_ics1_1) | !is.na(cc_pgy2_ics2_2))
    } else if (level == "PGY3") {
      # Check for PGY3-specific variables being non-NA
      filtered_data <- summative_evals %>%
        filter(!is.na(cc_pgy3_pc1_1) | !is.na(cc_pgy3_ics2_2) | !is.na(cc_pgy3_ics1_1) |
                 !is.na(cc_pgy3_pbl2_1) | !is.na(cc_pgy3_pbl2_2))
    } else {
      # If no specific level filtering, just use all summative evals
      filtered_data <- summative_evals
    }
    
    cat("After level-specific variable filtering, row count:", nrow(filtered_data), "\n")
    
    if (nrow(filtered_data) > 0) {
      filtered_data <- filtered_data %>%
        rename(any_of(cc_names)) %>%
        select(-any_of(c("Resident", "ID", "assess_a_resident_timestamp", 
                         "clin_context", "res_to_improve", "cc_res_does_well", 
                         "min_giv_feedback", "assess_a_resident_complete", 
                         "Rotation", "eval_type", "Year", "Level", "cc_csm_att",
                         "att_sign", "week", "year", "name", "start_year", 
                         "academic_year_start", "record_id"))) %>%
        select_if(~ any(!is.na(.)) & any(. != ""))
      
      cat("Final filtered data row count:", nrow(filtered_data), "\n")
      return(filtered_data)
    } else {
      return(data.frame(Message = paste("No summative data available for", level)))
    }
  }, error = function(e) {
    cat("Error caught:", e$message, "\n")
    return(data.frame(Message = paste("Error processing summative data:", e$message)))
  })
}

### Plus/delta:
#' @export
process_cc_pd_data <- function(data, resident_name) {
  data %>%
    filter(name == resident_name, (Rotation == "VA Continuity clinic") | (Rotation == "CSM Continuity clinic (including metabolism clinic)")) %>%
    select(Date, Level, cc_eval_type, cc_res_does_well, res_to_improve, min_giv_feedback, Evaluator) %>%
    rename(Quarter = cc_eval_type, Plus = cc_res_does_well, Delta = res_to_improve, Feedback = min_giv_feedback)
}

#inbasket:
#' @export
process_cc_inbasket_data <- function(data, resident_name) {
  bask <- c("cc_inb_resp", "cc_inb_resu", "cc_inb_mych", "cc_inb_comm", "cc_inb_comp", "Level", "Evaluator")
  data %>%
    filter(name == resident_name, cc_eval_type == "1st quarter - inbasket coverage") %>%
    select(starts_with('cc_'), Level, Evaluator) %>%
    select(any_of(bask)) %>%
    relocate('Level') %>%
    rename(Responsible = cc_inb_resp, `Result Response` = cc_inb_resu, MyChart = cc_inb_mych, Paperwork = cc_inb_comm, `Comms with preceptor` = cc_inb_comp)
}

# documentation
#' @export
process_cc_document_data <- function(data, resident_name) {
  data %>%
    filter(name == resident_name, cc_eval_type == "3rd quarter - documentation") %>%
    select(Level, cc_doc_update, cc_doc_review, cc_doc_medall, cc_doc_remind, cc_doc_diag, cc_doc_notes, cc_doc_comp, Evaluator) %>%
    rename(`Update EMR` = cc_doc_update, `Review EMR` = cc_doc_review, `Update meds` = cc_doc_medall, `Does reminders` = cc_doc_remind, `Accurate Diagnoses` = cc_doc_diag, `Notes clear` = cc_doc_notes, `Notes complete` = cc_doc_comp)
}


