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



### Display functions:

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

#' Stylized DT:
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


#'------
#Observation data:

#' @export
#' @title Parse Labels for ip_obs_type
#' @description Extracts only the textual labels for all possible choices
#'              from the REDCap dictionary's `select_choices_or_calculations`
#'              for a given field (default "ip_obs_type").
#'
#' @param dict A data frame or tibble of your REDCap dictionary, must have
#'             `field_name` and `select_choices_or_calculations`
#' @param ip_obs_field Character name of the field (default: "ip_obs_type")
#'
#' @return A character vector of labels (e.g., "Written H&P", "Verbal presentation", "Other").
parse_ip_obs_labels <- function(dict, ip_obs_field = "ip_obs_type") {
  dict_row <- dict %>%
    dplyr::filter(field_name == ip_obs_field)
  
  # e.g. "1, Written H&P | 2, Verbal presentation of H&P | 3, Physical Exam | ..."
  dict_string <- dict_row %>%
    dplyr::pull(select_choices_or_calculations) %>%
    .[1]
  
  # Split on '|'
  choice_list <- str_split(dict_string, "\\|")[[1]]
  
  # 1) Trim each piece
  # 2) Discard empty lines
  choice_list <- choice_list %>%
    map_chr(str_trim) %>%
    discard(~ .x == "")
  
  # Now parse out the label portion. If it's "1, Written H&P", we do:
  labels_only <- map_chr(choice_list, ~ {
    # Your existing regex or code to parse out the text
    # If you have "1, Written H&P"
    # match[1] is full text, match[2] is "1", match[3] is "Written H&P"
    match <- str_match(.x, "^(\\d+)\\s*,\\s*(.*)$")
    match[, 3]  # The label part
  })
  
  labels_only <- labels_only[!is.na(labels_only)]
  labels_only
}

#' @export
#' @title Summarize Observations
#' @description 
#'   Given a data frame of observations (with a text column for ip_obs_type),
#'   filter for one resident, count how many times each type appears,
#'   and include zero-count labels for any unused types.
#'
#' @param data A data frame with columns:
#'   - `name`: Resident name
#'   - `ip_obs_type` (by default) or a custom column specified by `ip_obs_field`
#' @param resident The resident's name (character)
#' @param labels A character vector of all possible labels (from parse_ip_obs_labels)
#' @param ip_obs_field The column name in `data` storing the text labels (default "ip_obs_type")
#'
#' @return A tibble with columns `ip_obs_type_label` and `Count`.
#'         If a label doesn't appear in the data, Count is 0.
summarize_observations <- function(data,
                                   resident,
                                   labels,
                                   ip_obs_field = "ip_obs_type") {
  
  # 1) Filter data for the resident, removing NA ip_obs_type
  plot_data <- data %>%
    dplyr::filter(
      .data[["name"]] == resident,
      !is.na(.data[[ip_obs_field]])
    ) %>%
    # <--- NEW: Trim or standardize the strings in your data so they match dictionary labels
    dplyr::mutate(
      # For case-insensitive matching, you could do stringr::str_to_lower(...). 
      # For now, let's just trim whitespace:
      temp_label = stringr::str_trim(.data[[ip_obs_field]])
    ) %>%
    # 2) Count how many times each label appears
    dplyr::group_by(temp_label) %>%
    dplyr::summarize(Count = dplyr::n(), .groups = "drop") %>%
    dplyr::rename(ip_obs_type_label = temp_label)
  
  # 3) Convert 'labels' to a tibble so we can right-join 
  all_labels <- tibble::tibble(ip_obs_type_label = labels)
  
  # 4) Right-join -> keep ALL labels
  final_df <- dplyr::left_join(all_labels, plot_data, by = "ip_obs_type_label") %>%
    dplyr::mutate(Count = tidyr::replace_na(Count, 0))
  
  # 5) Sort them in the same order as `labels`
  final_df <- final_df %>%
    dplyr::arrange(factor(ip_obs_type_label, levels = labels))
  
  final_df
}

#' @export
#' @title Compute Observation Progress
#' @description 
#'   Given a data frame with columns `ip_obs_type_label` and `Count`, 
#'   add a `Progress` column (0–100).
#'
#' @param summarized_df A data frame with columns `ip_obs_type_label` and `Count`.
#' @param target Numeric, how many of each type do we consider 100% (default 5).
#'
#' @return A modified data frame with `Progress` (integer 0–100),
#'         plus the original columns.
compute_obs_progress <- function(summarized_df, target = 5) {
  if (nrow(summarized_df) == 0) {
    return(summarized_df)
  }
  
  summarized_df %>%
    dplyr::mutate(
      # Round to nearest whole number and cap at 100
      Progress = pmin(round((Count / target) * 100), 100)
    )
}


prepare_progress_data <- function(summarized_df, target = 5) {
  summarized_df %>%
    compute_obs_progress(target = target) %>%
    dplyr::rename(
      Level = ip_obs_type_label,
      Total = Count
    )  # Keep `Progress` as is
}


prep_obs_table <- function(data,
                           dict,
                           resident,
                           selected_label,
                           caption = NULL) {
  # 1) Filter rows for the chosen resident & label
  filtered_data <- data %>%
    dplyr::filter(name == resident,
                  ip_obs_type == selected_label)
  
  # If there's no matching data, return the empty styled DT
  if (nrow(filtered_data) == 0) {
    return(create_styled_dt(data.frame(), 
                            caption = paste("No data for:", selected_label)))
  }
  
  # 2) Keep columns that are not all NA, plus columns we always include
  always_include_cols <- c("Date", "ip_obs_type", "Level", "Evaluator")
  non_empty <- colSums(!is.na(filtered_data)) > 0
  non_empty[intersect(always_include_cols, names(filtered_data))] <- TRUE
  final_data <- filtered_data[, non_empty, drop = FALSE]
  
  # 3) Remove columns we never want to display
  unwanted_cols <- c(
    "assess_a_resident_timestamp", "ass_date", "clin_context", 
    "slu_gim_att", "att_sign", "assess_a_resident_complete", 
    "week", "year", "name", "eval_type", "weekyr", 
    "start_year", "academic_year_start", "record_id", "ip_obs_type"
  )
  final_data <- final_data[, setdiff(names(final_data), unwanted_cols), drop = FALSE]
  
  # 4) Rename columns from field_name -> field_label if they match your dict
  rename_map <- setNames(dict$field_label, dict$field_name)
  final_data <- dplyr::rename_with(
    final_data,
    .cols = dplyr::everything(),
    .fn = ~ ifelse(.x %in% names(rename_map), rename_map[.x], .x)
  )
  
  # 5) Reorder columns so "Date", "Rotation", and "Level" go first
  front_cols <- c("Date", "Rotation", "Level")
  front_cols <- intersect(front_cols, names(final_data))
  other_cols <- setdiff(names(final_data), front_cols)
  final_data <- final_data[, c(front_cols, other_cols), drop = FALSE]
  
  # 6) Finally, return your **styled** DT
  create_styled_dt(final_data,
                   caption = if (is.null(caption)) {
                     paste("Details for:", selected_label)
                   } else {
                     caption
                   })
}


