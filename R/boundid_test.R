#' Evaluate (the extent of) boundary bias in a difference-in-differences setup
#'
#' @param Data An R dataframe.
#' @param X_var The outcome variable.
#' @param threshold The natural boundary of the outcome.
#' @param floor A logical statement. Should be set to TRUE if the boundary is a floor, or to FALSE if the boundary is a ceiling. Default is TRUE.
#' @param treatment The treatment variable.
#' @param time The time variable.
#' @param ID The unit ID.
#' @param first_period A numeric value indicating the first time period in which the treatment is administered.
#' @param S = A numeric value indicating the counterfactual fall. If NULL, the function automatically computes the counterfactual fall of the control group.
#' @param cut A logical statement. If TRUE, then the cutting/trimming approach will be used. If FALSE, then the weighting approach will be used.
#' @param more_info A logical statement. If TRUE, then additional information is included in the output, such as the distance of the furthest unit above or below the boundary (or the distance of the 1st percentile)
#' @param stag A logical statement. Should be TRUE if the design is staggered or FALSE for a basic 2x2 DiD.
#' @return The output of this function is a vector containing the newly generated weights to be included in the estimation step to counter-act boundary bias.
#' @examples
#' boundid_test(simulated_data, "X", 0, floor = T, simulated_data$treated, simulated_data$time, more_info = F, ID = "ID")
#' @export


boundid_test <- function(Data, X_var, threshold, floor = TRUE, treatment, time, ID, first_period, S = NULL, more_info = F, stag = FALSE) {

  if (stag) {
    # Extract the variables X, treatment and first period/group from the dataset
    data <- Data

    X <- data %>%
      dplyr::select(time, ID, treatment, X_var,first_period) %>%
    dplyr::rename(ID_alt = ID)  %>%
      dplyr::rename(X_var_alt = X_var)  %>%
      dplyr::rename(time_alt = time)  %>%
      dplyr::rename(treatment_alt = treatment)  %>%
      dplyr::rename(unique_g_values_alt = first_period)


    Tr <- data[[treatment]]
    g <- data[[first_period]]
    unique_g_values <- unique(g)

    # If t is not specified, set t to 0
    if (missing(t)) {
      t <- 0
    }

    # Find never-treated control groups per group gi


    # Step 1: Create subsets for each g
    X_A_0_list <- lapply(unique_g_values, function(unique_g_values) {
      X %>%
        group_by(ID_alt) %>% # Replace unit_id with your unique unit identifier column
        filter(any(treatment_alt != 1) & time_alt < unique_g_values) %>%
        ungroup()
    })

    # Step 2: Name the list elements by g value
    names(X_A_0_list) <- paste0("X_A_0_g", unique_g_values)



    # Step 3: Create subsets for each g
    X_A_1_list <- lapply(unique_g_values, function(unique_g_values) {
      X %>%
        group_by(ID_alt) %>% # Replace unit_id with your unique unit identifier column
        filter(any(treatment_alt != 1) & time_alt >= unique_g_values) %>%
        ungroup()
    })

    # Step 4: Name the list elements by g value
    names(X_A_1_list) <- paste0("X_A_1_g", unique_g_values)


    # Determine the counterfactual trend for each group gi

    # Step 5: Compute the mean for each element in X_A_0_list and X_A_1_list
    means_A_0 <- sapply(X_A_0_list, function(X) mean(X$X_var_alt, na.rm = TRUE)) # Replace `some_variable` with your target column
    means_A_1 <- sapply(X_A_1_list, function(X) mean(X$X_var_alt, na.rm = TRUE)) # Replace `some_variable` with your target column

    # Step 6: Compute the difference between X_A_1 and X_A_0 for each g
    differences <- means_A_1 - means_A_0

    # Step 7: Combine results into a named vector or data frame
    results <- data.frame(
      g_value = unique_g_values,
      mean_A_0 = means_A_0,
      mean_A_1 = means_A_1,
      difference = differences
    )
    results <- results %>%
      filter(!is.na(g_value))


    # Now get the pre-treatment values for each treatment group gi
    X_2_a_list <- lapply(unique_g_values, function(unique_g_values) {
      X %>%
        group_by(ID_alt) %>% # Replace unit_id with your unique unit identifier column
        filter(any(treatment_alt == 1) & time_alt < unique_g_values) %>%
        ungroup()
    })


    # Name the list elements by g value
    names(X_2_a_list) <- paste0("X_2_a_g", unique_g_values)

    # Calculate the mean for each treatment group g and ID within the subsets
    means_X_2_a <- lapply(X_2_a_list, function(X) {
      X %>%
        group_by(ID_alt, unique_g_values_alt) %>%
        summarise(mean_X = mean(X_var_alt, na.rm = TRUE), .groups = "drop")
    })



    # Add counterfactual trend

    # Convert the list to a single data frame for processing
    means_X_2_a_df <- bind_rows(means_X_2_a, .id = "g_value")

    # Join means with results to match unique_g_values
    adjusted_means <- means_X_2_a_df %>%
      inner_join(results, by = c("unique_g_values_alt" = "g_value")) %>%
      mutate(mean_X_adjusted = mean_X + difference)

    adjusted_means_filtered_long <- adjusted_means %>%
      filter(as.numeric(sub(".*_g(\\d+)$", "\\1", g_value)) == unique_g_values_alt)


    adjusted_means_filtered <- adjusted_means_filtered_long %>%
      # Add adjusted values based on the floor or ceiling logic
      mutate(
        mean_X_adjusted_X3 = if (floor) {
          pmax(mean_X_adjusted, threshold)
        } else {
          pmin(mean_X_adjusted, threshold)
        }
      ) %>%
      # Calculate mean per treatment group (g) for original and adjusted values
      group_by(unique_g_values_alt) %>%
      summarise(
        mean_X_g = mean(mean_X_adjusted, na.rm = TRUE),
        mean_X3_g = mean(mean_X_adjusted_X3, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Calculate differences and percentage differences
      mutate(
        difference = mean_X3_g - mean_X_g,
        percentage_difference = (difference / mean_X_g) * 100
      )

    # Display the results



    #

    # Perform t-test
    #t_test_result <- t.test(X_2, X_3_b, alternative = "two.sided",
      #                      paired = F, var.equal = TRUE, conf.level = 0.95)

    ## Print

    # Subheader
    cat("Difference between restricted and unrestricted mean:\n")


    # Print the percentage difference

    mean_per_unique_g <- adjusted_means_filtered_long %>%
      group_by(unique_g_values_alt) %>%
      summarise(mean_mean_X = mean(mean_X, na.rm = TRUE), .groups = "drop")


    # Print table headers
    cat(sprintf("%-10s %-35s %-35s %-25s\n", "Group",
                "Control Group Pre-Treatment Mean",
                "Treatment Group Pre-Treatment Mean",
                "Counterfactual Trend"))
    cat(sprintf("%-10s %-35s %-35s %-25s\n", "-----",
                "-----------------------------------",
                "-----------------------------------",
                "-------------------------"))

    # Print each row
    for (i in seq_along(results$g_value)) {
      cat(sprintf("%-10s %-35.2f %-35.2f %-25.2f\n",
                  results$g_value[i],
                  results$mean_A_0[i],
                  mean_per_unique_g$mean_mean_X[i],
                  results$difference[i]))
    }


    # Values below 0

    if (floor) {
      # Count values below the threshold for each g_value
      counts_below_boundary <- adjusted_means_filtered_long %>%
        group_by(unique_g_values_alt) %>%
        summarise(
          count_below = sum(mean_X_adjusted < threshold, na.rm = TRUE),
          total_count = n(),
          percentage_below = (count_below / total_count) * 100,
          .groups = "drop"
        )

      # Display results
      cat("\nValues below boundary per group (with percentages):\n")
      for (i in seq_along(counts_below_boundary$unique_g_values_alt)) {
        cat(sprintf("g_value: %s, Count Below: %d, Percentage Below: %.2f%%\n",
                    counts_below_boundary$unique_g_values_alt[i],
                    counts_below_boundary$count_below[i],
                    counts_below_boundary$percentage_below[i]))
      }
    } else {
      # Count values above the threshold for each g_value
      counts_above_boundary <- adjusted_means_filtered_long %>%
        group_by(unique_g_values_alt) %>%
        summarise(
          count_above = sum(mean_X_adjusted > threshold, na.rm = TRUE),
          total_count = n(),
          percentage_above = (count_above / total_count) * 100,
          .groups = "drop"
        )

      # Display results
      cat("\nValues above boundary per group (with percentages):\n")
      for (i in seq_along(counts_above_boundary$unique_g_values_alt)) {
        cat(sprintf("g_value: %s, Count Above: %d, Percentage Above: %.2f%%\n",
                    counts_above_boundary$unique_g_values_alt[i],
                    counts_above_boundary$count_above[i],
                    counts_above_boundary$percentage_above[i]))
      }
    }




    # Counterfactual Mean

    for (i in seq_along(adjusted_means_filtered$unique_g_values_alt)) {
      cat("\nFor g_value:", adjusted_means_filtered$unique_g_values_alt[i], "\n")
      cat("Counterfactual Mean with boundary: ", round(adjusted_means_filtered$mean_X3_g[i], 2), "\n")
      cat("Counterfactual Mean without boundary: ", round(adjusted_means_filtered$mean_X_g[i], 2), "\n")
      cat("Absolute Difference: ", round(adjusted_means_filtered$difference[i], 2), "\n")
    }


  } else {

   # Extract the variables X and treatment from the dataset
  data <- Data

  X <- data[[X_var]]

  # Subset X where T != 1 and time == 0 and 1
  X_A_0 <- X[treatment != 1 & time == 0]
  X_A_1 <- X[treatment != 1 & time == 1]

  # Calculate S if not provided
  if (is.null(S)) {
    mean_X_A_0 <- mean(X_A_0, na.rm = TRUE)
    mean_X_A_1 <- mean(X_A_1, na.rm = TRUE)
    S <- mean_X_A_1 - mean_X_A_0
  }

  # Subset X where treatment == 1 and time == t
  X_2_a <- X[treatment == 1 & time == 0]

  # Add S to X_2
  X_2 <- X_2_a + S

  # Calculate the mean and number of observations for X_2
  mean1 <- mean(X_2, na.rm = TRUE)

  # Create X_3 based on the threshold condition
  if (floor) {
    X_3 <- pmax(X_2, threshold)
    X_3_b <- pmax(X_2, threshold)
  } else {
    X_3 <- pmin(X_2, threshold)
    X_3_b <- pmin(X_2, threshold)
  }

  # Calculate the mean and number of observations for X_3
  mean2 <- mean(X_3, na.rm = TRUE)

  # Calculate the difference
  difference <- mean2 - mean1

  # Calculate percentage difference
  percentage_difference <- (difference / mean1) * 100

  # Calculate the smallest value of X_2 based on the value of floor
  if (floor) {
    extreme_value <- min(X_2, na.rm = TRUE)
    non_outlier_value <- quantile(X_2, probs = 0.01, na.rm = TRUE)
  } else {
    extreme_value <- max(X_2, na.rm = TRUE)
    non_outlier_value <- quantile(X_2, probs = 0.99, na.rm = TRUE)
  }




  #
  if (floor) {
    count_below_boundary <- sum(X_2 < threshold, na.rm = TRUE)
  } else {
    count_above_boundary <- sum(X_2 > threshold, na.rm = TRUE)
  }



  #

  # Perform t-test
  t_test_result <- t.test(X_2, X_3_b, alternative = "two.sided",
                          paired = F, var.equal = TRUE, conf.level = 0.95)

  # Subheader
  cat("Difference between restricted and unrestricted mean:\n")


  # Print the percentage difference

  # Pre-treatment mean
  cat("\nControl Group Pre-Treatment  Mean: ", mean_X_A_0, "\n")
  cat("Treatment Group Pre-Treatment  Mean: ", mean(X_2_a, na.rm = T), "\n")

  # Size of the fall
  cat("Counterfactual Trend: ", S, "\n")

  # Values below 0

  if (floor) {
    cat("\nValues below boundary:", count_below_boundary, "\n")
  } else {
    cat("\nValues above boundary:", count_above_boundary, "\n")
  }



  # Counterfactual Mean
  cat("Counterfactual Mean with boundary: ", mean2, "\n")
  cat("Counterfactual Mean without boundary: ", mean1, "\n")
  # Print the absolute difference
  cat("Absolute Difference: ", difference, "\n")

  # Subheader for t-test results
  cat("\nTwo Sample t-test Results:\n")


  # Extract and print specific values from the t-test result
  cat("p-value: ", t_test_result$p.value, "\n")
  cat("t-statistic: ", t_test_result$statistic, "\n")
  cat("degrees of freedom: ", t_test_result$parameter, "\n")
  cat("confidence interval: ", t_test_result$conf.int, "\n")


  if (more_info) {

  # Subheader for Distance to 0
  cat("\nFull Distance to 0:", extreme_value, "\n")

  # Subheader for Distance to 0
  cat("(1st Percentile) Distance to 0:", non_outlier_value, "\n")

  }

  }
}





