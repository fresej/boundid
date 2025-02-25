#' Prepare weighting or trimming parameters before the estimation step
#'
#' @param Data An R dataframe.
#' @param X_var The outcome variable.
#' @param threshold The natural boundary of the outcome.
#' @param floor A logical statement. Should be set to TRUE if the boundary is a floor, or to FALSE if the boundary is a ceiling. Default is TRUE.
#' @param treatment The treatment variable.
#' @param time The time variable.
#' @param ID The unit ID.
#' @param group2_mean A numeric value indicating the pre-treatment mean of the treatment group.
#' @param ATU A logical statement. TRUE if the ATU is the target estimand.
#' @param ATT A logical statement. TRUE if the ATT is the target estimand.
#' @param S A numeric value indicating the counterfactual fall. If NULL, the function automatically computes the counterfactual fall of the control group.
#' @param cut A logical statement. If TRUE, then the cutting/trimming approach will be used. If FALSE, then the weighting approach will be used.
#' @param panel A logical statement. If TRUE, then the function generates weights for the whole panel. If FALSE, then the function generates weights for two time periods.
#' @return The output of this function is a vector containing the newly generated weights to be included in the estimation step to counter-act boundary bias.
#' @examples
#' new_ATT_weights <- boundid_adjust(simulated_data, "X", 0, floor = T,
#' treatment = simulated_data$treated, time = simulated_data$time,
#' ID = simulated_data$ID, ATT = T, cut = F, panel = F,orig_ID = simulated_data$ID)
#' @export

boundid_adjust <- function(Data, X_var, threshold, floor = TRUE, treatment, time, group2_mean = NULL, ID, ATU = F, ATT = F, S = NULL, cut = FALSE, panel = F, orig_ID) {

  # Extract the variables X and treatment from the dataset
  data <- Data

  X <- data[[X_var]]


  # Subset indices where T != 1 and time == 0 and 1 for treated units
  index_A_0 <- which(treatment != 1 & time == 0)
  index_A_1 <- which(treatment != 1 & time == 1)

  # Calculate S if not provided
  if (is.null(S)) {
    mean_X_A_0 <- mean(X[index_A_0], na.rm = TRUE)
    mean_X_A_1 <- mean(X[index_A_1], na.rm = TRUE)
    S <- mean_X_A_1 - mean_X_A_0
  }


  # Subset indices where treatment == 1 and time == t for treated units
  index_2_a <- which(treatment == 1 & time == 0)

  # Subset X based on indices for treated units
  X_A_0 <- X[index_A_0]
  X_A_1 <- X[index_A_1]
  X_2_a <- X[index_2_a]


  # Add S to X_2 for treated units
  X_2 <- X_2_a + S

  if (ATU == TRUE) {
    # Apply "cut" if specified
    if (cut) {
      # Assign weights based on the sign of values

      if (floor) {
        weights <- ifelse(X_2 < threshold, 0, 1)
      } else {
        weights <- ifelse(X_2 > threshold, 0, 1)
      }

    } else {
      objective_function <- function(weights, group2_mean, X_2_a, S) {

        # Scale weights based on the distances from S, giving higher weights to positive distances
        scaled_weights <- abs(weights)

        # Multiply weights by values to give larger weights to larger values
        weighted_values <- scaled_weights * X_2_a

        # Calculate the adjusted weighted mean
        adjusted_mean <- sum(weighted_values) / sum(scaled_weights)

        # Return the squared difference between the adjusted weighted mean and group2_mean
        (adjusted_mean - group2_mean)^2
      }



      # Set an initial guess for weights (all equal)
      initial_weights <- rep(1, length(X_2_a))

      # Find the weights that minimize the objective function subject to the constraints
      result <- optim(par = initial_weights,
                      fn = objective_function,
                      method = "L-BFGS-B",
                      lower = rep(0.001, length(initial_weights)),  # Set lower bounds for weights
                      X_2_a = X_2_a,
                      S = S,
                      group2_mean = group2_mean)

      # Extract the optimized weights from the result
      optimized_weights <- pmax(result$par, 0)


      # Normalize weights to sum to 1
      weights <- (optimized_weights / sum(abs(optimized_weights)))
    }

    if (panel == FALSE) {
      # Create a data frame with ID and corresponding weights for treated units
      treated_data <- data.frame(ID = ID[index_2_a], weights = weights, value = X_2_a)


      # Create a data frame with ID and a weight of 1 for untreated units
      untreated_indices <- which(treatment == 0)
      untreated_data <- data.frame(ID = ID[untreated_indices], weights = rep(1, length(untreated_indices)), value = X[untreated_indices])

      # Merge treated and untreated data, maintaining the original order of IDs from the input dataset
      merged_data <- rbind(treated_data, untreated_data)
      merged_data <- merged_data[match(ID, merged_data$ID), ] # Match the order of IDs



      return(merged_data$weights)
    } else {

      treated_data <- data.frame(ID = ID[index_2_a], weights = weights, value = X_2_a)



      # Create a data frame with ID and a weight of 1 for untreated units
      untreated_indices <- which(treatment == 0)
      untreated_data <- data.frame(ID = ID[untreated_indices], weights = rep(1, length(untreated_indices)), value = X[untreated_indices])

      # Merge treated and untreated data, maintaining the original order of IDs from the input dataset
      merged_data <- rbind(treated_data, untreated_data)
      merged_data <- merged_data[match(ID, merged_data$ID), ] # Match the order of IDs

      ##
      ID <- orig_ID

      ID <- as.factor(ID)
      merged_data$ID <- as.factor(merged_data$ID)
      unique_merged_data <- merged_data[!duplicated(merged_data$ID), ]


      new_weights_data_frame <- left_join(as.data.frame(ID),unique_merged_data, by = "ID")

      return(new_weights_data_frame$weights)

    }
  }

  if (ATT == T) {

    if (cut) {

      if (floor) {
        # Create the control_data dataframe
        control_data <- data.frame(ID = ID[index_A_0], value = X_A_0)

        # Sort the values of group_A
        sorted_control_data <- control_data[order(control_data$value), ]

        # Target mean
        if (is.null(group2_mean)) {
          target_mean <- mean(X_2_a, na.rm = TRUE)
        } else {
          target_mean <- group2_mean
        }
      }

      else {
        # Create the control_data dataframe
        control_data <- data.frame(ID = ID[index_A_0], value = X_A_0)

        # Sort the values of group_A
        sorted_control_data <- control_data[order(control_data$value, decreasing = TRUE), ]

        # Target mean
        if (is.null(group2_mean)) {
          target_mean <- mean(X_2_a, na.rm = TRUE)
        } else {
          target_mean <- group2_mean
        }
      }

      # Initialize variables
      current_sum <- 0
      selected_units <- NULL
      previous_selected_units <- NULL
      selected_unit_ids <- NULL
      previous_selected_unit_ids <- NULL

      # Iterate through sorted values and calculate mean
      for (i in 1:nrow(sorted_control_data)) {
        value <- sorted_control_data$value[i]
        unit_id <- sorted_control_data$ID[i]

        selected_units <- c(selected_units, value)
        selected_unit_ids <- c(selected_unit_ids, unit_id)

        current_sum <- sum(selected_units)
        current_mean <- current_sum / length(selected_units)


        # Check if mean exceeds the target mean by more than 0.2
        if (current_mean >= target_mean) {
          # Revert to the previous subset

          selected_units <- previous_selected_units
          selected_unit_ids <- previous_selected_unit_ids
          break
        }

        # Update previous subset
        previous_selected_units <- selected_units
        previous_selected_unit_ids <- selected_unit_ids
      }

      # Assign weights
      weights <- ifelse(control_data$ID %in% selected_unit_ids, 1, 0)

      if (panel == FALSE) {
        # Print the result
        untreated_data <- data.frame(ID = control_data$ID, value = control_data$value, weight = weights)

        # Create a data frame with ID and a weight of 1 for untreated units
        treated_indices <- which(treatment == 1)
        treated_data <- data.frame(ID = ID[treated_indices], weight = rep(1, length(treated_indices)), value = X[treated_indices])

        # Merge treated and untreated data, maintaining the original order of IDs from the input dataset
        merged_data <- rbind(untreated_data, treated_data)
        merged_data <- merged_data[match(ID, merged_data$ID), ]

        return(merged_data$weight)








      } else {
        # Print the result
        untreated_data <- data.frame(ID = control_data$ID, value = control_data$value, weight = weights)

        # Create a data frame with ID and a weight of 1 for untreated units
        treated_indices <- which(treatment == 1)
        treated_data <- data.frame(ID = ID[treated_indices], weight = rep(1, length(treated_indices)), value = X[treated_indices])

        # Merge treated and untreated data, maintaining the original order of IDs from the input dataset
        merged_data <- rbind(untreated_data, treated_data)

        # Reorder based on original IDs
        merged_data_2 <- merged_data[match(ID, merged_data$ID), ]

        # Return the weights

        ##
        ID <- orig_ID

        ID <- as.factor(ID)
        merged_data_2$ID <- as.factor(merged_data_2$ID)
        unique_merged_data <- merged_data_2[!duplicated(merged_data_2$ID), ]


        new_weights_data_frame <- left_join(as.data.frame(ID),unique_merged_data, by = "ID")


        return(new_weights_data_frame$weight)
      }
    }      else {

      # group2_mean
      if (is.null(group2_mean)) {
        group2_mean <- mean(X_2_a, na.rm = TRUE)
      } else {
        group2_mean <- group2_mean
      }

      objective_function <- function(weights, group2_mean, X_A_0) {

        # Scale weights based on the distances from S, giving higher weights to positive distances
        scaled_weights <- abs(weights)

        # Multiply weights by values to give larger weights to larger values
        weighted_values <- scaled_weights * X_A_0

        # Calculate the adjusted weighted mean
        adjusted_mean <- sum(weighted_values) / sum(scaled_weights)

        # Return the squared difference between the adjusted weighted mean and group2_mean
        (adjusted_mean - group2_mean)^2
      }



      # Set an initial guess for weights (all equal)
      initial_weights <- rep(1, length(X_A_0))

      # Find the weights that minimize the objective function subject to the constraints
      result <- optim(par = initial_weights,
                      fn = objective_function,
                      method = "L-BFGS-B",
                      lower = rep(0.001, length(initial_weights)),  # Set lower bounds for weights
                      X_A_0 = X_A_0,
                      group2_mean = group2_mean)

      # Extract the optimized weights from the result
      optimized_weights <- pmax(result$par, 0)


      # Normalize weights to sum to 1
      weights <- (optimized_weights / sum(abs(optimized_weights)))
    }

    if (panel == FALSE) {
      # Create a data frame with ID and corresponding weights for treated units
      untreated_data <- data.frame(ID = ID[index_A_0], weights = weights, value = X_A_0)



      # Create a data frame with ID and a weight of 1 for untreated units
      treated_indices <- which(treatment == 1)
      treated_data <- data.frame(ID = ID[treated_indices], weights = rep(1, length(treated_indices)), value = X[treated_indices])

      # Merge treated and untreated data, maintaining the original order of IDs from the input dataset
      merged_data <- rbind(treated_data, untreated_data)
      merged_data <- merged_data[match(ID, merged_data$ID), ] # Match the order of IDs

      return(merged_data$weights)
    } else {

      untreated_data <- data.frame(ID = ID[index_A_0], weights = weights, value = X_A_0)



      # Create a data frame with ID and a weight of 1 for untreated units
      treated_indices <- which(treatment == 1)
      treated_data <- data.frame(ID = ID[treated_indices], weights = rep(1, length(treated_indices)), value = X[treated_indices])

      # Merge treated and untreated data, maintaining the original order of IDs from the input dataset
      merged_data <- rbind(treated_data, untreated_data)
      merged_data <- merged_data[match(ID, merged_data$ID), ] # Match the order of IDs

      ##
      ID <- orig_ID

      ID <- as.factor(ID)
      merged_data$ID <- as.factor(merged_data$ID)
      unique_merged_data <- merged_data[!duplicated(merged_data$ID), ]

      new_weights_data_frame <- left_join(as.data.frame(ID),unique_merged_data, by = "ID")

      return(new_weights_data_frame$weights)
    }
  }
}
