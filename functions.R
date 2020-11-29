##### Functions for: Welfare outcomes of segmentations under noise - related to Cummings et al (2020)
# main file: main.R
# Author: Erik Senn, erik.senn@gmx.de

# Function to return firm optimal prices and welfare outcomes given segmentation
optimal_pricing_given_segmentation <-
  function(segmentation,
            pricing_rule,
            weight_consumer,
            certainty_level,
            value_set,
            type_set,
            pdf_value,
            pdf_type,
            pdf_value_f_t_fct, 
           supress_print_messages) {
    # Input control:
    # normalize segmentation: Sum same type across segments needs to be 1
    segmentation <-  t(t(segmentation) / colSums(segmentation))
    if (supress_print_messages==F){
      print("Sum same type across segments needs to be 1!")
      print(colSums(segmentation) == rep(1, ncol(segmentation)))
    }
    
    # value pdf for each type
    pdf_value_f_t <-
      sapply(
        type_set,
        pdf_value_f_t_fct,
        certainty_level = certainty_level,
        n_types = length(type_set)
      )
    colnames(pdf_value_f_t) <- paste0("type", type_set)
    rownames(pdf_value_f_t) <- paste0("value", value_set)
    
    # check if aggregated mkt value = aggregated mkt type
    if (supress_print_messages==F){
      print("Value distribution of types matches value distribution?")
      print(pdf_value_f_t %*% pdf_type == pdf_value)
    }
    
    # probabilities segments
    pdf_segments <- as.vector(segmentation %*% pdf_type) # for type
    
    pdf_segments_value <-  segmentation %*% t(pdf_value_f_t)
    pdf_segments_value_normed_per_segment <-
      pdf_segments_value / rowSums(pdf_segments_value)
    
    # get value distribution from segments as backcheck
    value_distrib_from_segment <-
      colSums(pdf_segments_value) / nrow(segmentation)
    if (supress_print_messages==F){
      print("Value pdf matches value pdf from segments?")
      print(abs((value_distrib_from_segment) - pdf_value) < 0.00001) # If all true then no issues. check via difference (as somehow there is a rounding error)
    }
    # get optimal outcomes for all segments & summarize
    optimal_outcome_all_segments <- NULL
    number_segments_pos_probability <- 0
    for (i in 1:nrow(segmentation)) {
      pdf_value_one_segment <-
        pdf_segments_value_normed_per_segment[i, ] # probability value within segment
      prob_segment <- pdf_segments[i] # probability segment drawn
      if (prob_segment > 0) {
        number_segments_pos_probability <- number_segments_pos_probability+1
        output_opt_outcome_list <-
          optimal_outcome_for_one_segment(
            value_set,
            pdf_value_one_segment,
            prob_segment,
            pricing_rule = pricing_rule,
            weight_consumer = weight_consumer,
            certainty_level = certainty_level
          )
        optimal_outcome_all_segments <-
          rbind(optimal_outcome_all_segments,
                output_opt_outcome_list$optimal)
        rownames(optimal_outcome_all_segments)[number_segments_pos_probability] <- paste0("Segment", i)
      }
    }
    optimal_outcome_summary <-
      as.data.frame(t(colSums(optimal_outcome_all_segments[!is.na(optimal_outcome_all_segments$profit), c("profit","consumer_surplus","total_surplus", "weighed_total_surplus")])))
    colnames(optimal_outcome_summary) <-c("profit","consumer_surplus","total_surplus", "weighed_total_surplus")
    optimal_outcome_summary$pricing_rule <- optimal_outcome_all_segments$pricing_rule[1]  # add constant info over segments to summary (simplification for filtering in result table)
    optimal_outcome_summary$certainty_level <- optimal_outcome_all_segments$certainty_level[1]
    
    # collect all segmentation information and
    segmentation_list <-
      list(
        pricing_rule,
        weight_consumer,
        certainty_level,
        value_set,
        type_set,
        pdf_value,
        pdf_type,
        pdf_value_f_t,
        segmentation,
        optimal_outcome_all_segments,
        optimal_outcome_summary
      )
    names(segmentation_list) <-
      c(
        "pricing_rule",
        "weight_consumer",
        "certainty_level",
        "value_set",
        "type_set",
        "pdf_value",
        "pdf_type",
        "pdf_value_f_t",
        "segmentation",
        "optimal_outcome_all_segments",
        "optimal_outcome_summary"
      )
    
    return(segmentation_list)
  }

# Function to set optimal price for
optimal_outcome_for_one_segment <-
  function(value_set,
           pdf_value_normed_one_segment,
           probability_segment,
           pricing_rule,
           weight_consumer,
           certainty_level) {
    # input : value_set (1x#values vector)
    # input: pdf_value_one_segment: (1x#values vector) pdf of values conditional on segment (sum up to 1)
    # goal: find optimal price for segment
    
    segment_results <-
      as.data.frame(cbind(
        rep(NA, length(value_set)),
        rep(NA, length(value_set)),
        rep(NA, length(value_set)),
        rep(NA, length(value_set)),
        rep(NA, length(value_set)),
        rep(NA, length(value_set)),
        rep(NA, length(value_set))
      ))  # intialize results
    colnames(segment_results) <-
      c(
        "price",
        "profit",
        "consumer_surplus",
        "total_surplus",
        "weighed_total_surplus",
        "pricing_rule",
        "certainty_level"
      )
    # test all possible values as segments
    for (price_iter in 1:length(value_set)) {
      segment_results$price[price_iter] <- value_set[price_iter]
      segment_results$profit[price_iter] <-
        probability_segment * price_iter * pdf_value_normed_one_segment %*% as.numeric(segment_results$price[price_iter] <=
                                                                                         value_set)  # profit = prob_segment * price * #buying_cons with #buying_cons = sum (prob(v=x) * price <= x)
      segment_results$consumer_surplus[price_iter] <-
        probability_segment  * pdf_value_normed_one_segment %*% pmax(value_set -
                                                                       segment_results$price[price_iter], 0)  # cs = prob_segment * sum(max(value-price,0)*prob_value)
    }
    segment_results$total_surplus <-
      segment_results$profit + segment_results$consumer_surplus
    segment_results$weighed_total_surplus <-
      weight_consumer * segment_results$consumer_surplus + (1 - weight_consumer) *
      segment_results$profit
    segment_results$pricing_rule <- pricing_rule
    segment_results$certainty_level <- certainty_level
    
    # Which result is optimal?
    # pricing rule to determine which price to take if profit max for multiple prices
    segment_results_optimal <-
      segment_results[segment_results$profit == max(segment_results$profit), ]
    if (pricing_rule == "min") {
      segment_results_optimal <-
        segment_results_optimal[segment_results_optimal$price == min(segment_results_optimal$price), ]
    } else if (pricing_rule == "max") {
      segment_results_optimal <-
        segment_results_optimal[segment_results_optimal$price == max(segment_results_optimal$price), ]
    } else {
      print("Error: Invalid input for pricing rule. Enter min or max")
    }
    
    output = list(segment_results_optimal, segment_results)
    names(output) <- c("optimal", "all")
    return(output)
    # only possible values can be profit optimizing
  }
