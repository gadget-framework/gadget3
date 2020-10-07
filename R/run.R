# run: Common utilities from turning a set of steps into code

# Combine all provided action lists into one action list, throwing away duplicates
g3_collate <- function(action_list) {
    # Combine all lists into an environment, later lists win over previous
    # TODO: Just concatenate the lists backwards?
    actions <- new.env(parent = emptyenv())

    # For any lone formulas without names, assume they just go on the end
    # This will mostly be test case convenince, not general use
    if (is.null(names(action_list))) {
        names(action_list) <- vapply(seq_along(action_list), function (i) step_id(999, i), character(1))
    }

    for (n in names(action_list)) {
        l <- action_list[[n]]
        if (rlang::is_formula(l)) {
            # One level of list, add this formula
            actions[[n]] <- l
        } else {
            # 2 levels, recurse over inner list too
            for (sub_n in names(l)) {
                actions[[sub_n]] <- l[[sub_n]]
            }
        }
    }
    actions <- as.list(actions)

    # Order items in alphanumeric order
    return(actions[order(names(actions))])
}
