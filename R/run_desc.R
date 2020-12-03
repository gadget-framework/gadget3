# Turn model into a list of string steps
g3_to_desc <- function(actions, minor_steps = FALSE) {
    out <- vapply(
        g3_collate(actions),
        function (s) step_find_desc(s, minor_steps = minor_steps),
        character(1))
    out <- out[!is.na(out)]
    out
}
