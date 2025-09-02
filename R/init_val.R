g3_init_val <- function (
        param_template,
        name_spec,
        value = NULL,
        spread = NULL,
        lower = if (!is.null(spread)) min(value * (1-spread), value * (1+spread)),
        upper = if (!is.null(spread)) max(value * (1-spread), value * (1+spread)),
        optimise = !is.null(lower) & !is.null(upper),
        parscale = if (is.null(lower) || is.null(upper)) NULL else 'auto',
        random = NULL,
        auto_exponentiate = TRUE) {
    stopifnot(is.data.frame(param_template) || is.list(param_template))
    stopifnot(is.character(name_spec) && length(name_spec) == 1)
    stopifnot(is.numeric(value) || is.null(value))
    stopifnot(is.numeric(spread) || is.null(spread))
    stopifnot(is.numeric(lower) || is.na(lower) || is.null(lower))
    stopifnot(is.numeric(upper) || is.na(upper) || is.null(upper))
    stopifnot(is.logical(optimise) || is.null(optimise))
    stopifnot(identical(parscale, 'auto') || is.numeric(parscale) || is.null(parscale))
    stopifnot(is.logical(random) || is.null(random))
    stopifnot(is.logical(auto_exponentiate))

    # Parse name_spec --> regex
    name_re <- paste0(vapply(strsplit(name_spec, ".", fixed = TRUE)[[1]], function (part) {
        # [1979-1984] - range match
        m <- regmatches(part, regexec('^\\[(\\d+)[:-](\\d+)\\]$', part))
        if (all(vapply(m, length, numeric(1)) == 3)) {
            m <- m[[1]]
            return(paste0(
                '(?:',
                paste(seq(as.numeric(m[[2]]), as.numeric(m[[3]])), collapse = "|"),
                ')'))
        }

        # # - numeric match
        part <- gsub("#", "\\E\\d+\\Q", part, fixed = TRUE)

        # *  - string match
        part <- gsub("*", "\\E.*\\Q", part, fixed = TRUE)

        # | - or part
        part <- gsub("|", "\\E|\\Q", part, fixed = TRUE)

        # Make sure by default text in part is quoted, scope | above
        return(paste0('(?:\\Q', part, '\\E)'))
    }, character(1)), collapse = "\\.")

    name_re <- paste0(
        '^',
        name_re,
        if (auto_exponentiate) '(_exp)?',
        '$')
    names_in <- if (is.data.frame(param_template)) param_template$switch else names(param_template)
    m <- regmatches(names_in, regexec(name_re, names_in))

    matches <- sapply(m, length) > 0
    if (!any(matches)) {
        warning("g3_init_val('", name_spec, "') didn't match any parameters")
        return(param_template)
    }

    # Make boolean vector for all places to auto_exp 
    if (auto_exponentiate) {
        auto_exp <- vapply(m, function(x) length(x) >= 2 && x[[2]] == '_exp', logical(1))
    } else {
        auto_exp <- FALSE
    }

    if (is.data.frame(param_template)) {
        if (!is.null(value)) {
            param_template[matches, 'value'] <- value
            if (any(auto_exp)) param_template[auto_exp, 'value'] <- sapply(param_template[auto_exp, 'value'], log)
        }
        if (!is.null(lower)) {
            param_template[matches, 'lower'] <- lower
            if (any(auto_exp)) param_template[auto_exp, 'lower'] <- log(param_template[auto_exp, 'lower'])
        }
        if (!is.null(upper)) {
            param_template[matches, 'upper'] <- upper
            if (any(auto_exp)) param_template[auto_exp, 'upper'] <- log(param_template[auto_exp, 'upper'])
        }
        if (!is.null(random)) {
            param_template[matches, 'random'] <- random
            if (isTRUE(random)) {
                # If random is explicitly set, optimise should be off, lower/upper also make little sense
                if (is.null(optimise)) param_template[matches, 'optimise'] <- FALSE
                if (is.null(lower)) param_template[matches, 'lower'] <- NA
                if (is.null(upper)) param_template[matches, 'upper'] <- NA
                if (is.null(parscale)) param_template[matches, 'parscale'] <- NA
            }
        }
        if (!is.null(optimise)) param_template[matches, 'optimise'] <- optimise & !param_template[matches, 'random']
        if (identical(parscale, 'auto')) {
            # NB: Happens post-auto_exp, so don't need to apply it
            param_template[matches, 'parscale'] <- diff(c(
                param_template[matches, 'lower'],
                param_template[matches, 'upper']), lag = length(param_template[matches, 'lower']))
        } else if (!is.null(parscale)) {
            param_template[matches, 'parscale'] <- parscale
            if (any(auto_exp)) param_template[auto_exp, 'parscale'] <- log(param_template[auto_exp, 'parscale'])
        }

        m <- is.finite(unlist(param_template[matches, 'value'])) & is.finite(param_template[matches, 'lower']) &
            unlist(param_template[matches, 'value']) < param_template[matches, 'lower']
        if (any(m)) warning("Initial parameter values below lower bound: ", paste(param_template[matches, 'switch'][m], collapse = ", "))
        m <- is.finite(unlist(param_template[matches, 'value'])) & is.finite(param_template[matches, 'upper']) &
            param_template[matches, 'upper'] < unlist(param_template[matches, 'value'])
        if (any(m)) {
            warning("Initial parameter values above upper bound: ", paste(param_template[matches, 'switch'][m], collapse = ", "))
        }
    } else {  # is.list
        if (!is.null(value)) {
            param_template[matches] <- value
            if (any(auto_exp)) param_template[auto_exp] <- sapply(param_template[auto_exp], log)
        }
    }
    
    return(param_template)
}
