# NB: We copy the definitions of head / tail to avoid picking up
# S3 methods provided by other packages (read: reformulas)
# utils:::head.default isn't exported, so can't call it directly.

# Common case of utils:::head.default
head <- function (x, n = 6L) {
    stopifnot(length(n) == 1L)
    n <- if (n < 0L)
        max(length(x) + n, 0L)
    else min(n, length(x))
    x[seq_len(n)]
}

# Common case of utils:::tail.default
tail <- function (x, n = 6L)
{
    stopifnot(length(n) == 1L)
    xlen <- length(x)
    n <- if (n < 0L)
        max(xlen + n, 0L)
    else min(n, xlen)
    x[seq.int(to = xlen, length.out = n)]
}
