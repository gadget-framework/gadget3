library(unittest)
library(gadget3)

areas <- g3_areas(c('A', 'B', 'C'))

st1 <-
  g3_stock('st1', 1:5) |>
  g3s_livesonareas(areas[c('A', 'B')]) |>
  g3s_age(minage = 0, maxage = 3)

st2 <-
  g3_stock('st2', 3:6) |>
  g3s_livesonareas(areas[c('B', 'C')]) |>
  g3s_age(minage = 2, maxage = 4)

stp <- gadget3:::g3s_stockproduct(st1, prey = st2)
ok(ut_cmp_identical(
  dimnames(g3_stock_instance(stp)),
  list(
    length = c("1:2", "2:3", "3:4", "4:5", "5:Inf"),
    area = c("A", "B"),
    age = c("age0", "age1", "age2", "age3"),
    prey_length = c("3:4", "4:5", "5:6", "6:Inf"),
    prey_area = c("B", "C"),
    prey_age = c("age2", "age3", "age4"))), "g3s_stockproduct: Combined dimensions in right order")

stp <- gadget3:::g3s_stockproduct(a = st2, b = st1)
ok(ut_cmp_identical(
  dimnames(g3_stock_instance(stp)),
  list(
    a_length = c("3:4", "4:5", "5:6", "6:Inf"),
    a_area = c("B", "C"),
    a_age = c("age2", "age3", "age4"),
    b_length = c("1:2", "2:3", "3:4", "4:5", "5:Inf"),
    b_area = c("A", "B"),
    b_age = c("age0", "age1", "age2", "age3"))), "g3s_stockproduct: Combined dimensions in right order")
