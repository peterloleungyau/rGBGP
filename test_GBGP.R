# to test the functions


test_sixmultiplexor_rules <- list(
  S = rule("B", action = first),
  B = list(
    rule("and", "B", "B"),
    rule("or", "B", "B"),
    rule("not", "B"),
    rule("if", "B", "B", "B"),
    rule("T", action = first)
  ),
  T = list(
    rule("a0", action = first),
    rule("a1", action = first),
    rule("d0", action = first),
    rule("d1", action = first),
    rule("d2", action = first),
    rule("d3", action = first)
  )
)

tmp_rules <- normalize_and_name_rules(test_sixmultiplexor_rules)

tmp_hs <- cal_min_heights(tmp_rules)

test_sixmultiplexor_G <- grammar(rules = test_sixmultiplexor_rules,
                                 start = "S")
