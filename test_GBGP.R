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


# test generation
test_chr1 <- generate_chromosome(test_sixmultiplexor_G, G = test_sixmultiplexor_G, max_height = 5)
test_chr2 <- generate_chromosome(test_sixmultiplexor_G, G = test_sixmultiplexor_G, max_height = 5)

# test crossover
test_chr_c <- chr_crossover_func(chr1 = test_chr1,
                                 chr2 = test_chr2)

# (if a0 d0 a1)
# (not d2)

# test mutation
test_re_gen_node_mutator <- get_re_gen_node_mutator(G = test_sixmultiplexor_G)
test_chr_m <- chr_mutation_func(chr = test_chr1, default_node_mutator = test_re_gen_node_mutator)

want_only_T <- want_only_some_non_terminals(c("T"))

test_chr_m1 <- chr_mutation_func(chr = test_chr1, default_node_mutator = test_re_gen_node_mutator, is_wanted = want_only_T)
