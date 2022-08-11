# to test the functions


test_sixmultiplexor_rules <- list(
  #S = rule("B", action = first),
  B = list(
    rule("and", "B", "B", name = "r_and"),
    rule("or", "B", "B", name = "r_or"),
    rule("not", "B", name = "r_not"),
    rule("if", "B", "B", "B", name = "r_if"),
    rule("T", action = first)
  ),
  T = list(
    rule("a0", action = first_as_symbol),
    rule("a1", action = first_as_symbol),
    rule("d0", action = first_as_symbol),
    rule("d1", action = first_as_symbol),
    rule("d2", action = first_as_symbol),
    rule("d3", action = first_as_symbol)
  )
)

tmp_rules <- normalize_and_name_rules(test_sixmultiplexor_rules)

tmp_hs <- cal_min_heights(tmp_rules)

test_sixmultiplexor_G <- grammar(rules = test_sixmultiplexor_rules,
                                 start = "B")


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

#
test_phenotype1 <- convert_to_phenotype(test_chr1, test_sixmultiplexor_G)
test_phenotype2 <- convert_to_phenotype(test_chr2, test_sixmultiplexor_G)
