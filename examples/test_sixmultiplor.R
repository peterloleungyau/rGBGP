# To test the simple GBGP
#
# test on the simple 6-Multiplexor problem, using grammar described in
#  P.A. Whigham, Grammatically-based genetic programming.
#    in "Proceedings of the Workshop on Genetic Programming: From Theory to Real-World Applications,
#    ed. by J.P. Rosca (Tahoe City, California, USA, 1995), pp. 33--41.
# Briefly, the 6-Multiplexor is:
#   Given boolean inputs a0, a1, d0, d1, d2, d3,
#   Use operators (IF X Y Z), (AND X Y), (OR X Y), (NOT X), to output
#     d0 if a0=0, a1=0;
#     d1 if a0=0, a1=1;
#     d2 if a0=1, a1=0;
#     d3 if a0=1, a1=1;

library(rGBGP)

correct_6_multiplexor <- function(a0, a1, d0, d1, d2, d3) {
  ifelse(a0,
         ifelse(a1, d3, d2),
         ifelse(a1, d1, d0))
}

test_sixmultiplexor_rules <- list(
  B = list(
    # Note that we can optionally manually give a unique name to a
    # rule, otherwise a name will be generated.
    rule("&", "B", "B", name = "r_and"),
    rule("|", "B", "B", name = "r_or"),
    rule("!", "B", name = "r_not"),
    rule("ifelse", "B", "B", "B", name = "r_if"),
    # Rules have default actions for constructing the phenotype, but
    # the action can also be explicitly specified.
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

test_sixmultiplexor_G <- grammar(rules = test_sixmultiplexor_rules,
                                 start = "B")

#' Turn the parse-tree into R function, for evaluation.
#'
#' @param chr The evolved chromosome, as a tree of node.
compile_6_multiplexor_chr <- function(chr) {
  f <- function(a0, a1, d0, d1, d2, d3) {}
  environment(f) <- globalenv()
  body(f) <- convert_to_phenotype(chr, test_sixmultiplexor_G)
  f
}

df_6_multiplexor <- expand.grid(
  a0 = c(TRUE, FALSE),
  a1 = c(TRUE, FALSE),
  d0 = c(TRUE, FALSE),
  d1 = c(TRUE, FALSE),
  d2 = c(TRUE, FALSE),
  d3 = c(TRUE, FALSE)
)

df_6_multiplexor_correct <- with(
  df_6_multiplexor,
  correct_6_multiplexor(a0, a1, d0, d1, d2, d3))

#' To evaluate a function for 6-multiplexor
#'
#' @param f The function(a0, a1, d0, d1, d2, d3).
#' @return The number of correct output given all the possible inputs
#'   to the (a0, a1, d0, d1, d2, d3).
eval_6_multiplexor_func <- function(f) {
  pred <- with(df_6_multiplexor,
               f(a0, a1, d0, d1, d2, d3))
  sum(pred == df_6_multiplexor_correct)
}

eval_6_multiplexor_chr <- function(chr) {
  eval_6_multiplexor_func(compile_6_multiplexor_chr(chr))
}

#' To re-generate a node based on its non-terminal.
#' 
chr_6_multiplexor_re_gen <- function(chr_node) {
  generate_chromosome(chr_node$nt, test_sixmultiplexor_G, 5)
}

# test evolution -------------------------------------------------------------

run_6_multiplexor_res <- steady_state_elitism_GP(
  init_chrs = generate_init_chrs(n = 50,
                                 G = test_sixmultiplexor_G,
                                 max_height = 6),
  fitness_evaluator = eval_6_multiplexor_chr,
  when_to_stop = either_or(stop_when_max_gen(1000),
                           stop_when_max_fitness(64)),
  p_crossover = 0.8,
  chr_crossover = function(chr1, chr2) {
    # use default values for other parameters
    chr_crossover_func(chr1, chr2)
  },
  p_mutation = 0.2,
  chr_mutator = function(chr) {
    chr_mutation_func(chr, chr_6_multiplexor_re_gen)
  },
  maximize_fitness = TRUE,
  reporting_func = progress_reporter(report_every_n_gen = 1)
)

best_6_multiplexor_found <- run_6_multiplexor_res$best_ind

convert_to_phenotype(best_6_multiplexor_found$chr, test_sixmultiplexor_G)
