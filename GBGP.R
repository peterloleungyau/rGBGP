# Adaptation of GBGP from https://github.com/peter19852001/simple_GBGP
#
# Useful reference:
# Robert I. Mckay, Nguyen Xuan Hoai, Peter Alexander Whigham, Yin Shan, and Michael O'Neill. 2010.
#   Grammar-based Genetic Programming: a survey. Genetic Programming and Evolvable Machines 11,
#   3-4 (September 2010), 365-396. DOI=10.1007/s10710-010-9109-y http://dx.doi.org/10.1007/s10710-010-9109-y 

# helper functions -----------------------------------------------------------

#' Whether an object is a non-terminal in a grammar.
#'
#' @param x The object to test.
#' @param rules The rules as in \code{grammar()}.
#' @return TRUE if \code{x} is a non-terminal in the rules.
#' @export
is_non_terminal <- function(x, rules) {
  is.character(x) && (x %in% names(rules))
}

#' Helper function to normalize the rules.
#'
#' @param rules The named list of rules as accepted by
#'   \code{grammar()}.
#' @return Normalized named list of rules, such that each rule has a
#'   generated name if it does not already have a name, and each is a
#'   list of rule as constructed by \code{rule()}, even though a
#'   non-terminal may have only one rule.
normalize_and_name_rules <- function(rules) {
  # the terminals and non-terminals are left implicit, as determined by is_non_terminal()
  sapply(names(rules), function(nt) {
    rs <- rules[[nt]]
    if(inherits(rs, "rule")) {
      # a single rule
      if(is.null(rs[["name"]])) {
        rs[["name"]] <- paste0(nt, ":1") 
      }
      list(rs)
    } else if(is.list(rs)) {
      # should be a list of rules
      mapply(function(i, r) {
        if(!inherits(r, "rule")) {
          stop("The rule ", i, " of ", nt, " is not of class 'rule':",
               r, "\n")
        }
        if(is.null(r[["name"]])) {
          r[["name"]] <- paste0(nt, ":", i)
        }
        r
      }, seq_along(rs), rs,
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
    } else {
      stop("Unrecognized rule form for", nt,
           ", should be either a single rule or a list of rules, got",
           rs, "\n")
    }
  }, simplify = FALSE, USE.NAMES = TRUE)
}

#' Helper function to calculate the minimum heights of each
#' non-terminal and grammar rule. A terminal has height 1, and each
#' level of non-terminal adds 1 to the height.
#'
#' @param rules The named list of grammar rules with names as returned
#'   by \code{normlize_and_name_rules()}.
#' @return A named list of:
#' 
#'   nt_min_heights: named list of minimum heights of non-terminals,
#'   using the non-terminal name as name
#'
#'   rules_min_heights: named list of minimum heights of the rules,
#'   using the rule id as name
cal_min_heights <- function(rules) {
  # use a naive algorithm to iterative go through all rules of all
  # non-terminals to determine the minimum heights, until there are no
  # further updates.
  nt <- names(rules)
  nt_mh <- setNames(rep_len(NA, length.out = length(nt)),
                    nm = nt)
  r_mh <- list()

  height_of_item <- function(z) {
    # terminal has height 1
    if(is_non_terminal(z, rules)) nt_mh[[z]] else 1
  }
  
  has_update <- FALSE
  mh_of_rule <- function(r) {
    r_name <- r[["name"]]
    if(is.null(r_mh[[r_name]])) {
      # min height of a rule is 1 + the max of min height of each term
      mh <- 1 + max(sapply(r$body, height_of_item),
                    na.rm = FALSE)
      if(!is.na(mh)) {
        r_mh[[r_name]] <<- mh # outer scope
        has_update <<- TRUE
      }
      mh
    } else {
      # already determined
      r_mh[[r_name]]
    }
  }
  #
  while(TRUE) {
    # only care about update of min height of non-terminal. If the min
    # height of a non-terminal is already determined, update of min
    # height of any of its rules will not change the results (because
    # the min height should be larger than the already determined
    # one).
    has_update <- FALSE
    # go through each non-terminal
    
    # to be certain that the result does not depend on the order of
    # non-terminals considered, we update the mh of non-terminals in
    # batch.
    tmp_nt_mh <- list()
    for(n in nt) {
      # and each rule, even if the min height of the non-terminal has
      # already been determined, because some rules may not have min
      # height determined yet
      tmp_mhs <- sapply(rules[[n]], mh_of_rule)
      if(is.na(nt_mh[[n]])) {
        # NOTE: if some rules still have min height of NA, but some
        # rules already has min height, then the min height of the
        # non-terminal is still determined. The min height of the
        # rules still with NA will have larger min heights, possibly
        # infinity.
        mh <- suppressWarnings(min(tmp_mhs, na.rm = TRUE))
        if(is.finite(mh)) {
          tmp_nt_mh[[n]] <- mh
          has_update <- TRUE
        }
      }
    }

    for(n in names(tmp_nt_mh)) {
      nt_mh[[n]] <- tmp_nt_mh[[n]]
    }
    #
    if(!has_update) {break}
  }
  # done
  list(nt_min_heights = nt_mh,
       rules_min_heights = unlist(r_mh))
}

#' To define the grammar.
#'
#' @param rules A named list of the rules of the non-terminals. The
#'   names are the names of the non-terminals (which should avoid
#'   using character ':' which will be used as separator in rule
#'   names), and the value is either a rule as constructed by
#'   \code{rule()} or a unnamed list of rules, each constructed by
#'   \code{rule()}.
#' @param start A character of as non-terminal as starting symbol.
#'
#' @return a list of class "grammar", which contains:
#' 
#'   start: the name of the starting symbol
#'
#'   rules: named list of processed rules, each named by the
#'   non-terminal, and each rule will get a name.
#'
#'   rules_by_name: named list of each rule mapped by the rule name.
#'
#'   nt_min_heights: named list of minimum heights of non-terminals,
#'   using the non-terminal name as name.
#'
#'   rules_min_heights: named list of minimum heights of the rules,
#'   using the rule id as name.
#'
#' @examples TODO
#' @export
grammar <- function(rules, start) {
  rules_with_names <- normalize_and_name_rules(rules)
  md <- cal_min_heights(rules_with_names)
  # for convenience, also update each rule with the calculated min
  # height, where for some rules it may be NA.
  rules_with_mh <- lapply(rules_with_names, function(rs) {
    lapply(rs, function(r) {
      r_name <- r[["name"]]
      r[["min_height"]] <- md$rules_min_heights[r_name]
      r
    })
  })
  #
  rs_by_name <- list()
  for(rs in rules_with_mh) {
    for(r in rs) {
      r_name <- r[["name"]]
      rs_by_name[[r_name]] <- r
    }
  }
  #
  res <- list(
    start = start,
    rules = rules_with_mh,
    rules_by_name = rs_by_name,
    nt_min_heights = md$nt_min_heights,
    rules_min_heights = md$rules_min_heights
  )
  class(res) <- "grammar"
  res
}

#' To explicitly construct a terminal in a rule.
#'
#' @param x The terminal value.
#' @return An object of class "terminal", which is useful for
#'   explicitly constructing terminals in a rule, in case it might be
#'   regarded as a non-terminal.
#' @export
term <- function(x) {
  res <- list(val = x)
  class(res) <- "terminal"
  res
}

#' To construct a terminal with generated value using a function
#' instead of a simple constant value.
#'
#' @param func The function of no arguments, to generate a value for
#'   the terminal each time the body of a rule is instantiated.
#' @return An object of class c("generated_terminal", "terminal").
#' @export
gen_term <- function(func) {
  res <- list(func = func)
  class(res) <- c("generated_terminal", "terminal")
  res
}

#' To construct one rule for a non-terminal.
#'
#' @param name Optional character name to give to this rule. Note that
#'   you should make sure different rules have distinct names. And
#'   should avoid using the pattern of "non-terminal:number" as name,
#'   because it is the pattern of auto-generated name in
#'   \code{normalize_and_name_rules()}.
#' @param action A function that will construct the phenotype from a
#'   list of constructed values of the rule body. The default is
#'   \code{as_func_call} which will construct a function call where
#'   the first element of the list will be treated as function using
#'   \code{match.fun()}. Another useful action is \code{first} which
#'   simply returns the first value in the list, and is useful when a
#'   non-terminal is meant to give a leaf value instead of a function
#'   call.
#' @param ... The body of the rule, where character string with equal
#'   to the name of a non-terminal will be treated as a non-terminal,
#'   and other values are unaltered. In order to explicit mark a
#'   character string as terminal, use the \code{term()} function.
#' @return An object of class "rule".
#' @export
rule <- function(..., name = NULL, action = as_func_call) {
  ts <- list(...)
  res <- list(body = ts, name = name, action = action)
  class(res) <- "rule"
  res
}

# some useful rule actions ---------------------------------------------------

#' Rule action for constructing a function call.
#'
#' @param vs The list of values, the first of which will be treated as
#'   function using \code{match.fun()}, and the rest will be treated
#'   as arguments.
#' @return an R function call expression.
#' @export
as_func_call <- function(vs) {
  vs[[1]] <- as.symbol(vs[[1]])
  as.call(vs)
}

#' Rule action to get the first value.
#'
#' @param vs The list of value, where there should only be one, and
#'   will be returned as the constructed value.
#' @return The first value.
#' @export
first <- function(vs) {
  vs[[1]]
}

#' Rule action to get leaf as symbol.
#'
#' @param vs The list of value, where there should only be one, and
#'   will be coerced to symbol and returned as the constructed value.
#' @return The first value as symbol.
#' @export
first_as_symbol <- function(vs) {
  as.symbol(vs[[1]])
}

# generation of chromosome (tree) from grammar -------------------------------

#' To create an internal node corresponding to a non-terminal. Note
#' that terminal can be directly represented.
#'
#' @param nt The non-terminal as a character string.
#' @param rule_name The name of the rule generating the children.
#' @param children The list of children objects, which could be a node
#'   (for subtree corresponding to non-terminal) or other values
#'   (corresponding to terminals).
#' @return An object of class "node", which is a tree.
#' @export
create_node <- function(nt, rule_name, children) {
  res <- list(nt = nt, rn = rule_name, cs = children)
  class(res) <- "node"
  res
}

#' To generate a tree of nodes as chromsome
#'
#' @param x The object to create from, which could be a non-terminal,
#'   terminal, a grammar, a rule.
#' @param G The grammar as returned by \code{grammar()}.
#' @param max_height The allowed maximum height. The generation will
#'   try to avoid generating a chromsome with height exceeding this. A
#'   terminal has height 1, and every layer of non-terminal adds 1 to
#'   the height.
#' @return An object of class "node", which is a tree.
#' @export
generate_chromosome <- function(x, G, max_height) {
  UseMethod("generate_chromosome")
}

#' Default generation is to treat it as a terminal.
generate_chromosome.default <- function(x, G, max_height) {
  x
}

#' For character string, either generate as non-terminal, or a
#' terminal.
generate_chromosome.character <- function(x, G, max_height) {
  if(is_non_terminal(x, G$rules)) {
    # non-terminal
    rs_within_height <- Filter(function(r) {r[["min_height"]] <= max_height}, G$rules[[x]])
    if(length(rs_within_height) > 0) {
      r_idx <- sample.int(length(rs_within_height), size = 1)
      rule_to_use <- rs_within_height[[r_idx]]
      # generate form the rule body.
      
      # NOTE: do not set a separate method for rule because we also
      # want to set the non-terminal, which is now not recorded in the
      # rule itself. TODO: may consider adding the non-terminal to the
      # rule.
      create_node(nt = x,
                  rule_name = rule_to_use[["name"]],
                  children = lapply(rule_to_use[["body"]], function(z) {
                    generate_chromosome(z, G, max_height - 1)
                  }))
    } else {
      stop("No applicable rule for non-terminal ", nt,
           " with minium height within ", max_height, "\n")
    }
  } else {
    # terminal, as is
    x
  }
}

#' For grammar, just generate using the start non-terminal.
generate_chromosome.grammar <- function(x, G, max_height) {
  generate_chromosome(x$start, G, max_height)
}

#' For explicit simple terminal, just return its value
generate_chromosome.terminal <- function(x, G, max_height) {
  x$val
}

#' For explicit generated terminal, call its function to generate a terminal.
generate_chromosome.generated_terminal <- function(x, G, max_height) {
  x$func()
}

# crossover and mutation operators -------------------------------------------

# some utilities

always_true <- function(x) TRUE

#' To get all the nodes of a chromosome, for selection purpose.
#'
#' @param chr The chromosome, which should be a node.
#' @param is_wanted Optional predicate function the returns TRUE if a
#'   node is wanted.
#' @return a named list with:
#'
#'   nodes: the list of nodes
#'
#'   heights: a vector of the heights of the nodes corresponding to
#'   the nodes. This will be useful to adjust the selection
#'   probability by height.
#' @export
get_chr_nodes <- function(chr, is_wanted = always_true) {
  n <- 1
  ns <- list()
  hs <- list()
  visit <- function(z, h) {
    if(inherits(z, "node")) {
      if(is_wanted(z)) {
        ns[[n]] <<- z
        hs[[n]] <<- h
        n <<- n+1
      }
      # visit all the children
      for(w in z[["cs"]]) visit(w, h+1)
    }
  }
  #
  visit(chr, h = 1)
  #
  list(nodes = ns, heights = unlist(hs))
}

#' To replace old node with new node in chromosome.
#'
#' @param chr The chromosome to copy and modify.
#' @param old_node The old node in \code{chr} to replace.
#' @param new_node The new node to replace \code{old_node} in
#'   \code{chr}.
#' @return A copied and modified chromosome with \code{old_node}
#'   replaced with \code{new_node}.
#' @export
replace_node <- function(chr, old_node, new_node) {
  if(inherits(chr, "node")) {
    if(identical(chr, old_node)) {
      # replace
      new_node
    } else {
      # copy and replace in children
      new_n <- chr
      new_n[["cs"]] <- lapply(chr[["cs"]], function(z) {
        replace_node(z, old_node, new_node)
      })
      new_n
    }
  } else {
    chr
  }
}

#' A simple chromosome crossover function, that can serve as reference
#' for customization. This function will first select a node from the
#' first chromosome, and then select a node of the same non-terminal
#' in the second chromosome, and then crossover them. If no nodes of
#' the same non-terminal in the second chromosome can be found, the
#' function will retry by selecting another node from the first
#' chromosome, up to a maximum number of trials. If there is no
#' success after a maximum number of trials, the first chromosome will
#' simply be returned. If the first chromosome has no nodes (which
#' should not happen), then the second chromosome will be returned. In
#' the simplest case, the crossover simply use the selected node from
#' the second chromosome to replace the selected node in the first
#' chromosome, but custom node crossover function specific to the
#' non-terminal could be provided, see \code{nt_crossovers} for
#' details.
#'
#' @param chr1 The first chromosome.
#' @param chr2 The second chromosome.
#' @param n.tries The maximum number of tries before giving up and
#'   return the first chromosome.
#' @param is_wanted1 Optional predicate function whether to consider
#'   nodes of chr1.
#' @param is_wanted2 Optional predicate function whether to consider
#'   nodes of chr2.
#' @param height_prob_func1 Optional, default NULL. If non-NULL,
#'   should be a function(heights, nodes) that should return the
#'   vector of probability of choosing the nodes of chr1. If NULL,
#'   there is no bias in the nodes.
#' @param height_prob_func2 Optional, similar to height_prob_func1,
#'   but for biasing the nodes of chr2.
#' @param nt_crossovers Optional, default empty list. This can be a
#'   named list to maps non-terminal name to custom
#'   function(node_chr1, node_chr2) that returns a new node from
#'   selected nodes from chr1 and chr2, to replace the old node from
#'   chr1. If the selected non-terminal has no entry in
#'   \code{nt_crossovers}, then simply the node selected from chr2 is
#'   used as the new node.
#' @return A possibly crossovered chromsome.
#' @export
chr_crossover_func <- function(chr1, chr2,
                               n.tries = 10,
                               is_wanted1 = always_true,
                               is_wanted2 = always_true,
                               height_prob_func1 = NULL,
                               height_prob_func2 = height_prob_func1,
                               nt_crossovers = list()) {
  ns1 <- get_chr_nodes(chr1, is_wanted = is_wanted1)
  if(length(ns1$heights) == 0) {
    # hopeless
    return(chr2)
  }
  #
  ns1_prob <- if(!is.null(height_prob_func1)) {
    height_prob_func1(ns1$heights, ns1$nodes)
  } else {
    NULL
  }

  for(i in seq(n.tries)) {
    idx1 <- sample.int(length(ns1$heights), size = 1, prob = ns1_prob)
    node_chr1 <- ns1$nodes[[idx1]]
    nt1 <- node_chr1[["nt"]]
    # try to get another node from chr2
    ns2 <- get_chr_nodes(chr2, is_wanted = function(z) {
      (z[["nt"]] == nt1) && is_wanted2(z)
    })

    if(length(ns2$heights) > 0) {
      ns2_prob <- if(!is.null(height_prob_func2)) {
        height_prob_func2(ns2$heights, ns2$nodes)
      } else {
        NULL
      }

      idx2 <- sample.int(length(ns2$heights), size = 1, prob = ns2_prob)
      node_chr2 <- ns2$nodes[[idx2]]

      # finally got two nodes to crossover, see if we have custom node
      # crossover operators.
      new_node <- if(!is.null(nt_crossovers[[nt1]])) {
        special_crossover_func <- nt_crossovers[[nt1]]
        special_crossover_func(node_chr1, node_chr2)
      } else {
        # no custom one, just use the one from chr2
        node_chr2
      }

      #
      return(replace_node(chr1, old_node = node_chr1, new_node = new_node))
    }
  }
  # give up
  return(chr1)
}

#' A simple chromosome mutation function, that can serve as reference
#' for customization. This function will select a node from the
#' chromosome, then either the default node_mutator is applied or a
#' custom non-terminal specific mutator is applied if there is
#' one. Note that the mutation is applied only to a node corresponding
#' to non-terminal. If mutation of terminal is desired, then the
#' non-terminal specific custom mutator could be used to copy the node
#' but mutator one of the terminal, as appripriate.
#'
#' @param chr The chromosome to mutate.
#' @param default_node_mutator The function(node) that mutate a
#'   selected node, and the old node in \code{chr} will be replaced
#'   with the mutated node.
#' @param is_wanted Optional predicate function whether to consider
#'   nodes of chr.
#' @param height_prob_func Optional, default NULL. If non-NULL, should
#'   be a function(heights, nodes) that should return the vector of
#'   probability of choosing the nodes of chr. If NULL, there is no
#'   bias in the nodes.
#' @param nt_mutators Optional, default empty list. This can be a
#'   named list to maps non-terminal name to custom function(node)
#'   that returns a new node from selected node from chr, to replace
#'   the old node from chr. If the selected non-terminal has no entry
#'   in \code{nt_mutators}, then simply the
#'   \code{default_node_mutator} is applied to get the new node.
#' @return A possibly mutated chromosome.
#' @export
chr_mutation_func <- function(chr,
                              default_node_mutator,
                              is_wanted = always_true,
                              height_prob_func = NULL,
                              nt_mutators = list()) {
  ns <- get_chr_nodes(chr, is_wanted = is_wanted)
  if(length(ns$heights) == 0) {
    # hopeless
    return(chr)
  }
  #
  ns_prob <- if(!is.null(height_prob_func)) {
    height_prob_func(ns$heights, ns$nodes)
  } else {
    NULL
  }

  # select a node
  idx <- sample.int(length(ns$heights), size = 1, prob = ns_prob)
  node_chr <- ns$nodes[[idx]]
  nt <- node_chr[["nt"]]

  # generate new node
  special_mutation_func <- if(!is.null(nt_mutators[[nt]])) {
    nt_mutators[[nt]]
  } else {
    # no custom one, just use the default one
    default_node_mutator
  }

  new_node <- special_mutation_func(node_chr)
  #
  replace_node(chr, old_node = node_chr, new_node = new_node)
}

#' An example of generating height bias function. The root has a
#' separate weight as bias, and each nodes of height 2 and onward gets
#' exponential weight. Lastly the weights are normalized to get
#' probability bias.
#'
#' @param subtree_weight The exponential multiplier of nodes with
#'   height 2 or more, i.e. (subtree_weight)^(height - 1).
#' @param root_weight The weight for root node, i.e. height of 1.
#' @return A function that can be used as \code{height_prob_func1} and
#'   \code{height_prob_func2} in \code{chr_crossover_func}.
#' @export
get_height_prob_func <- function(subtree_weight, root_weight = 1) {
  function(heights, nodes) {
    ps <- subtree_weight(heights - 1)
    ps[heights == 1] <- root_weight
    ps/sum(ps)
  }
}

#' An example convenience function to get a default node mutator that
#' re-generates the node for the non-terminal, that can be used as
#' \code{default_node_mutator} in \code{chr_mutation_func}.
#'
#' @param G The grammar as returned by \code{grammar()}.
#' @param The allowed maximum height in the re-generation of the node.
#' @return A function(node) that can be used as
#'   \code{default_node_mutator} in \code{chr_mutation_func}.
#' @export
get_re_gen_node_mutator <- function(G, max_height = 5) {
  function(node) {
    generate_chromosome(node[["nt"]], G, max_height)
  }
}

#' A convenience function to generate node predicate that only wants
#' some non-terminals.
#'
#' @param
want_only_some_non_terminals <- function(wanted_non_terminals) {
  function(node) {
    node[["nt"]] %in% wanted_non_terminals
  }
}

# genotype to phenotype ------------------------------------------------------

convert_to_phenotype <- function(x, G) {
  if(inherits(x, "node")) {
    children_vals <- lapply(x[["cs"]],
                            function(z) convert_to_phenotype(z, G))
    r_name <- x[["rn"]]
    r <- G$rules_by_name[[r_name]]
    r_action <- r[["action"]]
    r_action(children_vals)
  } else {
    x
  }
}

# stopping conditions --------------------------------------------------------

# The stopping condition can look at a few information and returns
# TRUE if the evolution should end:
#
#   pop: the list of current population
#
#   n_gen_ended: the number of generations already done.
#
#   cur_best_fitness: the current best fitness value.
#
#   n_gen_of_best_fitness: the generation when the current best fitness is first obtained.
#
#   start_time: the starting time of the evolution.
#

#' Stopping up to maximum generations.
#'
#' @param max_gen The maximum generations after which the evolution
#'   should end.
#' @return A stopping condition function that would return TRUE if
#'   \code{max_gen} generations has been done.
#' @export
stop_when_max_gen <- function(max_gen) {
  function(pop, n_gen_ended, cur_best_fitness, n_gen_of_best_fitness, start_time) {
    n_gen_ended >= max_gen
  }
}

#' Stopping when no improvement in a certain number of generations
#' 
stop_when_no_improvement_in_n_gen <- function(n_gen_no_improvement) {
  function(pop, n_gen_ended, cur_best_fitness, n_gen_of_best_fitness, start_time) {
    (n_gen_ended - n_gen_of_best_fitness) >= n_gen_no_improvement
  }
}

#' Convenience function to combine stopping conditions
#'
#' @param ... The stopping condition functions, e.g. by
#'   \code{stop_when_max_gen()} or
#'   \code{stop_when_no_improvement_in_n_gen()} or any custom stopping
#'   condition function.
#' @return A stopping condition that is true if any of the stopping
#'   conditions is true.
#' @export
either_or <- function(...) {
  stopping_conds <- list(...)
  function(...) {
    for(stopping_cond in stopping_conds) {
      if(stopping_cond(...)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
}

# generate initial populations -----------------------------------------------

# evolution main function ----------------------------------------------------
