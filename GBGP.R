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
#'   unique name, and each is a list of rule as constructed by
#'   \code{rule()}, even though a non-terminal may have only one rule.
normalize_and_name_rules <- function(rules) {
  # the terminals and non-terminals are left implicit, as determined by is_non_terminal()
  sapply(names(rules), function(nt) {
    rs <- rules[[nt]]
    if(inherits(rs, "rule")) {
      # a single rule
      rs[["name"]] <- paste0(nt, ":1")
      list(rs)
    } else if(is.list(rs)) {
      # should be a list of rules
      mapply(function(i, r) {
        if(!inherits(r, "rule")) {
          stop("The rule ", i, " of ", nt, " is not of class 'rule':",
               r, "\n")
        }
        r[["name"]] <- paste0(nt, ":", i)
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
  rules_with_mh <- lapply(rules_with_names, function(r) {
    r_name <- r[["name"]]
    r[["min_height"]] <- md$rules_min_heights[r_name]
    r
  })
  #
  res <- list(
    start = start,
    rules = rules_with_mh,
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
rule <- function(..., action = as_func_call) {
  ts <- list(...)
  res <- list(body = ts, action = action)
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
