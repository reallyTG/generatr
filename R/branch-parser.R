library(purrr)
library(rlang)

# Toolbox:
# formals(fn) #> gives the arguments
# body(fn)    #> gives the body

# Get the AST for a function.
getASTForFn <- function(fn) {
    body(fn) # yeah it's that easy.
}

# Get all switch statement branch conditions in a given function.
findBranchConditionsForFn_Switch <- function(fn) {
    # Get switch expressions.
    isSwitchExpression <- function(ast) length(ast) > 1 && ast[[1]] == "switch"
    switchExpressions <- findMatchingExpressions(body(fn), isSwitchExpression)

    # Grab the condition.
    branchExpressions <- map(switchExpressions, function(e) e[[2]])

    # Check if the branch conditions involve the formals at all.
    fnFormals <- names(formals(fn))
    # Need as.character here cause %in% takes a dump with AST nodes are involved.
    isFormal <- function(ast) typeof(ast) == "symbol" && (ast %>% as.character %in% fnFormals)

    # Discard all branch expressions that don't have fn parameters as subexpressions.
    branchExpressionsInvolvingFormals <- branchExpressions %>% discard(function(ast) 0 == length(findMatchingExpressions(ast, isFormal)))
    branchExpressionsInvolvingFormals
}

# Get all if statement condition checks in a given function.
# Note: ast <- if (X) { B1 } else if (Y) { B2 } ==>
#       ast[[1]] == if
#       ast[[2]] == X
#       ast[[3]] == { B1 }
#       ast[[4]] == if (Y) { B2 }
findBranchConditionsForFn_If <- function(fn) {
    # Get if expressions.
    isIfExpr <- function(ast) length(ast) > 1 && ast[[1]] == "if"
    ifExpressions <- findMatchingExpressions(body(fn), isIfExpr)

    # Grab the condition.
    branchExpressions <- map(ifExpressions, function(e) e[[2]])

    # Check if the branch conditions involve the formals at all.
    fnFormals <- names(formals(fn))
    # Need as.character here cause %in% takes a dump with AST nodes are involved.
    isFormal <- function(ast) typeof(ast) == "symbol" && (ast %>% as.character %in% fnFormals)

    # Discard all branch expressions that don't have fn parameters as subexpressions.
    branchExpressionsInvolvingFormals <- branchExpressions %>% discard(function(ast) 0 == length(findMatchingExpressions(ast, isFormal)))
    branchExpressionsInvolvingFormals
}

# Tests:
# findBranchConditionsForFn_Switch(function(x, y) { 
#   switch(x, x, y)
#   switch(2, x, y) 
# }) 
#> [[1]] 
#> x

findMatchingExpressions <- function(ast, fn) {
    # Is this a switch?
    returnMe <- list()
    if (fn(ast)) {
        returnMe <- c(returnMe, list(ast)) # Add it to what we want to return.
    }
    
    # Either way, recurse.
    if (length(ast) > 1) {
        returnMe <- c(returnMe, reduce(map(ast, function(s_ast) findMatchingExpressions(s_ast, fn)), c, .init = list()))
    }

    returnMe
}