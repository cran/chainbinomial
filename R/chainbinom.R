

# Scenario (or chain) probabilities.
#
# @param x chain, including the initial number of infections
# @param s0 Initial number of suceptibles.
# @param sar secondary attack rate.
#
# @example
# chain_prob(x = c(1, 2, 1, 0), s0 = 4, sar=0.1)
chain_prob <- function(x, s0, sar){


  # Remaining number of suceptibles per generation.
  st <- c(s0, s0 - cumsum(x[-1]))

  probs <- numeric(length(x)-1)
  for (gg in 1:(length(x)-1)){

    # Per-person probability of infection.
    pi_g <- 1 - ((1 - sar)^x[gg])
    probs[gg] <- stats::dbinom(x = x[gg+1], size = st[gg], prob = pi_g)

  }

  res <- prod(probs)
  return(res)

}



# The number of scenarios is related to the composition of the
# number. It is the sum of composition from k = 1 to "steps" parts.
# https://en.wikipedia.org/wiki/Composition_(combinatorics)
n_scenarios <- function(target, steps){

  res <- 0

  for (ii in 1:steps){
    res <- res + choose(n = target - 1, k = ii - 1)
  }

  res

}



# Compute all possible scenarios that sums to target in steps steps, with no
# 0's in between (0's allowed at the end).
#
# Does not include the index case.
#
# @param target target sum.
# @param steps number of steps the sum should
all_scenarios <- function(target, steps){

  stopifnot(#steps <= target | target == 0,
            target >= 0,
            steps >= 1)

  if (target == 0){
    scenarios <- matrix(0, nrow = steps, ncol = 1)
    return(scenarios)
  }

  if (target == 1){
    scenarios <- matrix(0, nrow = steps, ncol = 1)
    scenarios[1,1] <- 1
    return(scenarios)
  }

  if (steps == 1){
    scenarios <- matrix(target, nrow = steps, ncol = 1)
    return(scenarios)
  }

  steps_input <- steps
  steps <- min(target, steps)

  # Matrix to store the scenarios in.
  number_of_scenarios <- n_scenarios(target = target, steps = steps)
  scenarios <- matrix(NA, nrow = steps, ncol = number_of_scenarios)

  # The first sequence, consisting of 1's in each generation,
  # except perhaps the last.
  initial_scenario <- rep(1, steps)
  initial_scenario[steps] <- (target - steps + 1)

  scenarios[, 1] <- initial_scenario

  next_sce <- initial_scenario

  max_depth <- steps
  depth <- steps


  # Find next sequence.
  for (ii in 2:number_of_scenarios) {

    # Set the last part of the vector to 0.
    next_sce[depth:steps] <- 0

    # Take a step back.
    depth <- depth - 1

    # Increase the value at current step by 1.
    next_sce[depth] <- next_sce[depth] + 1

    # Go deeper until target sum is reached.
    while (sum(next_sce) < target){
      depth <- depth + 1

      if (depth == max_depth){
        next_sce[depth] <- target - sum(next_sce[1:depth])
      } else {
        next_sce[depth] <- next_sce[depth] + 1
      }
    }

  scenarios[, ii] <- next_sce

  }


  if (steps_input > target){
    scenarios <- rbind(scenarios, matrix(0, nrow = steps_input - target, ncol = ncol(scenarios)))
  }

  return(scenarios)

}


# For the final size distribution.
# Compute the p(I,I) factors from 0 to maxi, for a given sar and i0.
compute_factors <- function(maxi, sar, i0){

  stopifnot(length(maxi) == 1,
            maxi >= 0,
            length(i0) == 1,
            length(sar) == 1)


  q0 <- (1-sar)^i0 # Mentioned in the line right before Eq. 1.6 in Ludwig (1975).

  # The p(i,i) factors
  pii <- numeric(maxi+1)

  pii[1] <- 1 # p(0,0)

  if (maxi == 0){
    return(pii)
  }

  pii[2] <- 1 - ((1-sar)^i0) # p(1,1)

  if (maxi == 1){
    return(pii)
  }

  # Loop over all possible number of susceptible from 2 to maxi
  for (ss in 2:maxi){

    pvec <- numeric(ss+1)

    pvec[1] <- ((1-sar)^i0)^ss  #(Q0^ss) # prob of 0 infected.

    # Loop over all number of infected, from 1 to ss-1.
    for (ii in 1:(ss-1)){
      idx <- ii+1
      pvec[idx] <- choose(ss, ii) * pii[ii+1] * q0^(ss-ii) * (1-sar)^(ii*(ss-ii))
    }

    # Eq. 1.14 in Ludwig (1975)
    pii[ss+1] <- max(0, 1 - sum(pvec))

  }

  return(pii)

}



# Probability mass function for the Chain Binomial distribution.
#' The Chain Binomial distribution
#'
#' Probability mass function, expected value, and random generation, for the chain binomial distribution,
#' with parameters s0, sar, i0, and number of generations, for the number of infected
#' cases in a population of size s0 after a given number of generations.
#'
#' @param x numeric vector of the number of infected.
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param s0 the number of initial susceptibles.
#' @param sar the secondary attack rate, or the per person risk of infection by an infected.
#' @param i0 the number of primary cases.
#' @param generations the number of generations. Default is Inf, which represents the entire epidemic.
#'
#' @returns `dchainbinom` gives the probability of x infected, given s0, i0, sar and generations. `echainbinom` gives the expected value.
#' `rchainbinom` generates random data.
#'
#' @examples
#' dchainbinom(x = 0:5, s0 = 5, sar = 0.2, i0 = 1, generations = Inf)
#' rchainbinom(n = 10, s0 = 5, sar = 0.2, i0 = 1, generations = Inf)
#' echainbinom(s0 = 5, sar = 0.2, i0 = 1, generations = Inf)
#'
#' @export
dchainbinom <- function(x, s0, sar, i0 = 1, generations = Inf){

  # Check input.
  stopifnot(is.numeric(x) | is.logical(x),
            is.numeric(s0) | is.logical(s0),
            is.numeric(sar) | is.logical(sar),
            is.numeric(i0) | is.logical(i0),
            is.numeric(generations) | is.logical(generations),
            all(sar >= 0, na.rm = TRUE),
            all(sar <= 1, na.rm = TRUE),
            all(x >= 0, na.rm = TRUE),
            all(s0 >= 0, na.rm = TRUE),
            all(i0 >= 1, na.rm = TRUE),
            all(generations >= 1, na.rm = TRUE))

  # Coerce to numericals, in case of logicals being provided.
  x <- as.numeric(x)
  s0 <- as.numeric(s0)
  sar <- as.numeric(sar)
  i0 <- as.numeric(i0)
  generations <- as.numeric(generations)


  # Combine input to matrix, to expand/recycle input data.
  inp <- as.matrix(cbind(x, s0, sar, i0, generations))

  # Set to Inf where the final size distribution should be computed.
  inp[,'generations'][inp[,'generations'] >= inp[,'s0']] <- Inf

  n <- nrow(inp)
  res <- numeric(n)

  for (ii in 1:n){

    if (any(is.na(inp[ii,]))){

      res[ii] <- NA

    } else if (inp[ii, 'x'] == 0){

      # Explicit formula for x=0, that applies for both final and incomplete outbreaks.
      res[ii] <- (1-inp[ii, 'sar'])^(inp[ii, 'i0']*inp[ii, 's0'])

    } else if (is.infinite(inp[ii, 'generations'])){

      pii <- compute_factors(maxi = inp[ii, 'x'], i0 = inp[ii, 'i0'], sar = inp[ii, 'sar'])

      # Probability of not being infected by initial infected.
      # Mentioned in the line right before Eq. 1.6 in Ludwig (1975).
      q0 <- (1 - inp[ii, 'sar'])^inp[ii, 'i0']

      res[ii] <- choose(inp[ii, 's0'], inp[ii, 'x']) * pii[length(pii)] * q0^(inp[ii, 's0'] - inp[ii, 'x']) * (1 - inp[ii, 'sar'])^(inp[ii, 'x'] * (inp[ii, 's0'] - inp[ii, 'x']))

    } else {

      if (inp[ii, 'x'] > inp[ii, 's0']){
        res[ii] <- 0
        next
      }

      ss <- all_scenarios(target = inp[ii, 'x'], steps = inp[ii, 'generations'])

      # Add the index cases to the chains.
      ss <- rbind(inp[ii, 'i0'], ss)

      pp <- 0
      for (jj in 1:ncol(ss)){
        pp <- pp + chain_prob(x = ss[,jj], s0 = inp[ii, 's0'], sar = inp[ii, 'sar'])
      }

      res[ii] <- pp

    }

  }

  return(res)

}





#' @rdname dchainbinom
#' @export
rchainbinom <- function(n, s0, sar, i0 = 1, generations = Inf){

  if (length(n) > 1){
    n <- length(n)
  }

  stopifnot(n >= 1)

  res <- numeric(n)

  # Combine input to matrix, to expand/recycle input data.
  inp <- as.matrix(cbind(s0, sar, i0, generations))

  maxg <- pmin(inp[,'s0'], inp[,'generations'])

  for (ii in 1:n){

    inp_idx <- ((ii-1) %% nrow(inp))+1

    if (any(is.na(inp[inp_idx,]))){
      res[ii] <- NA
      next
    }

    sg <- inp[inp_idx,'s0'] # Remaining suceptibles in generation g.
    ig <- inp[inp_idx,'i0'] # Infected in generation g.
    i_cum <- 0 # Cumulative number of infected.

    # Iterate over generations.
    for (gg in 1:maxg[inp_idx]){

      if (sg == 0){
        break
      }

      # Per-person probability of infection.
      pi_g <- 1 - (1 - inp[inp_idx,'sar'])^ig

      # Simulate the number of new infected in next generation.
      i_new <- stats::rbinom(n = 1, size = sg, prob = pi_g)

      if (i_new == 0){
        break
      }

      ig <- i_new
      i_cum <- i_cum + i_new
      sg <- sg - i_new

    }

    res[ii] <- i_cum


  }

  return(res)

}


#' @rdname dchainbinom
#' @export
echainbinom <- function(s0, sar, i0 = 1, generations = Inf){

  stopifnot(is.numeric(s0) | is.logical(s0),
            is.numeric(sar) | is.logical(sar),
            is.numeric(i0) | is.logical(i0),
            is.numeric(generations) | is.logical(generations),
            all(sar >= 0, na.rm = TRUE),
            all(sar <= 1, na.rm = TRUE),
            all(s0 >= 0, na.rm = TRUE),
            all(i0 >= 1, na.rm = TRUE),
            all(generations >= 1, na.rm = TRUE))

  # Coerce to numericals, in case of logicals being provided.
  s0 <- as.numeric(s0)
  sar <- as.numeric(sar)
  i0 <- as.numeric(i0)
  generations <- as.numeric(generations)

  # Combine input to matrix, to expand/recycle input data.
  inp <- as.matrix(cbind(s0, sar, i0, generations))

  n <- nrow(inp)
  res <- numeric(n)

  for (ii in 1:n){

    if (any(is.na(inp[ii,]))){
      res[ii] <- NA
      next
    }
    xx <- 0:inp[ii, 's0']
    pp <- dchainbinom(x  = xx, s0 = inp[ii, 's0'], i0 = inp[ii, 'i0'], sar = inp[ii, 'sar'], generations = inp[ii, 'generations'])
    res[ii] <- sum(xx * pp)
  }

  return(res)

}
