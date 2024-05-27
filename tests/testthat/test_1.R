

# Chain probabilities ----

# These should work, and return a single number (a probability).
cp_1 <- chain_prob(x = c(1,2,3), s0 = 6, sar = 0.1)
cp_2 <- chain_prob(x = c(2,1,0), s0 = 4, sar = 0.1)

cp_1b <- chain_prob(x = c(1,2,3), s0 = 6, sar = 0.8)
cp_2b <- chain_prob(x = c(2,1,0), s0 = 4, sar = 0.8)

# Impossible chain, Should return probability 0.
cp_0a <- chain_prob(x = c(0,1,0), s0 = 4, sar = 0.8)
cp_0b <- chain_prob(x = c(1,0,1), s0 = 4, sar = 0.8)


# Chain of length 1 (index case only) is not defined. Should give NA.
cp_na1 <- chain_prob(x = c(0), s0 = 4, sar = 0.8)
cp_na2 <- chain_prob(x = c(1), s0 = 4, sar = 0.8)


test_that("Chain probabilities", {
  expect_true(is.numeric(cp_1))
  expect_true(length(cp_1) == 1)

  expect_true(is.numeric(cp_2))
  expect_true(length(cp_2) == 1)

  expect_true(is.numeric(cp_1b))
  expect_true(length(cp_2b) == 1)

  expect_true(is.numeric(cp_0a))
  expect_true(length(cp_0a) == 1)
  expect_true(cp_0a == 0)

  expect_true(is.numeric(cp_0b))
  expect_true(length(cp_0b) == 1)
  expect_true(cp_0b == 0)

})



# Generating all scenarios ----

sc1 <- all_scenarios(target = 5, steps = 2)
sc2 <- all_scenarios(target = 5, steps = 1)
sc3 <- all_scenarios(target = 10, steps = 7)

test_that("All scenarions", {

  expect_true(nrow(sc1) == 2) # nrow == steps.
  expect_true(all(colSums(sc1) == 5)) # columns sum to target.

  expect_true(nrow(sc2) == 1)
  expect_true(all(colSums(sc2) == 5))

  expect_true(nrow(sc3) == 7)
  expect_true(all(colSums(sc3) == 10))

  # Same number of columns when the number of steps >= target.
  expect_true(ncol(all_scenarios(target = 5, steps = 5)) == ncol(all_scenarios(target = 5, steps = 6)))
  expect_true(ncol(all_scenarios(target = 2, steps = 2)) == ncol(all_scenarios(target = 2, steps = 4)))

  # Some checks that the scenarios sum to the correct number.
  expect_true(all(colSums(all_scenarios(target = 5, steps = 1)) == 5))
  expect_true(all(colSums(all_scenarios(target = 5, steps = 4)) == 5))
  expect_true(all(colSums(all_scenarios(target = 5, steps = 5)) == 5))
  expect_true(all(colSums(all_scenarios(target = 5, steps = 8)) == 5))

  expect_true(all(colSums(all_scenarios(target = 1, steps = 1)) == 1))
  expect_true(all(colSums(all_scenarios(target = 1, steps = 5)) == 1))

  expect_true(all(colSums(all_scenarios(target = 0, steps = 1)) == 0))
  expect_true(all(colSums(all_scenarios(target = 0, steps = 5)) == 0))


})



# Chain binomial PMF. ----

dcb_1_g1 <- dchainbinom(x = 0:5, s0 = 5, sar = 0.11, generations = 1)
dcb_1_g2 <- dchainbinom(x = 0:5, s0 = 5, sar = 0.11, generations = 2)
dcb_1_g3 <- dchainbinom(x = 0:5, s0 = 5, sar = 0.11, generations = 3)
dcb_1_g4 <- dchainbinom(x = 0:5, s0 = 5, sar = 0.11, generations = 4)
dcb_1_g5 <- dchainbinom(x = 0:5, s0 = 5, sar = 0.11, generations = 5)
dcb_1_g6 <- dchainbinom(x = 0:5, s0 = 5, sar = 0.11, generations = 6)
dcb_1_ginf <- dchainbinom(x = 0:5, s0 = 5, sar = 0.11, generations = Inf)

dcb_2_ginf <- dchainbinom(x = 0:5, s0 = 5, sar = 0.00014, i0 = 1, generations = Inf)


tol_sum_to_1 <- 2e-15

test_that("PMF is ok", {

  expect_true(abs(sum(dcb_1_g1) - 1) < tol_sum_to_1)
  expect_true(abs(sum(dcb_1_g2) - 1) < tol_sum_to_1)
  expect_true(abs(sum(dcb_1_g3) - 1) < tol_sum_to_1)
  expect_true(abs(sum(dcb_1_g4) - 1) < tol_sum_to_1)
  expect_true(abs(sum(dcb_1_g5) - 1) < tol_sum_to_1)
  expect_true(abs(sum(dcb_1_g6) - 1) < tol_sum_to_1)
  expect_true(abs(sum(dcb_1_ginf) - 1) < tol_sum_to_1)
  expect_true(abs(sum(dcb_2_ginf) - 1) < tol_sum_to_1)


  expect_true(all(dcb_1_g1 >= 0))
  expect_true(all(dcb_1_g2 >= 0))
  expect_true(all(dcb_1_g3 >= 0))
  expect_true(all(dcb_1_g4 >= 0))
  expect_true(all(dcb_1_g5 >= 0))
  expect_true(all(dcb_1_g6 >= 0))
  expect_true(all(dcb_1_ginf >= 0))
  expect_true(all(dcb_2_ginf >= 0))


  expect_true(all(dcb_1_g5 == dcb_1_g6))
  expect_true(all(dcb_1_g5 == dcb_1_ginf))

  # Probability of more infected than s0 should be 0
  expect_true(dchainbinom(x = 0:6, s0 = 5, sar = 0.11, generations = 1)[7] == 0)
  expect_true(dchainbinom(x = 0:6, s0 = 5, sar = 0.11, generations = 3)[7] == 0)
  expect_true(dchainbinom(x = 0:6, s0 = 5, sar = 0.11, generations = Inf)[7] == 0)
  expect_true(dchainbinom(x = 0:6, s0 = 5, sar = 0.11, generations = Inf)[2] != 0)

})


# Additional check of sum to 1


check_sum_to_1 <- function(s0, sar, g, i0 = 1){

  ss <- sum(dchainbinom(x = 0:s0, s0 = s0, i0=i0, sar= sar, g = g))

  if (ss == 1){
    return(TRUE)
  } else {
    abs(ss-1) < 1e-15
  }
}


test_that("PMF sum to 1", {

  expect_true(check_sum_to_1(s0 = 3, sar= 0.1, g = 1))
  expect_true(check_sum_to_1(s0 = 8, sar= 0.3, g = 1))
  expect_true(check_sum_to_1(s0 = 2, sar= 0.1, g = 1))
  expect_true(check_sum_to_1(s0 = 1, sar= 0.2, g = 1))
  expect_true(check_sum_to_1(s0 = 0, sar= 0.1, g = 1))

  expect_true(check_sum_to_1(s0 = 3, sar = 0.1, g = 3))
  expect_true(check_sum_to_1(s0 = 8, sar= 0.3, g = 3))
  expect_true(check_sum_to_1(s0 = 2, sar= 0.1, g = 3))
  expect_true(check_sum_to_1(s0 = 1, sar= 0.2, g = 3))
  expect_true(check_sum_to_1(s0 = 0, sar= 0.1, g = 3))

  expect_true(check_sum_to_1(s0 = 3, sar = 0.1, g = 8))
  expect_true(check_sum_to_1(s0 = 8, sar= 0.3, g = 8))
  expect_true(check_sum_to_1(s0 = 2, sar= 0.1, g = 8))
  expect_true(check_sum_to_1(s0 = 1, sar= 0.2, g = 8))
  expect_true(check_sum_to_1(s0 = 0, sar= 0.1, g = 8))


  expect_true(abs(sum(dcb_2_ginf) - 1) < 1e-15)

  # with i0 = 2.
  expect_true(check_sum_to_1(s0 = 3, sar= 0.1, g = 1, i0=2))
  expect_true(check_sum_to_1(s0 = 8, sar= 0.3, g = 1, i0=2))
  expect_true(check_sum_to_1(s0 = 2, sar= 0.1, g = 1, i0=2))
  expect_true(check_sum_to_1(s0 = 1, sar= 0.2, g = 1, i0=2))
  expect_true(check_sum_to_1(s0 = 0, sar= 0.1, g = 1, i0=2))

  expect_true(check_sum_to_1(s0 = 3, sar = 0.1, g = 3, i0=2))
  expect_true(check_sum_to_1(s0 = 8, sar= 0.3, g = 3, i0=2))
  expect_true(check_sum_to_1(s0 = 2, sar= 0.1, g = 3, i0=2))
  expect_true(check_sum_to_1(s0 = 1, sar= 0.2, g = 3, i0=2))
  expect_true(check_sum_to_1(s0 = 0, sar= 0.1, g = 3, i0=2))

  expect_true(check_sum_to_1(s0 = 3, sar = 0.1, g = 8, i0=2))
  expect_true(check_sum_to_1(s0 = 8, sar= 0.3, g = 8, i0=2))
  expect_true(check_sum_to_1(s0 = 2, sar= 0.1, g = 8, i0=2))
  expect_true(check_sum_to_1(s0 = 1, sar= 0.2, g = 8, i0=2))
  expect_true(check_sum_to_1(s0 = 0, sar= 0.1, g = 8, i0=2))

})


# PMF when g = 1. ----

# Function to test that when the numbers of genereations = 1,
# it should be the same as the ordinary binomial model.
compare_g1_binom <- function(s0, sar, tol = 0.0000001){

  probvec1 <- dchainbinom(x = 0:s0, s0 = s0, sar = sar, generations = 1)
  probvec1_binom <- dbinom(x = 0:s0, size = s0, prob = sar)

  all((probvec1 - probvec1_binom < tol))

}


test_that("PMF when g = 1", {

  expect_true(compare_g1_binom(s0 = 3, sar = 0.1))
  expect_true(compare_g1_binom(s0 = 8, sar = 0.3))
  expect_true(compare_g1_binom(s0 = 2, sar = 0.1))
  expect_true(compare_g1_binom(s0 = 1, sar = 0.2))
  expect_true(compare_g1_binom(s0 = 0, sar = 0.1))

  expect_true(dchainbinom(x = 1, s0 = 0, sar = 0.1, generations = 1) == 0)

})



# Prob for x = 0 ----
# The probability for x = 0 should be the same as the ordinary binomial, when i0=1,
# but not necessarily if i0 > 1.

# Explicit formula for x=0, also implemented in the dchainbinom function.
prob0 <- function(sar, s0, i0){
  (1 - sar)^(i0*s0)
}

prob0_tol <- 1e-06

test_that("P(x=0)", {

  expect_true(abs(prob0(sar = 0.1, s0=1, i0 = 1) - dbinom(x = 0, size = 1, prob = 0.1)) < prob0_tol)
  expect_true(abs(prob0(sar = 0.5, s0=1, i0 = 1) - dbinom(x = 0, size = 1, prob = 0.5)) < prob0_tol)

  expect_true(abs(prob0(sar = 0.1, s0=2, i0 = 1) - dbinom(x = 0, size = 2, prob = 0.1)) < prob0_tol)
  expect_true(abs(prob0(sar = 0.5, s0=2, i0 = 1) - dbinom(x = 0, size = 2, prob = 0.5)) < prob0_tol)

  expect_true(abs(prob0(sar = 0.1, s0=4, i0 = 1) - dbinom(x = 0, size = 4, prob = 0.1)) < prob0_tol)
  expect_true(abs(prob0(sar = 0.5, s0=4, i0 = 1) - dbinom(x = 0, size = 4, prob = 0.5)) < prob0_tol)

  expect_true(prob0(sar = 0.1, s0=2, i0 = 1) == dchainbinom(x = 0, s0 = 2, sar = 0.1, i0 = 1, generations = Inf))
  expect_true(prob0(sar = 0.1, s0=2, i0 = 1) == dchainbinom(x = 0, s0 = 2, sar = 0.1, i0 = 1, generations = 1))

  expect_true(prob0(sar = 0.1, s0=4, i0 = 1) == dchainbinom(x = 0, s0 = 4, sar = 0.1, i0 = 1, generations=Inf))
  expect_true(prob0(sar = 0.1, s0=4, i0 = 1) == dchainbinom(x = 0, s0 = 4, sar = 0.1, i0 = 1, generations=2))

  expect_true(dchainbinom(x = 0, s0 = 4, sar = 0.1, i0 = 1, generations = 2) == dchainbinom(x = 0, s0 = 4, sar = 0.1, i0=  1, generations = Inf))


  expect_false(prob0(sar = 0.1, s0=1, i0 = 2) == dbinom(x = 0, size = 1, prob = 0.1))
  expect_false(prob0(sar = 0.5, s0=1, i0 = 2) == dbinom(x = 0, size = 1, prob = 0.5))

  expect_false(prob0(sar = 0.1, s0=3, i0 = 2) == dbinom(x = 0, size = 3, prob = 0.1))
  expect_false(prob0(sar = 0.5, s0=3, i0 = 2) == dbinom(x = 0, size = 3, prob = 0.5))

  expect_false(dchainbinom(x = 0, s0 = 4, sar = 0.1, i0 = 2, generations = 2) == dchainbinom(x = 0, s0 = 4, sar = 0.1, i0=  1, generations = Inf))


})




# Compare PMF with simulation ----

# Function to approximate the chain binomial distribution after g generations
# using simulations.
#
# example:
# simulate_distribution(s0 = 5, sar = 0.1, g=3)
simulate_distribution <- function(s0, sar, g, i0 = 1){

  probs <- numeric(s0+1)
  simulated_cb <- rchainbinom(500000, s0 = s0, sar = sar, i0 = i0, g = g)
  sim_prop <- table(simulated_cb) / length(simulated_cb)
  probs[as.numeric(names(sim_prop)) + 1 ] <- sim_prop
  return(probs)
}


# Compares the PMF with simulated data.
compare_pmf_vs_simulation <- function(s0, sar, g, i0 = 1, tol = 0.001, maxtries = 3){

  for (ii in 1:maxtries){

    probvec <- dchainbinom(x = 0:s0, s0 = s0, sar = sar, i0 = i0, generations = g)
    probvec_sim <- simulate_distribution(s0 = s0, sar = sar, i0 = i0, g = g)

    if (all((probvec - probvec_sim < tol))){
      return(TRUE)
    } else {
      next
    }
  }

  return(FALSE)

}



if (FALSE){
  test_that("PMF vs simulation", {

    expect_true(compare_pmf_vs_simulation(s0 = 3, sar = 0.1, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 8, sar = 0.3, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 2, sar = 0.1, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 1, sar = 0.2, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 0, sar = 0.1, g = 1))

    expect_true(compare_pmf_vs_simulation(s0 = 3, sar = 0.1, g = 3))
    expect_true(compare_pmf_vs_simulation(s0 = 8, sar = 0.3, g = 3))
    expect_true(compare_pmf_vs_simulation(s0 = 2, sar = 0.1, g = 3))
    expect_true(compare_pmf_vs_simulation(s0 = 1, sar = 0.2, g = 3))
    expect_true(compare_pmf_vs_simulation(s0 = 0, sar = 0.1, g = 3))

    expect_true(compare_pmf_vs_simulation(s0 = 3, sar = 0.1, g = 8))
    expect_true(compare_pmf_vs_simulation(s0 = 8, sar = 0.3, g = 8))
    expect_true(compare_pmf_vs_simulation(s0 = 2, sar = 0.1, g = 8))
    expect_true(compare_pmf_vs_simulation(s0 = 1, sar = 0.2, g = 8))
    expect_true(compare_pmf_vs_simulation(s0 = 0, sar = 0.1, g = 8))


    # with i0 = 2
    expect_true(compare_pmf_vs_simulation(s0 = 3, sar = 0.1, i0 = 2, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 8, sar = 0.3, i0 = 2, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 2, sar = 0.1, i0 = 2, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 1, sar = 0.2, i0 = 2, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 0, sar = 0.1, i0 = 2, g = 1))

    expect_true(compare_pmf_vs_simulation(s0 = 3, sar = 0.1, i0 = 2, g = 3))
    expect_true(compare_pmf_vs_simulation(s0 = 8, sar = 0.3, i0 = 2, g = 3))
    expect_true(compare_pmf_vs_simulation(s0 = 2, sar = 0.1, i0 = 2, g = 3))
    expect_true(compare_pmf_vs_simulation(s0 = 1, sar = 0.2, i0 = 2, g = 3))
    expect_true(compare_pmf_vs_simulation(s0 = 0, sar = 0.1, i0 = 2, g = 3))

    expect_true(compare_pmf_vs_simulation(s0 = 3, sar = 0.1, i0 = 2, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 8, sar = 0.3, i0 = 2, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 2, sar = 0.1, i0 = 2, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 1, sar = 0.2, i0 = 2, g = 1))
    expect_true(compare_pmf_vs_simulation(s0 = 0, sar = 0.1, i0 = 2, g = 1))

  } )


}


# Expected value ----

# Expeced value should be same as for ordinary binomial when g=1.
ecb_vs_eb_g1 <- function(s0, sar){
  ecb <- echainbinom(s0 = s0, i0 = 1, sar = sar, generations = 1)
  eb <- s0*sar #sum(0:s0 * dbinom(x=0:s0, size=s0, prob = sar))

  abs(ecb - eb) < 0.00000001
}

test_that("Expected value g=1", {

  expect_true(ecb_vs_eb_g1(s0 = 3, sar=0.1))
  expect_true(ecb_vs_eb_g1(s0 = 3, sar=0.6))

  expect_true(ecb_vs_eb_g1(s0 = 2, sar=0.1))
  expect_true(ecb_vs_eb_g1(s0 = 2, sar=0.6))

  expect_true(ecb_vs_eb_g1(s0 = 9, sar=0.1))
  expect_true(ecb_vs_eb_g1(s0 = 9, sar=0.6))

})

# chain binomial expected value should be greater than ordinary binomial
# when g > 1.

ecb_vs_eb <- function(s0, sar, g){
  ecb <- echainbinom(s0 = s0, i0 = 1, sar = sar, generations = g)
  eb <- s0*sar

  ecb > eb
}


test_that("Expected value g>1", {

  expect_true(ecb_vs_eb(s0 = 3, sar=0.1, g=2))
  expect_true(ecb_vs_eb(s0 = 3, sar=0.6, g=2))
  expect_true(ecb_vs_eb(s0 = 2, sar=0.1, g=2))
  expect_true(ecb_vs_eb(s0 = 2, sar=0.6, g=2))
  expect_true(ecb_vs_eb(s0 = 9, sar=0.1, g=2))
  expect_true(ecb_vs_eb(s0 = 9, sar=0.6, g=2))

  expect_true(ecb_vs_eb(s0 = 3, sar=0.1, g=5))
  expect_true(ecb_vs_eb(s0 = 3, sar=0.6, g=5))
  expect_true(ecb_vs_eb(s0 = 2, sar=0.1, g=5))
  expect_true(ecb_vs_eb(s0 = 2, sar=0.6, g=5))
  expect_true(ecb_vs_eb(s0 = 9, sar=0.1, g=5))
  expect_true(ecb_vs_eb(s0 = 9, sar=0.6, g=5))

  expect_true(ecb_vs_eb(s0 = 3, sar=0.1, g=Inf))
  expect_true(ecb_vs_eb(s0 = 3, sar=0.6, g=Inf))
  expect_true(ecb_vs_eb(s0 = 2, sar=0.1, g=Inf))
  expect_true(ecb_vs_eb(s0 = 2, sar=0.6, g=Inf))
  expect_true(ecb_vs_eb(s0 = 9, sar=0.1, g=Inf))
  expect_true(ecb_vs_eb(s0 = 9, sar=0.6, g=Inf))

})

# The expected value should never be greater than s0.
ecb_le_s0 <- function(s0, sar, g){
  ecb <- echainbinom(s0 = s0, i0 = 1, sar = sar, generations = g)
  ecb < s0
}

test_that("Expected value < s0", {
  expect_true(ecb_le_s0(s0 = 3, sar=0.1, g=2))
  expect_true(ecb_le_s0(s0 = 3, sar=0.6, g=2))
  expect_true(ecb_le_s0(s0 = 2, sar=0.1, g=2))
  expect_true(ecb_le_s0(s0 = 2, sar=0.6, g=2))
  expect_true(ecb_le_s0(s0 = 9, sar=0.1, g=2))
  expect_true(ecb_le_s0(s0 = 9, sar=0.6, g=2))

  expect_true(ecb_le_s0(s0 = 3, sar=0.1, g=5))
  expect_true(ecb_le_s0(s0 = 3, sar=0.6, g=5))
  expect_true(ecb_le_s0(s0 = 2, sar=0.1, g=5))
  expect_true(ecb_le_s0(s0 = 2, sar=0.6, g=5))
  expect_true(ecb_le_s0(s0 = 9, sar=0.1, g=5))
  expect_true(ecb_le_s0(s0 = 9, sar=0.6, g=5))

  expect_true(ecb_le_s0(s0 = 3, sar=0.1, g=Inf))
  expect_true(ecb_le_s0(s0 = 3, sar=0.6, g=Inf))
  expect_true(ecb_le_s0(s0 = 2, sar=0.1, g=Inf))
  expect_true(ecb_le_s0(s0 = 2, sar=0.6, g=Inf))
  expect_true(ecb_le_s0(s0 = 9, sar=0.1, g=Inf))
  expect_true(ecb_le_s0(s0 = 9, sar=0.6, g=Inf))
})

# The expected value should equal s0 when sar = 1.
ecb_eq_s0_sar_eq_1 <- function(s0, i0, g){
  ecb <- echainbinom(s0 = s0, i0 = i0, sar = 1, generations = g)
  ecb == s0
}


test_that("Expected value == s0", {

  expect_true(ecb_eq_s0_sar_eq_1(s0  = 3, i0 = 1, g = 1))
  expect_true(ecb_eq_s0_sar_eq_1(s0  = 3, i0 = 1, g = 2))
  expect_true(ecb_eq_s0_sar_eq_1(s0  = 3, i0 = 1, g = 3))
  expect_true(ecb_eq_s0_sar_eq_1(s0  = 3, i0 = 1, g = Inf))

  expect_true(ecb_eq_s0_sar_eq_1(s0  = 3, i0 = 2, g = 1))
  expect_true(ecb_eq_s0_sar_eq_1(s0  = 3, i0 = 2, g = 2))
  expect_true(ecb_eq_s0_sar_eq_1(s0  = 3, i0 = 2, g = 3))
  expect_true(ecb_eq_s0_sar_eq_1(s0  = 3, i0 = 2, g = Inf))

  expect_true(ecb_eq_s0_sar_eq_1(s0  = 9, i0 = 2, g = 1))
  expect_true(ecb_eq_s0_sar_eq_1(s0  = 9, i0 = 2, g = 2))
  expect_true(ecb_eq_s0_sar_eq_1(s0  = 9, i0 = 2, g = 3))
  expect_true(ecb_eq_s0_sar_eq_1(s0  = 9, i0 = 2, g = Inf))
})


# Estimation and modelling ----

# Example data set that works for all link-functions.
mod_dat1 <- data.frame(infected = c(1, 0, 1, 0, 1, 0, 2, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1,0, 2, 1, 1, 0, 0, 1, 0, 1, 0, 0),
                       s0 = c(3, 1, 3, 2, 2, 1, 5, 2, 1, 1, 3, 2, 2, 1, 5, 3, 2, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 2, 1),
                       x = c(-0.03, 1.48, 0.06, -0.35, -1.69, 1.83, -0.05, 1.25, 1.28, -1.55,1.53, -0.17, 0.08, 0.32,
                             -1.39, 1.89, -0.47, -0.07, -1.19, -0.25, 0.65, 1.41, 0.84, -2.05, -0.15, 0.45, -1.48, -1.41, 2.16, -1.75))

# Make model matrix.
xmat <- model.matrix(~ x, data=mod_dat1)

# model matrix with missing values.
xmat_na <- xmat
xmat_na[16,2] <- NA

# Example data set where everyone are infected.
mod_dat_all <- data.frame(infected = c(2, 1, 2, 2, 2, 2, 5, 1, 1, 1, 1, 3, 1, 2, 2, 2, 1, 2, 1, 3),
                          s0 =  c(2, 1, 2, 2, 2, 2, 5, 1, 1, 1, 1, 3, 1, 2, 2, 2, 1, 2, 1, 3))


# Example data set where no one is infected.
mod_dat_none <- data.frame(infected = rep(0, 20),
                          s0 =  c(2, 2, 1, 2, 2, 2, 5, 1, 1, 1, 5, 3, 1, 2, 2, 4, 1, 1, 1, 1))


# Example data set where, which caused problems with 99% CI.
mod_dat2 <- data.frame(infected = c(2, 1, 2, 5, 2, 2, 2, 1, 3, 2),
           s0 = c(2, 2, 2, 5, 2, 2, 2, 1, 3, 2),
           generations = 1)


# modified data sets with missing values.
mod_dat1_na <- mod_dat1
mod_dat1_na$infected[c(5)] <- NA

mod_dat1_na2 <- mod_dat1
mod_dat1_na2$s0[c(8)] <- NA

mod_dat1_na3 <- mod_dat1
mod_dat1_na3$s0[c(8)] <- NA
mod_dat1_na3$infected[c(5)] <- NA


test_that("simple estimation works", {

  expect_no_condition(
    sar_est_1_ginf <- estimate_sar(infected = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf)
  )

  expect_no_condition(
    sar_est_1_g1 <- estimate_sar(infected = mod_dat1$infected, s0 = mod_dat1$s0, generations = 1)
  )

  expect_no_condition(
    sar_est_1_g2 <- estimate_sar(infected = mod_dat1$infected, s0 = mod_dat1$s0, generations = 2)
  )

  expect_no_condition(
    sar_est_2_g1 <- estimate_sar(infected = mod_dat2$infected, s0 = mod_dat2$s0, generations = 1)
  )

  expect_true('sar' %in% class(sar_est_1_ginf))

  # The reasonableness of results.
  expect_true(!is.na(sar_est_1_ginf$sar_hat))
  expect_true(is.numeric(sar_est_1_ginf$sar_hat))
  expect_true(sar_est_1_ginf$sar_hat <= 1)
  expect_true(sar_est_1_ginf$sar_hat >= 0)

  expect_true(!is.na(sar_est_1_g1$sar_hat))
  expect_true(is.numeric(sar_est_1_g1$sar_hat))
  expect_true(sar_est_1_g1$sar_hat <= 1)
  expect_true(sar_est_1_g1$sar_hat >= 0)

  expect_true(!is.na(sar_est_1_g2$sar_hat))
  expect_true(is.numeric(sar_est_1_g2$sar_hat))
  expect_true(sar_est_1_g2$sar_hat <= 1)
  expect_true(sar_est_1_g2$sar_hat >= 0)

  expect_true(!is.na(sar_est_2_g1$sar_hat))
  expect_true(is.numeric(sar_est_2_g1$sar_hat))
  expect_true(sar_est_2_g1$sar_hat <= 1)
  expect_true(sar_est_2_g1$sar_hat >= 0)

  # Estimates should not be the same.
  expect_false(sar_est_1_g2$sar_hat == sar_est_1_g1$sar_hat)
  expect_false(sar_est_1_g2$sar_hat == sar_est_1_ginf$sar_hat)
  expect_false(sar_est_1_g1$sar_hat == sar_est_1_ginf$sar_hat)


  # Confidence intervals.

  # Check default values
  expect_no_condition(
    sar_est_1_ginf_ci_default <- confint(sar_est_1_ginf)
  )

  expect_no_condition(
    sar_est_1_ginf_ci_99_chisq <- confint(sar_est_1_ginf, method = 'chisq', level = 0.99)
  )

  expect_no_condition(
    sar_est_1_ginf_ci_95_chisq <- confint(sar_est_1_ginf, method = 'chisq', level = 0.95)
  )

  expect_no_condition(
    sar_est_1_ginf_ci_90_chisq <- confint(sar_est_1_ginf, method = 'chisq', level = 0.9)
  )

  # Check that the upper and lower ends are correct.
  expect_true(sar_est_1_ginf_ci_default[1] < sar_est_1_ginf_ci_default[2])
  expect_true(sar_est_1_ginf_ci_99_chisq[1] < sar_est_1_ginf_ci_99_chisq[2])
  expect_true(sar_est_1_ginf_ci_95_chisq[1] < sar_est_1_ginf_ci_95_chisq[2])
  expect_true(sar_est_1_ginf_ci_90_chisq[1] < sar_est_1_ginf_ci_90_chisq[2])

  # Compare default arguments with explicitly provided arguments.
  expect_true(sar_est_1_ginf_ci_default[1] == sar_est_1_ginf_ci_95_chisq[1])
  expect_true(sar_est_1_ginf_ci_default[2] == sar_est_1_ginf_ci_95_chisq[2])


  expect_true(sar_est_1_ginf_ci_99_chisq[1] < sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_ginf_ci_95_chisq[1] < sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_ginf_ci_95_chisq[2] > sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_ginf_ci_99_chisq[2] > sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_ginf_ci_90_chisq[1] < sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_ginf_ci_90_chisq[2] > sar_est_1_ginf$sar_hat)


  # Check that the 95% interval is wider than the 90% interval.
  expect_true(sar_est_1_ginf_ci_99_chisq[1] < sar_est_1_ginf_ci_95_chisq[1])
  expect_true(sar_est_1_ginf_ci_95_chisq[1] < sar_est_1_ginf_ci_90_chisq[1])
  expect_true(sar_est_1_ginf_ci_95_chisq[2] > sar_est_1_ginf_ci_90_chisq[2])
  expect_true(sar_est_1_ginf_ci_99_chisq[2] > sar_est_1_ginf_ci_95_chisq[2])


  # for the generation = 1 estimates.

  expect_no_condition(
    sar_est_1_g1_ci_default <- confint(sar_est_1_g1)
  )

  expect_no_condition(
    sar_est_1_g1_ci_99_chisq <- confint(sar_est_1_g1, method = 'chisq', level = 0.99)
  )


  expect_no_condition(
    sar_est_1_g1_ci_95_chisq <- confint(sar_est_1_g1, method = 'chisq', level = 0.95)
  )

  expect_no_condition(
    sar_est_1_g1_ci_90_chisq <- confint(sar_est_1_g1, method = 'chisq', level = 0.9)
  )

  expect_true(sar_est_1_g1_ci_99_chisq[1] < sar_est_1_g1_ci_99_chisq[2])
  expect_true(sar_est_1_g1_ci_95_chisq[1] < sar_est_1_g1_ci_95_chisq[2])
  expect_true(sar_est_1_g1_ci_90_chisq[1] < sar_est_1_g1_ci_90_chisq[2])
  expect_true(sar_est_1_g1_ci_99_chisq[1] < sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_g1_ci_99_chisq[2] > sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_g1_ci_95_chisq[1] < sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_g1_ci_95_chisq[2] > sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_g1_ci_90_chisq[1] < sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_g1_ci_90_chisq[2] > sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_g1_ci_99_chisq[1] < sar_est_1_g1_ci_95_chisq[1])
  expect_true(sar_est_1_g1_ci_95_chisq[1] < sar_est_1_g1_ci_90_chisq[1])
  expect_true(sar_est_1_g1_ci_95_chisq[2] > sar_est_1_g1_ci_90_chisq[2])
  expect_true(sar_est_1_g1_ci_99_chisq[2] > sar_est_1_g1_ci_95_chisq[2])

  expect_true(sar_est_1_g1_ci_default[1] == sar_est_1_g1_ci_95_chisq[1])
  expect_true(sar_est_1_g1_ci_default[2] == sar_est_1_g1_ci_95_chisq[2])


  # Test confidence intervals computed using the the 'normal' method.

  expect_no_condition(
    sar_est_1_ginf_ci_95_norm <- confint(sar_est_1_ginf, method = 'normal', level = 0.95)
  )

  expect_no_condition(
    sar_est_1_ginf_ci_90_norm <- confint(sar_est_1_ginf, method = 'normal', level = 0.90)
  )


  # Check that the upper and lower ends are correct.
  expect_true(sar_est_1_ginf_ci_95_norm[1] < sar_est_1_ginf_ci_95_norm[2])
  expect_true(sar_est_1_ginf_ci_90_norm[1] < sar_est_1_ginf_ci_90_norm[2])

  expect_true(sar_est_1_ginf_ci_95_norm[1] < sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_ginf_ci_95_norm[2] > sar_est_1_ginf$sar_hat)

  expect_true(sar_est_1_ginf_ci_90_norm[1] < sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_ginf_ci_90_norm[2] > sar_est_1_ginf$sar_hat)

  # Check that the 95% interval is wider than the 90% interval.
  expect_true(sar_est_1_ginf_ci_95_norm[1] < sar_est_1_ginf_ci_90_norm[1])
  expect_true(sar_est_1_ginf_ci_95_norm[2] > sar_est_1_ginf_ci_90_norm[2])

  # Check that the normal and chisq methods does not give exactly the same answers.
  expect_false(sar_est_1_ginf_ci_95_norm[1] == sar_est_1_ginf_ci_95_chisq[1])
  expect_false(sar_est_1_ginf_ci_95_norm[2] == sar_est_1_ginf_ci_95_chisq[2])
  expect_false(sar_est_1_ginf_ci_90_norm[1] == sar_est_1_ginf_ci_90_chisq[1])
  expect_false(sar_est_1_ginf_ci_90_norm[2] == sar_est_1_ginf_ci_90_chisq[2])


  # Check the edge cases with none and all infected.

  expect_no_condition(
    sar_est_all_ginf <- estimate_sar(infected = mod_dat_all$infected, s0 = mod_dat_all$s0, generations = Inf)
  )

  expect_no_condition(
    sar_est_none_ginf <- estimate_sar(infected = mod_dat_none$infected, s0 = mod_dat_none$s0, generations = Inf)
  )

  # The reasonableness of results.
  expect_true(!is.na(sar_est_all_ginf$sar_hat))
  expect_true(is.numeric(sar_est_all_ginf$sar_hat))
  expect_true(sar_est_all_ginf$sar_hat <= 1)
  expect_true(sar_est_all_ginf$sar_hat >= 0)
  expect_true(sar_est_all_ginf$sar_hat >= 0.99) # Point estimate should be close to 1.

  expect_true(!is.na(sar_est_none_ginf$sar_hat))
  expect_true(is.numeric(sar_est_none_ginf$sar_hat))
  expect_true(sar_est_none_ginf$sar_hat <= 1)
  expect_true(sar_est_none_ginf$sar_hat >= 0)
  expect_true(sar_est_none_ginf$sar_hat <= 0.01) # Point estimate should be close to 0.




  # CI's for the "all infected" data.

  expect_no_condition(
    sar_est_all_ginf_ci_99_chisq <- confint(sar_est_all_ginf, method = 'chisq', level = 0.99)
  )

  expect_no_condition(
    sar_est_all_ginf_ci_95_chisq <- confint(sar_est_all_ginf, method = 'chisq', level = 0.95)
  )

  expect_no_condition(
    sar_est_all_ginf_ci_90_chisq <- confint(sar_est_all_ginf, method = 'chisq', level = 0.90)
  )

  expect_true(sar_est_all_ginf_ci_99_chisq[1] < sar_est_all_ginf_ci_99_chisq[2])
  expect_true(sar_est_all_ginf_ci_95_chisq[1] < sar_est_all_ginf_ci_95_chisq[2])
  expect_true(sar_est_all_ginf_ci_90_chisq[1] < sar_est_all_ginf_ci_90_chisq[2])

  expect_true(sar_est_all_ginf_ci_95_chisq[1] < sar_est_all_ginf$sar_hat)
  expect_true(sar_est_all_ginf_ci_95_chisq[2] >= sar_est_all_ginf$sar_hat)
  expect_true(sar_est_all_ginf_ci_90_chisq[1] < sar_est_all_ginf$sar_hat)
  expect_true(sar_est_all_ginf_ci_90_chisq[2] >= sar_est_all_ginf$sar_hat)

  expect_true(sar_est_all_ginf_ci_99_chisq[1] < sar_est_all_ginf_ci_95_chisq[1])
  expect_true(sar_est_all_ginf_ci_95_chisq[1] < sar_est_all_ginf_ci_90_chisq[1])
  expect_true(sar_est_all_ginf_ci_95_chisq[2] >= sar_est_all_ginf_ci_90_chisq[2])
  expect_true(sar_est_all_ginf_ci_99_chisq[2] >= sar_est_all_ginf_ci_95_chisq[2])

  # CI's for the "none infected" data.

  expect_no_condition(
    sar_est_none_ginf_ci_99_chisq <- confint(sar_est_none_ginf, method = 'chisq', level = 0.99)
  )


  expect_no_condition(
    sar_est_none_ginf_ci_95_chisq <- confint(sar_est_none_ginf, method = 'chisq', level = 0.95)
  )

  expect_no_condition(
    sar_est_none_ginf_ci_90_chisq <- confint(sar_est_none_ginf, method = 'chisq', level = 0.90)
  )


  expect_true(sar_est_none_ginf_ci_99_chisq[1] < sar_est_none_ginf_ci_99_chisq[2])
  expect_true(sar_est_none_ginf_ci_95_chisq[1] < sar_est_none_ginf_ci_95_chisq[2])
  expect_true(sar_est_none_ginf_ci_90_chisq[1] < sar_est_none_ginf_ci_90_chisq[2])

  expect_true(sar_est_none_ginf_ci_99_chisq[1] <= sar_est_none_ginf$sar_hat)
  expect_true(sar_est_none_ginf_ci_99_chisq[2] > sar_est_none_ginf$sar_hat)

  expect_true(sar_est_none_ginf_ci_95_chisq[1] <= sar_est_none_ginf$sar_hat)
  expect_true(sar_est_none_ginf_ci_95_chisq[2] > sar_est_none_ginf$sar_hat)
  expect_true(sar_est_none_ginf_ci_90_chisq[1] <= sar_est_none_ginf$sar_hat)
  expect_true(sar_est_none_ginf_ci_90_chisq[2] > sar_est_none_ginf$sar_hat)

  expect_true(sar_est_none_ginf_ci_99_chisq[1] <= sar_est_none_ginf_ci_95_chisq[1])
  expect_true(sar_est_none_ginf_ci_95_chisq[1] <= sar_est_none_ginf_ci_90_chisq[1])
  expect_true(sar_est_none_ginf_ci_95_chisq[2] > sar_est_none_ginf_ci_90_chisq[2])
  expect_true(sar_est_none_ginf_ci_99_chisq[2] > sar_est_none_ginf_ci_95_chisq[2])


  # Ci for mod_dat2.

  expect_no_condition(
    sar_est_2_g1_ci_99_chisq <- confint(sar_est_2_g1, method = 'chisq', level = 0.99)
  )

  expect_no_condition(
    sar_est_2_g1_ci_95_chisq <- confint(sar_est_2_g1, method = 'chisq', level = 0.95)
  )

  expect_true(sar_est_2_g1_ci_99_chisq[1] < sar_est_2_g1_ci_99_chisq[2])
  expect_true(sar_est_2_g1_ci_95_chisq[1] < sar_est_2_g1_ci_95_chisq[2])

  expect_true(sar_est_2_g1_ci_99_chisq[1] < sar_est_2_g1$sar_hat)
  expect_true(sar_est_2_g1_ci_99_chisq[2] > sar_est_2_g1$sar_hat)
  expect_true(sar_est_2_g1_ci_95_chisq[1] < sar_est_2_g1$sar_hat)
  expect_true(sar_est_2_g1_ci_95_chisq[2] > sar_est_2_g1$sar_hat)

  expect_true(sar_est_2_g1_ci_99_chisq[1] < sar_est_2_g1_ci_95_chisq[1])
  expect_true(sar_est_2_g1_ci_99_chisq[2] > sar_est_2_g1_ci_95_chisq[2])



  # Missing values.

  # Missing in infected
  expect_no_condition(
    sar_est_1_ginf_na <- estimate_sar(infected = mod_dat1_na$infected, s0 = mod_dat1_na$s0, generations = Inf)
  )

  expect_no_condition(
    sar_est_1_g1_na <- estimate_sar(infected = mod_dat1_na$infected, s0 = mod_dat1_na$s0, generations = 1)
  )

  expect_no_condition(
    sar_est_1_g2_na <- estimate_sar(infected = mod_dat1_na$infected, s0 = mod_dat1_na$s0, generations = 2)
  )


  # missing in s0
  expect_no_condition(
    sar_est_1_ginf_na2 <- estimate_sar(infected = mod_dat1_na2$infected, s0 = mod_dat1_na2$s0, generations = Inf)
  )

  expect_no_condition(
    sar_est_1_g1_na2 <- estimate_sar(infected = mod_dat1_na2$infected, s0 = mod_dat1_na2$s0, generations = 1)
  )

  expect_no_condition(
    sar_est_1_g2_na2 <- estimate_sar(infected = mod_dat1_na2$infected, s0 = mod_dat1_na2$s0, generations = 2)
  )

  # Check that the estimates are not identical.
  expect_true(sar_est_1_ginf_na$sar_hat != sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_g1_na$sar_hat != sar_est_1_g1$sar_hat)
  expect_true(sar_est_1_g2_na$sar_hat != sar_est_1_g2$sar_hat)

  expect_true(sar_est_1_ginf_na2$sar_hat != sar_est_1_ginf$sar_hat)
  expect_true(sar_est_1_g1_na2$sar_hat != sar_est_1_g1$sar_hat)
  expect_true(sar_est_1_g2_na2$sar_hat != sar_est_1_g2$sar_hat)

  # missing values CI
  expect_no_condition(
    sar_est_1_ginf_ci_95_chisq_na <- confint(sar_est_1_ginf_na, method = 'chisq', level = 0.95)
  )

  expect_no_condition(
    sar_est_1_ginf_ci_95_norm_na <- confint(sar_est_1_ginf_na, method = 'normal', level = 0.95)
  )

  expect_true(sar_est_1_ginf_ci_95_chisq_na[1] < sar_est_1_ginf_ci_95_chisq_na[2])
  expect_true(sar_est_1_ginf_ci_95_norm_na[1] < sar_est_1_ginf_ci_95_norm_na[2])

  # Reasonableness when there are missing values
  expect_true(!is.na(sar_est_1_ginf_na$sar_hat))
  expect_true(is.numeric(sar_est_1_ginf_na$sar_hat))
  expect_true(sar_est_1_ginf_na$sar_hat <= 1)
  expect_true(sar_est_1_ginf_na$sar_hat >= 0)


})


# cbmod(y = mod_dat1_na$infected, s0 = mod_dat1_na$s0, generations = Inf, x = xmat, link = 'identity')

# inp <- as.matrix(cbind(xmat, mod_dat1_na$infected, mod_dat1_na$s0, i0 = 1, generations = Inf))
# na_idx <- apply(inp, FUN = function(x){any(is.na(x))}, MARGIN = 1)
# inp <- inp[!na_idx,]
#
#
#
# glm_res_na <- glm(mod_dat1_na$infected ~ xmat, family = poisson())
# glm_res_na$na.action
#
# na.omit(mod_dat1_na$infected)
# na.omit(xmat, mod_dat1_na$infected)


test_that("modelling works", {

  expect_no_condition(
    cb_mod_res_id <- cbmod(y = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf, x = xmat, link = 'identity')
  )

  expect_no_condition(
    cb_mod_res_log <- cbmod(y = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf, x = xmat, link = 'log')
  )

  expect_no_condition(
    cb_mod_res_logit <- cbmod(y = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf, x = xmat, link = 'logit')
  )

  expect_no_condition(
    cb_mod_res_cloglog <- cbmod(y = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf, x = xmat, link = 'cloglog')
  )

  # with missing values
  expect_no_condition(
    cb_mod_res_id_na <- cbmod(y = mod_dat1_na$infected, s0 = mod_dat1_na$s0, generations = Inf, x = xmat, link = 'identity')
  )

  expect_no_condition(
    cb_mod_res_id_na2 <- cbmod(y = mod_dat1_na2$infected, s0 = mod_dat1_na2$s0, generations = Inf, x = xmat, link = 'identity')
  )

  expect_no_condition(
    cb_mod_res_id_na3 <- cbmod(y = mod_dat1_na3$infected, s0 = mod_dat1_na3$s0, generations = Inf, x = xmat, link = 'identity')
  )

  expect_no_condition(
    cb_mod_res_id_na4 <- cbmod(y = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf, x = xmat_na, link = 'identity')
  )


  expect_no_condition(
    cb_mod_res_id_na5 <- cbmod(y = mod_dat1_na3$infected, s0 = mod_dat1_na3$s0, generations = Inf, x = xmat_na, link = 'identity')
  )

  expect_true('cbmod' %in% class(cb_mod_res_id))
  expect_true('cbmod' %in% class(cb_mod_res_log))
  expect_true('cbmod' %in% class(cb_mod_res_logit))
  expect_true('cbmod' %in% class(cb_mod_res_cloglog))
  expect_true('cbmod' %in% class(cb_mod_res_id_na))
  expect_true('cbmod' %in% class(cb_mod_res_id_na2))
  expect_true('cbmod' %in% class(cb_mod_res_id_na3))
  expect_true('cbmod' %in% class(cb_mod_res_id_na4))
  expect_true('cbmod' %in% class(cb_mod_res_id_na5))

  expect_true(cb_mod_res_id$link == 'identity')
  expect_true(cb_mod_res_log$link == 'log')
  expect_true(cb_mod_res_logit$link == 'logit')
  expect_true(cb_mod_res_cloglog$link == 'cloglog')
  expect_true(cb_mod_res_id_na$link == 'identity')
  expect_true(cb_mod_res_id_na2$link == 'identity')
  expect_true(cb_mod_res_id_na3$link == 'identity')
  expect_true(cb_mod_res_id_na4$link == 'identity')
  expect_true(cb_mod_res_id_na5$link == 'identity')

  expect_true(length(cb_mod_res_id$parameters) == 2)
  expect_true(length(cb_mod_res_log$parameters) == 2)
  expect_true(length(cb_mod_res_logit$parameters) == 2)
  expect_true(length(cb_mod_res_cloglog$parameters) == 2)
  expect_true(length(cb_mod_res_id_na$parameters) == 2)
  expect_true(length(cb_mod_res_id_na2$parameters) == 2)
  expect_true(length(cb_mod_res_id_na3$parameters) == 2)
  expect_true(length(cb_mod_res_id_na4$parameters) == 2)
  expect_true(length(cb_mod_res_id_na5$parameters) == 2)

  expect_true(all(!is.na(cb_mod_res_id$parameters)))
  expect_true(all(!is.na(cb_mod_res_log$parameters)))
  expect_true(all(!is.na(cb_mod_res_logit$parameters)))
  expect_true(all(!is.na(cb_mod_res_cloglog$parameters)))
  expect_true(all(!is.na(cb_mod_res_id_na$parameters)))
  expect_true(all(!is.na(cb_mod_res_id_na2$parameters)))
  expect_true(all(!is.na(cb_mod_res_id_na3$parameters)))
  expect_true(all(!is.na(cb_mod_res_id_na4$parameters)))
  expect_true(all(!is.na(cb_mod_res_id_na5$parameters)))

  expect_true(!is.na(cb_mod_res_id$loglikelihood))
  expect_true(!is.na(cb_mod_res_log$loglikelihood))
  expect_true(!is.na(cb_mod_res_logit$loglikelihood))
  expect_true(!is.na(cb_mod_res_cloglog$loglikelihood))
  expect_true(!is.na(cb_mod_res_id_na$loglikelihood))
  expect_true(!is.na(cb_mod_res_id_na2$loglikelihood))
  expect_true(!is.na(cb_mod_res_id_na3$loglikelihood))
  expect_true(!is.na(cb_mod_res_id_na4$loglikelihood))
  expect_true(!is.na(cb_mod_res_id_na5$loglikelihood))

  expect_true(length(cb_mod_res_id$fitted_values) == nrow(mod_dat1))
  expect_true(length(cb_mod_res_log$fitted_values) == nrow(mod_dat1))
  expect_true(length(cb_mod_res_logit$fitted_values) == nrow(mod_dat1))
  expect_true(length(cb_mod_res_cloglog$fitted_values) == nrow(mod_dat1))
  expect_true(length(cb_mod_res_id_na$fitted_values) == nrow(mod_dat1) - 1) # minus 1 because of one missing value.
  expect_true(length(cb_mod_res_id_na2$fitted_values) == nrow(mod_dat1) - 1) # minus 1 because of one missing value.
  expect_true(length(cb_mod_res_id_na3$fitted_values) == nrow(mod_dat1) - 2) # minus 2 because of two missing values.
  expect_true(length(cb_mod_res_id_na4$fitted_values) == nrow(mod_dat1) - 1) # minus 1 because of one missing value.
  expect_true(length(cb_mod_res_id_na5$fitted_values) == nrow(mod_dat1) - 3) # minus 3 because of three missing value.

  expect_true(all(!is.na(cb_mod_res_id$fitted_values)))
  expect_true(all(!is.na(cb_mod_res_log$fitted_values)))
  expect_true(all(!is.na(cb_mod_res_logit$fitted_values)))
  expect_true(all(!is.na(cb_mod_res_cloglog$fitted_values)))
  expect_true(all(!is.na(cb_mod_res_id_na$fitted_values)))
  expect_true(all(!is.na(cb_mod_res_id_na2$fitted_values)))
  expect_true(all(!is.na(cb_mod_res_id_na3$fitted_values)))
  expect_true(all(!is.na(cb_mod_res_id_na4$fitted_values)))
  expect_true(all(!is.na(cb_mod_res_id_na5$fitted_values)))

  expect_true(length(cb_mod_res_id$sar_hat) == nrow(mod_dat1))
  expect_true(length(cb_mod_res_log$sar_hat) == nrow(mod_dat1))
  expect_true(length(cb_mod_res_logit$sar_hat) == nrow(mod_dat1))
  expect_true(length(cb_mod_res_cloglog$sar_hat) == nrow(mod_dat1))
  expect_true(length(cb_mod_res_id_na$sar_hat) == nrow(mod_dat1) - 1) # minus 1 because of one missing value.
  expect_true(length(cb_mod_res_id_na2$sar_hat) == nrow(mod_dat1) - 1) # minus 1 because of one missing value.
  expect_true(length(cb_mod_res_id_na3$sar_hat) == nrow(mod_dat1) - 2) # minus 2 because of two missing values.
  expect_true(length(cb_mod_res_id_na4$sar_hat) == nrow(mod_dat1) - 1) # minus 1 because of one missing value.
  expect_true(length(cb_mod_res_id_na5$sar_hat) == nrow(mod_dat1) - 3) # minus 3 because of three missing value.

  expect_true(nrow(cb_mod_res_id$vcov) == length(cb_mod_res_id$parameters))
  expect_true(nrow(cb_mod_res_log$vcov) == length(cb_mod_res_log$parameters))
  expect_true(nrow(cb_mod_res_logit$vcov) == length(cb_mod_res_logit$parameters))
  expect_true(nrow(cb_mod_res_cloglog$vcov) == length(cb_mod_res_cloglog$parameters))
  expect_true(nrow(cb_mod_res_id_na$vcov) == length(cb_mod_res_id_na$parameters))
  expect_true(nrow(cb_mod_res_id_na2$vcov) == length(cb_mod_res_id_na2$parameters))
  expect_true(nrow(cb_mod_res_id_na3$vcov) == length(cb_mod_res_id_na3$parameters))
  expect_true(nrow(cb_mod_res_id_na4$vcov) == length(cb_mod_res_id_na4$parameters))
  expect_true(nrow(cb_mod_res_id_na5$vcov) == length(cb_mod_res_id_na5$parameters))

  expect_true(nrow(cb_mod_res_id$vcov) == ncol(cb_mod_res_id$vcov))
  expect_true(nrow(cb_mod_res_log$vcov) == ncol(cb_mod_res_log$vcov))
  expect_true(nrow(cb_mod_res_logit$vcov) == ncol(cb_mod_res_logit$vcov))
  expect_true(nrow(cb_mod_res_cloglog$vcov) == ncol(cb_mod_res_cloglog$vcov))
  expect_true(nrow(cb_mod_res_id_na$vcov) == ncol(cb_mod_res_id_na$vcov))
  expect_true(nrow(cb_mod_res_id_na2$vcov) == ncol(cb_mod_res_id_na2$vcov))
  expect_true(nrow(cb_mod_res_id_na3$vcov) == ncol(cb_mod_res_id_na3$vcov))
  expect_true(nrow(cb_mod_res_id_na4$vcov) == ncol(cb_mod_res_id_na4$vcov))
  expect_true(nrow(cb_mod_res_id_na5$vcov) == ncol(cb_mod_res_id_na5$vcov))


  expect_true(length(cb_mod_res_id$omitted_values) == 0)
  expect_true(length(cb_mod_res_log$omitted_values) == 0)
  expect_true(length(cb_mod_res_logit$omitted_values) == 0)
  expect_true(length(cb_mod_res_cloglog$omitted_values) == 0)
  expect_true(length(cb_mod_res_id_na$omitted_values) == 1)
  expect_true(length(cb_mod_res_id_na2$omitted_values) == 1)
  expect_true(length(cb_mod_res_id_na3$omitted_values) == 2)
  expect_true(length(cb_mod_res_id_na4$omitted_values) == 1)
  expect_true(length(cb_mod_res_id_na5$omitted_values) == 3)


  # Confidence intervals.
  expect_no_condition(
    cbmod_ci_id <- confint(cb_mod_res_id, level = 0.95)
  )

  expect_no_condition(
    cbmod_ci_log <- confint(cb_mod_res_log, level = 0.95)
  )

  expect_no_condition(
    cbmod_ci_logit <- confint(cb_mod_res_logit, level = 0.95)
  )

  expect_no_condition(
    cbmod_ci_cloglog <- confint(cb_mod_res_cloglog, level = 0.95)
  )

  expect_no_condition(
    cbmod_ci_id_na <- confint(cb_mod_res_id_na, level = 0.95)
  )

  expect_no_condition(
    cbmod_ci_id_na2 <- confint(cb_mod_res_id_na2, level = 0.95)
  )

  expect_no_condition(
    cbmod_ci_id_na3 <- confint(cb_mod_res_id_na3, level = 0.95)
  )

  expect_no_condition(
    cbmod_ci_id_na4 <- confint(cb_mod_res_id_na4, level = 0.95)
  )

  expect_no_condition(
    cbmod_ci_id_na5 <- confint(cb_mod_res_id_na5, level = 0.95)
  )

  # Test parm argument
  expect_no_condition(confint(cb_mod_res_id, parm = c('x')))
  expect_no_condition(confint(cb_mod_res_id, parm = c('(Intercept)')))
  expect_no_condition(confint(cb_mod_res_id, parm = c('x', '(Intercept)')))
  expect_warning(confint(cb_mod_res_id, parm = c('x', '(Intercept)', 'zzz')))
  expect_error(confint(cb_mod_res_id, parm = c('zzz')))
  expect_no_condition(confint(cb_mod_res_id, parm = 1))
  expect_no_condition(confint(cb_mod_res_id, parm = 2))
  expect_no_condition(confint(cb_mod_res_id, parm = 1:2))
  expect_error(confint(cb_mod_res_id, parm = 1:3))
  expect_error(confint(cb_mod_res_id, parm = 3))
  expect_error(confint(cb_mod_res_id, parm = 0))


  expect_true(all(dim(cbmod_ci_id) == c(2,2)))
  expect_true(all(dim(cbmod_ci_log) == c(2,2)))
  expect_true(all(dim(cbmod_ci_logit) == c(2,2)))
  expect_true(all(dim(cbmod_ci_cloglog) == c(2,2)))
  expect_true(all(dim(cbmod_ci_id_na) == c(2,2)))
  expect_true(all(dim(cbmod_ci_id_na2) == c(2,2)))
  expect_true(all(dim(cbmod_ci_id_na3) == c(2,2)))
  expect_true(all(dim(cbmod_ci_id_na4) == c(2,2)))
  expect_true(all(dim(cbmod_ci_id_na5) == c(2,2)))

  expect_false(any(is.na(cbmod_ci_id)))
  expect_false(any(is.na(cbmod_ci_log)))
  expect_false(any(is.na(cbmod_ci_logit)))
  expect_false(any(is.na(cbmod_ci_cloglog)))
  expect_false(any(is.na(cbmod_ci_id_na)))
  expect_false(any(is.na(cbmod_ci_id_na2)))
  expect_false(any(is.na(cbmod_ci_id_na3)))
  expect_false(any(is.na(cbmod_ci_id_na4)))
  expect_false(any(is.na(cbmod_ci_id_na5)))

  expect_true(all(cbmod_ci_id[,1] < cbmod_ci_id[,2]))
  expect_true(all(cbmod_ci_log[,1] < cbmod_ci_log[,2]))
  expect_true(all(cbmod_ci_logit[,1] < cbmod_ci_logit[,2]))
  expect_true(all(cbmod_ci_cloglog[,1] < cbmod_ci_cloglog[,2]))
  expect_true(all(cbmod_ci_id_na[,1] < cbmod_ci_id_na[,2]))
  expect_true(all(cbmod_ci_id_na2[,1] < cbmod_ci_id_na2[,2]))
  expect_true(all(cbmod_ci_id_na3[,1] < cbmod_ci_id_na3[,2]))
  expect_true(all(cbmod_ci_id_na4[,1] < cbmod_ci_id_na4[,2]))
  expect_true(all(cbmod_ci_id_na5[,1] < cbmod_ci_id_na5[,2]))

  expect_true(all(cbmod_ci_id[,1] < cb_mod_res_id$parameters))
  expect_true(all(cbmod_ci_log[,1] < cb_mod_res_log$parameters))
  expect_true(all(cbmod_ci_logit[,1] < cb_mod_res_logit$parameters))
  expect_true(all(cbmod_ci_cloglog[,1] < cb_mod_res_cloglog$parameters))
  expect_true(all(cbmod_ci_id_na[,1] < cb_mod_res_id_na$parameters))
  expect_true(all(cbmod_ci_id_na2[,1] < cb_mod_res_id_na2$parameters))
  expect_true(all(cbmod_ci_id_na3[,1] < cb_mod_res_id_na3$parameters))
  expect_true(all(cbmod_ci_id_na4[,1] < cb_mod_res_id_na4$parameters))
  expect_true(all(cbmod_ci_id_na5[,1] < cb_mod_res_id_na5$parameters))

  expect_true(all(cbmod_ci_id[,2] > cb_mod_res_id$parameters))
  expect_true(all(cbmod_ci_log[,2] > cb_mod_res_log$parameters))
  expect_true(all(cbmod_ci_logit[,2] > cb_mod_res_logit$parameters))
  expect_true(all(cbmod_ci_cloglog[,2] > cb_mod_res_cloglog$parameters))
  expect_true(all(cbmod_ci_id_na[,2] > cb_mod_res_id_na$parameters))
  expect_true(all(cbmod_ci_id_na2[,2] > cb_mod_res_id_na2$parameters))
  expect_true(all(cbmod_ci_id_na3[,2] > cb_mod_res_id_na3$parameters))
  expect_true(all(cbmod_ci_id_na4[,2] > cb_mod_res_id_na4$parameters))
  expect_true(all(cbmod_ci_id_na5[,2] > cb_mod_res_id_na5$parameters))

})


# Copypasta from "modelling works"
cb_mod_res_id <- cbmod(y = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf, x = xmat, link = 'identity')
cb_mod_res_log <- cbmod(y = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf, x = xmat, link = 'log')
cb_mod_res_logit <- cbmod(y = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf, x = xmat, link = 'logit')
cb_mod_res_cloglog <- cbmod(y = mod_dat1$infected, s0 = mod_dat1$s0, generations = Inf, x = xmat, link = 'cloglog')

# extreme value x = -50 gives negative sar for identity link.
my_new_data <- data.frame(x=c(-50, -2, -1, 0, 1, 2))
newx <- model.matrix(~ x, data = my_new_data)


test_that("making predictions", {

  expect_no_condition(
    cb_mod_pred_link_id <- predict(cb_mod_res_id, x = newx, type = 'link')
  )

  expect_no_condition(
    cb_mod_pred_link_log <- predict(cb_mod_res_log, x = newx, type = 'link')
  )

  expect_no_condition(
    cb_mod_pred_link_logit <- predict(cb_mod_res_logit, x = newx, type = 'link')
  )

  expect_no_condition(
    cb_mod_pred_link_cloglog <- predict(cb_mod_res_cloglog, x = newx, type = 'link')
  )

  expect_warning(
    cb_mod_pred_sar_id <- predict(cb_mod_res_id, x = newx, type = 'sar') # gives a warning.
  )

  expect_no_condition(
    cb_mod_pred_sar_log <- predict(cb_mod_res_log, x = newx, type = 'sar')
  )

  expect_no_condition(
    cb_mod_pred_sar_logit <- predict(cb_mod_res_logit, x = newx, type = 'sar')
  )

  expect_no_condition(
    cb_mod_pred_sar_cloglog <- predict(cb_mod_res_cloglog, x = newx, type = 'sar')
  )



  expect_true(length(cb_mod_pred_link_id) == nrow(newx))
  expect_true(length(cb_mod_pred_link_log) == nrow(newx))
  expect_true(length(cb_mod_pred_link_logit) == nrow(newx))
  expect_true(length(cb_mod_pred_link_cloglog) == nrow(newx))

  expect_true(any(!is.na(cb_mod_pred_link_id)))
  expect_true(any(!is.na(cb_mod_pred_link_log)))
  expect_true(any(!is.na(cb_mod_pred_link_logit)))
  expect_true(any(!is.na(cb_mod_pred_link_cloglog)))

  expect_true(length(cb_mod_pred_sar_id) == nrow(newx))
  expect_true(length(cb_mod_pred_sar_log) == nrow(newx))
  expect_true(length(cb_mod_pred_sar_logit) == nrow(newx))
  expect_true(length(cb_mod_pred_sar_cloglog) == nrow(newx))

  expect_true(any(!is.na(cb_mod_pred_sar_id)))
  expect_true(any(!is.na(cb_mod_pred_sar_log)))
  expect_true(any(!is.na(cb_mod_pred_sar_logit)))
  expect_true(any(!is.na(cb_mod_pred_sar_cloglog)))

  expect_true(identical(cb_mod_pred_link_id, cb_mod_pred_sar_id))
  expect_false(identical(cb_mod_pred_link_log, cb_mod_pred_sar_log))
  expect_false(identical(cb_mod_pred_link_logit, cb_mod_pred_sar_logit))
  expect_false(identical(cb_mod_pred_link_cloglog, cb_mod_pred_sar_cloglog))

  expect_true(all(cb_mod_pred_sar_log > 0))
  expect_true(all(cb_mod_pred_sar_logit > 0))
  expect_true(all(cb_mod_pred_sar_cloglog > 0))

  expect_true(all(cb_mod_pred_sar_log < 1))
  expect_true(all(cb_mod_pred_sar_logit < 1))
  expect_true(all(cb_mod_pred_sar_cloglog < 1))

  # Check that predict gives the same sar hat as the fitted model object.
  expect_true(all(predict(cb_mod_res_id, x = xmat, type = 'sar') == cb_mod_res_id$sar_hat))
  expect_true(all(predict(cb_mod_res_log, x = xmat, type = 'sar') == cb_mod_res_log$sar_hat))
  expect_true(all(predict(cb_mod_res_logit, x = xmat, type = 'sar') == cb_mod_res_logit$sar_hat))
  expect_true(all(predict(cb_mod_res_cloglog, x = xmat, type = 'sar') == cb_mod_res_cloglog$sar_hat))


})


# Missing values ----

x_input_na <- c(NA, 0, 2, 3, NA, NA)
dcb_na1 <- dchainbinom(x = x_input_na, s0 = 5, sar = 0.11, generations = 1)

s0_input_na <- c(3, 3, 4, 5, 6, NA)
dcb_na2 <- dchainbinom(x = 0:5, s0 = s0_input_na, sar = 0.11, generations = 1)

sar_input_na <- c(NA, 0.2, 0.1, 0.5, 0.21, NA)
dcb_na3 <- dchainbinom(x = 0:5, s0 = 3, sar = sar_input_na, generations = Inf)

generations_input_na <- c(1, 2, 3, NA, Inf, NA)
dcb_na4 <- dchainbinom(x = 0:5, s0 = 5, sar = 0.11, generations = generations_input_na)

# One NA that causes all values to be NA because of recycling.
dcb_na5 <- dchainbinom(x = NA, s0 = 5, sar = 0.11, generations = 2)
dcb_na6 <- dchainbinom(x = 0:5, s0 = NA, sar = 0.11, generations = 2)
dcb_na7 <- dchainbinom(x = NA, s0 = 5, sar = NA, generations = Inf)
dcb_na8 <- dchainbinom(x = NA, s0 = 5, sar = 0.11, generations = NA)


test_that("dchainbinom NA", {

  expect_true(all(is.na(dcb_na1) == is.na(x_input_na)))
  expect_true(all(is.na(dcb_na2) == is.na(s0_input_na)))
  expect_true(all(is.na(dcb_na3) == is.na(sar_input_na)))
  expect_true(all(is.na(dcb_na4) == is.na(generations_input_na)))

  expect_true(all(is.na(dcb_na5)))
  expect_true(all(is.na(dcb_na6)))
  expect_true(all(is.na(dcb_na7)))
  expect_true(all(is.na(dcb_na8)))

})



rcb_na2 <- rchainbinom(n = 6, s0 = s0_input_na, sar = 0.11, generations = 2)
rcb_na3 <- rchainbinom(n = 6, s0 = 5, sar = sar_input_na, generations = Inf)
rcb_na4 <- rchainbinom(n = 6, s0 = 5, sar = 0.34, generations = generations_input_na)


test_that("rchainbinom NA", {

  expect_true(all(is.na(rcb_na2) == is.na(s0_input_na)))
  expect_true(all(is.na(rcb_na3) == is.na(sar_input_na)))
  expect_true(all(is.na(rcb_na4) == is.na(generations_input_na)))

})


ecb_na2 <- echainbinom(s0 = s0_input_na, sar = 0.11, generations = 2)
ecb_na3 <- echainbinom(s0 = 5, sar = sar_input_na, generations = Inf)
ecb_na4 <- echainbinom(s0 = 5, sar = 0.34, generations = generations_input_na)

# One NA that causes all values to be NA because of recycling.
ecb_na6 <- echainbinom(s0 = NA, sar = 0.41, generations = 1:4)
ecb_na7 <- echainbinom(s0 = 5, sar = NA, generations = 1:4)
ecb_na8 <- echainbinom(s0 = 1:4, sar = 0.41, generations = NA)

test_that("echainbinom NA", {

  expect_true(all(is.na(ecb_na2) == is.na(s0_input_na)))
  expect_true(all(is.na(ecb_na3) == is.na(sar_input_na)))
  expect_true(all(is.na(ecb_na4) == is.na(generations_input_na)))

  expect_true(all(is.na(ecb_na6)))
  expect_true(all(is.na(ecb_na7)))
  expect_true(all(is.na(ecb_na8)))

})









