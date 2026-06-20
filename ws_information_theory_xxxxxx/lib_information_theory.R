# =============================================================================
# Information Theory Helper Functions
# =============================================================================
# Provides functions for entropy calculation, simulation, channel modelling,
# source coding, and information-theoretic visualisation.
#
# Author: Mick Cooney <mickcooney@gmail.com>
# =============================================================================


# =============================================================================
# ENTROPY CALCULATIONS
# =============================================================================

#' Calculate Shannon Entropy of a Discrete Distribution
#'
#' Computes Shannon entropy H(X) = -sum(p * log2(p)) for a probability vector.
#' Zero probabilities are handled safely (0 * log(0) = 0 by convention).
#'
#' @param probs Numeric vector: Probability masses (must sum to 1, entries >= 0)
#' @param base  Numeric: Logarithm base. 2 = bits, exp(1) = nats, 10 = dits.
#'              Defaults to 2.
#'
#' @return Numeric scalar: Entropy in the specified units
#'
#' @examples
#' shannon_entropy(c(0.5, 0.5))          # 1 bit — fair coin
#' shannon_entropy(c(1, 0))              # 0 bits — certain outcome
#' shannon_entropy(rep(1/6, 6))          # ~2.585 bits — fair die
#' shannon_entropy(rep(1/6, 6), base=10) # in dits
#'
shannon_entropy <- function(probs, base = 2) {
  stopifnot(all(probs >= 0), abs(sum(probs) - 1) < 1e-9)

  safe_log <- function(p, b) ifelse(p == 0, 0, log(p, base = b))

  -sum(probs * safe_log(probs, base))
}


#' Shannon Entropy from a Frequency Table
#'
#' Convenience wrapper: computes empirical entropy from raw counts by first
#' normalising to probabilities then calling shannon_entropy().
#'
#' @param counts Integer vector: Non-negative event counts
#' @param base   Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Empirical entropy estimate
#'
#' @examples
#' entropy_from_counts(c(50, 50))        # ~1 bit
#' entropy_from_counts(c(90, 10))        # ~0.469 bits
#' entropy_from_counts(table(c("a","b","a","c","a","b")))
#'
entropy_from_counts <- function(counts, base = 2) {
  counts <- as.numeric(counts)
  stopifnot(all(counts >= 0), sum(counts) > 0)

  probs <- counts / sum(counts)
  shannon_entropy(probs, base = base)
}


#' Rényi Entropy of a Discrete Distribution
#'
#' Computes the Rényi entropy of order alpha:
#'   H_alpha(X) = (1 / (1 - alpha)) * log2(sum(p^alpha))
#'
#' Special cases:
#'   - alpha -> 1: recovers Shannon entropy
#'   - alpha = 0:  Hartley entropy (log2 of support size)
#'   - alpha = 2:  Collision entropy
#'   - alpha -> Inf: Min-entropy (log2(1/max(p)))
#'
#' @param probs Numeric vector: Probability distribution
#' @param alpha Numeric: Order parameter (alpha >= 0, alpha != 1)
#' @param base  Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Rényi entropy
#'
#' @examples
#' p <- c(0.4, 0.3, 0.2, 0.1)
#' renyi_entropy(p, alpha = 2)     # collision entropy
#' renyi_entropy(p, alpha = 0.5)   # Hartley-style
#' renyi_entropy(p, alpha = 0.999) # approx Shannon
#'
renyi_entropy <- function(probs, alpha, base = 2) {
  stopifnot(all(probs >= 0), abs(sum(probs) - 1) < 1e-9, alpha >= 0)

  if (abs(alpha - 1) < 1e-10) {
    # Limit as alpha -> 1 is Shannon entropy
    return(shannon_entropy(probs, base = base))
  }

  if (is.infinite(alpha)) {
    # Min-entropy
    return(-log(max(probs), base = base))
  }

  (1 / (1 - alpha)) * log(sum(probs^alpha), base = base)
}


#' Joint Entropy of Two Discrete Variables
#'
#' Computes H(X, Y) = -sum_x sum_y P(x,y) * log2(P(x,y)) from the joint
#' probability matrix.
#'
#' @param joint_probs Numeric matrix: Joint probabilities P(X=i, Y=j).
#'   Rows = X values, columns = Y values. Must sum to 1.
#' @param base Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Joint entropy H(X,Y)
#'
#' @examples
#' # Independent uniform
#' joint <- matrix(rep(0.25, 4), 2, 2)
#' joint_entropy(joint)  # 2 bits = H(X) + H(Y) when independent
#'
joint_entropy <- function(joint_probs, base = 2) {
  stopifnot(abs(sum(joint_probs) - 1) < 1e-9, all(joint_probs >= 0))

  safe_log <- function(p, b) ifelse(p == 0, 0, log(p, base = b))

  -sum(joint_probs * safe_log(joint_probs, base))
}


#' Conditional Entropy H(Y|X)
#'
#' Computes conditional entropy H(Y|X) = H(X,Y) - H(X).
#'
#' @param joint_probs Numeric matrix: Joint probabilities (rows=X, cols=Y)
#' @param base Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Conditional entropy H(Y|X)
#'
conditional_entropy <- function(joint_probs, base = 2) {
  hxy <- joint_entropy(joint_probs, base = base)
  hx  <- shannon_entropy(rowSums(joint_probs), base = base)
  hxy - hx
}


#' Mutual Information I(X;Y)
#'
#' Computes I(X;Y) = H(X) + H(Y) - H(X,Y).
#'
#' @param joint_probs Numeric matrix: Joint probabilities (rows=X, cols=Y)
#' @param base Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Mutual information (always >= 0)
#'
#' @examples
#' # Independent: MI should be ~0
#' p_indep <- outer(c(0.5, 0.5), c(0.3, 0.7))
#' mutual_information(p_indep)
#'
#' # Perfectly dependent (diagonal)
#' p_dep <- diag(c(0.5, 0.5))
#' mutual_information(p_dep)  # = H(X)
#'
mutual_information <- function(joint_probs, base = 2) {
  hx  <- shannon_entropy(rowSums(joint_probs), base = base)
  hy  <- shannon_entropy(colSums(joint_probs), base = base)
  hxy <- joint_entropy(joint_probs, base = base)
  hx + hy - hxy
}


# =============================================================================
# KL DIVERGENCE AND CROSS-ENTROPY
# =============================================================================

#' KL Divergence D_KL(P || Q)
#'
#' Computes the Kullback-Leibler divergence from Q to P:
#'   D_KL(P||Q) = sum_x P(x) * log2(P(x) / Q(x))
#'
#' Note: KL divergence is NOT symmetric. D_KL(P||Q) != D_KL(Q||P) in general.
#' Returns Inf if Q(x) = 0 for any x where P(x) > 0.
#'
#' @param p Numeric vector: True distribution P
#' @param q Numeric vector: Approximating distribution Q
#' @param base Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: KL divergence (>= 0)
#'
#' @examples
#' p <- c(0.4, 0.4, 0.2)
#' q <- c(1/3, 1/3, 1/3)
#' kl_divergence(p, q)   # > 0
#' kl_divergence(q, p)   # different value (asymmetry)
#' kl_divergence(p, p)   # 0
#'
kl_divergence <- function(p, q, base = 2) {
  stopifnot(
    length(p) == length(q),
    all(p >= 0), abs(sum(p) - 1) < 1e-9,
    all(q >= 0), abs(sum(q) - 1) < 1e-9
  )

  # Where p > 0 and q == 0: KL is infinite
  if (any(p > 0 & q == 0)) return(Inf)

  terms <- ifelse(p == 0, 0, p * log(p / q, base = base))
  sum(terms)
}


#' Cross-Entropy H(P, Q)
#'
#' Computes cross-entropy: H(P, Q) = -sum P(x) * log2(Q(x))
#' Decomposes as H(P, Q) = H(P) + D_KL(P||Q).
#'
#' @param p Numeric vector: True distribution P
#' @param q Numeric vector: Model distribution Q
#' @param base Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Cross-entropy
#'
cross_entropy <- function(p, q, base = 2) {
  stopifnot(
    length(p) == length(q),
    all(p >= 0), abs(sum(p) - 1) < 1e-9,
    all(q >= 0)
  )

  terms <- ifelse(p == 0, 0, -p * log(q, base = base))
  sum(terms)
}


#' Jensen-Shannon Divergence JSD(P || Q)
#'
#' A symmetric, bounded divergence: JSD = (KL(P||M) + KL(Q||M)) / 2
#' where M = (P + Q) / 2. The square root is a proper metric.
#'
#' @param p Numeric vector: First distribution
#' @param q Numeric vector: Second distribution
#' @param base Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: JSD in [0, 1] (bits, when base=2)
#'
js_divergence <- function(p, q, base = 2) {
  m <- (p + q) / 2
  0.5 * kl_divergence(p, m, base = base) + 0.5 * kl_divergence(q, m, base = base)
}


# =============================================================================
# DIFFERENTIAL ENTROPY
# =============================================================================

#' Differential Entropy of the Normal Distribution
#'
#' Analytical formula: h(X) = 0.5 * log2(2*pi*e*sigma^2)
#'
#' @param sigma Numeric: Standard deviation (> 0)
#' @param base  Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Differential entropy
#'
diff_entropy_normal <- function(sigma, base = 2) {
  0.5 * log(2 * pi * exp(1) * sigma^2, base = base)
}


#' Differential Entropy of the Exponential Distribution
#'
#' Analytical formula: h(X) = 1 - log2(lambda)
#'
#' @param lambda Numeric: Rate parameter (> 0)
#' @param base   Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Differential entropy
#'
diff_entropy_exp <- function(lambda, base = 2) {
  1 - log(lambda, base = base)
}


#' Differential Entropy of the Uniform Distribution
#'
#' Analytical formula: h(X) = log2(b - a)
#'
#' @param a Numeric: Lower bound
#' @param b Numeric: Upper bound (b > a)
#' @param base Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Differential entropy
#'
diff_entropy_uniform <- function(a, b, base = 2) {
  stopifnot(b > a)
  log(b - a, base = base)
}


#' Estimate Differential Entropy via Kernel Density
#'
#' Uses a KDE on samples to numerically approximate h(X) = -integral f(x) log f(x) dx.
#'
#' @param samples Numeric vector: Random samples from unknown distribution
#' @param n_grid  Integer: Grid resolution for integration (default 512)
#' @param base    Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Estimated differential entropy
#'
estimate_diff_entropy_kde <- function(samples, n_grid = 512L, base = 2) {
  dens <- density(samples, n = n_grid)
  f    <- dens$y
  dx   <- diff(dens$x)[1]

  # Numerical integration: -integral f log f dx
  terms <- ifelse(f <= 0, 0, -f * log(f, base = base))
  sum(terms) * dx
}


# =============================================================================
# CHANNEL MODELS
# =============================================================================

#' Simulate a Binary Symmetric Channel (BSC)
#'
#' Each bit in the input is flipped independently with probability p_error.
#'
#' @param bits    Integer vector: Input bits (0s and 1s)
#' @param p_error Numeric: Crossover probability in [0, 0.5]
#'
#' @return Integer vector: Received bits (same length as input)
#'
#' @examples
#' tx <- c(0L, 1L, 1L, 0L, 1L)
#' simulate_bsc(tx, p_error = 0.1)
#'
simulate_bsc <- function(bits, p_error) {
  stopifnot(all(bits %in% c(0L, 1L)), p_error >= 0, p_error <= 0.5)

  flips <- rbinom(length(bits), size = 1, prob = p_error)
  bitwXor(as.integer(bits), as.integer(flips))
}


#' BSC Channel Capacity
#'
#' C = 1 - H_b(p)  where H_b(p) = -p*log2(p) - (1-p)*log2(1-p) is the
#' binary entropy function.
#'
#' @param p_error Numeric or vector: Crossover probability
#'
#' @return Numeric: Channel capacity in bits per channel use
#'
bsc_capacity <- function(p_error) {
  h_b <- function(p) {
    ifelse(p == 0 | p == 1, 0, -p * log2(p) - (1 - p) * log2(1 - p))
  }
  1 - h_b(p_error)
}


#' Simulate a Binary Erasure Channel (BEC)
#'
#' Each bit is either received correctly (probability 1 - epsilon) or
#' erased (returned as NA, probability epsilon).
#'
#' @param bits    Integer vector: Input bits (0s and 1s)
#' @param epsilon Numeric: Erasure probability in [0, 1]
#'
#' @return Integer vector: Received bits (NA = erased)
#'
simulate_bec <- function(bits, epsilon) {
  stopifnot(all(bits %in% c(0L, 1L)), epsilon >= 0, epsilon <= 1)

  erased <- rbinom(length(bits), size = 1, prob = epsilon) == 1L
  result <- bits
  result[erased] <- NA_integer_
  result
}


#' BEC Channel Capacity
#'
#' C = 1 - epsilon
#'
#' @param epsilon Numeric or vector: Erasure probability
#'
#' @return Numeric: Channel capacity in bits per channel use
#'
bec_capacity <- function(epsilon) {
  1 - epsilon
}


#' AWGN Channel Capacity (Shannon-Hartley)
#'
#' C = (1/2) * log2(1 + SNR)  where SNR = signal power / noise power.
#'
#' @param snr    Numeric or vector: Signal-to-noise ratio (linear, not dB)
#' @param bw     Numeric: Bandwidth in Hz (default 1 — gives capacity per Hz)
#'
#' @return Numeric: Channel capacity in bits per channel use (or bits/s if bw > 1)
#'
awgn_capacity <- function(snr, bw = 1) {
  bw * log2(1 + snr)
}


# =============================================================================
# SOURCE CODING / HUFFMAN
# =============================================================================

#' Build a Huffman Code for a Symbol Distribution
#'
#' Implements a simple Huffman tree using a priority queue (base R, no extra
#' packages needed). Returns a data frame with symbol, probability, and binary
#' codeword.
#'
#' @param symbols Character vector: Symbol names
#' @param probs   Numeric vector: Probabilities (must sum to 1)
#'
#' @return Tibble with columns: symbol, probability, codeword, code_length
#'
#' @examples
#' build_huffman_code(c("A","B","C","D"), c(0.4, 0.3, 0.2, 0.1))
#'
build_huffman_code <- function(symbols, probs) {
  stopifnot(length(symbols) == length(probs), abs(sum(probs) - 1) < 1e-9)

  # Represent each leaf as a list(prob, symbol, left=NULL, right=NULL)
  nodes <- lapply(seq_along(symbols), function(i) {
    list(prob = probs[i], symbol = symbols[i], left = NULL, right = NULL)
  })

  # Build tree by repeatedly merging two smallest nodes
  while (length(nodes) > 1) {
    # Sort by probability
    probs_now <- vapply(nodes, `[[`, numeric(1), "prob")
    ord       <- order(probs_now)
    nodes     <- nodes[ord]

    # Merge the two smallest
    left  <- nodes[[1]]
    right <- nodes[[2]]

    parent <- list(
      prob   = left$prob + right$prob,
      symbol = NA_character_,
      left   = left,
      right  = right
    )

    nodes <- c(list(parent), nodes[-(1:2)])
  }

  root <- nodes[[1]]

  # Traverse tree to extract codewords
  codes <- character(length(symbols))
  names(codes) <- symbols

  traverse <- function(node, prefix) {
    if (!is.null(node$symbol) && !is.na(node$symbol)) {
      codes[node$symbol] <<- prefix
    } else {
      traverse(node$left,  paste0(prefix, "0"))
      traverse(node$right, paste0(prefix, "1"))
    }
  }

  traverse(root, "")

  tibble::tibble(
    symbol      = symbols,
    probability = probs,
    codeword    = codes[symbols],
    code_length = nchar(codes[symbols])
  )
}


#' Compute Expected Code Length of a Huffman Code
#'
#' @param huffman_tbl Tibble: Output of build_huffman_code()
#'
#' @return Numeric: Expected code length in bits per symbol
#'
huffman_expected_length <- function(huffman_tbl) {
  sum(huffman_tbl$probability * huffman_tbl$code_length)
}


#' Encode a Message Using a Huffman Code Table
#'
#' @param message     Character vector: Sequence of symbols to encode
#' @param huffman_tbl Tibble: Output of build_huffman_code()
#'
#' @return Character scalar: Concatenated binary string
#'
huffman_encode <- function(message, huffman_tbl) {
  code_map <- setNames(huffman_tbl$codeword, huffman_tbl$symbol)
  paste(code_map[message], collapse = "")
}


# =============================================================================
# SIMULATION UTILITIES
# =============================================================================

#' Simulate Entropy as a Function of Sample Size
#'
#' Draws increasing samples from a distribution and estimates entropy at each
#' step, allowing convergence to be visualised.
#'
#' @param probs      Numeric vector: True distribution
#' @param sample_sizes Integer vector: Sample sizes to try
#' @param n_reps     Integer: Repetitions per sample size (for CI)
#' @param base       Numeric: Logarithm base (default 2)
#'
#' @return Tibble with columns: n, rep, entropy_estimate
#'
simulate_entropy_convergence <- function(probs, sample_sizes, n_reps = 50L, base = 2) {
  symbols <- seq_along(probs)

  purrr::map_dfr(sample_sizes, function(n) {
    purrr::map_dfr(seq_len(n_reps), function(r) {
      draws  <- sample(symbols, size = n, replace = TRUE, prob = probs)
      counts <- tabulate(draws, nbins = length(probs))
      tibble::tibble(
        n                = n,
        rep              = r,
        entropy_estimate = entropy_from_counts(counts, base = base)
      )
    })
  })
}


#' Compute Theoretical Entropy Bounds for BSC Simulation
#'
#' Returns a data frame of crossover probability vs. capacity for plotting.
#'
#' @param p_seq Numeric vector: Grid of p_error values (default: 0 to 0.5)
#'
#' @return Tibble with columns: p_error, capacity, binary_entropy
#'
bsc_capacity_curve <- function(p_seq = seq(0, 0.5, by = 0.01)) {
  h_b <- function(p) ifelse(p == 0 | p == 1, 0, -p * log2(p) - (1 - p) * log2(1 - p))

  tibble::tibble(
    p_error       = p_seq,
    binary_entropy = h_b(p_seq),
    capacity       = 1 - h_b(p_seq)
  )
}


#' Monte Carlo Estimation of Mutual Information from Samples
#'
#' Estimates I(X;Y) from paired samples using plug-in histogram estimator.
#'
#' @param x        Integer vector: Samples of X
#' @param y        Integer vector: Samples of Y (same length as x)
#' @param n_bins_x Integer: Number of histogram bins for X
#' @param n_bins_y Integer: Number of histogram bins for Y
#' @param base     Numeric: Logarithm base (default 2)
#'
#' @return Numeric scalar: Estimated mutual information
#'
estimate_mi_histogram <- function(x, y, n_bins_x = 10L, n_bins_y = 10L, base = 2) {
  stopifnot(length(x) == length(y))

  joint <- table(
    cut(x, breaks = n_bins_x, include.lowest = TRUE),
    cut(y, breaks = n_bins_y, include.lowest = TRUE)
  )

  joint_probs <- joint / sum(joint)
  mutual_information(joint_probs, base = base)
}
