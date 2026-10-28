# Pure treaty functions: each maps losses to recoveries, with no hidden state.
# Used by the "Reinsurance Basics" slides.

fmt_m     <- scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = "m")
fmt_limit <- scales::label_number(accuracy = 1,   scale = 1e-6, suffix = "")

# Proportional: a fixed share of every loss
qs_recovery <- function(loss, cession) cession * loss

# Non-proportional: the part of the loss above the attachment, up to the limit
xol_recovery <- function(loss, attachment, limit) {
  pmin(pmax(loss - attachment, 0), limit)
}

# One loss through a tower of layers (tibble with columns layer, attachment, limit)
tower_recoveries <- function(loss, layers) {
  layers |>
    dplyr::mutate(recovery = xol_recovery(loss, attachment, limit))
}

# One loss split by who pays: cedant retention below the tower, each layer's recovery,
# and the part above the top of the tower (which reverts to the cedant).
tower_allocation <- function(loss, layers) {
  top <- max(layers$attachment + layers$limit)

  dplyr::bind_rows(
    tibble::tibble(
      region = "Retention",
      amount = pmin(loss, min(layers$attachment))
    ),
    tower_recoveries(loss, layers) |>
      dplyr::transmute(region = layer, amount = recovery),
    tibble::tibble(region = "Above tower", amount = pmax(loss - top, 0))
  )
}

# A sequence of event losses against one layer, with an optional cap on the total
# recovered over the period (Inf = occurrence limit only). State is threaded through
# Reduce(): `paid` is the vector of recoveries so far.
period_recoveries <- function(losses, attachment, limit, cap = Inf) {
  pay_next <- function(paid, loss) {
    due <- xol_recovery(loss, attachment, limit)
    c(paid, min(due, max(cap - sum(paid), 0)))
  }

  Reduce(pay_next, losses, numeric(0))
}

# Treaties applied in sequence: each sees the subject loss left by the previous one
apply_in_order <- function(loss, treaties) {
  step <- function(acc, treaty) {
    recovery <- treaty(acc$subject)

    list(subject = acc$subject - recovery, ceded = c(acc$ceded, recovery))
  }

  Reduce(step, treaties, list(subject = loss, ceded = numeric(0)))
}
