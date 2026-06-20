resolve_conflicts <- function(pkg_priority) {
  get_index <- function(pkg_name) {
    idx <- str_which(pkg_priority, pkg_name)

    if(length(idx) == 0) {
      idx <- 0L
    }

    return(idx)
  }

  conflict_lst <- conflict_scout()

  for(func_name in names(conflict_lst)) {
    pkg_index <- map_int(conflict_lst[[func_name]], get_index)

    pkg_index <- pkg_index[pkg_index > 0]

    if(length(pkg_index) == 0) {
      pkg_use <- conflict_lst[[func_name]][1]
    } else {
      pkg_use <- pkg_index %>%
        min() %>%
        pkg_priority[.]

    }

    conflict_prefer(func_name, pkg_use)
  }

  return(conflict_lst)
}


load_portfolio_file <- function(portfolio_file) {
  loaddata_dt <- read_ods(portfolio_file, skip = 9, col_types = NA)
  setDT(loaddata_dt)

  setnames(loaddata_dt, fix_column_names(names(loaddata_dt)))


  portfolio_dt <- loaddata_dt[!is.na(position)][!grep("Curncy", pk)]

  portfolio_dt <- create_portfolio_columns(portfolio_dt)

  portfolio_dt[, bond_id    := id]
  portfolio_dt[, position   := as.numeric(position)]
  portfolio_dt[, price      := as.numeric(price)]
  portfolio_dt[, fx_rate    := as.numeric(fx_rate)]
  portfolio_dt[, principal  := as.numeric(principal)]
  portfolio_dt[, accrued    := as.numeric(accrued)]
  portfolio_dt[, market_val := as.numeric(market_val)]
  portfolio_dt[, face_value := 1000]
  portfolio_dt[, position   := position * face_value]

  portfolio_dt <- portfolio_dt[, .(security, pk, bond_id, stock_symbol
                                   ,face_value, coupon, maturity_date
                                   ,currency, fx_rate, position, price
                                   ,market_val)]

  return(portfolio_dt)
}


create_coupon_schedule <- function(portfolio_dt, portfolio_date) {
  stopifnot(all(c('bond_id','currency','maturity_date','coupon') %in% names(portfolio_dt)))

  max_years <- portfolio_dt[, round(max(as.numeric(maturity_date - portfolio_date)) / 365, 0)]

  schedule_dt <- portfolio_dt[, {

    coupon_date      = maturity_date - period(month = 6 * 0:(2 * max_years))
    coupon_date      = coupon_date[coupon_date > portfolio_date]
    coupon_amount    = rep(position * coupon * 0.01 * 0.5, length(coupon_date))

    coupon_amount[1] = coupon_amount[1] + position

    .(coupon_date = coupon_date, cashflow = coupon_amount)
  }, by = bond_id]

  return(schedule_dt[order(bond_id, coupon_date)])
}


create_portfolio_columns <- function(portfolio_dt) {
  stopifnot(all(c('security','id') %in% names(portfolio_dt)))

  sec_df <- as.data.frame(parse_security_data(portfolio_dt$security))

  portfolio_dt <- cbind(portfolio_dt, sec_df)

  portfolio_dt[, currency := ifelse(grepl('^CA', id), "CAD", "USD")]

  return(portfolio_dt)
}


parse_security_data <- function(security_str) {
  sec_regex <- "(.*?) (.*) (\\d{2}\\/\\d{2}\\/\\d{2})"

  stock_symbol     <- gsub(sec_regex, "\\1", security_str)
  coupon_str       <- gsub(sec_regex, "\\2", security_str)
  maturity_datestr <- gsub(sec_regex, "\\3", security_str)

  maturity_date <- as.Date(maturity_datestr, format = '%m/%d/%y')

  coupon_str <- convert_vulgar_fractions(coupon_str)

  coupon <- sapply(strsplit(coupon_str, split = ' '),
                   function(x) sum(as.numeric(x)))

  return(list(stock_symbol  = stock_symbol
              ,coupon        = coupon
              ,maturity_date = maturity_date)
  )
}


convert_vulgar_fractions <- function(fraction_str) {
  output_str <- fraction_str

  output_str <- gsub("\u00BD", "0.5", output_str)
  output_str <- gsub("\u00BC", "0.25", output_str)
  output_str <- gsub("\u00BE", "0.75", output_str)


  output_str <- gsub("\u215B", "0.125", output_str)
  output_str <- gsub("\u215C", "0.375", output_str)
  output_str <- gsub("\u215D", "0.625", output_str)
  output_str <- gsub("\u215E", "0.875", output_str)

  return(output_str)
}


fix_column_names <- function(table_names) {
  table_names <- tolower(table_names)
  table_names <- gsub(" ", "_", table_names)

  return(table_names)
}


calculate_portfolio_amounts <- function(portfolio_dt
                                        ,fund_capital
                                        ,exchange_rate
                                        ,base_currency = 'CAD') {
  amount_per_issue <- fund_capital / nrow(portfolio_dt)

  position_dt <- portfolio_dt[, {
    rate = ifelse(currency != base_currency, exchange_rate, 1)

    contract_count = ((amount_per_issue * rate) / (face_value * price * 0.01))

    position = round(contract_count, 0) * face_value

    .(position = position)
  }, by = bond_id]

  portfolio_dt <- merge(portfolio_dt, position_dt, by = 'bond_id')

  return(portfolio_dt)
}


create_hedging_function <- function(units) {
  units <- rev(sort(units))

  create_hedge <- function(x) {
    contracts <- rep(0, length(units))

    hedge_amount <-  x
    i <- 1

    while(i <= length(units)) {
      contracts[i] <- round(hedge_amount / units[i], 0)
      hedge_amount <- hedge_amount - contracts[i] * units[i]

      i <- i + 1
    }

    names(contracts) <- dollar(units)

    return(contracts)
  }

  return(create_hedge)
}


calculate_duration_data <- function(days, cashflow, price) {

  calc_pv <- function(r) sum(c(-price, cashflow) * exp(-r * c(0, days) / 365))
  ytm     <- optimize(function(r) abs(calc_pv(r)), c(0, 1))$minimum

  duration <- weighted.mean(days, cashflow * exp(-ytm * days / 365))

  return(c(ytm      = ytm
           ,duration = duration / 365))
}
