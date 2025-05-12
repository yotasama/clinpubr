load_all()
x <- c("1.2(XXX)", "5-8POS", "NS", "FULL", "5.5", "4.2")
extract_num(x)
extract_num(x, res_type = "first", multimatch2na = TRUE, zero_regexp = "NEG|NS",
            max_regexp = "FULL")
extract_num(x, res_type = "range", allow_neg = FALSE, zero_regexp = "NEG|NS", max_regexp = "FULL")