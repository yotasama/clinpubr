load_all()
x=data.frame(subject=sample(c("a","b"),1000,replace = T),value=runif(1000))
x$unit=NA
x$unit[x$subject=='a']=sample(c("mg/L","g/l","g/L"),sum(x$subject=='a'),replace = T)
x$value[x$subject=='a' & x$unit=="mg/L"]=x$value[x$subject=='a' & x$unit=="mg/L"]*1000
x$unit[x$subject=='b']=sample(c(NA,"g","mg"),sum(x$subject=='b'),replace = T)
x$value[x$subject=='b' & x$unit%in%"mg"]=x$value[x$subject=='b' & x$unit%in%"mg"]*1000
x$value[x$subject=='b' & is.na(x$unit)]=x$value[x$subject=='b' & is.na(x$unit)]*
  sample(c(1,1000),size=sum(x$subject=='b' & is.na(x$unit)),replace = T)

df=x
subject_col="subject"
value_col="value"
unit_col="unit"
quantiles=c(0.025,0.975)
unit_view <- function(df, subject_col, value_col, unit_col, quantiles = c(0.025, 0.975), save_table = TRUE,
                      filename = NULL) {
  res <- df %>%
    group_by(!!as.symbol(subject_col), !!as.symbol(unit_col)) %>%
    reframe(
      label = NA, count = n(), nvalid = sum(!is.na(!!as.symbol(value_col))),
      mean = mean(!!as.symbol(value_col), na.rm = TRUE), sd = sd(!!as.symbol(value_col), na.rm = TRUE),
      median = median(!!as.symbol(value_col), na.rm = TRUE),
      data.frame(quant_val = quantile(!!as.symbol(value_col), quantiles, na.rm = TRUE), quant = quantiles)
    ) %>%
    pivot_wider(names_from = quant, values_from = quant_val, names_prefix = "q_") %>%
    group_by(!!as.symbol(subject_col)) %>%
    filter(n() > 1) %>%
    as.data.frame()
  if (save_table) {
    if (is.null(filename)) {
      filename <- "unit_view.csv"
    }
    write.csv(res, file = filename, na = "")
    invisible(res)
  } else {
    return(res)
  }
}
y=unit_view(df=x, subject_col="subject", value_col="value", unit_col="unit",save_table = F)
y$label=c('t',NA,1e-3,1000,NA,'r')

z=unit_standardize(df=x, subject_col="subject", value_col="value", unit_col="unit",change_rules = y)
tmp=unit_view(df=z, subject_col="subject", value_col="value", unit_col="unit",save_table = F)
change_rules=y
change_rules <- change_rules[!is.na(change_rules$label), ]
subjects <- unique(df[[subject_col]])
change_rules <- lapply(seq_along(subjects), function(i) {
  tmp <- filter(change_rules, subject == subjects[i])
  target_unit <- tmp$unit[which(tmp$label %in% "t")]
  coeffs <- suppressWarnings(as.numeric(tmp$label))
  units2change <- c(tmp$unit[!is.na(coeffs)], tmp$unit[which(tmp$label %in% "r")])
  coeffs <- c(coeffs[!is.na(coeffs)], rep(NA, sum(tmp$label %in% "r")))
  list(
    subject = subjects[i],
    target_unit = target_unit,
    units2change = units2change,
    coeffs = coeffs
  )
})
