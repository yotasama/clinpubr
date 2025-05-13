# interaction_scan returns valid data frame and saves table

    Code
      res_cox
    Output
        predictor group.by nvalid linear.p.int  rcs.p.int linear.p.adj  rcs.p.adj
      3       sex  ph.ecog    227   0.01706363         NA   0.05119089         NA
      2       age  ph.ecog    227   0.04202135 0.24748763   0.06303202 0.24748763
      1       age      sex    228   0.96353080 0.03957737   0.96353080 0.07915475

---

    Code
      res_logistic
    Output
        predictor group.by nvalid linear.p.int  rcs.p.int linear.p.adj  rcs.p.adj
      2       age  ph.ecog    227  0.004496487 0.03671699   0.01348946 0.07343398
      3       sex  ph.ecog    227  0.062415586         NA   0.09362338         NA
      1       age      sex    228  0.956186078 0.51523865   0.95618608 0.51523865

