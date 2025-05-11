# get_var_types correctly classifies variables

    Code
      res
    Output
      $factor_vars
      [1] "status"  "sex"     "ph.ecog"
      
      $exact_vars
      [1] "ph.ecog"
      
      $nonnormal_vars
      [1] "inst"      "time"      "ph.karno"  "pat.karno" "meal.cal"  "wt.loss"  
      
      $omit_vars
      NULL
      
      $strata
      [1] "sex"
      
      attr(,"class")
      [1] "var_types"

# baseline_table generates correct output files

    Code
      read.csv("test_output.csv")
    Output
                                X                  Overall                        X0
      1                         n                      228                        63
      2       inst (median [IQR])      11.00 [3.00, 16.00]        7.00 [3.00, 13.00]
      3       time (median [IQR])  255.50 [166.75, 396.50]   303.00 [224.50, 437.50]
      4            status = 2 (%)               165 (72.4)                 37 (58.7)
      5        age (median [IQR])     63.00 [56.00, 69.00]      61.00 [56.50, 68.00]
      6               sex = 2 (%)                90 (39.5)                 27 (42.9)
      7               ph.ecog (%)                                                   
      8                         0                63 (27.8)                63 (100.0)
      9                         1               113 (49.8)                   0 (0.0)
      10                        2                50 (22.0)                   0 (0.0)
      11                        3                  1 (0.4)                   0 (0.0)
      12  ph.karno (median [IQR])     80.00 [75.00, 90.00]     90.00 [90.00, 100.00]
      13 pat.karno (median [IQR])     80.00 [70.00, 90.00]      90.00 [80.00, 90.00]
      14  meal.cal (median [IQR]) 975.00 [635.00, 1150.00] 1000.00 [653.75, 1175.00]
      15   wt.loss (median [IQR])       7.00 [0.00, 15.75]        4.00 [0.00, 10.00]
                                X1                      X.2      p    test
      1                        113                       51               
      2        11.00 [5.00, 15.00]      11.50 [3.25, 16.00]  0.254 nonnorm
      3    243.00 [177.00, 426.00]  180.00 [100.00, 301.00]  0.001 nonnorm
      4                  82 (72.6)                45 (88.2)  0.002        
      5       63.00 [55.00, 68.00]     68.00 [60.50, 73.00]  0.002 nonnorm
      6                  42 (37.2)                21 (41.2)  0.737        
      7                                                     <0.001   exact
      8                    0 (0.0)                  0 (0.0)               
      9                113 (100.0)                  0 (0.0)               
      10                   0 (0.0)                50 (98.0)               
      11                   0 (0.0)                  1 (2.0)               
      12      80.00 [80.00, 90.00]     70.00 [60.00, 70.00] <0.001 nonnorm
      13      80.00 [70.00, 90.00]     60.00 [60.00, 70.00] <0.001 nonnorm
      14 1025.00 [825.00, 1150.00] 796.50 [472.00, 1075.00]  0.037 nonnorm
      15        6.00 [0.00, 15.00]      10.50 [3.50, 22.75]  0.009 nonnorm

---

    Code
      read.csv("test_output_missing.csv")
    Output
                            X   Overall        X0        X1      X.2     p test
      1                     n       228        63       113       51    NA   NA
      2       inst = TRUE (%)   1 (0.4)   0 (0.0)   0 (0.0)  1 (2.0) 0.177   NA
      3       time = TRUE (%)   0 (0.0)   0 (0.0)   0 (0.0)  0 (0.0)   NaN   NA
      4     status = TRUE (%)   0 (0.0)   0 (0.0)   0 (0.0)  0 (0.0)   NaN   NA
      5        age = TRUE (%)   0 (0.0)   0 (0.0)   0 (0.0)  0 (0.0)   NaN   NA
      6        sex = TRUE (%)   0 (0.0)   0 (0.0)   0 (0.0)  0 (0.0)   NaN   NA
      7    ph.ecog = TRUE (%)   1 (0.4)   0 (0.0)   0 (0.0)  0 (0.0)   NaN   NA
      8   ph.karno = TRUE (%)   1 (0.4)   0 (0.0)   0 (0.0)  1 (2.0) 0.177   NA
      9  pat.karno = TRUE (%)   3 (1.3)   1 (1.6)   0 (0.0)  2 (3.9) 0.123   NA
      10  meal.cal = TRUE (%) 47 (20.6) 13 (20.6) 27 (23.9) 7 (13.7) 0.331   NA
      11   wt.loss = TRUE (%)  14 (6.1)   2 (3.2)   7 (6.2)  5 (9.8) 0.343   NA

---

    Code
      read.csv("test_output_pairwise.csv")
    Output
                 X         X0_1        X0_.2        X1_.2
      1       inst 2.225074e-01 2.225074e-01 7.774247e-01
      2       time 1.457843e-01 8.157666e-04 1.011014e-02
      3     status 8.680617e-02 3.154529e-03 6.501237e-02
      4        age 8.454807e-01 2.960642e-03 2.960642e-03
      5        sex 1.000000e+00 1.000000e+00 1.000000e+00
      6    ph.ecog 9.999000e-05 9.999000e-05 9.999000e-05
      7   ph.karno 8.136144e-12 2.425136e-34 2.078767e-13
      8  pat.karno 1.364685e-02 1.158220e-13 9.885573e-10
      9   meal.cal 5.094785e-01 1.265793e-01 3.162490e-02
      10   wt.loss 9.078558e-02 6.261166e-03 9.078558e-02

# alpha_by_n calculates appropriate thresholds

    Code
      alpha_by_n(500)
    Output
      [1] 5.431508e-06

