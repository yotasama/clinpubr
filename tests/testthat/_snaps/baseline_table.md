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
                                X                  Overall                        X1
      1                         n                      228                       138
      2       inst (median [IQR])      11.00 [3.00, 16.00]       11.00 [3.00, 15.00]
      3       time (median [IQR])  255.50 [166.75, 396.50]   224.00 [144.75, 369.25]
      4            status = 2 (%)               165 (72.4)                112 (81.2)
      5           age (mean (SD))             62.45 (9.07)              63.34 (9.14)
      6               ph.ecog (%)                                                   
      7                         0                63 (27.8)                 36 (26.3)
      8                         1               113 (49.8)                 71 (51.8)
      9                         2                50 (22.0)                 29 (21.2)
      10                        3                  1 (0.4)                   1 (0.7)
      11  ph.karno (median [IQR])     80.00 [75.00, 90.00]      80.00 [70.00, 90.00]
      12 pat.karno (median [IQR])     80.00 [70.00, 90.00]      80.00 [70.00, 90.00]
      13  meal.cal (median [IQR]) 975.00 [635.00, 1150.00] 1025.00 [768.00, 1175.00]
      14   wt.loss (median [IQR])       7.00 [0.00, 15.75]        8.00 [0.75, 18.50]
                               X2      p    test
      1                        90               
      2       11.00 [3.25, 16.00]  0.416 nonnorm
      3   292.50 [195.25, 448.50]  0.013 nonnorm
      4                 53 (58.9) <0.001        
      5              61.08 (8.85)  0.064        
      6                            0.827   exact
      7                 27 (30.0)               
      8                 42 (46.7)               
      9                 21 (23.3)               
      10                  0 (0.0)               
      11     80.00 [80.00, 90.00]  0.882 nonnorm
      12     80.00 [70.00, 90.00]  0.332 nonnorm
      13 925.00 [588.00, 1067.50]  0.022 nonnorm
      14       4.00 [0.00, 11.00]  0.029 nonnorm

---

    Code
      read.csv("test_output_missing.csv")
    Output
                            X   Overall        X1        X2     p test
      1                     n       228       138        90    NA   NA
      2       inst = TRUE (%)   1 (0.4)   1 (0.7)   0 (0.0) 1.000   NA
      3       time = TRUE (%)   0 (0.0)   0 (0.0)   0 (0.0)   NaN   NA
      4     status = TRUE (%)   0 (0.0)   0 (0.0)   0 (0.0)   NaN   NA
      5        age = TRUE (%)   0 (0.0)   0 (0.0)   0 (0.0)   NaN   NA
      6    ph.ecog = TRUE (%)   1 (0.4)   1 (0.7)   0 (0.0) 1.000   NA
      7   ph.karno = TRUE (%)   1 (0.4)   1 (0.7)   0 (0.0) 1.000   NA
      8  pat.karno = TRUE (%)   3 (1.3)   2 (1.4)   1 (1.1) 1.000   NA
      9   meal.cal = TRUE (%) 47 (20.6) 24 (17.4) 23 (25.6) 0.186   NA
      10   wt.loss = TRUE (%)  14 (6.1)  10 (7.2)   4 (4.4) 0.562   NA

