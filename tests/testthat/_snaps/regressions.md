# regression_basic_results CSV output matches snapshot

    Code
      read.csv(csv_file)
    Output
                           Terms Count            Crude Crude.1            Model1
      1                age (All)   227               HR       P                HR
      2               Continuous    NA  1.02(1.00,1.04)   0.041  1.01(0.995,1.03)
      3      Continuous, per 0.1    NA  1.00(1.00,1.00)   0.041  1.00(0.999,1.00)
      4       Continuous, per 10    NA  1.21(1.01,1.45)   0.041  1.14(0.949,1.38)
      5      Continuous, per 100    NA  6.56(1.08,40.0)   0.041  3.80(0.595,24.2)
      6     Continuous, per 1 SD    NA  1.19(1.01,1.40)   0.041  1.13(0.954,1.34)
      7    Continuous, logarithm    NA  3.03(1.02,9.06)   0.047  2.20(0.723,6.72)
      8     Grouped by Quartiles    NA             <NA>    <NA>              <NA>
      9                       Q1    49    1 (Reference)    <NA>     1 (Reference)
      10                      Q2    56 1.08(0.676,1.72)   0.754  1.11(0.697,1.77)
      11                      Q3    55 1.02(0.638,1.63)   0.929 0.937(0.582,1.51)
      12                      Q4    67 1.39(0.893,2.15)   0.145  1.25(0.800,1.96)
      13             P for trend    NA             <NA>   0.160              <NA>
      14 Grouped by Median Value    NA             <NA>    <NA>              <NA>
      15                     Low   105    1 (Reference)    <NA>     1 (Reference)
      16                    High   122 1.16(0.850,1.58)   0.354  1.04(0.750,1.43)
         Model1.1
      1         P
      2     0.158
      3     0.158
      4     0.158
      5     0.158
      6     0.158
      7     0.165
      8      <NA>
      9      <NA>
      10    0.658
      11    0.787
      12    0.325
      13    0.426
      14     <NA>
      15     <NA>
      16    0.833

# regression_basic_results logistic CSV matches snapshot

    Code
      read.csv(csv_file)
    Output
                           Terms Count               Crude Crude.1
      1                age (All)   227                  OR       P
      2               Continuous    NA     1.04(1.00,1.07)   0.025
      3      Continuous, per 0.1    NA     1.00(1.00,1.01)   0.025
      4       Continuous, per 10    NA     1.44(1.05,2.00)   0.025
      5      Continuous, per 100    NA 39.3(1.61,1.03e+03)   0.025
      6     Continuous, per 1 SD    NA     1.40(1.04,1.88)   0.025
      7    Continuous, logarithm    NA     9.14(1.41,61.1)   0.021
      8     Grouped by Quartiles    NA                <NA>    <NA>
      9                       Q1    49       1 (Reference)    <NA>
      10                      Q2    56    1.33(0.581,3.05)   0.501
      11                      Q3    55    1.29(0.565,2.98)   0.540
      12                      Q4    67    2.01(0.878,4.68)   0.100
      13             P for trend    NA                <NA>   0.119
      14 Grouped by Median Value    NA                <NA>    <NA>
      15                     Low   105       1 (Reference)    <NA>
      16                    High   122    1.41(0.785,2.53)   0.252
                      Model1 Model1.1
      1                   OR        P
      2     1.03(0.996,1.06)    0.083
      3      1.00(1.00,1.01)    0.083
      4     1.34(0.964,1.87)    0.083
      5  18.7(0.695,   533.)    0.083
      6     1.31(0.967,1.77)    0.083
      7     5.96(0.870,41.8)    0.069
      8                 <NA>     <NA>
      9        1 (Reference)     <NA>
      10    1.28(0.551,2.97)    0.568
      11    1.23(0.529,2.85)    0.634
      12    1.68(0.716,3.98)    0.234
      13                <NA>    0.271
      14                <NA>     <NA>
      15       1 (Reference)     <NA>
      16    1.26(0.695,2.30)    0.440

