# Statistical_Functions
Some functions to help perform and investigate statistical tests 

## Non-Parametric Tests
* **Goodness of fit**:
  + [Kolmogorov-Smirnov](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Kolmogorov%E2%80%93Smirnov.R): Functions to generate a data frame with the cumulatives distributions of the data (or function) and the difference between them with the purpose of finding the test statistics and understanding how the process is made.
* **Tests for the location, one or two samples**: Functions to generate data frame of data and posts, test statistics and parameters for the assintotic distribution. Other functions to help build the confidence interval for the median using Walsh-Means.
  + [Wilcoxon Signed Rank Test for one sample](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Wilcoxon_Signed_Rank_Test_One_Sample.R)
  + [Wilcoxon Signed Rank Test for two paired samples](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Wilcox_Signed_Rank_Test_Paired_Samples.R)
  + [Mann-Whitney Test for two independent samples](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Mann_Whitney.R)

* **Tests for the location, three or more samples**: Functions to generate data frame of posts and test statistics. Also include function to perform multiple comparisons. 
  + [Kruskal-Wallis Test (Independent samples)](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Kruskal_Wallis.R)
  + [Friedman Test (Dependent samples)](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Friedman.R)
* **Tests for homogeneity of variances, three or more samples**: Function to generate parameters, test statistics and perform multiple comparisons.
  + [Homogeneity of variances, test similar to Kruskal-Wallis](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Homogeneity_of_Variances.R)
  + [Levene's test](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Levene_Test.R)
* **Correlation Coefficients**: Functions to calculate nonparametric correlation coefficients and parameters to perform hypothesis tests
  + [Spearman](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Spearman.R)
  + [Kendall](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Kendall.R)
* **Bootstrap**:
  + [Bootstrap](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Bootstrap.R)
* **Permutation**:
  + [Permutation Test](https://github.com/vitormarquesr/Statistical_Functions/blob/main/Nonparametric/Permutation_Test.R)
