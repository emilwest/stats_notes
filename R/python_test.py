import numpy as np


a=[1,2,2,3]
b=[2,2,2,2]
np.add(a,b)
a+b

np.dot(a,b)

import scipy.stats as stats
oddsratio, pvalue = stats.fisher_exact([[2, 10], [20, 3]])
