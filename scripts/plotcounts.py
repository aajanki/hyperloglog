#!/usr/bin/env python2

import sys
import numpy as np
import matplotlib.pyplot as plt

observed = np.array([int(line) for line in sys.stdin])
x = np.arange(1, len(observed) + 1)

plt.plot(x, observed, 'r', linewidth=2)
plt.plot(x, x, 'k')
plt.legend(['HyperLogLog', 'true count'])
plt.show()
