HyperLogLog implementation in Haskell
=====================================

HyperLogLog is an algorithm for approximately counting the number of
distinct elements in a stream using only a small amount of memory. Can
process cardinalities up to 10<sup>18</sup> with an error around 1% in
a few kilobytes of memory.

References
----------

Stefan Heule, Marc Nunkesser, Alexander Hall: [HyperLogLog in Practice: Algorithmic Engineering of a State of The Art Cardinality Estimation Algorithm](http://research.google.com/pubs/pub40671.html), Proceedings of EDBT, 2013

License
-------

MIT License. See the LICENSE file.
