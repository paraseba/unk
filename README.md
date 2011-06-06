# unk

A pluggable, manipulable memoization framework for Clojure.

# done

- pluggable memoization 
- manipulable memoization
- a `memo` function to replace `core.memoize`
- a fifo memoization strategy
- lru memoization
- soft references memoization
- expiry memoization

# todo

- swappable memoization strategies
- more docs and examples
- cache profiling
- code cleanup

# credits

unk is inspired by section 12.4 in *[The Joy of Clojure](http://joyofclojure.com)* which is in turn inspired by the [memoization philosophy](http://kotka.de/blog/2010/03/memoize_done_right.html) espoused by [Christophe Grand](http://clj-me.cgrand.net/), Eugen Dück, and [Meikel Brandmeyer](http://kotka.de/).  In addition, I would like to thank [Chas Emerick](http://cemerick.com/) for his [memoization based on SoftReferences](https://gist.github.com/747395).

License
-------

Copyright (C) 2011 Fogus

Distributed under the Eclipse Public License, the same as Clojure.
