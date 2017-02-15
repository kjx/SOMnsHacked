import "richard/GBenchmark" as g

class factorial {
  inherit g.benchmark

  method innerBenchmarkLoop(innerIterations) { 
         fact(innerIterations)
         true
  }

  method fact(n) {
    if (n == 0) then {
        return 1
    } else {
        return n * fact(n - 1)
    }
  }
}

method newInstance {factorial}
