import "GBenchmarks/GBenchmark" as g

class sieve {
  inherit g.benchmark

  method benchmark  {
      def flags = platform.kernel.Array.new(5000.asInteger)
      self.sieve(flags)size(5000.asInteger)
  }

  method verifyResult (result) {
      self.assert(669) equals(result) 
  }

  method sieve(flags)size(size) {
      var primeCount := 0.asInteger
      flags.putAll(true)
      2.asInteger.to(size) do { i ->
        (flags.at((i - 1).asInteger)).ifTrue {
            primeCount := primeCount + 1.asInteger
            var k := i + i
            { k <= size }.whileTrue {
                flags.at((k - 1).asInteger) put(false)
                k := k + i } } }
      primeCount
    }
}

method newInstance {sieve}
method setupVerifiedRun(run) {run.innerIterations(1)}
