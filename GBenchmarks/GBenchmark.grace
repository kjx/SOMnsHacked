class benchmark {
  method innerBenchmarkLoop(innerIterations) { 
    1.asInteger.to(innerIterations.asInteger) do { i ->
        (verifyResult(benchmark)).ifFalse { return false }
      }
    true
  }

  method benchmark { self.subclassResponsibility }
  method verifyResult(result) { self.subclassResponsibility }

  method assert(expected) equals(value) {
      (expected == value).ifFalse {
        self.error ("Expected value (" + expected.asString + 
                    ") differs from actual (" + value.asString + 
                    ") benchmark result.")
      }
      true
    }
}
