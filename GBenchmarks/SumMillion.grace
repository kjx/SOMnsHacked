import "GBenchmarks/GBenchmark" as g

class sumMillion {
  inherit g.benchmark   

  method benchmark { 
    var sum := 0
    for (1.to(1000 * 1000))
      do { each -> sum := sum + each }
    sum
  }

  method verifyResult(result) { 500000500000 == result }
}

method newInstance {sumMillion}



