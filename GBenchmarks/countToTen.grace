import "GBenchmarks/GBenchmark" as g

class countToTen {
  inherit g.benchmark   

  method benchmark { 
    var sum := 0
    for (1.to(10))
      do { each -> sum := sum + each }
    sum
  }

  method verifyResult(result) { 55 == result }
}

method newInstance {countToTen}


