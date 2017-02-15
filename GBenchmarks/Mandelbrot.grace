import "richard/GBenchmark" as g

class mandelbrot {
  inherit g.benchmark

  method BROKENinnerBenchmarkLoop(innerIterations) { 
     self.verify(self.mandelbrot(innerIterations)) inner(innerIterations) }

  method innerBenchmarkLoop(i) {
    mandelbrot(i)
    true
  }

  method BROKENTHREEverify(result) inner(innerIterations) {
      if (innerIterations == 750) then {return (result == 50)}
      true
      }

  method BROKENbenchmark { mandelbrot(500) }
  method BROKENverifyResult(result) { true }


  method BROKENTOOverify(result) inner(innerIterations) {
      (innerIterations == 750).ifTrue {return (result == 50)}
      (firstSet).ifFalse {
        firstSet := true
        firstResult := result}
      (result == firstResult)
  }

  method BROKENverify(result) inner(innerIterations) {
      if (innerIterations == 750) then {return (result == 50)}
      if (false == firstResult)  then {firstResult := result}
      (result == firstResult)
  }




  method mandelbrot(size) {
    var sum := 0
    var byteAcc := 0
    var bitNum := 0
    var y := 0

    {y < size}.whileTrue {
        var ci := (2.0 * y / size) - 1.0
        var x := 0

        { x < size}.whileTrue {
            var zrzr := 0
            var zr := 0
            var zizi := 0
            var zi := 0
            var cr := (2.0 * x / size) - 1.5
            var z := 0

            var notDone := true
            var escape := 0

            while {(notDone).andAlso {z < 50}} do {
                zr := zrzr - zizi + cr
                zi := 2.0 * zr * zi + ci

                zrzr := zr * zr
                zizi := zi * zi

                if ((zrzr + zizi) > 4.0) then {
                    notDone := false
                    escape := 1
                }
                z := z + 1
            }

            byteAcc := (byteAcc * 2) + escape
            bitNum := bitNum + 1

            (bitNum.asInteger == 8).ifTrue {
                sum := sum.asInteger.bitXor(byteAcc.asInteger)
                byteAcc := 0
                bitNum := 0
            } ifFalse {
                if (x == (size - 1)) then {
                    byteAcc := byteAcc.asInteger << (8.asInteger - bitNum.asInteger)
                    sum := sum.asInteger.bitXor(byteAcc.asInteger)
                    byteAcc := 0
                    bitNum := 0
                }
            }

            x := x + 1
        }
        y := y + 1
    }
    sum
  }
}

method newInstance {mandelbrot}

// print "BROKEN BROKEN BROKEN BROKEN SHIT BROKEN"

method measure(bench) {
 def startTime = platform.system.ticks
 (bench.innerBenchmarkLoop(500))
 def endTime = platform.system.ticks
 def runTime = endTime - startTime
 print "BROKEN runTIME {runTime}"
}

// def myBench = newInstance


// measure(myBench) 
// measure(myBench) 
// measure(myBench) 
// measure(myBench) 
// measure(myBench) 

// measure(myBench) 
// measure(myBench) 
// measure(myBench) 
// measure(myBench) 
// measure(myBench) 
