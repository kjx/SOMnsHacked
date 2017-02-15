dialect "none"

method print (x) {x.asString.println}

method if(test) then(this) else(that) {
   test.ifTrue(this) ifFalse(that)
}

method if(test) then(this)  {
   test.ifTrue(this)
}

method for(collection) do(block) {
  collection.do( block )
}

method while(condition) do(block) {
   condition.whileTrue(block)
}

method loadGraceModule (mod) {
 print "prelude##loadGraceModule {mod}.grace"
 def m = mod + ".grace"
 var gh := platform.system.getGraceHook 
 gh.ifNil { 
   gh := platform.collections.Dictionary.new(10.asInteger)
   platform.system.setGraceHook(gh)} 


 gh.at(m) ifAbsent {
  def wrapperModuleClass = platform.system.loadModule(m)
  def wrapperModuleObject = wrapperModuleClass.usingPlatform(platform)
  print "PRE prelude wrapperModuleClass = {wrapperModuleClass}"
  print "PRE prelude wrapperModuleObject = {wrapperModuleObject}"
  def graceModuleObject = wrapperModuleObject.main(platform.kernel.Vector.new(0.asInteger))  
  gh.at(m) put(graceModuleObject)
  print "PRE prelude graceModuleObject = {graceModuleObject}"
  return graceModuleObject
 } 

}



method false {(1 == 2)}
method true {false.not}

print "prelude loaded"
