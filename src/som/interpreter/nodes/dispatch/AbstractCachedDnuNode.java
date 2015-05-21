package som.interpreter.nodes.dispatch;

import som.compiler.AccessModifier;
import som.interpreter.SArguments;
import som.interpreter.nodes.dispatch.AbstractDispatchNode.AbstractCachedDispatchNode;
import som.vm.Symbols;
import som.vmobjects.SClass;
import som.vmobjects.SSymbol;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.frame.VirtualFrame;


public abstract class AbstractCachedDnuNode extends AbstractCachedDispatchNode {
  private final SSymbol selector;

  public static CallTarget getDnu(final SClass rcvrClass) {
    return rcvrClass.lookupMessage(
          Symbols.symbolFor("doesNotUnderstand:arguments:"), AccessModifier.PROTECTED).
          getCallTargetIfAvailable();
  }

  public AbstractCachedDnuNode(final SClass rcvrClass,
      final SSymbol selector, final AbstractDispatchNode nextInCache) {
    super(getDnu(rcvrClass), nextInCache);
    this.selector = selector;
  }

  protected final Object performDnu(final VirtualFrame frame, final Object[] arguments,
      final Object rcvr) {
    Object[] argsArr = new Object[] {
        rcvr, selector, SArguments.getArgumentsWithoutReceiver(arguments) };
    return cachedMethod.call(frame, argsArr);
  }
}
