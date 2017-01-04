package som.interpreter.nodes.dispatch;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.InstrumentableFactory.WrapperNode;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;

import som.VM;
import som.compiler.AccessModifier;
import som.compiler.MixinBuilder.MixinDefinitionId;
import som.interpreter.TruffleCompiler;
import som.interpreter.Types;
import som.interpreter.nodes.ISuperReadNode;
import som.interpreter.nodes.MessageSendNode.GenericMessageSendNode;
import som.vm.NotYetImplementedException;
import som.vmobjects.SClass;
import som.vmobjects.SInvokable;
import som.vmobjects.SObject;
import som.vmobjects.SSymbol;


public final class UninitializedDispatchNode {

  private abstract static class AbstractUninitialized extends AbstractDispatchNode {

    protected final SSymbol selector;

    protected AbstractUninitialized(final SourceSection source, final SSymbol selector) {
      super(source);
      this.selector = selector;
    }

    protected abstract AccessModifier    getMinimalVisibility();
    protected abstract MixinDefinitionId getMixinForPrivateLockupOrNull();
    protected abstract Dispatchable      doLookup(SClass rcvrClass);
    protected abstract AbstractUninitialized createNewChainEnd(Object rcvr, SClass rcvrClass, Dispatchable result);

    private AbstractDispatchNode specialize(final Object[] arguments,
        final int chainDepth, final AbstractDispatchNode first) {
      Object rcvr = arguments[0];
      assert rcvr != null;

      if (chainDepth < INLINE_CACHE_SIZE) {
        Object firstArg = arguments.length > 1 ? arguments[1] : null;
        return insertSpecialization(rcvr, firstArg);
      } else {
        return generalizeChain((GenericMessageSendNode) first.getParent());
      }
    }

    protected final AbstractDispatchNode insertSpecialization(final Object rcvr,
        final Object firstArg) {
      VM.insertInstrumentationWrapper(this);

      SClass rcvrClass = Types.getClassOf(rcvr);
      Dispatchable dispatchable = doLookup(rcvrClass);

      AbstractUninitialized newChainEnd = createNewChainEnd(rcvr, rcvrClass, dispatchable);
      if (newChainEnd == null) {
        newChainEnd = this; // TODO: this is a hack to pass always a source section to the getDispatchNode method
      }

      AbstractDispatchNode node;
      if (dispatchable == null) {
        node = new CachedDnuNode(rcvrClass, selector,
            DispatchGuard.create(rcvr), newChainEnd);
      } else {
        node = dispatchable.getDispatchNode(rcvr, firstArg, newChainEnd);
      }

      replace(node);
      VM.insertInstrumentationWrapper(node);
      return node;
    }

    protected final AbstractDispatchNode generalizeChain(
        final GenericMessageSendNode sendNode) {
      // the chain is longer than the maximum defined by INLINE_CACHE_SIZE and
      // thus, this callsite is considered to be megamorphic, and we generalize
      // it.
      GenericDispatchNode genericReplacement = new GenericDispatchNode(
          getSourceSection(), selector,
          getMinimalVisibility(), getMixinForPrivateLockupOrNull());
      sendNode.replaceDispatchListHead(genericReplacement);
      return genericReplacement;
    }

    @Override
    public final Object executeDispatch(final VirtualFrame frame, final Object[] arguments) {
      TruffleCompiler.transferToInterpreterAndInvalidate("Initialize a dispatch node.");

      // Determine position in dispatch node chain, i.e., size of inline cache
      Node i = this;
      int chainDepth = 0;
      while (i.getParent() instanceof AbstractDispatchNode) {
        i = i.getParent();
        if (!(i instanceof WrapperNode)) {
          chainDepth++;
        }
      }
      AbstractDispatchNode first = (AbstractDispatchNode) i;

      // First we need is to make sure the object layout is up to date.
      // If the object's layout was updated, we rerun the lookup chain
      // to make sure we hit a cached item of the new layout. Otherwise we could
      // add multiple.
      // TODO: this means objects are only migrated to the new shape on the
      //       very slow path, I think this is ok, because we do not expect
      //       many shape transitions. But, is this true?
      // TODO: this approach is recursive, and we might run out of Java stack.
      //       convert it to iterative approach, perhaps by exposing the guards
      //       and checking them directly to find matching node
      Object receiver = arguments[0];
      if (receiver instanceof SObject) {
        SObject rcvr = (SObject) receiver;
        if (rcvr.updateLayoutToMatchClass() && first != this) { // if first is this, short cut and directly continue...
          return first.executeDispatch(frame, arguments);
        }
      }

      RootNode root = getRootNode();
      assert root != null;

      AbstractDispatchNode newNode;
      // we modify a dispatch chain here, so, better grab the root node before we do anything
      synchronized (root) {
        newNode = specialize(arguments, chainDepth, first);
      }
      return newNode.executeDispatch(frame, arguments);
    }

    @Override
    public final int lengthOfDispatchChain() {
      return 0;
    }
  }

  private static final class UninitializedReceiverSend extends AbstractUninitialized {
    private final AccessModifier minimalVisibility;

    UninitializedReceiverSend(final SourceSection source, final SSymbol selector,
        final AccessModifier minimalVisibility) {
      super(source, selector);
      assert minimalVisibility == AccessModifier.PROTECTED
          || minimalVisibility == AccessModifier.PUBLIC;
      this.minimalVisibility = minimalVisibility;
    }

    @Override
    protected AbstractUninitialized createNewChainEnd(final Object rcvr,
        final SClass rcvrClass, final Dispatchable result) {
      if (result != null) {
        assert result.getAccessModifier() != AccessModifier.PRIVATE;
      }
      return new UninitializedReceiverSend(
          getSourceSection(), selector, minimalVisibility);
    }

    @Override
    protected Dispatchable doLookup(final SClass rcvrClass) {
      return rcvrClass.lookupMessage(selector, minimalVisibility);
    }

    @Override
    protected AccessModifier getMinimalVisibility() {
      return minimalVisibility;
    }

    @Override
    protected MixinDefinitionId getMixinForPrivateLockupOrNull() {
      return null;
    }
  }

  /**
   * Dispatch node for outer sends (name based on Newspeak spec), which includes
   * self sends (i.e., outer sends with a degree k=0.).
   */
  private static final class UninitializedLexicallyBound extends AbstractUninitialized {
    private final MixinDefinitionId mixinForPrivateLookup;

    UninitializedLexicallyBound(final SourceSection source, final SSymbol selector,
        final MixinDefinitionId mixinForPrivateLookup) {
      super(source, selector);
      this.mixinForPrivateLookup = mixinForPrivateLookup;
    }

    @Override
    protected AbstractUninitialized createNewChainEnd(final Object rcvr,
        final SClass rcvrClass, final Dispatchable result) {
      if (result instanceof SInvokable && result.getAccessModifier() == AccessModifier.PRIVATE) {
        // This is an optimization. For lexical dispatches to methods,
        // we don't need guards. So, there is no future failure,
        // and no uninit node needed. For slots however, we need the guard on
        // the object layout, which can change...
        return null;
      }
      return new UninitializedLexicallyBound(
          getSourceSection(), selector, mixinForPrivateLookup);
    }

    @Override
    protected Dispatchable doLookup(final SClass rcvrClass) {
      return rcvrClass.lookupPrivate(selector, mixinForPrivateLookup);
    }

    @Override
    protected AccessModifier getMinimalVisibility() {
      return (mixinForPrivateLookup == null)
          ? AccessModifier.PROTECTED : AccessModifier.PRIVATE;
    }

    @Override
    protected MixinDefinitionId getMixinForPrivateLockupOrNull() {
      return mixinForPrivateLookup;
    }
  }

  /**
   * This is a checking dispatch. Because the superclass
   * hierarchy is dynamic, and it is perfectly possible that super sends
   * bind in the same lexical location to different methods
   */
  private static final class UninitializedSuper extends AbstractUninitialized {

    private final MixinDefinitionId holderMixin;
    private final boolean classSide;

    UninitializedSuper(final SourceSection source, final SSymbol selector,
      final MixinDefinitionId holderMixin, final boolean classSide) {
      super(source, selector);
      this.holderMixin = holderMixin;
      this.classSide   = classSide;
    }

    private SClass getSuperClass(final SClass rcvrClass) {
      SClass cls = rcvrClass.getClassCorrespondingTo(holderMixin);
      SClass superClass = cls.getSuperClass();

      if (classSide) {
        return superClass.getSOMClass();
      } else {
        return superClass;
      }
    }

    @Override
    protected Dispatchable doLookup(final SClass rcvrClass) {
      return getSuperClass(rcvrClass).lookupMessage(
          selector, AccessModifier.PROTECTED);
    }

    @Override
    protected AbstractUninitialized createNewChainEnd(final Object rcvr,
        final SClass rcvrClass, final Dispatchable result) {
      return new UninitializedSuper(getSourceSection(), selector, holderMixin, classSide);
    }

    @Override
    public String toString() {
      return "UninitSuper(" + selector.toString() + (classSide ? ", clsSide" : "") + ")";
    }

    @Override
    protected AccessModifier getMinimalVisibility() {
      // TODO: if we actually get a megamorphic super send,
      //       then we need to implement this
      return AccessModifier.PUBLIC; //KJX WHAT A HACK
      //throw new NotYetImplementedException();
    }

    @Override
    protected MixinDefinitionId getMixinForPrivateLockupOrNull() {
      // TODO: if we actually get a megamorphic super send,
      //       then we need to implement this
      //return mixinForPrivateLookup;
      throw new NotYetImplementedException();
    }
  }

  public static AbstractDispatchNode createSuper(final SourceSection source,
      final SSymbol selector, final ISuperReadNode superNode) {
    CompilerAsserts.neverPartOfCompilation("SuperDispatchNode.create1");
    return new UninitializedSuper(source, selector,
        superNode.getEnclosingMixinId(), superNode.isClassSide());
  }

  public static AbstractDispatchNode createLexicallyBound(
      final SourceSection source, final SSymbol selector,
      final MixinDefinitionId mixinId) {
    return new UninitializedLexicallyBound(source, selector, mixinId);
  }

  public static AbstractDispatchNode createRcvrSend(final SourceSection source,
      final SSymbol selector, final AccessModifier minimalVisibility) {
    return new UninitializedReceiverSend(source, selector, minimalVisibility);
  }
}
