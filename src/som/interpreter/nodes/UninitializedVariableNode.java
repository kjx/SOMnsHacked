package som.interpreter.nodes;

import static som.interpreter.TruffleCompiler.transferToInterpreterAndInvalidate;

import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

import som.compiler.Variable.Local;
import som.interpreter.InlinerAdaptToEmbeddedOuterContext;
import som.interpreter.InlinerForLexicallyEmbeddedMethods;
import som.interpreter.SplitterForLexicallyEmbeddedCode;
import som.interpreter.nodes.LocalVariableNode.LocalVariableReadNode;
import som.interpreter.nodes.LocalVariableNode.LocalVariableWriteNode;
import som.interpreter.nodes.LocalVariableNodeFactory.LocalVariableReadNodeGen;
import som.interpreter.nodes.LocalVariableNodeFactory.LocalVariableWriteNodeGen;
import som.interpreter.nodes.NonLocalVariableNode.NonLocalVariableReadNode;
import som.interpreter.nodes.NonLocalVariableNode.NonLocalVariableWriteNode;
import som.interpreter.nodes.NonLocalVariableNodeFactory.NonLocalVariableReadNodeGen;
import som.interpreter.nodes.NonLocalVariableNodeFactory.NonLocalVariableWriteNodeGen;
import tools.dym.Tags.LocalVarRead;
import tools.dym.Tags.LocalVarWrite;


public abstract class UninitializedVariableNode extends ContextualNode {
  protected final Local variable;

  public UninitializedVariableNode(final Local variable,
      final int contextLevel, final SourceSection source) {
    super(contextLevel, source);
    this.variable = variable;
  }

  protected UninitializedVariableNode(
      final UninitializedVariableNode wrappedNode) {
    super(wrappedNode.contextLevel, wrappedNode.getSourceSection());
    this.variable = wrappedNode.variable;
  }

  @Override
  public void replaceWithCopyAdaptedToEmbeddedOuterContext(
      final InlinerAdaptToEmbeddedOuterContext inliner) {
    throw new UnsupportedOperationException("for wrapping, we don't yet have splitting support");
  }

  @Override
  public void replaceWithLexicallyEmbeddedNode(
      final InlinerForLexicallyEmbeddedMethods inliner) {
    throw new UnsupportedOperationException("for wrapping, we don't yet have splitting support");
  }

  public static final class UninitializedVariableReadNode extends UninitializedVariableNode {

    public UninitializedVariableReadNode(final Local variable,
        final int contextLevel, final SourceSection source) {
      super(variable, contextLevel, source);
    }

    public UninitializedVariableReadNode(final UninitializedVariableReadNode node,
        final FrameSlot inlinedVarSlot) {
      this(node.variable.cloneForInlining(inlinedVarSlot), node.contextLevel,
          node.getSourceSection());
    }

    @Override
    protected boolean isTaggedWith(final Class<?> tag) {
      if (tag == LocalVarRead.class) {
        return true;
      } else {
        return super.isTaggedWith(tag);
      }
    }

    @Override
    public Object executeGeneric(final VirtualFrame frame) {
      transferToInterpreterAndInvalidate("UninitializedVariableReadNode");

      if (contextLevel > 0) {
        NonLocalVariableReadNode node = NonLocalVariableReadNodeGen.create(
            contextLevel, variable.getSlot(), getSourceSection());
        return replace(node).executeGeneric(frame);
      } else {
        // assert frame.getFrameDescriptor().findFrameSlot(variable.getSlotIdentifier()) == variable.getSlot();
        LocalVariableReadNode node = LocalVariableReadNodeGen.create(variable, getSourceSection());
        return replace(node).executeGeneric(frame);
      }
    }

    @Override
    public void replaceWithIndependentCopyForInlining(final SplitterForLexicallyEmbeddedCode inliner) {
      FrameSlot varSlot = inliner.getFrameSlot(this, variable.getSlotIdentifier());
      assert varSlot != null;
      replace(new UninitializedVariableReadNode(this, varSlot));
    }

    @Override
    public void replaceWithLexicallyEmbeddedNode(
        final InlinerForLexicallyEmbeddedMethods inliner) {
      UninitializedVariableReadNode inlined;

      if (contextLevel == 0) {
        // might need to add new frame slot in outer method
        inlined = inliner.getLocalRead(variable.getSlotIdentifier(),
            getSourceSection());
      } else {
        inlined = new UninitializedVariableReadNode(variable, contextLevel - 1,
            getSourceSection());
      }
      replace(inlined);
    }

    @Override
    public void replaceWithCopyAdaptedToEmbeddedOuterContext(
        final InlinerAdaptToEmbeddedOuterContext inliner) {
      // if the context level is 1, the variable is in the outer context,
      // which just got inlined, so, we need to adapt the slot id
      UninitializedVariableReadNode node;
      if (inliner.appliesTo(contextLevel)) {
        node = new UninitializedVariableReadNode(this,
            inliner.getOuterSlot(variable.getSlotIdentifier()));
        replace(node);
        return;
      } else if (inliner.needToAdjustLevel(contextLevel)) {
        node = new UninitializedVariableReadNode(variable, contextLevel - 1,
            getSourceSection());
        replace(node);
        return;
      }
    }
  }

  public static final class UninitializedVariableWriteNode extends UninitializedVariableNode {
    @Child private ExpressionNode exp;

    public UninitializedVariableWriteNode(final Local variable,
        final int contextLevel, final ExpressionNode exp,
        final SourceSection source) {
      super(variable, contextLevel, source);
      this.exp = exp;
    }

    public UninitializedVariableWriteNode(final UninitializedVariableWriteNode node,
        final FrameSlot inlinedVarSlot) {
      this(node.variable.cloneForInlining(inlinedVarSlot),
          node.contextLevel, node.exp, node.getSourceSection());
    }

    @Override
    protected boolean isTaggedWith(final Class<?> tag) {
      if (tag == LocalVarWrite.class) {
        return true;
      } else {
        return super.isTaggedWith(tag);
      }
    }

    @Override
    public Object executeGeneric(final VirtualFrame frame) {
      transferToInterpreterAndInvalidate("UninitializedVariableWriteNode");

      if (accessesOuterContext()) {
        NonLocalVariableWriteNode node = NonLocalVariableWriteNodeGen.create(
            contextLevel, variable.getSlot(), getSourceSection(), exp);
        return replace(node).
            executeGeneric(frame);
      } else {
        // not sure about removing this assertion :(((
        // assert frame.getFrameDescriptor().findFrameSlot(variable.getSlotIdentifier()) == variable.getSlot();
        LocalVariableWriteNode node = LocalVariableWriteNodeGen.create(
            variable, getSourceSection(), exp);
        return replace(node).
            executeGeneric(frame);
      }
    }

    @Override
    public void replaceWithIndependentCopyForInlining(final SplitterForLexicallyEmbeddedCode inliner) {
      FrameSlot varSlot = inliner.getFrameSlot(this, variable.getSlotIdentifier());
      assert varSlot != null;
      replace(new UninitializedVariableWriteNode(this, varSlot));
    }

    @Override
    public void replaceWithCopyAdaptedToEmbeddedOuterContext(
        final InlinerAdaptToEmbeddedOuterContext inliner) {
      // if the context level is 1, the variable is in the outer context,
      // which just got inlined, so, we need to adapt the slot id
      UninitializedVariableWriteNode node;
      if (inliner.appliesTo(contextLevel)) {
        node = new UninitializedVariableWriteNode(this,
            inliner.getOuterSlot(variable.getSlotIdentifier()));
        replace(node);
        return;
      } else if (inliner.needToAdjustLevel(contextLevel)) {
        node = new UninitializedVariableWriteNode(variable, contextLevel - 1,
            exp, getSourceSection());
        replace(node);
        return;
      }
    }

    @Override
    public String toString() {
      return "UninitVarWrite(" + variable.toString() + ")";
    }

    @Override
    public void replaceWithLexicallyEmbeddedNode(
        final InlinerForLexicallyEmbeddedMethods inliner) {
      UninitializedVariableWriteNode inlined;

      if (contextLevel == 0) {
        // might need to add new frame slot in outer method
        inlined = inliner.getLocalWrite(variable.getSlotIdentifier(),
            exp, getSourceSection());
      } else {
        inlined = new UninitializedVariableWriteNode(variable, contextLevel - 1,
            exp, getSourceSection());
      }
      replace(inlined);
    }
  }
}
