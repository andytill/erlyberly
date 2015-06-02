package erlyberly;

import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.CallGraphView.CGModFunc;
import erlyberly.node.OtpUtil;


public class CallGraphView extends TreeView<CGModFunc> {
    
    public CallGraphView() {
        setRoot(new TreeItem<CGModFunc>());
        setShowRoot(false);
    }

    public void callGraph(OtpErlangTuple callStack) {
        populateCallGraph(getRoot(), callStack);
    }
    
    /**
     * Parses erlang terms in the format, into a JavaFX tree.
     * <code>
     * {{M::atom(), F::atom(), A::integer()}, [{M,F,A}]}
     * </code>
     */
    private void populateCallGraph(TreeItem<CGModFunc> parentModFuncItem, OtpErlangTuple callGraph) {
        OtpErlangTuple mfaTuple = (OtpErlangTuple) OtpUtil.tupleElement(0, callGraph);
        OtpErlangList calls = (OtpErlangList) OtpUtil.tupleElement(1, callGraph);

        OtpErlangAtom module = (OtpErlangAtom) OtpUtil.tupleElement(0, mfaTuple);
        OtpErlangAtom function = (OtpErlangAtom) OtpUtil.tupleElement(1, mfaTuple);
        OtpErlangLong arity = (OtpErlangLong) OtpUtil.tupleElement(2, mfaTuple);
        
        try {
            // just put something in, this isn't used
            boolean exported = false;
            boolean synthetic = false;
            
            ModFunc modFunc = new ModFunc(module.atomValue(), function.atomValue(), arity.intValue(), exported, synthetic);
            
            TreeItem<CGModFunc> modFuncItem;
            
            modFuncItem = new TreeItem<>(new CGModFunc(modFunc));
            modFuncItem.setExpanded(true);
            
            parentModFuncItem.getChildren().add(modFuncItem);
            
            for (OtpErlangObject e : OtpUtil.iterableElements(calls)) {
                populateCallGraph(modFuncItem, (OtpErlangTuple) e);
            }
        } 
        catch (OtpErlangRangeException e) {
            e.printStackTrace();
        }
    }
    
    class CGModFunc {
        private final ModFunc modFunc;

        public CGModFunc(ModFunc aModFunc) {
            modFunc = aModFunc;
        }

        @Override
        public String toString() {
            return modFunc.toFullString();
        }
    }
}
