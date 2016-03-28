package erlyberly;

import java.util.Arrays;
import java.util.List;

import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;


public class CallGraphView extends TreeView<ModFunc> {

    /**
     * A list of module names that will not be expanded in the call graph tree, since
     * they have lots of sub-calls, cluttering up the tree and it is assumed most usage
     * will be for application calls, not for the standard libs.
     */
    private static List<String> UNEXPANDED_MODULES = Arrays.asList(
            "erlang", "gen_server", "io", "io_lib",  "lists", "rpc", "unicode");

    private ModFuncContextMenu modFuncContextMenu;

    public CallGraphView(DbgController aDbgController) {
        assert aDbgController != null;

        modFuncContextMenu = new ModFuncContextMenu(aDbgController);

        getSelectionModel()
            .selectedItemProperty()
            .addListener((o, old, newItem) -> {
                modFuncContextMenu.selectedTreeItemProperty().set(newItem);
                if(newItem != null)
                    modFuncContextMenu.selectedItemProperty().set(newItem.getValue());
            });


        ModFuncTreeCellFactory modFuncTreeCellFactory;

        modFuncTreeCellFactory = new ModFuncTreeCellFactory(aDbgController);
        modFuncTreeCellFactory.setShowModuleName(true);

        setRoot(new TreeItem<>());
        setShowRoot(false);
        setContextMenu(modFuncContextMenu);
        setCellFactory(modFuncTreeCellFactory);
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
    private void populateCallGraph(TreeItem<ModFunc> parentModFuncItem, OtpErlangTuple callGraph) {
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

            TreeItem<ModFunc> modFuncItem;

            modFuncItem = new TreeItem<>(modFunc);
            String atomString = module.atomValue();
            boolean value = !UNEXPANDED_MODULES.contains(atomString);
            modFuncItem.setExpanded(value);

            parentModFuncItem.getChildren().add(modFuncItem);

            for (OtpErlangObject e : OtpUtil.iterableElements(calls)) {
                populateCallGraph(modFuncItem, (OtpErlangTuple) e);
            }
        }
        catch (OtpErlangRangeException e) {
            e.printStackTrace();
        }
    }
}
