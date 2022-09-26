/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 * <p>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly;

import com.ericsson.otp.erlang.*;
import erlyberly.node.OtpUtil;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import java.util.Arrays;
import java.util.List;


class CallGraphView extends TreeView<ModFunc> {

    /**
     * A list of module names that will not be expanded in the call graph tree, since
     * they have lots of sub-calls, cluttering up the tree and it is assumed most usage
     * will be for application calls, not for the standard libs.
     */
    private static final List<String> UNEXPANDED_MODULES = Arrays.asList("erlang", "gen_server", "io", "io_lib", "lists", "rpc", "unicode");

    private final ModFuncContextMenu modFuncContextMenu;

    CallGraphView(final DbgController aDbgController) {
        super();
        assert null != aDbgController;

        this.modFuncContextMenu = new ModFuncContextMenu(aDbgController);

        this.getSelectionModel().selectedItemProperty().addListener((o, old, newItem) -> {
            this.modFuncContextMenu.selectedTreeItemProperty().set(newItem);
            if (null != newItem) this.modFuncContextMenu.selectedItemProperty().set(newItem.getValue());
        });


        final ModFuncTreeCellFactory modFuncTreeCellFactory;

        modFuncTreeCellFactory = new ModFuncTreeCellFactory(aDbgController);
        modFuncTreeCellFactory.setShowModuleName(true);

        this.setRoot(new TreeItem<>());
        this.setShowRoot(false);
        this.setContextMenu(this.modFuncContextMenu);
        this.setCellFactory(modFuncTreeCellFactory);
    }

    void callGraph(final OtpErlangTuple callStack) {
        assert null != callStack;
        this.populateCallGraph(this.getRoot(), callStack);
    }

    /**
     * Parses erlang terms in the format, into a JavaFX tree.
     * {@code
     * {{M::atom(), F::atom(), A::integer()}, [{M,F,A}]}
     * }
     */
    private void populateCallGraph(final TreeItem<ModFunc> parentModFuncItem, final OtpErlangTuple callGraph) {

        assert null != callGraph;
        System.out.println(callGraph);
        final OtpErlangTuple mfaTuple = (OtpErlangTuple) OtpUtil.tupleElement(0, callGraph);
        final OtpErlangList calls = (OtpErlangList) OtpUtil.tupleElement(1, callGraph);

        final OtpErlangAtom module = (OtpErlangAtom) OtpUtil.tupleElement(0, mfaTuple);
        final OtpErlangAtom function = (OtpErlangAtom) OtpUtil.tupleElement(1, mfaTuple);
        final OtpErlangLong arity = (OtpErlangLong) OtpUtil.tupleElement(2, mfaTuple);

        try {
            // just put something in, this isn't used
            final boolean exported = false;
            final boolean synthetic = false;

            final ModFunc modFunc = new ModFunc(module.atomValue(), function.atomValue(), arity.intValue(), exported, synthetic);

            final TreeItem<ModFunc> modFuncItem;

            modFuncItem = new TreeItem<>(modFunc);
            final String atomString = module.atomValue();
            final boolean value = !UNEXPANDED_MODULES.contains(atomString);
            modFuncItem.setExpanded(value);

            parentModFuncItem.getChildren().add(modFuncItem);

            for (final OtpErlangObject e : OtpUtil.iterableElements(calls)) {
                this.populateCallGraph(modFuncItem, (OtpErlangTuple) e);
            }
        } catch (final OtpErlangRangeException e) {
            e.printStackTrace();
        }
    }
}
