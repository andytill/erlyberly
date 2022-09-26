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
import erlyberly.node.NodeAPI;
import erlyberly.node.OtpUtil;
import javafx.application.Platform;
import javafx.beans.binding.BooleanBinding;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.input.KeyCombination;

import java.awt.*;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.*;
import java.util.stream.Collectors;

public class ModFuncContextMenu extends ContextMenu {

    private static final String VIEW_SOURCE_CODE = "View Source Code";

    private static final String VIEW_ABST_CODE = "View Abstract Code";

    private final DbgController dbgController;

    private final SimpleObjectProperty<ModFunc> selectedItem;

    private final SimpleObjectProperty<TreeItem<ModFunc>> selectedTreeItem;

    private final SimpleBooleanProperty isSelectionModule, isSelectionFunction;

    /**
     * Root tree item of the module tree.
     */
    private final SimpleObjectProperty<TreeItem<ModFunc>> rootProperty;

    private final MenuItem moduleTraceMenuItem;

    public SimpleObjectProperty<TreeItem<ModFunc>> selectedTreeItemProperty() {
        return this.selectedTreeItem;
    }

    public SimpleObjectProperty<ModFunc> selectedItemProperty() {
        return this.selectedItem;
    }

    public SimpleObjectProperty<TreeItem<ModFunc>> rootProperty() {
        return this.rootProperty;
    }

    public ModFuncContextMenu(final DbgController aDbgController) {
        super();
        this.isSelectionModule = new SimpleBooleanProperty();
        this.isSelectionFunction = new SimpleBooleanProperty();
        this.rootProperty = new SimpleObjectProperty<>();

        this.dbgController = aDbgController;
        this.selectedItem = new SimpleObjectProperty<>();
        this.selectedTreeItem = new SimpleObjectProperty<>();

        this.selectedItem.addListener((o, oldv, newv) -> {
            if (null == newv) {
                this.isSelectionModule.set(false);
                this.isSelectionFunction.set(false);
            } else {
                final boolean module = newv.isModule();
                this.isSelectionModule.set(module);
                this.isSelectionFunction.set(!module);
            }
        });

        final MenuItem functionTraceMenuItem;
        final MenuItem exportsTraceMenuItem;
        final MenuItem traceAllMenuItem;
        final MenuItem seqTraceMenuItem;
        final MenuItem callGraphMenuItem;
        final MenuItem moduleSourceCodeItem;
        final MenuItem moduleAbstCodeItem;

        functionTraceMenuItem = new MenuItem("Function Trace");
        functionTraceMenuItem.setOnAction(this::onFunctionTrace);
        functionTraceMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+t"));
        functionTraceMenuItem.disableProperty().bind(this.isSelectionFunction.not());

        exportsTraceMenuItem = new MenuItem("Exported Function Trace");
        exportsTraceMenuItem.setOnAction(this::onExportedFunctionTrace);
        exportsTraceMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+e"));

        traceAllMenuItem = new MenuItem("Trace All");
        traceAllMenuItem.setOnAction((e) -> this.toggleTracesToAllFunctions());
        traceAllMenuItem.disableProperty().bind(this.rootProperty.isNull());
        traceAllMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+shift+t"));

        this.moduleTraceMenuItem = new MenuItem("Recursive Trace");
        this.moduleTraceMenuItem.setOnAction(this::onModuleTrace);

        seqTraceMenuItem = new MenuItem("Seq Trace (experimental)");
        seqTraceMenuItem.setOnAction(this::onSeqTrace);
        seqTraceMenuItem.disableProperty().bind(this.isSelectionFunction.not());

        final BooleanBinding any = this.isSelectionFunction.or(this.isSelectionModule);

        moduleSourceCodeItem = new MenuItem(VIEW_SOURCE_CODE);
        moduleSourceCodeItem.setOnAction(this::onViewCode);
        moduleSourceCodeItem.disableProperty().bind(any.not());

        moduleAbstCodeItem = new MenuItem(VIEW_ABST_CODE);
        moduleAbstCodeItem.setOnAction(this::onViewCode);
        moduleAbstCodeItem.disableProperty().bind(any.not());

        final NodeAPI nodeAPI = ErlyBerly.nodeAPI();
        final SimpleBooleanProperty connectedProperty = nodeAPI.connectedProperty();

        callGraphMenuItem = new MenuItem("View Call Graph");
        callGraphMenuItem.setOnAction(this::onViewCallGraph);
        callGraphMenuItem.disableProperty().bind(connectedProperty.not().or(nodeAPI.xrefStartedProperty().not()).or(this.isSelectionFunction.not()));
        this.getItems().addAll(functionTraceMenuItem, exportsTraceMenuItem, traceAllMenuItem, this.moduleTraceMenuItem, seqTraceMenuItem, new SeparatorMenuItem(), callGraphMenuItem, new SeparatorMenuItem(), moduleSourceCodeItem, moduleAbstCodeItem);
    }

    private void onSeqTrace(final ActionEvent ae) {
        final ModFunc func = this.selectedItem.get();
        if (null == func) return;

        this.dbgController.seqTrace(func);

        ErlyBerly.showPane("Seq Trace", new SeqTraceView(this.dbgController.getSeqTraceLogs()));
    }

    private void onViewCallGraph(final ActionEvent ae) {
        final ModFunc func = this.selectedItem.get();
        if (null == func) return;
        if (func.isModule()) return;

        final List<String> defaultSkippedModules = Arrays.asList("app_helper", "application", "binary", "code", "compile", "crypto", "dets", "dict", "erl_pp", "erl_syntax", "erlang", "ets", "exometer", "exometer_admin", "file", "gb_sets", "gen", "gen_event", "gen_fsm", "gen_server", "httpc", "httpd_util", "inet", "inet_parse", "inets", "io", "io_lib", "io_lib_pretty", "lager", "lager_config", "lists", "mnesia", "msgpack", "orddict", "proplists", "sets", "string", "timer");
        final List<String> skippedModules = ModFuncContextMenu.getConfiguredSkippedModuleNames(defaultSkippedModules);
        final List<OtpErlangAtom> skippedModuleAtoms = skippedModules.stream().map(OtpUtil::atom).collect(Collectors.toList());
        ErlyBerly.runIO(() -> {
            try {
                final OtpErlangObject rpcResult = ErlyBerly.nodeAPI().callGraph(new OtpErlangList(skippedModuleAtoms.toArray(new OtpErlangAtom[]{})), OtpUtil.atom(func.getModuleName()), OtpUtil.atom(func.getFuncName()), new OtpErlangLong(func.getArity()));
                if (!OtpUtil.isTupleTagged(NodeAPI.OK_ATOM, rpcResult)) {
                    throw new RuntimeException(rpcResult.toString());
                }
                final OtpErlangTuple rpcResultTuple = (OtpErlangTuple) rpcResult;
                final OtpErlangObject callGraph = rpcResultTuple.elementAt(1);
                Platform.runLater(() -> {
                    final CallGraphView callGraphView = new CallGraphView(this.dbgController);
                    callGraphView.callGraph((OtpErlangTuple) callGraph);
                    ErlyBerly.showPane(func.toFullString() + " call graph", ErlyBerly.wrapInPane(callGraphView));
                });
            } catch (final OtpErlangException | IOException e) {
                e.printStackTrace();
            }
        });
    }

    private static List<String> getConfiguredSkippedModuleNames(final List<String> defaultSkippedModules) {
        return (List<String>) PrefBind.getOrDefault("xrefSkippedModules", defaultSkippedModules);
    }

    private void onFunctionTrace(final ActionEvent e) {
        final ModFunc mf = this.selectedItem.get();

        this.dbgController.toggleTraceModFunc(mf);
    }

    private void onModuleTrace(final ActionEvent e) {
        TreeItem<ModFunc> selectedItem = this.selectedTreeItemProperty().get();

        if (null == selectedItem) return;
        if (null == selectedItem.getValue()) return;
        if (!selectedItem.getValue().isModule()) selectedItem = selectedItem.getParent();

        final HashSet<ModFunc> funcs = new HashSet<>();
        this.recurseModFuncItems(selectedItem, funcs);

        this.toggleTraceMod(funcs);
    }

    private void onExportedFunctionTrace(final ActionEvent e) {
        TreeItem<ModFunc> selectedItem = this.selectedTreeItemProperty().get();

        if (null == selectedItem) return;
        if (null == selectedItem.getValue()) return;
        if (!selectedItem.getValue().isModule()) selectedItem = selectedItem.getParent();

        // get all the functions we may trace
        final HashSet<ModFunc> funcs = new HashSet<>();
        this.recurseModFuncItems(selectedItem, funcs);

        // filter the exported ones
        final Set<ModFunc> exported = new HashSet<>();
        for (final ModFunc modFunc : funcs) {
            if (modFunc.isExported()) {
                exported.add(modFunc);
            }
        }

        // trace 'em
        this.toggleTraceMod(exported);
    }

    private void recurseModFuncItems(final TreeItem<ModFunc> item, final HashSet<ModFunc> funcs) {
        if (null == item) return;
        if (null == item.getValue() || (!item.getValue().isModule() && item.getValue().isModuleInfo()))
            funcs.add(item.getValue());

        for (final TreeItem<ModFunc> childItem : item.getChildren()) {
            this.recurseModFuncItems(childItem, funcs);
        }
    }

    private void toggleTraceMod(final Collection<ModFunc> functions) {
        for (final ModFunc func : functions) {
            this.dbgController.toggleTraceModFunc(func);
        }
    }

    private void onViewCode(final ActionEvent ae) {
        final MenuItem mi = (MenuItem) ae.getSource();
        final String menuItemClicked = mi.getText();
        final ModFunc mf = this.selectedItem.get();
        if (null == mf) return;
        final String moduleName = mf.getModuleName();
        ErlyBerly.runIO(() -> {
            try {
                final String title;
                final String modSrc;
                if (mf.isModule()) {
                    modSrc = ModFuncContextMenu.fetchModuleCode(menuItemClicked, moduleName);
                    title = moduleName + " Source code";
                } else {
                    final String functionName = mf.getFuncName();
                    final Integer arity = mf.getArity();
                    modSrc = ModFuncContextMenu.fetchFunctionCode(menuItemClicked, moduleName, functionName, arity);
                    title = moduleName;
                }
                Platform.runLater(() -> ModFuncContextMenu.showModuleSourceCode(title, modSrc));
            } catch (final Exception e) {
                throw new RuntimeException("failed to load the source code.", e);
            }
        });
    }

    private static String fetchModuleCode(final String menuItemClicked, final String moduleName) throws IOException, OtpErlangException {
        switch (menuItemClicked) {
            case VIEW_SOURCE_CODE:
                return DbgController.moduleFunctionSourceCode(moduleName);
            case VIEW_ABST_CODE:
                return DbgController.moduleFunctionAbstCode(moduleName);
            default:
                return "";
        }
    }

    private static String fetchFunctionCode(final String menuItemClicked, final String moduleName, final String function, final Integer arity) throws IOException, OtpErlangException {
        switch (menuItemClicked) {
            case VIEW_SOURCE_CODE:
                return DbgController.moduleFunctionSourceCode(moduleName, function, arity);
            case VIEW_ABST_CODE:
                return DbgController.moduleFunctionAbstCode(moduleName, function, arity);
            default:
                return "";
        }
    }

    private static void showModuleSourceCode(final String title, final String moduleSourceCode) {
        final boolean showSourceInSystemEditor = PrefBind.getOrDefaultBoolean("showSourceInSystemEditor", false);
        if (showSourceInSystemEditor) {
            try {
                final File tmpSourceFile = File.createTempFile(title, ".erl");
                final FileOutputStream out = new FileOutputStream(tmpSourceFile);
                out.write(moduleSourceCode.getBytes(StandardCharsets.UTF_8));
                out.close();
                Desktop.getDesktop().edit(tmpSourceFile);
            } catch (final IOException e) {
                e.printStackTrace();
                ErlyBerly.showPane(title, ErlyBerly.wrapInPane(new CodeView(moduleSourceCode)));
            }
        } else {
            ErlyBerly.showPane(title, ErlyBerly.wrapInPane(new CodeView(moduleSourceCode)));
        }
    }

    /**
     * Toggle tracing on all unfiltered (visible, even unexpanded) functions.
     */
    public void toggleTracesToAllFunctions() {
        final Iterable<ModFunc> funs = this.findUnfilteredFunctions();
        for (final ModFunc func : funs) {
            this.dbgController.toggleTraceModFunc(func);
        }
    }

    private Iterable<ModFunc> findUnfilteredFunctions() {
        final ObservableList<TreeItem<ModFunc>> filteredTreeModules = this.rootProperty.get().getChildren();
        final List<ModFunc> funs = new ArrayList<>();
        for (final TreeItem<ModFunc> treeItem : filteredTreeModules) {
            for (final TreeItem<ModFunc> modFunc : treeItem.getChildren()) {
                if (modFunc.getValue().isModuleInfo()) {
                    funs.add(modFunc.getValue());
                }
            }
        }
        return funs;
    }

    public void setModuleTraceMenuText(final String menuText) {
        this.moduleTraceMenuItem.setText(menuText);
    }
}
