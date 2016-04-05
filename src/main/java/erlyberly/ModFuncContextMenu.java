package erlyberly;

import java.awt.Desktop;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.NodeAPI;
import erlyberly.node.OtpUtil;
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

    private MenuItem moduleTraceMenuItem;

    public SimpleObjectProperty<TreeItem<ModFunc>> selectedTreeItemProperty() {
        return selectedTreeItem;
    }

    public SimpleObjectProperty<ModFunc> selectedItemProperty() {
        return selectedItem;
    }

    public SimpleObjectProperty<TreeItem<ModFunc>> rootProperty() {
        return rootProperty;
    }

    public ModFuncContextMenu(DbgController aDbgController) {
        isSelectionModule = new SimpleBooleanProperty();
        isSelectionFunction = new SimpleBooleanProperty();
        rootProperty = new SimpleObjectProperty<>();

        dbgController = aDbgController;
        selectedItem = new SimpleObjectProperty<>();
        selectedTreeItem = new SimpleObjectProperty<>();

        selectedItem.addListener((o, oldv, newv) -> {
            if(newv == null) {
                isSelectionModule.set(false);
                isSelectionFunction.set(false);
            }
            else {
                boolean module = newv.isModule();
                isSelectionModule.set(module);
                isSelectionFunction.set(!module);
            }
        });

        MenuItem functionTraceMenuItem, exportsTraceMenuItem, traceAllMenuItem, seqTraceMenuItem, callGraphMenuItem, moduleSourceCodeItem, moduleAbstCodeItem;

        functionTraceMenuItem = new MenuItem("Function Trace");
        functionTraceMenuItem.setOnAction(this::onFunctionTrace);
        functionTraceMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+t"));
        functionTraceMenuItem.disableProperty().bind(isSelectionFunction.not());

        exportsTraceMenuItem = new MenuItem("Exported Function Trace");
        exportsTraceMenuItem.setOnAction(this::onExportedFunctionTrace);
        exportsTraceMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+e"));

        traceAllMenuItem = new MenuItem("Trace All");
        traceAllMenuItem.setOnAction((e) -> { toggleTracesToAllFunctions(); });
        traceAllMenuItem.disableProperty().bind(rootProperty.isNull());
        traceAllMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+shift+t"));

        moduleTraceMenuItem = new MenuItem("Recursive Trace");
        moduleTraceMenuItem.setOnAction(this::onModuleTrace);

        seqTraceMenuItem = new MenuItem("Seq Trace (experimental)");
        seqTraceMenuItem.setOnAction(this::onSeqTrace);
        seqTraceMenuItem.disableProperty().bind(isSelectionFunction.not());

        BooleanBinding any = isSelectionFunction.or(isSelectionModule);

        moduleSourceCodeItem = new MenuItem(VIEW_SOURCE_CODE);
        moduleSourceCodeItem.setOnAction(this::onModuleCode);
        moduleSourceCodeItem.disableProperty().bind(any.not());

        moduleAbstCodeItem = new MenuItem(VIEW_ABST_CODE);
        moduleAbstCodeItem.setOnAction(this::onModuleCode);
        moduleAbstCodeItem.disableProperty().bind(any.not());

        NodeAPI nodeAPI = ErlyBerly.nodeAPI();
        SimpleBooleanProperty connectedProperty = nodeAPI.connectedProperty();

        callGraphMenuItem = new MenuItem("View Call Graph");
        callGraphMenuItem.setOnAction(this::onViewCallGraph);
        callGraphMenuItem.disableProperty().bind(connectedProperty.not().or(nodeAPI.xrefStartedProperty().not()).or(isSelectionFunction.not()));
        getItems().addAll(
            functionTraceMenuItem, exportsTraceMenuItem, traceAllMenuItem, moduleTraceMenuItem, seqTraceMenuItem,
            new SeparatorMenuItem(), callGraphMenuItem,
            new SeparatorMenuItem(), moduleSourceCodeItem, moduleAbstCodeItem);
    }

    private void onSeqTrace(ActionEvent ae) {
        ModFunc func = selectedItem.get();
        if(func == null)
            return;

        dbgController.seqTrace(func);

        ErlyBerly.showPane("Seq Trace", new SeqTraceView(dbgController.getSeqTraceLogs()));
    }

    private void onViewCallGraph(ActionEvent ae) {
        ModFunc func = selectedItem.get();
        if(func == null)
            return;
        if(func.isModule())
            return;

        try {
            OtpErlangObject callGraph = ErlyBerly.nodeAPI().callGraph(OtpUtil.atom(func.getModuleName()), OtpUtil.atom(func.getFuncName()), new OtpErlangLong(func.getArity()));
            CallGraphView callGraphView = new CallGraphView(dbgController);
            callGraphView.callGraph((OtpErlangTuple) callGraph);
            ErlyBerly.showPane(func.toFullString() + " call graph", ErlyBerly.wrapInPane(callGraphView));
        }
        catch (OtpErlangException | IOException e) {
            e.printStackTrace();
        }
    }

    private void onFunctionTrace(ActionEvent e) {
        ModFunc mf = selectedItem.get();

        if(selectedItem == null)
            return;
        dbgController.toggleTraceModFunc(mf);
    }

    private void onModuleTrace(ActionEvent e) {
        TreeItem<ModFunc> selectedItem = selectedTreeItemProperty().get();

        if(selectedItem == null)
            return;
        if(selectedItem.getValue() == null)
            return;
        if(!selectedItem.getValue().isModule())
            selectedItem = selectedItem.getParent();

        HashSet<ModFunc> funcs = new HashSet<ModFunc>();
        recurseModFuncItems(selectedItem, funcs);

        toggleTraceMod(funcs);
    }

    private void onExportedFunctionTrace(ActionEvent e) {
        TreeItem<ModFunc> selectedItem = selectedTreeItemProperty().get();

        if(selectedItem == null)
            return;

        // get all the functions we may trace
        HashSet<ModFunc> funcs = new HashSet<ModFunc>();
        recurseModFuncItems(selectedItem, funcs);

        // filter the exported ones
        HashSet<ModFunc> exported = new HashSet<ModFunc>();
        for (ModFunc modFunc : funcs) {
            if(modFunc.isExported()) {
                exported.add(modFunc);
            }
        }

        // trace 'em
        toggleTraceMod(exported);
    }

    private void recurseModFuncItems(TreeItem<ModFunc> item, HashSet<ModFunc> funcs) {
        if(item == null)
            return;
        if(item.getValue() == null || (!item.getValue().isModule() && !item.getValue().isModuleInfo()))
            funcs.add(item.getValue());

        for (TreeItem<ModFunc> childItem : item.getChildren()) {
            recurseModFuncItems(childItem, funcs);
        }
    }

    private void toggleTraceMod(Collection<ModFunc> functions){
       for (ModFunc func : functions) {
           dbgController.toggleTraceModFunc(func);
       }
    }

   private void onModuleCode(ActionEvent ae){
        try{
            MenuItem mi = (MenuItem) ae.getSource();
            String menuItemClicked = mi.getText();
            ModFunc mf = selectedItem.get();

            if(mf == null)
                return;

            String moduleName = mf.getModuleName();
            String modSrc;
            if(mf.isModule()) {
                modSrc = fetchModCode(menuItemClicked, moduleName);
                showModuleSourceCode(moduleName + " Source code ", modSrc);
            }
            else {
                String functionName = mf.getFuncName();
                Integer arity = mf.getArity();
                modSrc = fetchModCode(menuItemClicked, moduleName, functionName, arity);
                showModuleSourceCode(moduleName, modSrc);
            }
        } catch (Exception e) {
            throw new RuntimeException("failed to load the source code.", e);
        }
    }

    private String fetchModCode(String menuItemClicked, String moduleName) throws IOException, OtpErlangException {
        switch (menuItemClicked){
            case VIEW_SOURCE_CODE:
                return dbgController.moduleFunctionSourceCode(moduleName);
            case VIEW_ABST_CODE:
                return dbgController.moduleFunctionAbstCode(moduleName);
            default:
                return "";
        }
    }

    private String fetchModCode(String menuItemClicked, String moduleName, String function, Integer arity) throws IOException, OtpErlangException {
        switch (menuItemClicked){
            case VIEW_SOURCE_CODE:
                return dbgController.moduleFunctionSourceCode(moduleName, function, arity);
            case VIEW_ABST_CODE:
                return dbgController.moduleFunctionAbstCode(moduleName, function, arity);
            default:
                return "";
        }
    }

    private void showModuleSourceCode(String title, String moduleSourceCode) {
        Boolean showSourceInSystemEditor = PrefBind.getOrDefaultBoolean("showSourceInSystemEditor", false);
        if(showSourceInSystemEditor) {
            try {
                File tmpSourceFile = File.createTempFile(title, ".erl");
                FileOutputStream out = new FileOutputStream(tmpSourceFile);
                out.write(moduleSourceCode.getBytes());
                out.close();
                Desktop.getDesktop().edit(tmpSourceFile);
            }
            catch (IOException e) {
                e.printStackTrace();
                ErlyBerly.showPane(title, ErlyBerly.wrapInPane(new CodeView(moduleSourceCode)));
            }
        }
        else {
            ErlyBerly.showPane(title, ErlyBerly.wrapInPane(new CodeView(moduleSourceCode)));
        }
    }

    /**
     * Toggle tracing on all unfiltered (visible, even unexpanded) functions.
     */
    public void toggleTracesToAllFunctions() {
        ArrayList<ModFunc> funs = findUnfilteredFunctions();
        for (ModFunc func : funs) {
            dbgController.toggleTraceModFunc(func);
        }
    }

    private ArrayList<ModFunc> findUnfilteredFunctions() {
        ObservableList<TreeItem<ModFunc>> filteredTreeModules = rootProperty.get().getChildren();
        ArrayList<ModFunc> funs = new ArrayList<>();
        for (TreeItem<ModFunc> treeItem : filteredTreeModules) {
            for (TreeItem<ModFunc> modFunc : treeItem.getChildren()) {
                if(!modFunc.getValue().isModuleInfo()) {
                    funs.add(modFunc.getValue());
                }
            }
        }
        return funs;
    }

    public void setModuleTraceMenuText(String menuText) {
        moduleTraceMenuItem.setText(menuText);
    }
}
