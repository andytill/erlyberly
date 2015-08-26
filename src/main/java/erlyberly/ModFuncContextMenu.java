package erlyberly;

import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;

import javafx.beans.property.SimpleObjectProperty;
import javafx.event.ActionEvent;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TextArea;
import javafx.scene.control.TreeItem;
import javafx.scene.input.KeyCombination;
import javafx.stage.Stage;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class ModFuncContextMenu extends ContextMenu {

    private final String VIEW_SOURCE_CODE = "View Source Code";
    
    private final String VIEW_ABST_CODE = "View Abstract Code";

    private final DbgController dbgController;
    
    private final SimpleObjectProperty<ModFunc> selectedItem;

    private final SimpleObjectProperty<TreeItem<ModFunc>> selectedTreeItem;
    
    public SimpleObjectProperty<TreeItem<ModFunc>> selectedTreeItemProperty() {
        return selectedTreeItem;
    }

    public SimpleObjectProperty<ModFunc> selectedItemProperty() {
        return selectedItem;
    }

    public ModFuncContextMenu(DbgController aDbgController) {
        dbgController = aDbgController;
        selectedItem = new SimpleObjectProperty<>();
        selectedTreeItem = new SimpleObjectProperty<>();
        
        MenuItem functionTraceMenuItem, exportsTraceMenuItem, moduleTraceMenuItem, seqTraceMenuItem, callGraphMenuItem, moduleSourceCodeItem, moduleAbstCodeItem;

        functionTraceMenuItem = new MenuItem("Function Trace");
        functionTraceMenuItem.setOnAction(this::onFunctionTrace);
        functionTraceMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+t"));

        exportsTraceMenuItem = new MenuItem("Exported Function Trace");
        exportsTraceMenuItem.setOnAction(this::onExportedFunctionTrace);
        exportsTraceMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+e"));
        
        moduleTraceMenuItem = new MenuItem("Recursive Trace");
        moduleTraceMenuItem.setOnAction(this::onModuleTrace);
        
        seqTraceMenuItem = new MenuItem("Seq Trace (experimental)");
        seqTraceMenuItem.setOnAction(this::onSeqTrace);
                
        moduleSourceCodeItem = new MenuItem(VIEW_SOURCE_CODE);
        moduleSourceCodeItem.setOnAction(this::onModuleCode);
        
        moduleAbstCodeItem = new MenuItem(VIEW_ABST_CODE);
        moduleAbstCodeItem.setOnAction(this::onModuleCode);
        
        callGraphMenuItem = new MenuItem("View Call Graph");
        callGraphMenuItem.setOnAction(this::onViewCallGraph);

        getItems().addAll(
            functionTraceMenuItem, exportsTraceMenuItem, moduleTraceMenuItem, seqTraceMenuItem,
            new SeparatorMenuItem(),
            callGraphMenuItem, moduleSourceCodeItem, moduleAbstCodeItem);
    }
    
    private void onSeqTrace(ActionEvent ae) {
        ModFunc func = selectedItem.get();
        if(func == null)
            return;
        
        dbgController.seqTrace(func);
        
        showWindow(new SeqTraceView(dbgController.getSeqTraceLogs()), "Seq Trace");
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
            showWindow(callGraphView, func.toFullString() + " call graph");
        } 
        catch (OtpErlangException | IOException e) {
            e.printStackTrace();
        }
    }

    private void showWindow(Parent parent, CharSequence sb) {
        Stage termsStage = new Stage();
        Scene scene  = new Scene(parent);
        
        CloseWindowOnEscape.apply(scene, termsStage);
        
        termsStage.setScene(scene);
        termsStage.setWidth(800);
        termsStage.setHeight(600);
        termsStage.setTitle(sb.toString());
        termsStage.show();
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
        if(item.getValue() == null || !item.getValue().isModule())
            funcs.add(item.getValue());
        
        for (TreeItem<ModFunc> childItem : item.getChildren()) {
            recurseModFuncItems(childItem, funcs);
        }
    }

    private void toggleTraceMod(Collection<ModFunc> functions){
       for (ModFunc func : functions) {
           if(!isModuleInfo(func)){
               dbgController.toggleTraceModFunc(func);
           }
       }
    }

    private boolean isModuleInfo(ModFunc func) {
        return func.toString().equals("module_info/0") || func.toString().equals("module_info/1");
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
                showModuleSourceCode(moduleName + ":" + functionName + "/" + arity.toString() + " Source code "+moduleName, modSrc);
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
        TextArea textArea;
        
        textArea = new TextArea(moduleSourceCode);
        textArea.getStyleClass().add("mod-src");
        textArea.setEditable(false);
        
        Scene scene = new Scene(textArea, 800, 800);
        ErlyBerly.applyCssToWIndow(scene);

        Stage primaryStage;
        
        primaryStage = new Stage();
        primaryStage.setTitle(title);
        primaryStage.setScene(scene);
        primaryStage.sizeToScene();
        primaryStage.show();

        CloseWindowOnEscape.apply(scene, primaryStage);
    }
}
