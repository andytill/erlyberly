package erlyberly;

import java.io.IOException;
import java.util.List;

import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.text.Text;
import javafx.stage.Stage;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class ModFuncContextMenu extends ContextMenu {
    
    public interface FindModFuncChildren {
        List<ModFunc> find(ModFunc mf);
    }

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
        
        MenuItem seqTraceMenuItem, functionTraceMenuItem, moduleTraceMenuItem, callGraphMenuItem, moduleSourceCodeItem, moduleAbstCodeItem;
        
        moduleTraceMenuItem = new MenuItem("Module Trace");
        moduleTraceMenuItem.setOnAction(this::onModuleTrace);

        functionTraceMenuItem = new MenuItem("Function Trace");
        functionTraceMenuItem.setOnAction(this::onFunctionTrace);
        
        seqTraceMenuItem = new MenuItem("Seq Trace (experimental)");
        seqTraceMenuItem.setOnAction(this::onSeqTrace);
                
        moduleSourceCodeItem = new MenuItem(VIEW_SOURCE_CODE);
        moduleSourceCodeItem.setOnAction(this::onModuleCode);
        
        moduleAbstCodeItem = new MenuItem(VIEW_ABST_CODE);
        moduleAbstCodeItem.setOnAction(this::onModuleCode);
        
        callGraphMenuItem = new MenuItem("View Call Graph");
        callGraphMenuItem.setOnAction(this::onViewCallGraph);
        

        getItems().addAll(
            moduleTraceMenuItem, functionTraceMenuItem, seqTraceMenuItem,
            new SeparatorMenuItem(),
            callGraphMenuItem, moduleSourceCodeItem, moduleAbstCodeItem);
    }
    
    class ViewCodeHandler implements EventHandler<ActionEvent> {

        public void handle(ActionEvent ae) {
            try{
                MenuItem mi = (MenuItem) ae.getSource();
                String menuItemClicked = mi.getText();
                ModFunc mf = selectedItem.getValue();
                
                if(mf == null) 
                    return;
                
                String moduleName = mf.getModuleName();
                
                if(mf.isModule()){
                    String modSrc = fetchModCode(menuItemClicked, moduleName);
                    showModuleSourceCode(moduleName + " Source code ", modSrc);
                }
                else {
                    String functionName = mf.getFuncName();
                    int arity = mf.getArity();
                    String functionSrc = fetchFunctionCode(menuItemClicked, moduleName, functionName, arity);
                    showModuleSourceCode(mf.toFullString() + " Source code ", functionSrc);
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
        
        private String fetchFunctionCode(String menuItemClicked, String moduleName, String function, Integer arity) throws IOException, OtpErlangException {
            switch (menuItemClicked){
                case VIEW_SOURCE_CODE:
                    return dbgController.moduleFunctionSourceCode(moduleName, function, arity);
                case VIEW_ABST_CODE:
                    return dbgController.moduleFunctionAbstCode(moduleName, function, arity);
                default:
                    return "";
            }
        }

        private void showModuleSourceCode(String title, String moduleSourceCode){
            Stage primaryStage = new Stage();
            
            ScrollPane scrollPane;
            
            scrollPane = new ScrollPane();
            scrollPane.getStyleClass().add("mod-src");

            Scene scene = new Scene(scrollPane, 800, 800);
            ErlyBerly.applyCssToWIndow(scene);

            Text text = new Text(10, 10, moduleSourceCode);
            text.wrappingWidthProperty().bind(scene.widthProperty());

            scrollPane.setFitToWidth(true);
            scrollPane.setContent(text);
            
            primaryStage.setTitle(title);
            primaryStage.setScene(scene);
            primaryStage.show();
        }
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
            CallGraphView callGraphView = new CallGraphView();
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

    private void onModuleTrace(ActionEvent e){
        TreeItem<ModFunc> selectedItem = selectedTreeItemProperty().get();
        ObservableList<TreeItem<ModFunc>> moduleFunctions = selectedItem.getChildren();

        if(selectedItem != null && selectedItem.getValue() != null) {
            ModFunc mf = (ModFunc) selectedItem.getValue();
            if(mf.isModule()){
                toggleTraceMod(moduleFunctions);
            }
            else{
                TreeItem<ModFunc> rootModule = selectedTreeItemProperty().get().getParent();
                ObservableList<TreeItem<ModFunc>> rootModuleFunctions = rootModule.getChildren();
                toggleTraceMod(rootModuleFunctions);
            }
        }
    }

    private void toggleTraceMod(ObservableList<TreeItem<ModFunc>> functions){
       for (TreeItem<ModFunc> func : functions) {
           if(!func.getValue().toString().equals("module_info/0") &&
              !func.getValue().toString().equals("module_info/1")){
               ModFunc function = (ModFunc) func.getValue();
               dbgController.toggleTraceModFunc(function);
           }
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

    private void showModuleSourceCode(String title, String moduleSourceCode){
        Stage primaryStage = new Stage();
        
        ScrollPane scrollPane = new ScrollPane();
        scrollPane.getStyleClass().add("mod-src");

        Scene scene = new Scene(scrollPane, 800, 800);
        ErlyBerly.applyCssToWIndow(scene);

        Text text = new Text(10, 10, moduleSourceCode);
        text.wrappingWidthProperty().bind(scene.widthProperty());

        scrollPane.setFitToWidth(true);
        scrollPane.setContent(text);
        
        primaryStage.setTitle(title);
        primaryStage.setScene(scene);
        primaryStage.show();
    }
}
