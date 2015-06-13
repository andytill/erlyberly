package erlyberly;

import javafx.beans.Observable;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeView;
import javafx.util.Callback;
import ui.FXTreeCell;

class ModFuncTreeCellFactory implements Callback<TreeView<ModFunc>, TreeCell<ModFunc>> {

    private final DbgController dbgController;
    
    private boolean showModuleName;

    public ModFuncTreeCellFactory(DbgController aDbgController) {
        dbgController = aDbgController;
    }

    @Override
    public TreeCell<ModFunc> call(TreeView<ModFunc> tree) {
        ModFuncGraphic mfg;
        
        mfg = new ModFuncGraphic(
            dbgController::toggleTraceModFunc, 
            dbgController::isTraced
        );
        mfg.setShowModuleName(isShowModuleName());
        
        dbgController.addTraceListener((Observable o) -> { 
            mfg.onTracesChange(); 
        });
        
        return new FXTreeCell<ModFunc>(mfg, mfg);
    }

    public boolean isShowModuleName() {
        return showModuleName;
    }

    public void setShowModuleName(boolean showModuleName) {
        this.showModuleName = showModuleName;
    }
}