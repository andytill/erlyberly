/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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
