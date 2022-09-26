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
package ui;

import javafx.scene.Parent;
import javafx.scene.control.TreeCell;

public class FXTreeCell<T> extends TreeCell<T> {

    private final CellController<T> controller;

    private Parent aControl;

    public FXTreeCell(CellController<T> aController, Parent aControl) {
        controller = aController;
        this.aControl = aControl;

    }

    @Override
    public void cancelEdit() {
        super.cancelEdit();

        if (controller != null) {
            controller.cancelEdit();
        }
    }

    @Override
    public void commitEdit(T newValue) {
        super.commitEdit(newValue);

        if (controller != null) {
            controller.commitEdit(newValue);
        }
    }

    @Override
    public void startEdit() {
        super.startEdit();

        if (controller != null) {
            controller.startEdit();
        }
    }

    @Override
    protected void updateItem(T item, boolean empty) {
        super.updateItem(item, empty);

        if (item == null || empty) {
            setGraphic(null);
            setText(null);
        } else {
            setGraphic(aControl);
            setText(null);
        }

        if (controller != null) {
            controller.updateItem(item, empty);
        }
    }
}
