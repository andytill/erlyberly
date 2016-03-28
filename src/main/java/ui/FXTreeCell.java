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

        if(controller != null) {
            controller.cancelEdit();
        }
    }

    @Override
    public void commitEdit(T newValue) {
        super.commitEdit(newValue);

        if(controller != null) {
            controller.commitEdit(newValue);
        }
    }

    @Override
    public void startEdit() {
        super.startEdit();

        if(controller != null) {
            controller.startEdit();
        }
    }

    @Override
    protected void updateItem(T item, boolean empty) {
        super.updateItem(item, empty);

        if(item == null || empty) {
            setGraphic(null);
            setText(null);
        }
        else {
            setGraphic(aControl);
            setText(null);
        }

        if(controller != null) {
            controller.updateItem(item, empty);
        }
    }
}
