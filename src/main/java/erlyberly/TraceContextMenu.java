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


import java.util.ArrayList;

import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.KeyCombination;

public class TraceContextMenu extends ContextMenu {

    private final DbgController dbgController;

    private ObservableList<TraceLog> items, selectedItems;

    public TraceContextMenu(DbgController aDbgContoller) {
        dbgController = aDbgContoller;
        getItems().add(menuItem("Copy All", "shortcut+c", this::onCopy));
        getItems().add(menuItem("Copy Function Call", null, this::onCopyCalls));
        getItems().add(new SeparatorMenuItem());
        getItems().add(menuItem("Delete", "delete", this::onDelete));
        getItems().add(menuItem("Delete All", "shortcut+n", this::onDeleteAll));
        getItems().add(menuItem("Add Breaker", "shortcut+b", this::onAddBreaker));
        getItems().add(menuItem("Toggle Trace", null, this::onTraceToggle));
    }

    private MenuItem menuItem(String text, String accelerator, EventHandler<ActionEvent> e) {
        MenuItem menuItem;

        menuItem = new MenuItem(text);
        menuItem.setOnAction(e);

        if(accelerator != null)
            menuItem.setAccelerator(KeyCombination.keyCombination(accelerator));

        return menuItem;
    }

    private void onCopy(ActionEvent e) {
        StringBuilder sbuilder = new StringBuilder();

        for (TraceLog traceLog : selectedItems) {
            sbuilder.append(traceLog.toString()).append("\n");
        }

        copyToClipboard(sbuilder);
    }

    private void onCopyCalls(ActionEvent e) {
        StringBuilder sbuilder = new StringBuilder();

        for (TraceLog traceLog : selectedItems) {
            sbuilder.append(traceLog.toCallString()).append("\n");
        }

        copyToClipboard(sbuilder);
    }

    private void copyToClipboard(StringBuilder sbuilder) {
        final Clipboard clipboard = Clipboard.getSystemClipboard();
        final ClipboardContent content = new ClipboardContent();

        content.putString(sbuilder.toString());
        clipboard.setContent(content);
    }

    private void onTraceToggle(ActionEvent e) {
        for (TraceLog log : selectedItems) {
        	dbgController.toggleTraceModFunc(log.getModFunc());
        }
    }

    private void onDelete(ActionEvent e) {
        ArrayList<TraceLog> arrayList = new ArrayList<TraceLog>(selectedItems);
        items.removeAll(arrayList);
    }

    private void onDeleteAll(ActionEvent e) {
        items.clear();
    }

    public void setSelectedItems(ObservableList<TraceLog> selectedItems2) {
        selectedItems = selectedItems2;
    }

    public void setItems(ObservableList<TraceLog> items2) {
        items = items2;
    }

    private void onAddBreaker(ActionEvent e) {
        items.add(TraceLog.newBreakLog());
    }
}
