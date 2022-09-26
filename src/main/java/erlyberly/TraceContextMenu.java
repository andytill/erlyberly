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


import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.KeyCombination;

import java.util.ArrayList;
import java.util.Collection;

class TraceContextMenu extends ContextMenu {

    private final DbgController dbgController;

    private ObservableList<TraceLog> items, selectedItems;

    TraceContextMenu(final DbgController aDbgContoller) {
        super();
        this.dbgController = aDbgContoller;
        this.getItems().add(TraceContextMenu.menuItem("Copy All", "shortcut+c", this::onCopy));
        this.getItems().add(TraceContextMenu.menuItem("Copy Function Call", null, this::onCopyCalls));
        this.getItems().add(new SeparatorMenuItem());
        this.getItems().add(TraceContextMenu.menuItem("Delete", "delete", this::onDelete));
        this.getItems().add(TraceContextMenu.menuItem("Delete All", "shortcut+n", this::onDeleteAll));
        this.getItems().add(TraceContextMenu.menuItem("Add Breaker", "shortcut+b", this::onAddBreaker));
        this.getItems().add(TraceContextMenu.menuItem("Toggle Trace", null, this::onTraceToggle));
    }

    private static MenuItem menuItem(final String text, final String accelerator, final EventHandler<ActionEvent> e) {
        final MenuItem menuItem;

        menuItem = new MenuItem(text);
        menuItem.setOnAction(e);

        if (null != accelerator) menuItem.setAccelerator(KeyCombination.keyCombination(accelerator));

        return menuItem;
    }

    private void onCopy(final ActionEvent e) {
        final StringBuilder sbuilder = new StringBuilder();

        for (final TraceLog traceLog : this.selectedItems) {
            sbuilder.append(traceLog.toString()).append("\n");
        }

        TraceContextMenu.copyToClipboard(sbuilder);
    }

    private void onCopyCalls(final ActionEvent e) {
        final StringBuilder sbuilder = new StringBuilder();

        for (final TraceLog traceLog : this.selectedItems) {
            sbuilder.append(traceLog.toCallString()).append("\n");
        }

        TraceContextMenu.copyToClipboard(sbuilder);
    }

    private static void copyToClipboard(final StringBuilder sbuilder) {
        final Clipboard clipboard = Clipboard.getSystemClipboard();
        final ClipboardContent content = new ClipboardContent();

        content.putString(sbuilder.toString());
        clipboard.setContent(content);
    }

    private void onTraceToggle(final ActionEvent e) {
        for (final TraceLog log : this.selectedItems) {
            this.dbgController.toggleTraceModFunc(log.getModFunc());
        }
    }

    private void onDelete(final ActionEvent e) {
        final Collection<TraceLog> arrayList = new ArrayList<>(this.selectedItems);
        this.items.removeAll(arrayList);
    }

    private void onDeleteAll(final ActionEvent e) {
        this.items.clear();
    }

    void setSelectedItems(final ObservableList<TraceLog> selectedItems2) {
        this.selectedItems = selectedItems2;
    }

    void setItems(final ObservableList<TraceLog> items2) {
        this.items = items2;
    }

    private void onAddBreaker(final ActionEvent e) {
        this.items.add(TraceLog.newBreakLog());
    }
}
