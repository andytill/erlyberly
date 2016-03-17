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
    
    private ObservableList<TraceLog> items, selectedItems;

    public TraceContextMenu() {
        getItems().add(menuItem("Copy All", "shortcut+c", this::onCopy));
        getItems().add(menuItem("Copy Function Call", null, this::onCopyCalls));
        getItems().add(new SeparatorMenuItem());
        getItems().add(menuItem("Delete", "delete", this::onDelete));
        getItems().add(menuItem("Delete All", "shortcut+n", this::onDeleteAll));
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
    
    private void onDelete(ActionEvent e) {
        ArrayList<TraceLog> arrayList = new ArrayList<TraceLog>(selectedItems);
        
        for (TraceLog traceLog : arrayList) {
            items.remove(traceLog);
        }
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
}
