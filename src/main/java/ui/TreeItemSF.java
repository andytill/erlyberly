package ui;

import java.util.Comparator;
import java.util.function.Predicate;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.scene.control.TreeItem;

/**
 * Short for TreeItem sorted and filtered.
 * 
 * Use the input items when mutating the list, the getChildren method
 * returns the sorted and filtered items and mutations on that are likely
 * to go badly wrong.
 */
public class TreeItemSF<T> extends TreeItem<T> {
    
    private final ObservableList<TreeItem<T>> inputItems;
    
    private final SortedList<TreeItem<T>> sortedItems;
    
    private final FilteredList<TreeItem<T>> filteredItems;

    public TreeItemSF() {
        this(null);
    }
    
    public TreeItemSF(T aValue) {
        setValue(aValue);
        setExpanded(true);
        
        inputItems = FXCollections.observableArrayList();
        
        sortedItems = new SortedList<>(inputItems);
        
        filteredItems = new FilteredList<>(sortedItems);
        
        Bindings.bindContentBidirectional(getChildren(), filteredItems);
    }
    
    public ObservableList<TreeItem<T>> getInputItems() {
        return inputItems;
    }
    
    public ObjectProperty<Predicate<? super TreeItem<T>>> predicateProperty() {
        return filteredItems.predicateProperty();
    }

    public void setPredicate(Predicate<? super TreeItem<T>> object) {
        filteredItems.setPredicate(object);
    }
    
    public ObjectProperty<Comparator<? super TreeItem<T>>> comparatorProperty() {
        return sortedItems.comparatorProperty();
    }
}
