package ui;

public interface CellController<T> {

    void updateItem(T item, boolean empty);

    //
    // cell editing, this is optional for implementers
    //

    default boolean isEditable() {
        return false;
    }

    default void startEdit() { }

    default void cancelEdit() { }

    default void commitEdit(T newValue) { }
}
