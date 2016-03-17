package erlyberly;

import javafx.scene.control.TableColumn.SortType;

/**
 * Description of how to sort {@link ProcInfo} objects. 
 */
public class ProcSort {
    private final String sortField;
    
    private final SortType sortType;

    public ProcSort(String sortField, SortType sortType) {
        this.sortField = sortField;
        this.sortType = sortType;
    }

    public String getSortField() {
        return sortField;
    }

    public SortType getSortType() {
        return sortType;
    }

    @Override
    public String toString() {
        return "ProcSort [sortField=" + sortField + ", sortType=" + sortType
                + "]";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((sortField == null) ? 0 : sortField.hashCode());
        result = prime * result
                + ((sortType == null) ? 0 : sortType.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ProcSort other = (ProcSort) obj;
        if (sortField == null) {
            if (other.sortField != null)
                return false;
        } else if (!sortField.equals(other.sortField))
            return false;
        if (sortType != other.sortType)
            return false;
        return true;
    }
    
    
}
