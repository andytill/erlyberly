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

import javafx.scene.control.TableColumn;

/**
 * Description of how to sort {@link ProcInfo} objects.
 */
public class ProcSort {
    private final String sortField;

    private final TableColumn.SortType sortType;

    public ProcSort(final String sortField, final TableColumn.SortType sortType) {
        super();
        this.sortField = sortField;
        this.sortType = sortType;
    }

    public String getSortField() {
        return this.sortField;
    }

    public TableColumn.SortType getSortType() {
        return this.sortType;
    }

    @Override
    public String toString() {
        return "ProcSort [sortField=" + this.sortField + ", sortType=" + this.sortType + "]";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((null == this.sortField) ? 0 : this.sortField.hashCode());
        result = prime * result + ((null == this.sortType) ? 0 : this.sortType.hashCode());
        return result;
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) return true;
        if (null == obj) return false;
        if (this.getClass() != obj.getClass()) return false;
        final ProcSort other = (ProcSort) obj;
        if (null == this.sortField) {
            if (null != other.sortField) return false;
        } else if (!this.sortField.equals(other.sortField)) return false;
        return this.sortType == other.sortType;
    }


}
