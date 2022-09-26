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
        return "ProcSort [sortField=" + sortField + ", sortType=" + sortType + "]";
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((sortField == null) ? 0 : sortField.hashCode());
        result = prime * result + ((sortType == null) ? 0 : sortType.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        ProcSort other = (ProcSort) obj;
        if (sortField == null) {
            if (other.sortField != null) return false;
        } else if (!sortField.equals(other.sortField)) return false;
        return sortType == other.sortType;
    }


}
