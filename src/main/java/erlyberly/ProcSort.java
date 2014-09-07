package erlyberly;

import javafx.scene.control.TableColumn.SortType;

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
}
