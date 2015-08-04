package erlyberly;

import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

class TraceLogListCellFactory implements Callback<ListView<TraceLog>, ListCell<TraceLog>> {
	@Override
	public ListCell<TraceLog> call(ListView<TraceLog> view) {
		return new TraceLogListCell();
	}
}