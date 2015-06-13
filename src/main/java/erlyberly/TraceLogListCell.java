package erlyberly;

import javafx.beans.value.ChangeListener;
import javafx.scene.control.ListCell;

final class TraceLogListCell extends ListCell<TraceLog> {
	private ChangeListener<? super Boolean> listener;
	
	public TraceLogListCell() {
		listener = (o, noldV, newV) -> {
			TraceLog item = TraceLogListCell.this.getItem();
			
			removeCompletionListender(item);

			getStyleClass().remove("not-completed");
			
			putCompletionStyle(item);
		};
	}

	private void removeCompletionListender(TraceLog item) {
		if(item != null) {
			item.isCompleteProperty().removeListener(listener);
		}
	}
	
	@Override
	protected void updateItem(TraceLog item, boolean empty) {

		// clean up
		getStyleClass().remove("exception");
		getStyleClass().remove("not-completed");
		
		textProperty().unbind();
		
		removeCompletionListender(getItem());
		
		// setup
		super.updateItem(item, empty);
	    
		// set new UI for item
		if (item == null || empty) {
	        setText(null);
	    }
		else {
			textProperty().bind(item.summaryProperty());
			
			putCompletionStyle(item);
			
			if(!item.isComplete())
				item.isCompleteProperty().addListener(listener);
		}
	}

	private void putCompletionStyle(TraceLog item) {
		if(item.isExceptionThrower())
			getStyleClass().add("exception");
		else if(!item.isComplete())
			getStyleClass().add("not-completed");
	}
}