package erlyberly;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import ui.CellController;
import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;

class ModFuncGraphic extends HBox implements CellController<ModFunc> {
	
	public interface TraceFn {
		void trace(ModFunc modFunc);
	}
	
	public interface IsTracedFn {
		boolean isTraced(ModFunc mf);
	}

	private static final String ICON_STYLE = "-fx-font-family: FontAwesome; -fx-font-size: 1em;";
	
	private final SimpleStringProperty text = new SimpleStringProperty();

	private final SimpleStringProperty exportIconText = new SimpleStringProperty();

	private final SimpleStringProperty tracedIconText = new SimpleStringProperty(AwesomeIcon.STAR_ALT.toString());
	
	private final SimpleBooleanProperty tracable = new SimpleBooleanProperty();

	private final TraceFn traceFn;

	private final IsTracedFn isTracedFn;

	private ModFunc modFunc;
	
	public ModFuncGraphic(TraceFn aTraceFn, IsTracedFn isTracedFn) {
		traceFn = aTraceFn;
		this.isTracedFn = isTracedFn;
		
		getChildren().addAll(
			exportIconGraphic(),
			traceIcon(),
			functionLabel()
		);
	}

	private Icon exportIconGraphic() {
		Icon treeIcon;
		
		treeIcon = treeIcon(AwesomeIcon.SQUARE);
		treeIcon.textProperty().bind(exportIconText);
		
		return treeIcon;
	}

	private Icon traceIcon() {
		Icon traceIcon;
		
		traceIcon = Icon.create().style(ICON_STYLE);
		traceIcon.textProperty().bind(tracedIconText);
		traceIcon.visibleProperty().bind(tracable);
		traceIcon.setOnMouseClicked((e) -> {
			traceFn.trace(modFunc);
		});
		return traceIcon;
	}

	private Label functionLabel() {
		Label label;
		
		label = new Label();
		label.textProperty().bind(text);
		
		return label;
	}

	private Icon treeIcon(AwesomeIcon treeIcon) {
		Icon icon = Icon.create().icon(treeIcon).style(ICON_STYLE);
		return icon;
	}

	@Override
	public void updateItem(ModFunc item, boolean empty) {
		if (item == null || empty) {
			text.set("");
	    }
		else {
			text.set(item.toString());
			exportIconText.set(exportIcon(item).toString());
			
			// no tracing of the whole module for now!
			tracable.set(!item.isModule());
		}
		modFunc = item;
	}

	private AwesomeIcon exportIcon(ModFunc item) {
		if(item.isModule())
			return (AwesomeIcon.CUBE);
		else if(item.isExported())
			return (AwesomeIcon.SQUARE);
		else
			return (AwesomeIcon.SQUARE_ALT);
	}

	public void onTracesChange() {
		if(isTracedFn.isTraced(modFunc))
			tracedIconText.set(AwesomeIcon.STAR.toString());
		else
			tracedIconText.set(AwesomeIcon.STAR_ALT.toString());
	}
}