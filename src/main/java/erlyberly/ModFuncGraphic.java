package erlyberly;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
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

	private final SimpleStringProperty exportToolTipText = new SimpleStringProperty();

	private final SimpleStringProperty tracedIconText = new SimpleStringProperty(AwesomeIcon.STAR_ALT.toString());
	
	private final SimpleBooleanProperty tracable = new SimpleBooleanProperty();

	private final TraceFn traceFn;

	private final IsTracedFn isTracedFn;

	private ModFunc modFunc;
	
	public ModFuncGraphic(TraceFn aTraceFn, IsTracedFn isTracedFn) {
		traceFn = aTraceFn;
		this.isTracedFn = isTracedFn;
		
		getStyleClass().add("mod-func-graphic");
		
		getChildren().addAll(
			exportIconGraphic(),
			traceIcon(),
			functionLabel()
		);
	}

	private Icon exportIconGraphic() {
		Tooltip tooltip;
		
		tooltip = new Tooltip();
		tooltip.textProperty().bind(exportToolTipText);
		
		Icon treeIcon;
		
		treeIcon = treeIcon(AwesomeIcon.SQUARE);
		treeIcon.textProperty().bind(exportIconText);
		treeIcon.setTooltip(tooltip);
		return treeIcon;
	}

	private Icon traceIcon() {
		Icon traceIcon;
		
		traceIcon = Icon.create().style(ICON_STYLE);
		traceIcon.textProperty().bind(tracedIconText);
		traceIcon.visibleProperty().bind(tracable);
		traceIcon.setTooltip(new Tooltip("Toggle tracing, double click on this star or ctrl+t when selected"));
		traceIcon.getStyleClass().add("erlyberly-icon-button");
		traceIcon.setOnMouseClicked((e) -> {
			if(e.getClickCount() == 2)
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
		return Icon.create().icon(treeIcon).style(ICON_STYLE);
	}

	@Override
	public void updateItem(ModFunc item, boolean empty) {
		if (item == null || empty) {
			text.set(null);
	    }
		else {
			text.set(item.toString());
			updateExportIcon(item);
			
			// no tracing of the whole module for now!
			tracable.set(!item.isModule());
		}
		modFunc = item;
		
		onTracesChange();
	}

	private void updateExportIcon(ModFunc item) {
		AwesomeIcon icon;
		String tooltipText;
		
		if(item.isModule()) {
			tooltipText = "Module";
			icon = AwesomeIcon.CUBE;
		}
		else if(item.isExported()) {
			tooltipText = "Exported function";
			icon = AwesomeIcon.SQUARE;
		}
		else {
			tooltipText = "Unexported function";
			icon = AwesomeIcon.SQUARE_ALT;
		}

		exportToolTipText.set(tooltipText);
		exportIconText.set(icon.toString());
	}

	public void onTracesChange() {
		if(modFunc != null && isTracedFn.isTraced(modFunc))
			tracedIconText.set(AwesomeIcon.STAR.toString());
		else
			tracedIconText.set(AwesomeIcon.STAR_ALT.toString());
	}
}