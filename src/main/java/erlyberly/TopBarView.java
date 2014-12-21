package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;
import javafx.beans.Observable;
import javafx.beans.property.BooleanProperty;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.Tooltip;

public class TopBarView implements Initializable {
	@FXML
	private ToggleButton hideProcessesButton;

	@Override
	public void initialize(URL url, ResourceBundle r) {
		hideProcessesButton.setGraphic(Icon.create().icon(AwesomeIcon.RANDOM));
		
		hideProcessesButton.setContentDisplay(ContentDisplay.TOP);
		hideProcessesButton.setGraphicTextGap(0d);
		hideProcessesButton.setSelected(true);
		hideProcessesButton.setTooltip(new Tooltip("Show/Hide the processes table"));
		hideProcsProperty().addListener((Observable o) -> { toggleHideProcsIcon(); });
		toggleHideProcsIcon();
	}
	
	public BooleanProperty hideProcsProperty() {
		return hideProcessesButton.selectedProperty();
	}

	
	private void toggleHideProcsIcon() {
		String buttonText = "";
		
		if(hideProcessesButton.isSelected())
			buttonText = "Show Processes";
		else
			buttonText = "Hide Processes";
		
		hideProcessesButton.setText(buttonText);
	}
}
