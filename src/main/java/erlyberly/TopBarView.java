package erlyberly;

import java.net.URL;
import java.time.format.DateTimeFormatter;
import java.util.ResourceBundle;

import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.property.BooleanProperty;
import javafx.collections.ObservableMap;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Orientation;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Separator;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;
import erlyberly.node.AppProcs;
import floatyfield.FloatyFieldView;

public class TopBarView implements Initializable {
	private static final KeyCodeCombination TOGGLE_HIDE_PROCESSES_SHORTCUT = new KeyCodeCombination(KeyCode.P, KeyCombination.CONTROL_DOWN);

	private static final KeyCodeCombination TOGGLE_HIDE_MODULES_SHORTCUT = new KeyCodeCombination(KeyCode.M, KeyCombination.CONTROL_DOWN);
	
	@FXML
	private ToggleButton hideProcessesButton;
	@FXML
	private ToggleButton hideFunctionsButton;
	@FXML
	private Button refreshModulesButton;
  	@FXML
	private ToolBar topBox;
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
		hideProcessesButton.setGraphic(Icon.create().icon(AwesomeIcon.RANDOM));
		hideProcessesButton.setContentDisplay(ContentDisplay.TOP);
		hideProcessesButton.setGraphicTextGap(0d);
		hideProcessesButton.setTooltip(new Tooltip("Show/Hide the Processes"));

		hideFunctionsButton.setGraphic(Icon.create().icon(AwesomeIcon.CUBE));
		hideFunctionsButton.setContentDisplay(ContentDisplay.TOP);
		hideFunctionsButton.setGraphicTextGap(0d);
		hideFunctionsButton.setTooltip(new Tooltip("Show/Hide the Modules"));

		refreshModulesButton.setGraphic(Icon.create().icon(AwesomeIcon.ROTATE_LEFT));
		refreshModulesButton.setContentDisplay(ContentDisplay.TOP);
		refreshModulesButton.setGraphicTextGap(0d);
		refreshModulesButton.setTooltip(new Tooltip("Refresh Modules and Functions to show new, hot-loaded code"));
		refreshModulesButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());		
		
		hideProcsProperty().addListener((Observable o) -> { toggleHideProcsText(); });
		hideFunctionsProperty().addListener((Observable o) -> { toggleHideFuncsText(); });
		
		FxmlLoadable loader = processCountStat();	
		
		topBox.getItems().add(new Separator(Orientation.VERTICAL));
		topBox.getItems().add(loader.fxmlNode);
		
		toggleHideProcsText();
		toggleHideFuncsText();
	}

	/**
	 * these have to be run after initialisation is complete or an exception occurs
	 */
	public void addAccelerators() {
		Platform.runLater(() -> {
			accelerators().put(TOGGLE_HIDE_PROCESSES_SHORTCUT, () -> { invertSelection(hideProcessesButton); });
		});
		Platform.runLater(() -> {
			accelerators().put(TOGGLE_HIDE_MODULES_SHORTCUT, () -> { invertSelection(hideFunctionsButton); });
		});
	}

	private FxmlLoadable processCountStat() {
		FxmlLoadable loader = new FxmlLoadable("/floatyfield/floaty-field.fxml");
		
		loader.load();
		
		Parent fxmlNode;
		
		fxmlNode = loader.fxmlNode;
		fxmlNode.getStyleClass().add("floaty-label");

		FloatyFieldView ffView;
		
		ffView = (FloatyFieldView) loader.controller;
		ffView.promptTextProperty().set("Processes");
		ffView.textProperty().set("0");
		ffView.disableProperty().set(true);

		ErlyBerly.nodeAPI().appProcsProperty().addListener((o, ov, nv) -> { upateProcsStat(ffView, nv); });
		
		return loader;
	}
	
	private void upateProcsStat(FloatyFieldView ffView, AppProcs nv) {
		String dateString = nv.getDateTime().format(DateTimeFormatter.ISO_TIME);
		
		ffView.textProperty().set(Integer.toString(nv.getProcCount()));
		ffView.promptTextProperty().set("Processes @ " + dateString);
	}

	private ObservableMap<KeyCombination, Runnable> accelerators() {
		Scene scene = hideProcessesButton.getScene();
		
		assert scene != null : "button not added to scene";
		
		return scene.getAccelerators();
	}
	
	private void invertSelection(ToggleButton toggleButton) {
		toggleButton.setSelected(!toggleButton.isSelected());
	}
	
	public BooleanProperty hideProcsProperty() {
		return hideProcessesButton.selectedProperty();
	}
	
	public BooleanProperty hideFunctionsProperty() {
		return hideFunctionsButton.selectedProperty();
	}
	
	private void toggleHideProcsText() {
		String buttonText = "";
		
		if(hideProcessesButton.isSelected())
			buttonText = "Show Processes";
		else
			buttonText = "Hide Processes";
		
		hideProcessesButton.setText(buttonText);
	}
	
	private void toggleHideFuncsText() {
		String buttonText = "";
		
		if(hideFunctionsButton.isSelected())
			buttonText = "Show Modules";
		else
			buttonText = "Hide Modules";
		
		hideFunctionsButton.setText(buttonText);
	}

	public final void setOnRefreshModules(EventHandler<ActionEvent> e) {
		refreshModulesButton.setOnAction(e);
	}
}
