package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

public class PreferencesView implements Initializable {
	
	@FXML
	private TextField nodeNameField;
	@FXML
	private TextField cookieField;
	@FXML
	private Button saveButton;
	@FXML
    private CheckBox autoConnectField;
	@FXML
    private CheckBox hideProcesses;
	@FXML
    private CheckBox hideModules;
//	@FXML
//	private TextField processPaneWidth;
//	@FXML
//	private TextField modulesPaneWidth;
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
		PrefBind.bind("targetNodeName", nodeNameField.textProperty());
		PrefBind.bind("cookieName", cookieField.textProperty());
        PrefBind.bind_boolean("autoConnect", autoConnectField.selectedProperty());
		PrefBind.bind_boolean("hideProcesses", hideProcesses.selectedProperty());
        PrefBind.bind_boolean("hideModules", hideModules.selectedProperty());
	}
	
	@FXML
	public void onSave(){
		PrefBind.store();
		closeThisWindow();
	}
	
	// TODO: make into a more generic stage handling function.
	private void closeThisWindow() {
		Stage stage;	
		stage = (Stage) saveButton.getScene().getWindow();
		stage.close();
	}

	
}