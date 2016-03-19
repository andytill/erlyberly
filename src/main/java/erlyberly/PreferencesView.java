package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;

public class PreferencesView implements Initializable {
    
    @FXML
    private TextField nodeNameField;
    @FXML
    private TextField cookieField;
    @FXML
    private CheckBox autoConnectField;
    @FXML
    private CheckBox showSourceInSystemEditorBox;
    @FXML
    private CheckBox hideProcesses;
    @FXML
    private CheckBox hideModules;
    
    @Override
    public void initialize(URL url, ResourceBundle r) {
        PrefBind.bind("targetNodeName", nodeNameField.textProperty());
        PrefBind.bind("cookieName", cookieField.textProperty());
        PrefBind.bindBoolean("autoConnect", autoConnectField.selectedProperty());
        PrefBind.bindBoolean("hideProcesses", hideProcesses.selectedProperty());
        PrefBind.bindBoolean("hideModules", hideModules.selectedProperty());
        PrefBind.bindBoolean("showSourceInSystemEditor", showSourceInSystemEditorBox.selectedProperty());
    }
}