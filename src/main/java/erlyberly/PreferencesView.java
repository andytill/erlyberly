package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import erlyberly.format.ErlangFormatter;
import erlyberly.format.LFEFormatter;
import javafx.beans.Observable;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleGroup;

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
    @FXML
    private RadioButton erlangTermsButton;
    @FXML
    private RadioButton lfeTermsButton;

    @Override
    public void initialize(URL url, ResourceBundle r) {
        selectFormattingButton();

        final ToggleGroup group;
        group = new ToggleGroup();
        group.selectedToggleProperty().addListener((Observable o) -> {
            storeFormattingPreferenceChange();
        });
        erlangTermsButton.setToggleGroup(group);
        lfeTermsButton.setToggleGroup(group);

        PrefBind.bind("targetNodeName", nodeNameField.textProperty());
        PrefBind.bind("cookieName", cookieField.textProperty());
        PrefBind.bindBoolean("autoConnect", autoConnectField.selectedProperty());
        PrefBind.bindBoolean("hideProcesses", hideProcesses.selectedProperty());
        PrefBind.bindBoolean("hideModules", hideModules.selectedProperty());
        PrefBind.bindBoolean("showSourceInSystemEditor", showSourceInSystemEditorBox.selectedProperty());
    }

    private void storeFormattingPreferenceChange() {
        if(erlangTermsButton.isSelected())
            PrefBind.set("termFormatting", "erlang");
        else if(lfeTermsButton.isSelected())
            PrefBind.set("termFormatting", "lfe");
        selectFormattingButton();
    }

    private void selectFormattingButton() {
        String formattingPref = PrefBind.getOrDefault("termFormatting", "erlang").toString();
        if("erlang".equals(formattingPref)) {
            erlangTermsButton.setSelected(true);
            ErlyBerly.setTermFormatter(new ErlangFormatter());
        }
        else if("lfe".equals(formattingPref)) {
            lfeTermsButton.setSelected(true);
            ErlyBerly.setTermFormatter(new LFEFormatter());
        }
        else
            throw new RuntimeException("Invalid configuration for property 'termFormatting' it must be 'erlang' or 'lfe' but was " + formattingPref);
    }
}
