/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import erlyberly.format.ErlangFormatter;
import erlyberly.format.ElixirFormatter;
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
    private RadioButton elixirTermsButton;
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
        elixirTermsButton.setToggleGroup(group);
        lfeTermsButton.setToggleGroup(group);

        PrefBind.bind("targetNodeName", nodeNameField.textProperty());
        PrefBind.bind("cookieName", cookieField.textProperty());
        PrefBind.bindBoolean("autoConnect", autoConnectField.selectedProperty());
        PrefBind.bindBoolean("hideProcesses", hideProcesses.selectedProperty());
        PrefBind.bindBoolean("hideModules", hideModules.selectedProperty());
        PrefBind.bindBoolean("showSourceInSystemEditor", showSourceInSystemEditorBox.selectedProperty());
    }

    private void storeFormattingPreferenceChange() {
        if(erlangTermsButton.isSelected()) {
            PrefBind.set("termFormatting", "erlang");
            ErlyBerly.setTermFormatter(new ErlangFormatter());
        }
        else if(elixirTermsButton.isSelected()) {
            PrefBind.set("termFormatting", "elixir");
            ErlyBerly.setTermFormatter(new ElixirFormatter());
        }
        else if(lfeTermsButton.isSelected()) {
            PrefBind.set("termFormatting", "lfe");
            ErlyBerly.setTermFormatter(new LFEFormatter());
        }
        selectFormattingButton();
    }

    private void selectFormattingButton() {
        String formattingPref = PrefBind.getOrDefault("termFormatting", "erlang").toString();
        if("erlang".equals(formattingPref)) {
            erlangTermsButton.setSelected(true);
        }
        else if("elixir".equals(formattingPref)) {
            elixirTermsButton.setSelected(true);
        }
        else if("lfe".equals(formattingPref)) {
            lfeTermsButton.setSelected(true);
        }
        else
            throw new RuntimeException("Invalid configuration for property 'termFormatting' it must be 'erlang' or 'lfe' but was " + formattingPref);
    }
}
