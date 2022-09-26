/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 * <p>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly;

import erlyberly.format.ElixirFormatter;
import erlyberly.format.ErlangFormatter;
import erlyberly.format.LFEFormatter;
import javafx.beans.Observable;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleGroup;

import java.net.URL;
import java.util.ResourceBundle;

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
    public void initialize(final URL url, final ResourceBundle r) {
        this.selectFormattingButton();

        final ToggleGroup group;
        group = new ToggleGroup();
        group.selectedToggleProperty().addListener((Observable o) -> this.storeFormattingPreferenceChange());
        this.erlangTermsButton.setToggleGroup(group);
        this.elixirTermsButton.setToggleGroup(group);
        this.lfeTermsButton.setToggleGroup(group);

        PrefBind.bind("targetNodeName", this.nodeNameField.textProperty());
        PrefBind.bind("cookieName", this.cookieField.textProperty());
        PrefBind.bindBoolean("autoConnect", this.autoConnectField.selectedProperty());
        PrefBind.bindBoolean("hideProcesses", this.hideProcesses.selectedProperty());
        PrefBind.bindBoolean("hideModules", this.hideModules.selectedProperty());
        PrefBind.bindBoolean("showSourceInSystemEditor", this.showSourceInSystemEditorBox.selectedProperty());
    }

    private void storeFormattingPreferenceChange() {
        if (this.erlangTermsButton.isSelected()) {
            PrefBind.set("termFormatting", "erlang");
            ErlyBerly.setTermFormatter(new ErlangFormatter());
        } else if (this.elixirTermsButton.isSelected()) {
            PrefBind.set("termFormatting", "elixir");
            ErlyBerly.setTermFormatter(new ElixirFormatter());
        } else if (this.lfeTermsButton.isSelected()) {
            PrefBind.set("termFormatting", "lfe");
            ErlyBerly.setTermFormatter(new LFEFormatter());
        }
        this.selectFormattingButton();
    }

    private void selectFormattingButton() {
        final String formattingPref = PrefBind.getOrDefault("termFormatting", "erlang").toString();
        if ("erlang".equals(formattingPref)) {
            this.erlangTermsButton.setSelected(true);
        } else if ("elixir".equals(formattingPref)) {
            this.elixirTermsButton.setSelected(true);
        } else if ("lfe".equals(formattingPref)) {
            this.lfeTermsButton.setSelected(true);
        } else
            throw new RuntimeException("Invalid configuration for property 'termFormatting' it must be 'erlang' or 'lfe' but was " + formattingPref);
    }
}
