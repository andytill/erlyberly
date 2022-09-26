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

import java.io.IOException;
import java.net.URL;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.Map.Entry;
import java.util.ResourceBundle;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;

import erlyberly.node.AppProcs;
import erlyberly.node.OtpUtil;
import floatyfield.FloatyFieldControl;
import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.chart.PieChart;
import javafx.scene.chart.PieChart.Data;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Label;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.control.Separator;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

public class TopBarView implements Initializable {
    private static final KeyCodeCombination TOGGLE_HIDE_PROCESSES_SHORTCUT = new KeyCodeCombination(KeyCode.P, KeyCombination.SHORTCUT_DOWN);

    private static final KeyCodeCombination TOGGLE_HIDE_MODULES_SHORTCUT = new KeyCodeCombination(KeyCode.M, KeyCombination.SHORTCUT_DOWN);

    private static final KeyCodeCombination REFRESH_MODULES_SHORTCUT = new KeyCodeCombination(KeyCode.R, KeyCombination.SHORTCUT_DOWN);

    private final SimpleIntegerProperty unreadCrashReportsProperty = new SimpleIntegerProperty(0);

    private final SimpleBooleanProperty isXrefAnalysing = new SimpleBooleanProperty();

    @FXML
    private ToggleButton hideProcessesButton;
    @FXML
    private ToggleButton hideFunctionsButton;
    @FXML
    private Button refreshModulesButton;
    @FXML
    private Button erlangMemoryButton;

    private MenuButton crashReportsButton = new MenuButton("Crash Reports");
    @FXML
    private Button xrefAnalysisButton;
    @FXML
    private Button disconnectButton;
    @FXML
    private Button prefButton;
    @FXML
    private Button suspendButton;
    @FXML
    private ToolBar topBox;

    private EventHandler<ActionEvent> refreshModulesAction;

    @Override
    public void initialize(URL url, ResourceBundle r) {
        topBox.getItems().add(crashReportsButton);

        // TODO: Should we hide these buttons, when disconnected ?
        hideProcessesButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.RANDOM));
        hideProcessesButton.setContentDisplay(ContentDisplay.TOP);
        hideProcessesButton.setGraphicTextGap(0d);
        hideProcessesButton.setTooltip(new Tooltip("Show/Hide the Processes (ctrl+p)"));

        // TODO: Should we hide these buttons, when disconnected ?
        hideFunctionsButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.CUBE));
        hideFunctionsButton.setContentDisplay(ContentDisplay.TOP);
        hideFunctionsButton.setGraphicTextGap(0d);
        hideFunctionsButton.setTooltip(new Tooltip("Show/Hide the Modules (ctrl+m)"));

        refreshModulesButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.ROTATE_LEFT));
        refreshModulesButton.setContentDisplay(ContentDisplay.TOP);
        refreshModulesButton.setGraphicTextGap(0d);
        refreshModulesButton.setTooltip(new Tooltip("Refresh Modules and Functions to show new, hot-loaded code (ctrl+r)"));
        refreshModulesButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());

        erlangMemoryButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.PIE_CHART));
        erlangMemoryButton.setContentDisplay(ContentDisplay.TOP);
        erlangMemoryButton.setGraphicTextGap(0d);
        erlangMemoryButton.setTooltip(new Tooltip("Refresh Modules and Functions to show new, hot-loaded code (ctrl+r)"));
        erlangMemoryButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());

        // TODO: maybe make this button available, sometimes, there are crashes causing the node to go down,
        // and then it's disabled when disconnected....( i would like to see the crashes ) :)
        crashReportsButton.setGraphic(crashReportsGraphic());
        crashReportsButton.setContentDisplay(ContentDisplay.LEFT);
        crashReportsButton.setGraphicTextGap(0d);
        crashReportsButton.setTooltip(new Tooltip("View crash reports received from the connected node."));
        // disable the button if we're not connected or there are no crash report menu items
        crashReportsButton.disableProperty().bind(
                ErlyBerly.nodeAPI().connectedProperty().not()
                        .or(Bindings.size(crashReportsButton.getItems()).isEqualTo(2)));
        crashReportsButton.setStyle("-fx-font-size: 10; -fx-padding: 5 5 5 5;");
        crashReportsButton.getItems().addAll(removeCrashReportsMenuItem(), new SeparatorMenuItem());

        ErlyBerly.nodeAPI().getCrashReports()
                .addListener((ListChangeListener.Change<? extends OtpErlangObject> e) -> {
                    while (e.next()) {
                        for (OtpErlangObject obj : e.getAddedSubList()) {
                            CrashReport crashReport = new CrashReport(obj);
                            MenuItem menuItem;
                            menuItem = new MenuItem();
                            menuItem.setGraphic(new CrashReportGraphic(crashReport));
                            menuItem.setOnAction((action) -> {
                                unreadCrashReportsProperty.set(0);
                                ErlyBerly.showPane("Crash Report", ErlyBerly.wrapInPane(crashReportView(crashReport)));
                            });
                            crashReportsButton.getItems().add(menuItem);
                        }
                    }
                });

        xrefAnalysisButton.setGraphic(xrefAnalysisGraphic());
        xrefAnalysisButton.setContentDisplay(ContentDisplay.TOP);
        xrefAnalysisButton.setGraphicTextGap(0d);
        xrefAnalysisButton.setTooltip(new Tooltip("Start xref analysis. This may take a while, an ok is displayed when complete."));
        xrefAnalysisButton.disableProperty().bind(
                ErlyBerly.nodeAPI().connectedProperty().not().or(isXrefAnalysing).or(ErlyBerly.nodeAPI().xrefStartedProperty()));
        xrefAnalysisButton.setOnAction((e) -> {
            isXrefAnalysing.set(true);
            ErlyBerly.runIO(() -> {
                try {
                    ErlyBerly.nodeAPI().ensureXRefStarted();
                } catch (Exception e1) {
                    e1.printStackTrace();
                }
            });
        });

        disconnectButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.EJECT));
        disconnectButton.setContentDisplay(ContentDisplay.TOP);
        disconnectButton.setGraphicTextGap(0d);
        disconnectButton.setTooltip(new Tooltip("Disconnect"));
        disconnectButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());
        disconnectButton.setOnAction((e) -> {
            try {
                disconnect();
            } catch (Exception e1) {
                e1.printStackTrace();
            }
        });

        prefButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.GEARS));
        prefButton.setContentDisplay(ContentDisplay.TOP);
        prefButton.setGraphicTextGap(0d);
        prefButton.setTooltip(new Tooltip("Preferences"));
        prefButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());
        prefButton.setOnAction((e) -> {
            displayPreferencesPane();
        });

        suspendButton.setContentDisplay(ContentDisplay.TOP);
        suspendButton.setGraphicTextGap(0d);
        suspendButton.setTooltip(new Tooltip("Toggle Trace Suspension"));
        suspendButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());
        suspendButton.setOnAction((e) -> {
            suspendTraces();
        });
        // set the default text and icon
        onSuspendedStateChanged(false);
        // listen to when tracing is suspend or not, and update the button text and icon
        ErlyBerly.nodeAPI().suspendedProperty().addListener((o, oldv, suspended) -> {
            onSuspendedStateChanged(suspended);
        });

        hideProcsProperty().addListener((Observable o) -> {
            toggleHideProcs();
        });
        hideFunctionsProperty().addListener((Observable o) -> {
            toggleHideFuncs();
        });

        erlangMemoryButton.setOnAction((e) -> {
            showErlangMemory();
        });

        FloatyFieldControl processCountField = processCountStat();

        topBox.getItems().add(new Separator(Orientation.VERTICAL));
        topBox.getItems().add(processCountField);

        // let's store the ui preferences, as the end user changes them...
        PrefBind.bindBoolean("hideProcesses", hideProcessesButton.selectedProperty());
        PrefBind.bindBoolean("hideModules", hideFunctionsButton.selectedProperty());

        boolean hideProcs = PrefBind.getOrDefaultBoolean("hideProcesses", false);
        boolean hideMods = PrefBind.getOrDefaultBoolean("hideModules", false);

        if (hideProcs) {
            // click the hide button manually.
            hideProcessesButton.setSelected(true);
        }
        if (hideMods) {
            // click the hide button manually.
            hideFunctionsButton.setSelected(true);
        }

        toggleHideProcs();
        toggleHideFuncs();

        ErlyBerly.nodeAPI()
                .getCrashReports()
                .addListener(this::traceLogsChanged);
        ErlyBerly.nodeAPI()
                .xrefStartedProperty()
                .addListener((e, oldv, newv) -> {
                    if (newv) isXrefAnalysing.set(false);
                });
    }

    private void onSuspendedStateChanged(Boolean suspended) {
        if (suspended) {
            suspendButton.setText("Unsuspend");
            suspendButton.getStyleClass().add("button-suspended");
        } else {
            suspendButton.setText("Suspend");
            suspendButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.PAUSE));
            suspendButton.getStyleClass().remove("button-suspended");
        }
    }

    private void suspendTraces() {
        try {
            ErlyBerly.nodeAPI().toggleSuspended();
        } catch (OtpErlangException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private MenuItem removeCrashReportsMenuItem() {
        MenuItem menuItem;
        menuItem = new MenuItem("Remove All Reports");
        menuItem.setOnAction((e) -> {
            ObservableList<MenuItem> items = crashReportsButton.getItems();
            if (items.size() == 2)
                return;
            // the first two items are this menu item and a separator, delete
            // everything after that
            items.remove(2, items.size());
            unreadCrashReportsProperty.set(0);
        });
        return menuItem;
    }

    public void traceLogsChanged(ListChangeListener.Change<? extends OtpErlangObject> e) {
        while (e.next()) {
            int size = e.getAddedSubList().size();
            unreadCrashReportsProperty.set(unreadCrashReportsProperty.get() + size);
        }
    }

    private CrashReportView crashReportView(CrashReport crashReport) {
        CrashReportView crashReportView;
        crashReportView = new CrashReportView();
        crashReportView.setCrashReport(crashReport);
        return crashReportView;
    }

    private Parent crashReportsGraphic() {
        Glyph icon;

        icon = new FontAwesome().create(FontAwesome.Glyph.WARNING);
        icon.setPadding(new Insets(0, 5, 0, 5));

        Label reportCountLabel;

        reportCountLabel = new Label("122");
        reportCountLabel.setStyle("-fx-background-color:red; -fx-font-size:9; -fx-padding: 0 2 0 2; -fx-opacity:0.7");
        reportCountLabel.setTextFill(Color.WHITE);

        reportCountLabel.setText(unreadCrashReportsProperty.getValue().toString());
        unreadCrashReportsProperty.addListener((o, oldv, newv) -> {
            reportCountLabel.setText(newv.toString());
        });
        reportCountLabel.visibleProperty().bind(unreadCrashReportsProperty.greaterThan(0));

        StackPane stackPane = new StackPane(icon, reportCountLabel);
        StackPane.setAlignment(reportCountLabel, Pos.TOP_RIGHT);
        return stackPane;
    }

    private Parent xrefAnalysisGraphic() {
        Glyph icon;

        icon = new FontAwesome().create(FontAwesome.Glyph.TH_LARGE);
        icon.setPadding(new Insets(0, 5, 0, 5));
        icon.visibleProperty().bind(isXrefAnalysing.not());
        Label reportCountLabel;

        reportCountLabel = new Label("ok");
        reportCountLabel.setStyle("-fx-background-color:green; -fx-font-size:9; -fx-padding: 0 2 0 2; -fx-opacity:0.9");
        reportCountLabel.setTextFill(Color.WHITE);

        ProgressIndicator analysisProgressIndicator;
        analysisProgressIndicator = new ProgressIndicator();
        analysisProgressIndicator.visibleProperty().bind(isXrefAnalysing);
        analysisProgressIndicator.setPrefSize(10d, 10d);
        analysisProgressIndicator.setStyle("-fx-progress-color: black;");

        reportCountLabel.visibleProperty().bind(ErlyBerly.nodeAPI().xrefStartedProperty());

        StackPane stackPane = new StackPane(icon, reportCountLabel, analysisProgressIndicator);
        StackPane.setAlignment(reportCountLabel, Pos.TOP_RIGHT);
        return stackPane;
    }

    private void showErlangMemory() {
        ObservableList<PieChart.Data> data = FXCollections.observableArrayList();

        showPieChart(data);

        ErlangMemoryThread emThread;
        emThread = new ErlangMemoryThread(data);
        emThread.start();
    }

    private void showPieChart(ObservableList<PieChart.Data> data) {
        String title = "Erlang Memory";

        PieChart pieChart;

        pieChart = new PieChart(data);
        pieChart.setTitle(title);
        ErlyBerly.showPane(title, ErlyBerly.wrapInPane(pieChart));
    }

    /**
     * these have to be run after initialisation is complete or an exception occurs
     */
    public void addAccelerators() {
        Platform.runLater(() -> {
            accelerators().put(TOGGLE_HIDE_PROCESSES_SHORTCUT, () -> {
                invertSelection(hideProcessesButton);
            });
        });
        Platform.runLater(() -> {
            accelerators().put(TOGGLE_HIDE_MODULES_SHORTCUT, () -> {
                invertSelection(hideFunctionsButton);
            });
        });
        Platform.runLater(() -> {
            accelerators().put(REFRESH_MODULES_SHORTCUT, () -> {
                if (refreshModulesAction != null)
                    refreshModulesAction.handle(null);
            });
        });
    }

    private FloatyFieldControl processCountStat() {
        FloatyFieldControl floatyFieldControl;
        floatyFieldControl = new FloatyFieldControl();
        floatyFieldControl.getStyleClass().add("floaty-label");
        floatyFieldControl.getModel().promptTextProperty().set("Processes");
        floatyFieldControl.getModel().textProperty().set("0");
        floatyFieldControl.getModel().disableProperty().set(true);

        ErlyBerly.nodeAPI().appProcsProperty().addListener((o, ov, nv) -> {
            upateProcsStat(floatyFieldControl, nv);
        });

        return floatyFieldControl;
    }

    private void upateProcsStat(FloatyFieldControl floatyFieldControl, AppProcs nv) {
        String dateString = nv.getDateTime().format(DateTimeFormatter.ISO_TIME);
        floatyFieldControl.getModel().textProperty().set(Integer.toString(nv.getProcCount()));
        floatyFieldControl.getModel().promptTextProperty().set("Processes @ " + dateString);
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

    private void toggleHideProcs() {
        String buttonText = "";

        if (hideProcessesButton.isSelected())
            buttonText = "Show Processes";
        else
            buttonText = "Hide Processes";

        hideProcessesButton.setText(buttonText);
    }

    private void toggleHideFuncs() {
        String buttonText = "";

        if (hideFunctionsButton.isSelected())
            buttonText = "Show Modules";
        else
            buttonText = "Hide Modules";

        hideFunctionsButton.setText(buttonText);
    }

    public final void setOnRefreshModules(EventHandler<ActionEvent> e) {
        refreshModulesAction = e;

        refreshModulesButton.setOnAction(refreshModulesAction);
    }

    public void disconnect() throws IOException, OtpErlangException {
        ErlyBerly.runIOAndWait(() -> {
            try {
                ErlyBerly.nodeAPI().manuallyDisconnect();
                ErlyBerly.nodeAPI().disconnect();
            } catch (Exception e) {
                System.out.println(e);
            }
        });
        ErlyBerly.displayConnectionPopup();
    }

    private void displayPreferencesPane() {
        ErlyBerly.showPreferencesPane();
    }

    class ErlangMemoryThread extends Thread {
        private final ObservableList<Data> pieData;

        public ErlangMemoryThread(ObservableList<PieChart.Data> thePieData) {
            pieData = thePieData;

            setName("Erlang Memory Thread");
            setDaemon(true);
        }

        @Override
        public void run() {
            try {
                final Map<Object, Object> erlangMemory = ErlyBerly.nodeAPI().erlangMemory();

                // remove stats which are combinations of other stats
                erlangMemory.remove(OtpUtil.atom("maximum"));
                erlangMemory.remove(OtpUtil.atom("total"));
                erlangMemory.remove(OtpUtil.atom("system"));
                erlangMemory.remove(OtpUtil.atom("processes_used"));
                erlangMemory.remove(OtpUtil.atom("atom_used"));

                Platform.runLater(() -> {
                    populatePieData(erlangMemory);
                });
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        private void populatePieData(final Map<Object, Object> erlangMemory) {
            for (Entry<Object, Object> entry : erlangMemory.entrySet()) {
                long kb = (long) (Double.parseDouble(entry.getValue().toString()) / 1024);
                String label = entry.getKey().toString() + " (" + kb + " KB)";
                pieData.add(new Data(label, kb));
            }
        }
    }
}
