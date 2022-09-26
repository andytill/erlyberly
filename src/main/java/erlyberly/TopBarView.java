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
import javafx.scene.control.*;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

import java.io.IOException;
import java.net.URL;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.ResourceBundle;

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

    private final MenuButton crashReportsButton = new MenuButton("Crash Reports");
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
    public void initialize(final URL url, final ResourceBundle r) {
        this.topBox.getItems().add(this.crashReportsButton);

        // TODO: Should we hide these buttons, when disconnected ?
        this.hideProcessesButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.RANDOM));
        this.hideProcessesButton.setContentDisplay(ContentDisplay.TOP);
        this.hideProcessesButton.setGraphicTextGap(0d);
        this.hideProcessesButton.setTooltip(new Tooltip("Show/Hide the Processes (ctrl+p)"));

        // TODO: Should we hide these buttons, when disconnected ?
        this.hideFunctionsButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.CUBE));
        this.hideFunctionsButton.setContentDisplay(ContentDisplay.TOP);
        this.hideFunctionsButton.setGraphicTextGap(0d);
        this.hideFunctionsButton.setTooltip(new Tooltip("Show/Hide the Modules (ctrl+m)"));

        this.refreshModulesButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.ROTATE_LEFT));
        this.refreshModulesButton.setContentDisplay(ContentDisplay.TOP);
        this.refreshModulesButton.setGraphicTextGap(0d);
        this.refreshModulesButton.setTooltip(new Tooltip("Refresh Modules and Functions to show new, hot-loaded code (ctrl+r)"));
        this.refreshModulesButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());

        this.erlangMemoryButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.PIE_CHART));
        this.erlangMemoryButton.setContentDisplay(ContentDisplay.TOP);
        this.erlangMemoryButton.setGraphicTextGap(0d);
        this.erlangMemoryButton.setTooltip(new Tooltip("Refresh Modules and Functions to show new, hot-loaded code (ctrl+r)"));
        this.erlangMemoryButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());

        // TODO: maybe make this button available, sometimes, there are crashes causing the node to go down,
        // and then it's disabled when disconnected....( i would like to see the crashes ) :)
        this.crashReportsButton.setGraphic(this.crashReportsGraphic());
        this.crashReportsButton.setContentDisplay(ContentDisplay.LEFT);
        this.crashReportsButton.setGraphicTextGap(0d);
        this.crashReportsButton.setTooltip(new Tooltip("View crash reports received from the connected node."));
        // disable the button if we're not connected or there are no crash report menu items
        this.crashReportsButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not().or(Bindings.size(this.crashReportsButton.getItems()).isEqualTo(2)));
        this.crashReportsButton.setStyle("-fx-font-size: 10; -fx-padding: 5 5 5 5;");
        this.crashReportsButton.getItems().addAll(this.removeCrashReportsMenuItem(), new SeparatorMenuItem());

        ErlyBerly.nodeAPI().getCrashReports().addListener((ListChangeListener.Change<? extends OtpErlangObject> e) -> {
            while (e.next()) {
                for (final OtpErlangObject obj : e.getAddedSubList()) {
                    final CrashReport crashReport = new CrashReport(obj);
                    final MenuItem menuItem;
                    menuItem = new MenuItem();
                    menuItem.setGraphic(new CrashReportGraphic(crashReport));
                    menuItem.setOnAction((action) -> {
                        this.unreadCrashReportsProperty.set(0);
                        ErlyBerly.showPane("Crash Report", ErlyBerly.wrapInPane(TopBarView.crashReportView(crashReport)));
                    });
                    this.crashReportsButton.getItems().add(menuItem);
                }
            }
        });

        this.xrefAnalysisButton.setGraphic(this.xrefAnalysisGraphic());
        this.xrefAnalysisButton.setContentDisplay(ContentDisplay.TOP);
        this.xrefAnalysisButton.setGraphicTextGap(0d);
        this.xrefAnalysisButton.setTooltip(new Tooltip("Start xref analysis. This may take a while, an ok is displayed when complete."));
        this.xrefAnalysisButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not().or(this.isXrefAnalysing).or(ErlyBerly.nodeAPI().xrefStartedProperty()));
        this.xrefAnalysisButton.setOnAction((e) -> {
            this.isXrefAnalysing.set(true);
            ErlyBerly.runIO(() -> {
                try {
                    ErlyBerly.nodeAPI().ensureXRefStarted();
                } catch (final Exception e1) {
                    e1.printStackTrace();
                }
            });
        });

        this.disconnectButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.EJECT));
        this.disconnectButton.setContentDisplay(ContentDisplay.TOP);
        this.disconnectButton.setGraphicTextGap(0d);
        this.disconnectButton.setTooltip(new Tooltip("Disconnect"));
        this.disconnectButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());
        this.disconnectButton.setOnAction((e) -> {
            try {
                TopBarView.disconnect();
            } catch (final Exception e1) {
                e1.printStackTrace();
            }
        });

        this.prefButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.GEARS));
        this.prefButton.setContentDisplay(ContentDisplay.TOP);
        this.prefButton.setGraphicTextGap(0d);
        this.prefButton.setTooltip(new Tooltip("Preferences"));
        this.prefButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());
        this.prefButton.setOnAction((e) -> TopBarView.displayPreferencesPane());

        this.suspendButton.setContentDisplay(ContentDisplay.TOP);
        this.suspendButton.setGraphicTextGap(0d);
        this.suspendButton.setTooltip(new Tooltip("Toggle Trace Suspension"));
        this.suspendButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());
        this.suspendButton.setOnAction((e) -> TopBarView.suspendTraces());
        // set the default text and icon
        this.onSuspendedStateChanged(false);
        // listen to when tracing is suspend or not, and update the button text and icon
        ErlyBerly.nodeAPI().suspendedProperty().addListener((o, oldv, suspended) -> this.onSuspendedStateChanged(suspended));

        this.hideProcsProperty().addListener((Observable o) -> this.toggleHideProcs());
        this.hideFunctionsProperty().addListener((Observable o) -> this.toggleHideFuncs());

        this.erlangMemoryButton.setOnAction((e) -> TopBarView.showErlangMemory());

        final FloatyFieldControl processCountField = TopBarView.processCountStat();

        this.topBox.getItems().add(new Separator(Orientation.VERTICAL));
        this.topBox.getItems().add(processCountField);

        // let's store the ui preferences, as the end user changes them...
        PrefBind.bindBoolean("hideProcesses", this.hideProcessesButton.selectedProperty());
        PrefBind.bindBoolean("hideModules", this.hideFunctionsButton.selectedProperty());

        final boolean hideProcs = PrefBind.getOrDefaultBoolean("hideProcesses", false);
        final boolean hideMods = PrefBind.getOrDefaultBoolean("hideModules", false);

        if (hideProcs) {
            // click the hide button manually.
            this.hideProcessesButton.setSelected(true);
        }
        if (hideMods) {
            // click the hide button manually.
            this.hideFunctionsButton.setSelected(true);
        }

        this.toggleHideProcs();
        this.toggleHideFuncs();

        ErlyBerly.nodeAPI().getCrashReports().addListener(this::traceLogsChanged);
        ErlyBerly.nodeAPI().xrefStartedProperty().addListener((e, oldv, newv) -> {
            if (newv.booleanValue()) this.isXrefAnalysing.set(false);
        });
    }

    private void onSuspendedStateChanged(final Boolean suspended) {
        if (suspended.booleanValue()) {
            this.suspendButton.setText("Unsuspend");
            this.suspendButton.getStyleClass().add("button-suspended");
        } else {
            this.suspendButton.setText("Suspend");
            this.suspendButton.setGraphic(new FontAwesome().create(FontAwesome.Glyph.PAUSE));
            this.suspendButton.getStyleClass().remove("button-suspended");
        }
    }

    private static void suspendTraces() {
        try {
            ErlyBerly.nodeAPI().toggleSuspended();
        } catch (final OtpErlangException e) {
            e.printStackTrace();
        } catch (final IOException e) {
            e.printStackTrace();
        }
    }

    private MenuItem removeCrashReportsMenuItem() {
        final MenuItem menuItem;
        menuItem = new MenuItem("Remove All Reports");
        menuItem.setOnAction((e) -> {
            final ObservableList<MenuItem> items = this.crashReportsButton.getItems();
            if (2 == items.size()) return;
            // the first two items are this menu item and a separator, delete
            // everything after that
            items.remove(2, items.size());
            this.unreadCrashReportsProperty.set(0);
        });
        return menuItem;
    }

    public void traceLogsChanged(final ListChangeListener.Change<? extends OtpErlangObject> e) {
        while (e.next()) {
            final int size = e.getAddedSubList().size();
            this.unreadCrashReportsProperty.set(this.unreadCrashReportsProperty.get() + size);
        }
    }

    private static CrashReportView crashReportView(final CrashReport crashReport) {
        final CrashReportView crashReportView;
        crashReportView = new CrashReportView();
        crashReportView.setCrashReport(crashReport);
        return crashReportView;
    }

    private Parent crashReportsGraphic() {
        final Glyph icon;

        icon = new FontAwesome().create(FontAwesome.Glyph.WARNING);
        icon.setPadding(new Insets(0, 5, 0, 5));

        final Label reportCountLabel;

        reportCountLabel = new Label("122");
        reportCountLabel.setStyle("-fx-background-color:red; -fx-font-size:9; -fx-padding: 0 2 0 2; -fx-opacity:0.7;");
        reportCountLabel.setTextFill(Color.WHITE);

        reportCountLabel.setText(this.unreadCrashReportsProperty.getValue().toString());
        this.unreadCrashReportsProperty.addListener((o, oldv, newv) -> reportCountLabel.setText(newv.toString()));
        reportCountLabel.visibleProperty().bind(this.unreadCrashReportsProperty.greaterThan(0));

        final StackPane stackPane = new StackPane(icon, reportCountLabel);
        StackPane.setAlignment(reportCountLabel, Pos.TOP_RIGHT);
        return stackPane;
    }

    private Parent xrefAnalysisGraphic() {
        final Glyph icon;

        icon = new FontAwesome().create(FontAwesome.Glyph.TH_LARGE);
        icon.setPadding(new Insets(0, 5, 0, 5));
        icon.visibleProperty().bind(this.isXrefAnalysing.not());
        final Label reportCountLabel;

        reportCountLabel = new Label("ok");
        reportCountLabel.setStyle("-fx-background-color:green; -fx-font-size:9; -fx-padding: 0 2 0 2; -fx-opacity:0.9;");
        reportCountLabel.setTextFill(Color.WHITE);

        final ProgressIndicator analysisProgressIndicator;
        analysisProgressIndicator = new ProgressIndicator();
        analysisProgressIndicator.visibleProperty().bind(this.isXrefAnalysing);
        analysisProgressIndicator.setPrefSize(10d, 10d);
        analysisProgressIndicator.setStyle("-fx-progress-color: black;");

        reportCountLabel.visibleProperty().bind(ErlyBerly.nodeAPI().xrefStartedProperty());

        final StackPane stackPane = new StackPane(icon, reportCountLabel, analysisProgressIndicator);
        StackPane.setAlignment(reportCountLabel, Pos.TOP_RIGHT);
        return stackPane;
    }

    private static void showErlangMemory() {
        final ObservableList<PieChart.Data> data = FXCollections.observableArrayList();

        TopBarView.showPieChart(data);

        final ErlangMemoryThread emThread;
        emThread = new ErlangMemoryThread(data);
        emThread.start();
    }

    private static void showPieChart(final ObservableList<PieChart.Data> data) {
        final String title = "Erlang Memory";

        final PieChart pieChart;

        pieChart = new PieChart(data);
        pieChart.setTitle(title);
        ErlyBerly.showPane(title, ErlyBerly.wrapInPane(pieChart));
    }

    /**
     * these have to be run after initialisation is complete or an exception occurs
     */
    public void addAccelerators() {
        Platform.runLater(() -> this.accelerators().put(TOGGLE_HIDE_PROCESSES_SHORTCUT, () -> TopBarView.invertSelection(this.hideProcessesButton)));
        Platform.runLater(() -> this.accelerators().put(TOGGLE_HIDE_MODULES_SHORTCUT, () -> TopBarView.invertSelection(this.hideFunctionsButton)));
        Platform.runLater(() -> this.accelerators().put(REFRESH_MODULES_SHORTCUT, () -> {
            if (null != this.refreshModulesAction) this.refreshModulesAction.handle(null);
        }));
    }

    private static FloatyFieldControl processCountStat() {
        final FloatyFieldControl floatyFieldControl;
        floatyFieldControl = new FloatyFieldControl();
        floatyFieldControl.getStyleClass().add("floaty-label");
        floatyFieldControl.getModel().promptTextProperty().set("Processes");
        floatyFieldControl.getModel().textProperty().set("0");
        floatyFieldControl.getModel().disableProperty().set(true);

        ErlyBerly.nodeAPI().appProcsProperty().addListener((o, ov, nv) -> TopBarView.upateProcsStat(floatyFieldControl, nv));

        return floatyFieldControl;
    }

    private static void upateProcsStat(final FloatyFieldControl floatyFieldControl, final AppProcs nv) {
        final String dateString = nv.getDateTime().format(DateTimeFormatter.ISO_TIME);
        floatyFieldControl.getModel().textProperty().set(Integer.toString(nv.getProcCount()));
        floatyFieldControl.getModel().promptTextProperty().set("Processes @ " + dateString);
    }

    private ObservableMap<KeyCombination, Runnable> accelerators() {
        final Scene scene = this.hideProcessesButton.getScene();

        assert null != scene : "button not added to scene";

        return scene.getAccelerators();
    }

    private static void invertSelection(final ToggleButton toggleButton) {
        toggleButton.setSelected(!toggleButton.isSelected());
    }

    public BooleanProperty hideProcsProperty() {
        return this.hideProcessesButton.selectedProperty();
    }

    public BooleanProperty hideFunctionsProperty() {
        return this.hideFunctionsButton.selectedProperty();
    }

    private void toggleHideProcs() {
        final String buttonText;

        if (this.hideProcessesButton.isSelected()) buttonText = "Show Processes";
        else buttonText = "Hide Processes";

        this.hideProcessesButton.setText(buttonText);
    }

    private void toggleHideFuncs() {
        final String buttonText;

        if (this.hideFunctionsButton.isSelected()) buttonText = "Show Modules";
        else buttonText = "Hide Modules";

        this.hideFunctionsButton.setText(buttonText);
    }

    public void setOnRefreshModules(final EventHandler<ActionEvent> e) {
        this.refreshModulesAction = e;

        this.refreshModulesButton.setOnAction(this.refreshModulesAction);
    }

    public static void disconnect() {
        ErlyBerly.runIOAndWait(() -> {
            try {
                ErlyBerly.nodeAPI().manuallyDisconnect();
                ErlyBerly.nodeAPI().disconnect();
            } catch (final Exception e) {
                System.out.println(e);
            }
        });
        ErlyBerly.displayConnectionPopup();
    }

    private static void displayPreferencesPane() {
        ErlyBerly.showPreferencesPane();
    }

    static class ErlangMemoryThread extends Thread {
        private final ObservableList<PieChart.Data> pieData;

        ErlangMemoryThread(final ObservableList<PieChart.Data> thePieData) {
            super();
            this.pieData = thePieData;

            this.setName("Erlang Memory Thread");
            this.setDaemon(true);
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

                Platform.runLater(() -> this.populatePieData(erlangMemory));
            } catch (final Exception e) {
                e.printStackTrace();
            }
        }

        private void populatePieData(final Map<Object, Object> erlangMemory) {
            for (final Map.Entry<Object, Object> entry : erlangMemory.entrySet()) {
                final long kb = (long) (Double.parseDouble(entry.getValue().toString()) / 1024);
                final String label = entry.getKey().toString() + " (" + kb + " KB)";
                this.pieData.add(new PieChart.Data(label, kb));
            }
        }
    }
}
