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

import erlyberly.format.ElixirFormatter;
import erlyberly.format.ErlangFormatter;
import erlyberly.format.LFEFormatter;
import erlyberly.format.TermFormatter;
import erlyberly.node.NodeAPI;
import javafx.application.Application;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.SplitPane;
import javafx.scene.control.SplitPane.Divider;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.Pane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Modality;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.concurrent.*;

public class ErlyBerly extends Application {

    private static final ScheduledExecutorService IO_EXECUTOR = Executors.newScheduledThreadPool(4);

    private static final NodeAPI NODE_API;

    static {
        startEpmd();
        NODE_API = new NodeAPI();
    }

    /**
     * The preferences tab, that is added when the user presses the 'Preferences'
     * button in the top bar. Static because there can be only one.
     */
    private static Tab prefstab;

    private SplitPane splitPane;

    private Region entopPane;

    private double entopDivPosition;

    private static TabPane tabPane;

    private static TermFormatter termFormatter;

    private static Stage primaryStage;

    public static void main(String[] args) throws Exception {
        launch(args);
    }

    public static void runIO(Runnable runnable) {
        IO_EXECUTOR.execute(runnable);
    }

    public static void runIOAndWait(Runnable runnable) {
        Future<?> future = IO_EXECUTOR.submit(runnable);
        try {
            future.get();
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static ScheduledFuture<?> scheduledIO(long delayMillis, Runnable runnable) {
        return IO_EXECUTOR.schedule(runnable, delayMillis, TimeUnit.MILLISECONDS);
    }

    @Override
    public void start(Stage aPrimaryStage) {
        primaryStage = aPrimaryStage;
        try {
            PrefBind.setup();
        } catch (IOException e) {
            e.printStackTrace();
        }
        termFormatter = formatterFromConfig();

        FxmlLoadable topBarFxml;
        topBarFxml = new FxmlLoadable("/erlyberly/topbar.fxml");
        topBarFxml.load();

        FxmlLoadable dbgFxml;
        dbgFxml = new FxmlLoadable("/erlyberly/dbg.fxml");
        dbgFxml.load();
        DbgView dbgView = (DbgView)dbgFxml.controller;
        tabPane = dbgView.getTabPane();

        splitPane = new SplitPane();
        entopPane = (Region) loadEntopPane();
        splitPane.getItems().add(entopPane);
        splitPane.getItems().add(dbgFxml.load());

        setupProcPaneHiding(topBarFxml, dbgFxml);

        VBox rootView;
        rootView = new VBox(topBarFxml.fxmlNode, splitPane);
        rootView.setMaxWidth(Double.MAX_VALUE);
        VBox.setVgrow(splitPane, Priority.ALWAYS);

        Scene scene;
        scene = new Scene(rootView);
        scene.addEventFilter(KeyEvent.KEY_PRESSED, new EventHandler<KeyEvent>() {
            @Override
            public void handle(KeyEvent t) {
                if (KeyCode.W.equals(t.getCode()) && t.isShortcutDown()) {
                    Tab selectedItem = tabPane.getSelectionModel().getSelectedItem();
                    if(selectedItem != null && selectedItem.isClosable()) {
                        tabPane.getTabs().remove(selectedItem);
                        t.consume();
                    }
                }
            }
        });

        // the window size needs to be set before the scene is added or else it
        // counts as a window resize and that also resizes the split panes
        final double windowWidth = PrefBind.getOrDefaultDouble("windowWidth", 800D);
        primaryStage.setWidth(windowWidth);
        primaryStage.widthProperty().addListener((o, ov, nv) -> {
            PrefBind.set("windowWidth", nv);
        });
        final double windowHeight = PrefBind.getOrDefaultDouble("windowHeight", 600D);
        primaryStage.setHeight(windowHeight);
        primaryStage.heightProperty().addListener((o, ov, nv) -> {
            PrefBind.set("windowHeight", nv);
        });


        applyCssToWIndow(scene);
        primaryStage.setScene(scene);
        primaryStage.titleProperty().bind(NODE_API.summaryProperty());
        primaryStage.setResizable(true);
        primaryStage.show();

        displayConnectionPopup();

        FilterFocusManager.init(scene);

        primaryStage.setOnCloseRequest((windowEvent) -> {
            ErlyBerly.runIOAndWait(() -> {
                try {
                    nodeAPI().manuallyDisconnect();
                }
                catch(Exception e) {
                    System.out.println(e);
                }
            });
            Platform.exit();
            System.exit(0);
        });
        // run this later because it requires the control's scene to be set, which
        // may not have happened yet.
        Platform.runLater(() -> {
            sizeSplitPanes(splitPane);
            dbgView.sizeSplitPanes();
        });
    }

    private static TermFormatter formatterFromConfig() {
        String formattingPref = PrefBind.getOrDefault("termFormatting", "erlang").toString();
        if("erlang".equals(formattingPref))
            return new ErlangFormatter();
        else if("elixir".equals(formattingPref))
            return new ElixirFormatter();
        else if("lfe".equals(formattingPref))
            return new LFEFormatter();
        else
            throw new RuntimeException("Invalid configuration for property 'termFormatting' it must be 'erlang' or 'lfe' but was " + formattingPref);
    }

    public static void applyCssToWIndow(Scene scene) {
        scene.getStylesheets().add(ErlyBerly.class.getResource("/floatyfield/floaty-field.css").toExternalForm());
        scene.getStylesheets().add(ErlyBerly.class.getResource("/erlyberly/erlyberly.css").toString());
    }

    private void setupProcPaneHiding(FxmlLoadable topBarFxml, FxmlLoadable dbgFxml) {

        TopBarView topView;
        DbgView dbgView;

        topView = (TopBarView) topBarFxml.controller;
        dbgView = (DbgView) dbgFxml.controller;

        topView.hideProcsProperty().addListener((ObservableValue<? extends Boolean> o, Boolean ob, Boolean nb) -> {
            if(!nb) {
                showProcsPane();
            }
            else {
                hideProcsPane();
            }
        });

        topView.hideFunctionsProperty().addListener((ObservableValue<? extends Boolean> o, Boolean ob, Boolean nb) -> {
            dbgView.setFunctionsVisibility(nb);
        });

        boolean hideProcs = PrefBind.getOrDefaultBoolean("hideProcesses", false);
        if(hideProcs){
            hideProcsPane();
        }
        boolean hideMods = PrefBind.getOrDefaultBoolean("hideModules", false);
        if(hideMods){
            dbgView.setFunctionsVisibility(true);
        }

        topView.setOnRefreshModules(dbgView::onRefreshModules);

        Platform.runLater(() -> { topView.addAccelerators(); });
    }

    private void showProcsPane(){
        splitPane.getItems().add(0, entopPane);

        Divider div = splitPane.getDividers().get(0);
        div.setPosition(entopDivPosition);
    }

    private void hideProcsPane(){
        Divider div = splitPane.getDividers().get(0);

        entopDivPosition = div.getPosition();

        div.setPosition(0d);
        splitPane.getItems().remove(0);
    }

    private Parent loadEntopPane() {
        Parent entopPane = new FxmlLoadable("/erlyberly/entop.fxml").load();
        SplitPane.setResizableWithParent(entopPane, Boolean.FALSE);

        return entopPane;
    }

    public static void displayConnectionPopup() {
        Scene scene = new Scene(new ConnectionView());

        // close the app when escape is pressed on the connection window
        scene.setOnKeyPressed((e) -> {
            if(e.getCode() == KeyCode.ESCAPE) {
               Stage aStage = (Stage) scene.getWindow();
               aStage.close();
               primaryStage.close();
            }
        });
        applyCssToWIndow(scene);

        Stage connectStage;
        connectStage = new Stage();
        connectStage.initModality(Modality.WINDOW_MODAL);
        connectStage.setScene(scene);
        connectStage.setAlwaysOnTop(true);

        // javafx vertical resizing is laughably ugly, lets just disallow it
        //connectStage.setResizable(false);
        connectStage.setWidth(800d);
        connectStage.setHeight(400d);
        connectStage.setTitle("Connect to Remote Node");
        // if the user closes the window without connecting then close the app
        connectStage.setOnCloseRequest((e) -> {
            if(!NODE_API.connectedProperty().get()) {
                Platform.exit();
            }
            Platform.runLater(() -> { primaryStage.setResizable(true); });
        });
        connectStage.show();
    }

    public static NodeAPI nodeAPI() {
        return NODE_API;
    }

    /**
     * Show a new control in the tab pane. The tab is closable.
     */
    public static void showPane(String title, Pane parentControl) {
        assert Platform.isFxApplicationThread();
        Tab newTab;
        newTab = new Tab(title);
        newTab.setContent(parentControl);
        addAndSelectTab(newTab);
    }

    private static void addAndSelectTab(Tab newTab) {
        tabPane.getTabs().add(newTab);
        tabPane.getSelectionModel().select(newTab);
    }


    public static void showPreferencesPane() {
        if(prefstab == null) {
            FxmlLoadable fxmlLoadable = new FxmlLoadable("/erlyberly/preferences.fxml");
            Parent parent = fxmlLoadable.load();
            Pane tabPane = ErlyBerly.wrapInPane(parent);
            prefstab = new Tab("Preferences");
            prefstab.setContent(tabPane);
        }

        if(tabPane.getTabs().contains(prefstab)) {
            tabPane.getSelectionModel().select(prefstab);
        }
        else {
            addAndSelectTab(prefstab);
        }
    }

    /**
     * All I know is pane.
     */
    public static Pane wrapInPane(Node node) {
        if(node instanceof Pane)
            return (Pane) node;
        VBox.setVgrow(node, Priority.ALWAYS);
        VBox vBox = new VBox(node);
        return vBox;
    }

    public static TermFormatter getTermFormatter() {
        return termFormatter;
    }

    public static void setTermFormatter(TermFormatter aTermFormatter) {
        termFormatter = aTermFormatter;
    }

    public void sizeSplitPanes(SplitPane splitpane) {
        assert splitpane.getScene() != null;
        assert splitpane.getScene().getWidth() > 0.0d;
        try {
            double configuredProcessesWidth = configuredProcessesWidth();
            double sceneWidth = splitpane.getScene().getWidth();
            double percent = (configuredProcessesWidth / sceneWidth);
            // the split pane divider position can only be set as a percentage of the split pane
            splitpane.setDividerPosition(0, percent);
            splitpane.setDividerPosition(1, 1D - percent);
        }
        catch (NumberFormatException e) {
            e.printStackTrace();
        }
        // whenever the width of the pane changes, write it to configuration
        // this is buffered so rapid writes do not cause rapid writes to disk
        entopPane.widthProperty().addListener((o, ov, nv) -> {
            PrefBind.set("processesWidth", nv);
        });
    }

    private double configuredProcessesWidth() {
        double w = PrefBind.getOrDefaultDouble("processesWidth", 300D);
        return w;
    }

    private static void startEpmd() {
        try {
            Process epmd = Runtime.getRuntime().exec("epmd -daemon");
            int exitV = epmd.waitFor();
            if (exitV != 0) {
                System.err.println(
                        "Epmd process finished with exit value: " + exitV);
            }
        } catch (Exception e) {
            System.err.println("Failed to start epmd: " + e);
        }
    }
}
