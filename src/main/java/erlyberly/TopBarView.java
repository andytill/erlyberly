package erlyberly;

import com.ericsson.otp.erlang.OtpErlangException;
import java.awt.Desktop;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.ResourceBundle;

import com.ericsson.otp.erlang.OtpErlangObject;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;
import erlyberly.node.AppProcs;
import erlyberly.node.OtpUtil;
import floatyfield.FloatyFieldView;
import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.BooleanProperty;
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
import javafx.scene.control.Separator;
import javafx.scene.control.SeparatorMenuItem;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

import java.io.IOException;
import javafx.stage.Modality;
import javafx.stage.WindowEvent;

public class TopBarView implements Initializable {
	private static final KeyCodeCombination TOGGLE_HIDE_PROCESSES_SHORTCUT = new KeyCodeCombination(KeyCode.P, KeyCombination.SHORTCUT_DOWN);

	private static final KeyCodeCombination TOGGLE_HIDE_MODULES_SHORTCUT = new KeyCodeCombination(KeyCode.M, KeyCombination.SHORTCUT_DOWN);
	
	private static final KeyCodeCombination REFRESH_MODULES_SHORTCUT = new KeyCodeCombination(KeyCode.R, KeyCombination.SHORTCUT_DOWN);
	
	private final SimpleIntegerProperty unreadCrashReportsProperty = new SimpleIntegerProperty(0);
	
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
	private ToolBar topBox;
    
	private EventHandler<ActionEvent> refreshModulesAction;
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
	    topBox.getItems().add(crashReportsButton);
        
		// TODO: Should we hide these buttons, when disconnected ?
		hideProcessesButton.setGraphic(Icon.create().icon(AwesomeIcon.RANDOM));
		hideProcessesButton.setContentDisplay(ContentDisplay.TOP);
		hideProcessesButton.setGraphicTextGap(0d);
		hideProcessesButton.setTooltip(new Tooltip("Show/Hide the Processes (ctrl+p)"));

		// TODO: Should we hide these buttons, when disconnected ?
		hideFunctionsButton.setGraphic(Icon.create().icon(AwesomeIcon.CUBE));
		hideFunctionsButton.setContentDisplay(ContentDisplay.TOP);
		hideFunctionsButton.setGraphicTextGap(0d);
		hideFunctionsButton.setTooltip(new Tooltip("Show/Hide the Modules (ctrl+m)"));

		refreshModulesButton.setGraphic(Icon.create().icon(AwesomeIcon.ROTATE_LEFT));
		refreshModulesButton.setContentDisplay(ContentDisplay.TOP);
		refreshModulesButton.setGraphicTextGap(0d);
		refreshModulesButton.setTooltip(new Tooltip("Refresh Modules and Functions to show new, hot-loaded code (ctrl+r)"));
		refreshModulesButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());	

		erlangMemoryButton.setGraphic(Icon.create().icon(AwesomeIcon.PIE_CHART));
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
                while(e.next()) {
                    for (OtpErlangObject obj : e.getAddedSubList()) {
                        CrashReport crashReport = new  CrashReport(obj);
                        MenuItem menuItem;
                        menuItem = new MenuItem();
                        menuItem.setGraphic(new CrashReportGraphic(crashReport));
                        menuItem.setOnAction((action) -> { 
                            unreadCrashReportsProperty.set(0);
                            showWindow("Crash Report", crashReportView(crashReport));
                        });
                        crashReportsButton.getItems().add(menuItem);
                        }
                    }
                });

		xrefAnalysisButton.setGraphic(xrefAnalysisGraphic());
		xrefAnalysisButton.setContentDisplay(ContentDisplay.TOP);
		xrefAnalysisButton.setGraphicTextGap(0d);
		xrefAnalysisButton.setTooltip(new Tooltip("Start xref analysis. This may take a while, an ok is displayed when complete."));
		xrefAnalysisButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());
		xrefAnalysisButton.setOnAction((e) -> {
		    try {
                ErlyBerly.nodeAPI().asyncEnsureXRefStarted();
            } catch (Exception e1) {
                e1.printStackTrace();
            }
        });

        Button tweetButton;
        tweetButton = new Button("Tweet");
        tweetButton.setGraphic(Icon.create().icon(AwesomeIcon.TWITTER));
        tweetButton.setContentDisplay(ContentDisplay.TOP);
        tweetButton.setGraphicTextGap(0d);
        tweetButton.setOnAction((e) -> { tweet(); });
        tweetButton.setStyle("-fx-font-size: 10; -fx-padding: 5 5 5 5;");
		
		disconnectButton.setGraphic(Icon.create().icon(AwesomeIcon.EJECT));
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
		
		prefButton.setGraphic(Icon.create().icon(AwesomeIcon.GEARS));
		prefButton.setContentDisplay(ContentDisplay.TOP);
		prefButton.setGraphicTextGap(0d);
		prefButton.setTooltip(new Tooltip("Preferences"));
		prefButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());
		prefButton.setOnAction((e) -> { showPreferences(); });
		
        hideProcsProperty().addListener((Observable o) -> { toggleHideProcs(); });
        hideFunctionsProperty().addListener((Observable o) -> { toggleHideFuncs(); });
		
        erlangMemoryButton.setOnAction((e) -> { showErlangMemory(); });
        
		FxmlLoadable loader = processCountStat();	
		
		topBox.getItems().add(new Separator(Orientation.VERTICAL));
		topBox.getItems().add(loader.fxmlNode);

        topBox.getItems().addAll(rhsSpacer(), tweetButton);
		
        // let's store the ui preferences, as the end user changes them...
		PrefBind.bind_boolean("hideProcesses", hideProcessesButton.selectedProperty());
        PrefBind.bind_boolean("hideModules", hideFunctionsButton.selectedProperty());
        
        boolean hideProcs = PrefBind.getOrDefault("hideProcesses", "false").equals("true");
        boolean hideMods = PrefBind.getOrDefault("hideModules", "false").equals("true");
        
        if(hideProcs){
            // click the hide button manually.
            hideProcessesButton.setSelected(true);
        }
        if(hideMods){
            // click the hide button manually.
            hideFunctionsButton.setSelected(true);
        }
        
        toggleHideProcs();
		toggleHideFuncs();
        
		ErlyBerly.nodeAPI()
		    .getCrashReports()
		    .addListener(this::traceLogsChanged);
	}

    private MenuItem removeCrashReportsMenuItem() {
        MenuItem menuItem;
        menuItem = new MenuItem("Remove All Reports");
        menuItem.setOnAction((e) -> {
            ObservableList<MenuItem> items = crashReportsButton.getItems();
            if(items.size() == 2)
                return;
            // the first two items are this menu item and a separator, delete
            // everything after that
            items.remove(2, items.size());
            unreadCrashReportsProperty.set(0);
        });
        return menuItem;
    }

    /**
     * Create a node that just takes up space, so everything after
     * is laid out on the right hand side.
     */
    private HBox rhsSpacer() {
        HBox spacer = new HBox();
        HBox.setHgrow(spacer, Priority.ALWAYS);
        return spacer;
    }

    private void tweet() {
        String message = 
            "Using erlyberly to debug my node @erlyberlytips";
        URI uri;
        try {
            uri = new URI("https://twitter.com/intent/tweet?text=" + URLEncoder.encode(message, "utf-8"));
            Desktop.getDesktop().browse(uri);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void traceLogsChanged(ListChangeListener.Change<? extends OtpErlangObject> e) {
        while(e.next()) {
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
        Icon icon;
        
        icon = Icon.create().icon(AwesomeIcon.WARNING);
        icon.setPadding(new Insets(0, 5, 0, 5));
        
        Label reportCountLabel;
        
        reportCountLabel = new Label("122");
        reportCountLabel.setStyle("-fx-background-color:red; -fx-font-size:9; -fx-padding: 0 2 0 2; -fx-opacity:0.7");
        reportCountLabel.setTextFill(Color.WHITE);
        
        reportCountLabel.setText(unreadCrashReportsProperty.getValue().toString());
        unreadCrashReportsProperty.addListener((o, oldv, newv) -> { reportCountLabel.setText(newv.toString()); });
        reportCountLabel.visibleProperty().bind(unreadCrashReportsProperty.greaterThan(0));
        
        StackPane stackPane = new StackPane(icon, reportCountLabel);
        StackPane.setAlignment(reportCountLabel, Pos.TOP_RIGHT);
        return stackPane;
    }
    
    private Parent xrefAnalysisGraphic() {
        Icon icon;
        
        icon = Icon.create().icon(AwesomeIcon.TH_LARGE);
        icon.setPadding(new Insets(0, 5, 0, 5));
        
        Label reportCountLabel;
        
        reportCountLabel = new Label("ok");
        reportCountLabel.setStyle("-fx-background-color:green; -fx-font-size:9; -fx-padding: 0 2 0 2; -fx-opacity:0.9");
        reportCountLabel.setTextFill(Color.WHITE);
        
        reportCountLabel.visibleProperty().bind(ErlyBerly.nodeAPI().xrefStartedProperty());
        
        StackPane stackPane = new StackPane(icon, reportCountLabel);
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
        
		showWindow(title, pieChart);
	}

    private void showWindow(String title, Parent pieChart) {
        Stage stage = new Stage();
		Scene scene = new Scene(pieChart);
    
		CloseWindowOnEscape.apply(scene, stage);
		
		stage.setScene(scene);
        stage.setWidth(800);
        stage.setHeight(600);
        stage.setTitle(title);
        
        stage.show();
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
		Platform.runLater(() -> {
			accelerators().put(REFRESH_MODULES_SHORTCUT, () -> { 
				if(refreshModulesAction != null)
					refreshModulesAction.handle(null);
			});
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
	
    private void toggleHideProcs() {
		String buttonText = "";
		
		if(hideProcessesButton.isSelected())
			buttonText = "Show Processes";
		else
			buttonText = "Hide Processes";
		
		hideProcessesButton.setText(buttonText);
	}
    
	private void toggleHideProcs(Boolean preferenceBool) {
        String buttonText;
		if(preferenceBool){
            buttonText = "Hide Processes";
        }else{
            buttonText = "Show Processes";
        }
        hideProcessesButton.setText(buttonText);
	}
	
	private void toggleHideFuncs() {
		String buttonText = "";
		
		if(hideFunctionsButton.isSelected())
			buttonText = "Show Modules";
		else
			buttonText = "Hide Modules";
		
		hideFunctionsButton.setText(buttonText);
	}

	public final void setOnRefreshModules(EventHandler<ActionEvent> e) {
		refreshModulesAction = e;
		
		refreshModulesButton.setOnAction(refreshModulesAction);
	}
	
	public void disconnect() throws IOException, OtpErlangException{
		ErlyBerly.nodeAPI().manually_disconnect();
		ErlyBerly.nodeAPI().disconnect();
		Stage s = new Stage();
		displayConnectionPopup(s);
	}
	
	public void showPreferences(){
		Stage s = new Stage();
		displayPreferencesPopup(s);
	}
	
	// TODO: (improve) lazy copy paste 
	// TODO: THIS was a ugly copy paste effort
	private void displayConnectionPopup(Stage primaryStage) {
		Stage connectStage;
        
		connectStage = new Stage();
        connectStage.initModality(Modality.WINDOW_MODAL);
        connectStage.setScene(new Scene(new FxmlLoadable("/erlyberly/connection.fxml").load()));
        connectStage.setAlwaysOnTop(true);
        
        // javafx vertical resizing is laughably ugly, lets just disallow it
        connectStage.setResizable(false);
        connectStage.setWidth(400);
        
        // if the user closes the window without connecting then close the app
        connectStage.setOnCloseRequest(new EventHandler<WindowEvent>() {
			@Override
			public void handle(WindowEvent e) {
				if(!ErlyBerly.nodeAPI().connectedProperty().get()) {
					Platform.exit();
				}
				
				Platform.runLater(() -> { 
					//primaryStage.setResizable(true);
				});
			}});

        connectStage.show();
	}
	
	// TODO: (improve) lazy copy paste 
	// TODO: THIS was a ugly copy paste effort
	private void displayPreferencesPopup(Stage stage){
		Stage prefStage;
        
		prefStage = new Stage();
        prefStage.initModality(Modality.WINDOW_MODAL);
        prefStage.setScene(new Scene(new FxmlLoadable("/erlyberly/preferences.fxml").load()));
        prefStage.setAlwaysOnTop(true);
        
        // javafx vertical resizing is laughably ugly, lets just disallow it
        prefStage.setResizable(false);
        prefStage.setWidth(400);
        
        // if the user closes the window without connecting then close the app
        prefStage.setOnCloseRequest(new EventHandler<WindowEvent>() {
			@Override
			public void handle(WindowEvent e) {
				if(!ErlyBerly.nodeAPI().connectedProperty().get()) {
					// save the preferences
				}
				
				Platform.runLater(() -> { 
					//primaryStage.setResizable(true);
				});
			}});

        prefStage.show();
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
				final HashMap<Object, Object> erlangMemory = ErlyBerly.nodeAPI().erlangMemory();
				
				// remove stats which are combinations of other stats
				erlangMemory.remove(OtpUtil.atom("maximum"));
				erlangMemory.remove(OtpUtil.atom("total"));
				erlangMemory.remove(OtpUtil.atom("system"));
				erlangMemory.remove(OtpUtil.atom("processes_used"));
                erlangMemory.remove(OtpUtil.atom("atom_used"));
				
				Platform.runLater(() -> {					
					populatePieData(erlangMemory);
				});
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}

		private void populatePieData(final HashMap<Object, Object> erlangMemory) {
			for (Entry<Object, Object> entry : erlangMemory.entrySet()) {
				long kb = (long) (Double.parseDouble(entry.getValue().toString()) / 1024);
				String label = entry.getKey().toString() + " (" + kb + " KB)";
				pieData.add(new Data(label, kb));
			}
		}
	}
}
