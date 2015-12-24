package erlyberly;

import java.awt.Desktop;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.ResourceBundle;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;
import erlyberly.node.AppProcs;
import erlyberly.node.OtpUtil;
import floatyfield.FloatyFieldView;
import javafx.application.Platform;
import javafx.beans.Observable;
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
import javafx.scene.control.ListView;
import javafx.scene.control.Separator;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

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
    @FXML
    private Button crashReportsButton;
    @FXML
    private Button xrefAnalysisButton;
  	@FXML
	private ToolBar topBox;
    
	private EventHandler<ActionEvent> refreshModulesAction;
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
		hideProcessesButton.setGraphic(Icon.create().icon(AwesomeIcon.RANDOM));
		hideProcessesButton.setContentDisplay(ContentDisplay.TOP);
		hideProcessesButton.setGraphicTextGap(0d);
		hideProcessesButton.setTooltip(new Tooltip("Show/Hide the Processes (ctrl+p)"));

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

		crashReportsButton.setGraphic(crashReportsGraphic());
		crashReportsButton.setContentDisplay(ContentDisplay.TOP);
		crashReportsButton.setGraphicTextGap(0d);
		crashReportsButton.setTooltip(new Tooltip("View crash reports received from the connected node."));
		crashReportsButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());
		crashReportsButton.setOnAction((e) -> { showCrashReportWindow(); });

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

        hideProcsProperty().addListener((Observable o) -> { toggleHideProcsText(); });
        hideFunctionsProperty().addListener((Observable o) -> { toggleHideFuncsText(); });
        erlangMemoryButton.setOnAction((e) -> { showErlangMemory(); });
        
		FxmlLoadable loader = processCountStat();	
		
		topBox.getItems().add(new Separator(Orientation.VERTICAL));
		topBox.getItems().add(loader.fxmlNode);

        topBox.getItems().addAll(rhsSpacer(), tweetButton);
		
		toggleHideProcsText();
		toggleHideFuncsText();
		
		ErlyBerly.nodeAPI()
		    .getCrashReports()
		    .addListener(this::traceLogsChanged);
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
    
    private void showCrashReportWindow() {
        unreadCrashReportsProperty.set(0);
        
        ListView<OtpErlangObject> crashReportListView;
        
        crashReportListView = new ListView<OtpErlangObject>(ErlyBerly.nodeAPI().getCrashReports());
        crashReportListView.setTooltip(new Tooltip("Double click on the crash report to see it in more detail."));
        crashReportListView.setOnMouseClicked((me) -> {
            if(me.getButton().equals(MouseButton.PRIMARY) && me.getClickCount() == 2) {
                OtpErlangObject obj = crashReportListView.getSelectionModel().getSelectedItem();
                
                if(obj != null && obj != null) {
                    showWindow("Crash Report", crashReportView(obj));
                }
            }
        });
        showWindow("Crash Reports", crashReportListView);
    }


    private CrashReportView crashReportView(OtpErlangObject obj) {
        CrashReportView crashReportView;
        crashReportView = new CrashReportView();
        crashReportView.setCrashReport((OtpErlangList) obj);
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
		refreshModulesAction = e;
		
		refreshModulesButton.setOnAction(refreshModulesAction);
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
