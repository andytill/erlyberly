package erlyberly;

import javafx.beans.binding.Bindings;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.OverrunStyle;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.Separator;
import javafx.scene.control.SplitPane;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;
import floatyfield.FloatyFieldView;


public class DbgTraceView extends VBox {
	
	private final ObservableList<TraceLog> traceLogs = FXCollections.observableArrayList();
	
	private final SortedList<TraceLog> sortedTtraces = new SortedList<TraceLog>(traceLogs);
	
	private final FilteredList<TraceLog> filteredTraces = new FilteredList<TraceLog>(sortedTtraces);

	private ListView<TraceLog> tracesBox;
	
    /**
     * Set insertTracesAtTop=true in the .erlyberly file in your home directory to
     * make traces be inserted at the top of the list.
     */
	private boolean insertTracesAtTop;
	
	public DbgTraceView(DbgController dbgController) {
		setSpacing(5d);
		setStyle("-fx-background-insets: 5;");
		insertTracesAtTop = PrefBind.getOrDefault("insertTracesAtTop", "false").equals("true");
	    
		dbgController.getTraceLogs().addListener(this::traceLogsChanged);
		
		tracesBox = new ListView<TraceLog>();
		tracesBox.setOnMouseClicked(this::onTraceClicked);
		tracesBox.setCellFactory(new TraceLogListCellFactory());
		Bindings.bindContentBidirectional(tracesBox.getItems(), filteredTraces);
		HBox.setHgrow(tracesBox, Priority.ALWAYS);
		
		putTraceContextMenu();
		
		Button clearTraceLogsButton;
		clearTraceLogsButton = new Button("Clear");
		clearTraceLogsButton.setTextOverrun(OverrunStyle.CLIP);
		clearTraceLogsButton.setGraphic(Icon.create().icon(AwesomeIcon.FILE_ALT));
		clearTraceLogsButton.setGraphicTextGap(8d);
		clearTraceLogsButton.setOnAction((e) -> { onTraceLogClear(); });
		HBox.setHgrow(clearTraceLogsButton, Priority.ALWAYS);
		
		HBox hBox;
		hBox = new HBox();
		hBox.setSpacing(5d);
		hBox.setPadding(new Insets(5, 5, 0, 5));
		addTraceLogFloatySearchControl(hBox);
		hBox.getChildren().add(clearTraceLogsButton);
		
		getChildren().addAll(hBox, tracesBox);
	}

	private void putTraceContextMenu() {
		TraceContextMenu traceContextMenu = new TraceContextMenu();
		traceContextMenu.setItems(traceLogs);
		traceContextMenu
				.setSelectedItems(tracesBox.getSelectionModel().getSelectedItems());
		
		tracesBox.setContextMenu(traceContextMenu);
		tracesBox.selectionModelProperty().get().setSelectionMode(SelectionMode.MULTIPLE);
	}

	private void onTraceLogClear() {
		traceLogs.clear();
		tracesBox.getItems().clear();
	}

	private void onTraceClicked(MouseEvent me) {
		if(me.getButton().equals(MouseButton.PRIMARY)) {
            if(me.getClickCount() == 2) {
            	TraceLog selectedItem = tracesBox.getSelectionModel().getSelectedItem();
            	
            	if(selectedItem != null) {
                	showTraceTermView(selectedItem); 
            	}
        	}
        }
	}

	private void showWindow(Parent parent, CharSequence sb) {
		Stage termsStage = new Stage();
		Scene scene  = new Scene(parent);
		
		CloseWindowOnEscape.apply(scene, termsStage);
		
		termsStage.setScene(scene);
        termsStage.setWidth(800);
        termsStage.setHeight(600);
        termsStage.setTitle(sb.toString());
        termsStage.show();
	}

	private TermTreeView newTermTreeView() {
		TermTreeView termTreeView;
		
		termTreeView = new TermTreeView();
		termTreeView.setMaxHeight(Integer.MAX_VALUE);
		VBox.setVgrow(termTreeView, Priority.ALWAYS);
		
		return termTreeView;
	}


	private void showTraceTermView(final TraceLog traceLog) {
		OtpErlangObject args = traceLog.getArgs(); 
		OtpErlangObject result = traceLog.getResult();
		
		TermTreeView resultTermsTreeView, argTermsTreeView;
		
		resultTermsTreeView = newTermTreeView();
		
		if(result != null) {
			resultTermsTreeView.populateFromTerm(traceLog.getResult()); 
		}
		else {
			WeakChangeListener<Boolean> listener = new WeakChangeListener<Boolean>((o, oldV, newV) -> {
				if(newV)
					resultTermsTreeView.populateFromTerm(traceLog.getResult()); 
			});

			traceLog.isCompleteProperty().addListener(listener);
		}
		
		argTermsTreeView = newTermTreeView();
		argTermsTreeView.populateFromListContents((OtpErlangList)args);
		
		SplitPane splitPane;
		
		splitPane = new SplitPane();
		splitPane.getItems().addAll(
			labelledTreeView("Function arguments", argTermsTreeView), 
			labelledTreeView("Result", resultTermsTreeView)
		);
		
		StringBuilder sb = new StringBuilder(traceLog.getPidString());
		sb.append(" ");
		boolean appendArity = false;
		traceLog.appendFunctionToString(sb, appendArity);
		
		showWindow(splitPane, sb);
	}
	
	private Node labelledTreeView(String label, TermTreeView node) {		
		return new VBox(new Label(label), node);
	}
	
	private void onTraceFilterChange(String searchText) {
		BasicSearch basicSearch = new BasicSearch(searchText);
		filteredTraces.setPredicate((t) -> {
			String logText = t.toString();
			return basicSearch.matches(logText); 
		});
	}

	private FxmlLoadable addTraceLogFloatySearchControl(HBox traceLogSearchBox) {
		FxmlLoadable loader = new FxmlLoadable("/floatyfield/floaty-field.fxml");
		
		loader.load();

		FloatyFieldView ffView;
		
		ffView = (FloatyFieldView) loader.controller;
		ffView.promptTextProperty().set("Search trace logs");
		
		HBox.setHgrow(loader.fxmlNode, Priority.ALWAYS);
		
		traceLogSearchBox.getChildren().add(0, new Separator(Orientation.VERTICAL));
		traceLogSearchBox.getChildren().add(0, loader.fxmlNode);

		ffView.textProperty().addListener((o, ov, nv) -> { onTraceFilterChange(nv); });
		
		return loader;
	}
	

	public void traceLogsChanged(ListChangeListener.Change<? extends TraceLog> e) {
		while(e.next()) {
			for (TraceLog trace : e.getAddedSubList()) {
				if(insertTracesAtTop)
					traceLogs.add(0, trace);
				else
					traceLogs.add(trace);
			}
		}
	}
}
