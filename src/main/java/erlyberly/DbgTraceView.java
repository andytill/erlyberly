package erlyberly;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

import floatyfield.FloatyFieldView;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.ListChangeListener;
import javafx.css.PseudoClass;
import javafx.geometry.Insets;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Control;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableRow;
import javafx.scene.control.TreeTableView;
import javafx.scene.control.cell.TreeItemPropertyValueFactory;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import ui.TreeItemSF;


public class DbgTraceView extends VBox {

	private final TreeTableView<TraceLog> tracesBox;
	
    /**
     * Set insertTracesAtTop=true in the .erlyberly file in your home directory to
     * make traces be inserted at the top of the list.
     */
	private boolean insertTracesAtTop;
	
	private TreeItemSF<TraceLog> rootItem = new TreeItemSF<>();
	
	public DbgTraceView(DbgController dbgController) {
		setSpacing(5d);
		setStyle("-fx-background-insets: 5;");
		setMaxHeight(Double.MAX_VALUE);
		
		insertTracesAtTop = PrefBind.getOrDefault("insertTracesAtTop", "false").equals("true");
		
		tracesBox = new TreeTableView<TraceLog>();
		tracesBox.setShowRoot(false);
		tracesBox.setRoot(rootItem);
		tracesBox.setOnMouseClicked(this::onTraceClicked);
		tracesBox.setMaxHeight(Double.MAX_VALUE);
		VBox.setVgrow(tracesBox, Priority.ALWAYS);
		
		putTableColumns();

        // #47 double wrapping the filtered list in another sorted list, otherwise
        // the table cannot be sorted on columns. Binding the items will throw exceptions
        // when sorting on columns.
        // see http://code.makery.ch/blog/javafx-8-tableview-sorting-filtering/
        //SortedList<TraceLog> sortedData = new SortedList<>(filteredTraces);
        //ReadOnlyObjectProperty<Comparator<TreeItem<TraceLog>>> comparator = tracesBox.comparatorProperty();
        //sortedData.comparatorProperty().bind(comparator);

		putTraceContextMenu();

        Parent p = traceLogFloatySearchControl();

        getChildren().addAll(p, tracesBox);

		dbgController.getTraceLogs().addListener(this::traceLogsChanged);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
    private void putTableColumns() {
	    TreeTableColumn<TraceLog,Long> seqColumn;
        seqColumn = new TreeTableColumn<TraceLog,Long>("Seq.");
        seqColumn.setCellValueFactory(new TreeItemPropertyValueFactory("instanceNum"));
        
        TreeTableColumn<TraceLog,String> pidColumn;
		pidColumn = new TreeTableColumn<TraceLog,String>("Pid");
		pidColumn.setCellValueFactory(new TreeItemPropertyValueFactory("pid"));

		TreeTableColumn<TraceLog,String> regNameColumn;
		regNameColumn = new TreeTableColumn<TraceLog,String>("Reg. Name");
		regNameColumn.setCellValueFactory(new TreeItemPropertyValueFactory("regName"));

		TreeTableColumn<TraceLog,String> durationNameColumn;
		durationNameColumn = new TreeTableColumn<TraceLog,String>("Duration");
		durationNameColumn.setCellValueFactory(new TreeItemPropertyValueFactory("duration"));

		TreeTableColumn<TraceLog,String> functionnNameColumn;
		functionnNameColumn = new TreeTableColumn<TraceLog,String>("Function");
		functionnNameColumn.setCellValueFactory(new TreeItemPropertyValueFactory("function"));

		TreeTableColumn<TraceLog,String> argsColumn;
		argsColumn = new TreeTableColumn<TraceLog,String>("Args");
		argsColumn.setCellValueFactory(new TreeItemPropertyValueFactory("args"));
		
		TreeTableColumn<TraceLog,String> resultColumn;
		resultColumn = new TreeTableColumn<TraceLog,String>("Result");
		resultColumn.setCellValueFactory(new TreeItemPropertyValueFactory("result"));
		
		tracesBox.getColumns().setAll(
		        functionnNameColumn, seqColumn, pidColumn, regNameColumn, durationNameColumn, argsColumn, resultColumn
		);

		// based on http://stackoverflow.com/questions/27015961/tableview-row-style
		PseudoClass exceptionClass = PseudoClass.getPseudoClass("exception");
		PseudoClass notCompletedClass = PseudoClass.getPseudoClass("not-completed");
		tracesBox.setRowFactory(tv -> {
		    TreeTableRow<TraceLog> row = new TreeTableRow<>();
		    row.itemProperty().addListener((obs, oldTl, tl) -> {
		        if (tl != null) {
		            row.pseudoClassStateChanged(exceptionClass, tl.isExceptionThrower());
		            row.pseudoClassStateChanged(notCompletedClass, !tl.isComplete());
		        }
		        else {
		            row.pseudoClassStateChanged(exceptionClass, false);
		            row.pseudoClassStateChanged(notCompletedClass, false);
		        }
		    });
		    return row ;
		});
		
		tracesBox.setRowFactory(tv -> {  
		    TreeTableRow<TraceLog> row = new TreeTableRow<>();
		    ChangeListener<Boolean> completeListener = (obs, oldComplete, newComplete) -> {
	            row.pseudoClassStateChanged(exceptionClass, row.getItem().isExceptionThrower());
	            row.pseudoClassStateChanged(notCompletedClass, !row.getItem().isComplete());
		    };
		    row.itemProperty().addListener((obs, oldTl, tl) -> {
		    	if (oldTl != null) {
		    		oldTl.isCompleteProperty().removeListener(completeListener);
		        }
		        if (tl != null) {
		    		tl.isCompleteProperty().addListener(completeListener);
		            row.pseudoClassStateChanged(exceptionClass, tl.isExceptionThrower());
		            row.pseudoClassStateChanged(notCompletedClass, !tl.isComplete());
		        }
		        else {
		            row.pseudoClassStateChanged(exceptionClass, false);
		            row.pseudoClassStateChanged(notCompletedClass, false);
		        }
		    });
		    return row ;
		});
    }

	private void putTraceContextMenu() {
		TraceContextMenu traceContextMenu = new TraceContextMenu();
		traceContextMenu.setItems(rootItem.getInputItems());
		traceContextMenu
				.setSelectedItems(tracesBox.getSelectionModel().getSelectedItems());
		
		tracesBox.setContextMenu(traceContextMenu);
		tracesBox.selectionModelProperty().get().setSelectionMode(SelectionMode.MULTIPLE);
	}

	private void onTraceClicked(MouseEvent me) {
		if(me.getButton().equals(MouseButton.PRIMARY) && me.getClickCount() == 2) {
        	TreeItem<TraceLog> selectedItem = tracesBox.getSelectionModel().getSelectedItem();
        	
        	if(selectedItem != null && selectedItem.getValue() != null) {
            	showTraceTermView(selectedItem.getValue()); 
        	}
        }
	}

	private void showWindow(Parent parent, CharSequence sb) {
		Stage termsStage = new Stage();
		Scene scene  = new Scene(parent);
		ErlyBerly.applyCssToWIndow(scene);
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
		OtpErlangObject args = traceLog.getArgsList(); 
		OtpErlangObject result = traceLog.getResultFromMap();
		
		TermTreeView resultTermsTreeView, argTermsTreeView;
		
		resultTermsTreeView = newTermTreeView();
		
		if(result != null) {
			resultTermsTreeView.populateFromTerm(traceLog.getResultFromMap()); 
		}
		else {
			WeakChangeListener<Boolean> listener = new WeakChangeListener<Boolean>((o, oldV, newV) -> {
				if(newV)
					resultTermsTreeView.populateFromTerm(traceLog.getResultFromMap()); 
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
		//BasicSearch basicSearch = new BasicSearch(searchText);
		//filteredTraces.setPredicate((t) -> {
		//	String logText = t.toString();
		//	return basicSearch.matches(logText); 
		//});
	}

    private Region traceLogFloatySearchControl() {
		FxmlLoadable loader = new FxmlLoadable("/floatyfield/floaty-field.fxml");

		loader.load();

		FloatyFieldView ffView;

		ffView = (FloatyFieldView) loader.controller;
		ffView.promptTextProperty().set("Search trace logs");

		HBox.setHgrow(loader.fxmlNode, Priority.ALWAYS);

		ffView.textProperty().addListener((o, ov, nv) -> { onTraceFilterChange(nv); });

        Region fxmlNode = (Region) loader.fxmlNode;
        fxmlNode.setPadding(new Insets(5, 5, 0, 5));
        Platform.runLater(() -> {
            FilterFocusManager.addFilter((Control) loader.fxmlNode.getChildrenUnmodifiable().get(1), 2);
        });

        return fxmlNode;
    }

	public void traceLogsChanged(ListChangeListener.Change<? extends TreeItem<TraceLog>> e) {
		while(e.next()) {
			for (TreeItem<TraceLog> trace : e.getAddedSubList()) {
				TreeItemSF<TraceLog> traceRoot = (TreeItemSF<TraceLog>) tracesBox.getRoot();
                if(insertTracesAtTop)
					traceRoot.getInputItems().add(0, trace);
				else
				    traceRoot.getInputItems().add(trace);
			}
		}
	}
}
