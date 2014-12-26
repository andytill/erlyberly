package erlyberly;

import java.net.URL;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.ResourceBundle;

import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.SplitPane;
import javafx.scene.control.SplitPane.Divider;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.util.Callback;
import ui.FXTreeCell;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;
import floatyfield.FloatyFieldView;


public class DbgView implements Initializable {

	private static final String ICON_STYLE = "-fx-font-family: FontAwesome; -fx-font-size: 1em;";
	
	private final DbgController dbgController = new DbgController();
	
	private final ObservableList<TreeItem<ModFunc>> treeModules = FXCollections.observableArrayList();
	
	private final SortedList<TreeItem<ModFunc>> sortedTreeModules = new SortedList<TreeItem<ModFunc>>(treeModules);
	
	private final FilteredList<TreeItem<ModFunc>> filteredTreeModules = new FilteredList<TreeItem<ModFunc>>(sortedTreeModules);
	
	private final ObservableList<TraceLog> traceLogs = FXCollections.observableArrayList();
	
	private final SortedList<TraceLog> sortedTtraces = new SortedList<TraceLog>(traceLogs);
	
	private final FilteredList<TraceLog> filteredTraces = new FilteredList<TraceLog>(sortedTtraces);
	
	/**
	 * A list of all the filtered lists for functions, so a predicate can be set on them.  Binding
	 * the predicate property does not seem to work.
	 */
	private final ObservableList<FilteredList<TreeItem<ModFunc>>> functionLists = FXCollections.observableArrayList();
	
	private final ObservableList<ModFunc> traces = FXCollections.observableArrayList();
	
	@FXML
	private TreeView<ModFunc> modulesTree;
	@FXML
	private ListView<TraceLog> tracesBox;
	@FXML
	private VBox modulesBox;
	@FXML
	private TextField traceSearchField;
	@FXML
	private TextField filterTracesField;
	@FXML
	private Label noTracesLabel;
	@FXML
	private SplitPane dbgSplitPane;

	private double functionsDivPosition;
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
		sortedTreeModules.setComparator(treeItemModFuncComparator());
		
		SplitPane.setResizableWithParent(modulesBox, Boolean.FALSE);
		
		traceSearchField.textProperty().addListener((o) -> { onTraceFilterChange(); });
		filterTracesField.textProperty().addListener((o) -> { onTraceFilterChange(); });
		
		ErlyBerly.nodeAPI().connectedProperty().addListener(this::onConnected);
		
		dbgController.getTraceLogs().addListener(this::traceLogsChanged);
		tracesBox.setOnMouseClicked(this::onTraceClicked);
		tracesBox.setCellFactory(new TraceLogListCellFactory());
		
		modulesTree.setCellFactory((tree) -> {
			ModFuncGraphic mfg = new ModFuncGraphic(this::toggleTraceModFunc, traces::contains);
			traces.addListener((Observable o) -> { 
				mfg.onTracesChange(); 
			});
			
			return new FXTreeCell<ModFunc>(mfg, mfg);
		});
		Bindings.bindContentBidirectional(tracesBox.getItems(), filteredTraces);
		
		addFloatySearchControl();
		
		dbgController.initialize(url, r);
	}

	private FxmlLoadable addFloatySearchControl() {
		FxmlLoadable loader = new FxmlLoadable("/floatyfield/floaty-field.fxml");
		
		loader.load();

		FloatyFieldView ffView;
		
		ffView = (FloatyFieldView) loader.controller;
		ffView.promptTextProperty().set("Search for functions i.e. gen_s:call");
		
		loader.fxmlNode.setStyle("-fx-padding: 5 5 0 5;");
		
		HBox.setHgrow(loader.fxmlNode, Priority.ALWAYS);
		modulesBox.getChildren().add(0, loader.fxmlNode);
		
		ffView.textProperty().addListener(this::onFunctionSearchChange);
		
		return loader;
	}
	

	public void traceLogsChanged(ListChangeListener.Change<? extends TraceLog> e) {
		while(e.next()) {
			for (TraceLog trace : e.getAddedSubList()) {
				traceLogs.add(0, trace);
			}
		}
	}

	public void onRefreshModules(ActionEvent e) {
		treeModules.clear();
		
		refreshModules();
	}

	@FXML
	private void onTraceLogClear() {
		traceLogs.clear();
		tracesBox.getItems().clear();
	}
	
	private void onTraceFilterChange() {
		filteredTraces.setPredicate((t) -> {
			String filterText = filterTracesField.getText();
			String logText = t.toString();
			if(!filterText.isEmpty() && logText.contains(filterTracesField.getText())) {
				return false;
			}
			return logText.contains(traceSearchField.getText()); });
	}
	
	public void onFunctionSearchChange(Observable o, String oldValue, String search) {
		String[] split = search.split(":");
		
		if(split.length == 0)
			return;
		
		final String moduleName = split[0];
		final String funcName = (split.length > 1) ? split[1] : ""; 
		
		if(search.contains(":")) {
			for (TreeItem<ModFunc> treeItem : filteredTreeModules) {
				treeItem.setExpanded(true);
			}
		}
		
		filteredTreeModules.setPredicate((t) -> { return isMatchingModFunc(moduleName, t); });
		
	    for (FilteredList<TreeItem<ModFunc>> funcItemList : functionLists) {
			funcItemList.setPredicate((t) -> { return isMatchingModFunc(funcName, t); });
		}
	}

	private void onTraceClicked(MouseEvent me) {
		if(me.getButton().equals(MouseButton.PRIMARY)){
            if(me.getClickCount() == 2) {
            	TraceLog selectedItem = tracesBox.getSelectionModel().getSelectedItem();
            	
            	if(selectedItem != null) {
                	showTraceTermView(selectedItem); 
            	}
        	}
        }
	}

	private Comparator<TreeItem<ModFunc>> treeItemModFuncComparator() {
		return new Comparator<TreeItem<ModFunc>>() {
			@Override
			public int compare(TreeItem<ModFunc> o1, TreeItem<ModFunc> o2) {
				return o1.getValue().compareTo(o2.getValue());
			}};
	}


	private void showTraceTermView(final TraceLog traceLog) {
		OtpErlangObject args = traceLog.getArgs(); 
		OtpErlangObject result = traceLog.getResult();
		
		Stage termsStage = new Stage();
		
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
		
		Scene scene  = new Scene(splitPane);
		
		CloseWindowOnEscape.apply(scene, termsStage);
		
		StringBuilder sb = new StringBuilder(traceLog.getPidString());
		sb.append(" ");
		traceLog.appendFunctionToString(sb);
		
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
	
	private Node labelledTreeView(String label, TermTreeView node) {		
		return new VBox(new Label(label), node);
	}

	private boolean isMatchingModFunc(String searchText, TreeItem<ModFunc> t) {
		if(searchText.isEmpty())
			return true;
		return t.getValue().toString().contains(searchText);
	}
	
	private void toggleTraceModFunc(ModFunc function) {
		if(traces.contains(function))
			onRemoveTracer(null, function);
		else
			traceModFunc(function);
	}

	private void traceModFunc(ModFunc function) {
		try {
			ErlyBerly.nodeAPI().startTrace(function);
			
			traces.add(function);
			
			dbgController.setCollectingTraces(true);
			
			Button newRemoveTraceButton;
			newRemoveTraceButton = newRemoveTraceButton(function);
			newRemoveTraceButton
					.setOnAction((e) -> onRemoveTracer(e, function));
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	private void onRemoveTracer(ActionEvent e, ModFunc function) {
		try {
			ErlyBerly.nodeAPI().stopTrace(function);

			traces.remove(function);
		} 
		catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	private Button newRemoveTraceButton(ModFunc value) {
		String graphicCSS = "-fx-font-family: FontAwesome; -fx-font-size: 1.6em; -fx-padding: 0 0 2 0;";
		
		Button removeTraceButton;
		
		removeTraceButton = new Button(value.toFullString());
		removeTraceButton.setMnemonicParsing(false);
		removeTraceButton.setContentDisplay(ContentDisplay.RIGHT);
		removeTraceButton.setGraphic(Icon.create().icon(AwesomeIcon.CLOSE).style(graphicCSS));
		
		return removeTraceButton;
	}

	private void onConnected(Observable o) {
		if(!ErlyBerly.nodeAPI().connectedProperty().get()) {
			treeModules.clear();
		}
		else {
			refreshModules();
		}
		
		for (ModFunc function : traces) {
			try {
				ErlyBerly.nodeAPI().startTrace(function);
			} catch (Exception e) {
				e.printStackTrace();
				
				traces.remove(function);
			}
		}
	}

	private void refreshModules() {
		try {
			modulesTree.setShowRoot(false);
			modulesTree.setRoot(buildObjectTreeRoot());
		} 
		catch (Exception e) {
			throw new RuntimeException("failed to build module/function tree", e);
		}
	}
	
	private TreeItem<ModFunc> buildObjectTreeRoot() throws Exception {		
		TreeItem<ModFunc> root;
		
		root = new TreeItem<ModFunc>();
		root.setExpanded(true);
		OtpErlangList requestFunctions = ErlyBerly.nodeAPI().requestFunctions();

		boolean isExported;
		
		for (OtpErlangObject e : requestFunctions) {
			OtpErlangTuple tuple = (OtpErlangTuple) e;
			
			OtpErlangAtom moduleNameAtom = (OtpErlangAtom) tuple.elementAt(0);
			OtpErlangList exportedFuncs = (OtpErlangList) tuple.elementAt(1);
			OtpErlangList localFuncs = (OtpErlangList) tuple.elementAt(2);
			
			TreeItem<ModFunc> moduleItem;
			
			moduleItem = new TreeItem<ModFunc>(ModFunc.toModule(moduleNameAtom));
			moduleItem.setGraphic(treeIcon(AwesomeIcon.CUBE));
			
			ObservableList<TreeItem<ModFunc>> modFuncs = FXCollections.observableArrayList();
			
			SortedList<TreeItem<ModFunc>> sortedFuncs = new SortedList<TreeItem<ModFunc>>(modFuncs);
			
			FilteredList<TreeItem<ModFunc>> filteredFuncs = new FilteredList<TreeItem<ModFunc>>(sortedFuncs);

			sortedFuncs.setComparator(treeItemModFuncComparator());
			
			isExported = true;			
			addTreeItems(toModFuncs(moduleNameAtom, exportedFuncs, isExported), modFuncs);

			isExported = false;
			addTreeItems(toModFuncs(moduleNameAtom, localFuncs, isExported), modFuncs);
			functionLists.add(filteredFuncs);
			
			Bindings.bindContentBidirectional(moduleItem.getChildren(), filteredFuncs);
			
			treeModules.add(moduleItem);
		}
		Bindings.bindContentBidirectional(root.getChildren(), filteredTreeModules);
		
		return root;
	}
	
	private void addTreeItems(List<ModFunc> modFuncs, ObservableList<TreeItem<ModFunc>> modFuncTreeItems) {
		for (ModFunc modFunc : modFuncs) {
			if(!modFunc.isSynthetic()) {
				TreeItem<ModFunc> item = newFuncTreeItem(modFunc);
				
				modFuncTreeItems.add(item);
			}
		}
	}

	private TreeItem<ModFunc> newFuncTreeItem(ModFunc modFunc) {
		return new TreeItem<ModFunc>(modFunc);
	}


	private Icon treeIcon(AwesomeIcon treeIcon) {
		return Icon.create().icon(treeIcon).style(ICON_STYLE);
	}

	private ArrayList<ModFunc> toModFuncs(OtpErlangAtom moduleNameAtom, OtpErlangList exportedFuncs, boolean isExported) throws OtpErlangRangeException {
		ArrayList<ModFunc> mfs = new ArrayList<>();
		for (OtpErlangObject exported : exportedFuncs) {
			ModFunc modFunc = ModFunc.toFunc(moduleNameAtom, exported, isExported);
			mfs.add(modFunc);
		}
		return mfs;
	}
	
	private final class TraceLogListCellFactory implements Callback<ListView<TraceLog>, ListCell<TraceLog>> {
		@Override
		public ListCell<TraceLog> call(ListView<TraceLog> view) {
			return new TraceLogListCell();
		}
	}
	
	private final class TraceLogListCell extends ListCell<TraceLog> {
		private ChangeListener<? super Boolean> listener;
		
		public TraceLogListCell() {
			listener = (o, noldV, newV) -> {
				TraceLog item = TraceLogListCell.this.getItem();
				
				removeCompletionListender(item);

				getStyleClass().remove("not-completed");
				
				putCompletionStyle(item);
			};
		}

		private void removeCompletionListender(TraceLog item) {
			if(item != null) {
				item.isCompleteProperty().removeListener(listener);
			}
		}
		
		@Override
		protected void updateItem(TraceLog item, boolean empty) {

			// clean up
			getStyleClass().remove("exception");
			getStyleClass().remove("not-completed");
			
			textProperty().unbind();
			
			removeCompletionListender(getItem());
			
			// setup
			super.updateItem(item, empty);
		    
			// set new UI for item
			if (item == null || empty) {
		        setText(null);
		    }
			else {
				textProperty().bind(item.summaryProperty());
				
				putCompletionStyle(item);
				
				if(!item.isComplete())
					item.isCompleteProperty().addListener(listener);
			}
		}

		private void putCompletionStyle(TraceLog item) {
			if(item.isExceptionThrower())
				getStyleClass().add("exception");
			else if(!item.isComplete())
				getStyleClass().add("not-completed");
		}
	}

	public void setFunctionsVisibility(Boolean hidden) {
		if(!hidden) {
			dbgSplitPane.getItems().add(0, modulesBox);
			
			Divider div = dbgSplitPane.getDividers().get(0);
			div.setPosition(functionsDivPosition);
		}
		else {
			Divider div = dbgSplitPane.getDividers().get(0);

			functionsDivPosition = div.getPosition();
			
			div.setPosition(0d);
			dbgSplitPane.getItems().remove(0);
		}
	}
}
