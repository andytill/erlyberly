package erlyberly;

import java.net.URL;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.ResourceBundle;

import javafx.application.Platform;
import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.beans.property.BooleanProperty;
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
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.Tooltip;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.util.Callback;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;


public class DbgView implements Initializable {
	
	private static final String ICON_STYLE = "-fx-font-family: FontAwesome; -fx-font-size: 1em;";
	
	private final ObservableList<TreeItem<ModFunc>> treeModules = FXCollections.observableArrayList();
	
	private final SortedList<TreeItem<ModFunc>> sortedTreeModules = new SortedList<TreeItem<ModFunc>>(treeModules);
	
	private final FilteredList<TreeItem<ModFunc>> filteredTreeModules = new FilteredList<TreeItem<ModFunc>>(sortedTreeModules);
	
	private final ObservableList<TraceLog> traces = FXCollections.observableArrayList();
	
	private final SortedList<TraceLog> sortedTtraces = new SortedList<TraceLog>(traces);
	
	private final FilteredList<TraceLog> filteredTraces = new FilteredList<TraceLog>(sortedTtraces);
	
	/**
	 * A list of all the filtered lists for functions, so a predicate can be set on them.  Binding
	 * the predicate property does not seem to work.
	 */
	private final ObservableList<FilteredList<TreeItem<ModFunc>>> functionLists = FXCollections.observableArrayList();
	
	@FXML
	private TreeView<ModFunc> modulesTree;
	@FXML
	private Button goButton;
	@FXML
	private ListView<TraceLog> tracesBox;
	@FXML
	private TextField searchField;
	@FXML
	private VBox modulesBox;
	@FXML
	private TextField traceSearchField;
	@FXML
	private TextField filterTracesField;
	@FXML
	private FlowPane currentTraceBox;
	@FXML
	private Label noTracesLabel;
	@FXML
	private ToggleButton hideProcsButton;
	@FXML
	private Button refreshModulesButton;
	
	private final DbgController dbgController = new DbgController();
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
		currentTraceBox.getChildren().addListener(this::onRemoveTraceButtonsAdded);
		
		sortedTreeModules.setComparator(treeItemModFuncComparator());
		
		SplitPane.setResizableWithParent(modulesBox, Boolean.FALSE);
		
		traceSearchField.textProperty().addListener((o) -> { onTraceFilterChange(); });
		filterTracesField.textProperty().addListener((o) -> { onTraceFilterChange(); });
		
		searchField.setPromptText("Search for functions i.e. gen_s:call");
		searchField.textProperty().addListener(this::onFunctionSearchChange);
		
		ErlyBerly.nodeAPI().connectedProperty().addListener(this::onConnected);
		
		dbgController.getTraces().addListener(new ListChangeListener<TraceLog>() {
			@Override
			public void onChanged(ListChangeListener.Change<? extends TraceLog> e) {
				while(e.next()) {
					for (TraceLog trace : e.getAddedSubList()) {
						traces.add(0, trace);
					}
				}
			}});
		tracesBox.setOnMouseClicked(this::onTraceClicked);
		tracesBox.setCellFactory(new Callback<ListView<TraceLog>, ListCell<TraceLog>>() {
			@Override
			public ListCell<TraceLog> call(ListView<TraceLog> view) {
				return new ListCell<TraceLog>() {
					@Override
					protected void updateItem(TraceLog item, boolean empty) {
						super.updateItem(item, empty);

						getStyleClass().remove("exception");
						textProperty().unbind();
		                
						if (item == null || empty) {
			                setText(null);
			            }
						else {
							textProperty().bind(item.summaryProperty());
							
							if(item.isExceptionThrower()) {
								getStyleClass().add("exception");
							}
						}
					}
				};
			}});
		
		goButton.disableProperty().bind(modulesTree.getSelectionModel().selectedItemProperty().isNull());
		
		modulesTree.getSelectionModel().selectedItemProperty().addListener(new InvalidationListener() {
			@Override
			public void invalidated(Observable o) {
				TreeItem<ModFunc> selectedItem = modulesTree.getSelectionModel().getSelectedItem();
				
				if(selectedItem == null)
					return;
				
				ModFunc modFunc = selectedItem.getValue();
				
				if(modFunc.getFuncName() == null) {
					goButton.setText("Select a function");
				}
				else {
					goButton.setText("Trace " + modFunc.toFullString());
				}
			}});

		
		Bindings.bindContentBidirectional(tracesBox.getItems(), filteredTraces);
		
		dbgController.initialize(url, r);
		
		hideProcsButton.setContentDisplay(ContentDisplay.CENTER);
		hideProcsButton.setGraphicTextGap(0d);
		hideProcsButton.setSelected(true);
		hideProcsButton.setTooltip(new Tooltip("Show/Hide the processes table"));
		hideProcsProperty().addListener((Observable o) -> { toggleHideProcsIcon(); });
		toggleHideProcsIcon();
		
		refreshModulesButton.setContentDisplay(ContentDisplay.CENTER);
		refreshModulesButton.setGraphicTextGap(0d);
		refreshModulesButton.disableProperty().bind(ErlyBerly.nodeAPI().connectedProperty().not());		
		refreshModulesButton.setTooltip(new Tooltip("Refresh the module list to view newly loaded modules"));
		refreshModulesButton.setGraphic(Icon.create().icon(AwesomeIcon.ROTATE_LEFT).style("-fx-font-family: FontAwesome; -fx-font-size: 1em;"));
		refreshModulesButton.setOnAction(this::onRefreshModules);
	}

	private void onRefreshModules(ActionEvent e) {
		treeModules.clear();
		
		refreshModules();
	}
	
	public BooleanProperty hideProcsProperty() {
		return hideProcsButton.selectedProperty();
	}
	
	private void toggleHideProcsIcon() {
		AwesomeIcon icon;
		
		if(hideProcsButton.isSelected())
			icon = AwesomeIcon.BACKWARD;
		else
			icon = AwesomeIcon.FORWARD;
		
		hideProcsButton.setGraphic(Icon.create().icon(icon).style("-fx-font-family: FontAwesome; -fx-font-size: 1em;"));
	}

	@FXML
	private void onTraceLogClear() {
		traces.clear();
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
	
	public void onFunctionSearchChange(Observable o) {
		
		String search = searchField.getText();
		
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
                	showTraceTermView(selectedItem.getArgs(), selectedItem.getResult()); 
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


	private void showTraceTermView(OtpErlangObject args, OtpErlangObject result) {
		TermTreeView resultTermsTreeView, argTermsTreeView;
		
		resultTermsTreeView = newTermTreeView();
		resultTermsTreeView.populateFromTerm(result);
		
		argTermsTreeView = newTermTreeView();
		argTermsTreeView.populateFromListContents((OtpErlangList)args);
		
		SplitPane splitPane;
		
		splitPane = new SplitPane();
		splitPane.getItems().addAll(
			labelledTreeView("Function arguments", argTermsTreeView), 
			labelledTreeView("Result", resultTermsTreeView)
		);

		Stage termsStage = new Stage();
		
		Scene scene  = new Scene(splitPane);
		
		CloseWindowOnEscape.apply(scene, termsStage);
		
		termsStage.setScene(scene);
        termsStage.setWidth(800);
        termsStage.setHeight(600);

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

	public boolean modulePredicate(TreeItem<ModFunc> t) {
		String searchText = searchField.getText();
		return isMatchingModFunc(searchText, t);
	}


	private boolean isMatchingModFunc(String searchText, TreeItem<ModFunc> t) {
		if(searchText.isEmpty())
			return true;
		return t.getValue().toString().contains(searchText);
	}
	
	@FXML
	private void onGo() throws Exception {
		ModFunc function = modulesTree.getSelectionModel().getSelectedItem().getValue();
		
		if(function == null || function.getFuncName() == null) {
			return;
		}
		
		ErlyBerly.nodeAPI().startTrace(function);
		
		dbgController.setCollectingTraces(true);
		
		Button newRemoveTraceButton;
		
		newRemoveTraceButton = newRemoveTraceButton(function);
		newRemoveTraceButton.setOnAction((e) -> onRemoveTracer(e, function));
		
		currentTraceBox.getChildren().add(newRemoveTraceButton);
	}
	
	private void onRemoveTracer(ActionEvent e, ModFunc mf) {
		try {
			ErlyBerly.nodeAPI().stopTrace(mf);
			
			currentTraceBox.getChildren().remove(e.getSource());
		} 
		catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	/**
	 * When the number of trace remove buttons changes, we need to show or hide
	 * the label saying there are no active traces.
	 */
	private void onRemoveTraceButtonsAdded(Observable o) {
		Platform.runLater(() -> {
			ObservableList<Node> currentTraces = currentTraceBox.getChildren();
			if(currentTraces.size() > 1 && noTracesLabel.getParent() != null) {
				currentTraces.remove(0);
			}
			else if(currentTraces.isEmpty()) {
				currentTraces.add(0, noTracesLabel);
			}
		});
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
		TreeItem<ModFunc> item = new TreeItem<ModFunc>(modFunc);

		Icon icon;
		
		if(modFunc.isExported()) {
			icon = treeIcon(AwesomeIcon.SQUARE);
		}
		else {
			icon = treeIcon(AwesomeIcon.SQUARE_ALT);
		}
		item.setGraphic(icon);
		return item;
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
}
