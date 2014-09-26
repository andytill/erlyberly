package erlyberly;

import java.net.URL;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.ResourceBundle;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.binding.Bindings;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;
import erlyberly.node.OtpUtil;


public class DbgView implements Initializable {
	
	private static final String ICON_STYLE = "-fx-font-family: FontAwesome; -fx-font-size: 1em;";
	
	private final ObservableList<TreeItem<ModFunc>> treeModules = FXCollections.observableArrayList();
	
	private final SortedList<TreeItem<ModFunc>> sortedTreeModules = new SortedList<TreeItem<ModFunc>>(treeModules);
	
	private final FilteredList<TreeItem<ModFunc>> filteredTreeModules = new FilteredList<TreeItem<ModFunc>>(sortedTreeModules);
	
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
	
	private final DbgController dbgController = new DbgController();
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
		sortedTreeModules.setComparator(treeItemModFuncComparator());
		
		SplitPane.setResizableWithParent(modulesBox, Boolean.FALSE);
		
		searchField.textProperty().addListener(new InvalidationListener() {
			@Override
			public void invalidated(Observable o) {
				
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
			}});
		
		ErlyBerly.nodeAPI().connectedProperty().addListener(new InvalidationListener() {
			@Override
			public void invalidated(Observable o) {
				onConnected();
			}});
		
		dbgController.getTraces().addListener(new ListChangeListener<Object>() {
			@Override
			public void onChanged(ListChangeListener.Change<? extends Object> e) {
				while(e.next()) {
					for (Object obj : e.getAddedSubList()) {
						HashMap<Object, Object> map = OtpUtil.propsToMap((OtpErlangList) obj);
							                              
						TraceLog trace = new TraceLog(map);
						
						tracesBox.getItems().add(0, trace);
					}
				}
			}});
		tracesBox.setOnMouseClicked(this::onTraceClicked);
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
		
		dbgController.initialize(url, r);
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
		
		resultTermsTreeView = new TermTreeView();
		resultTermsTreeView.populateFromTerm(result);
		
		argTermsTreeView = new TermTreeView();
		argTermsTreeView.populateFromTerm(args);
		
		SplitPane splitPane;
		
		splitPane = new SplitPane();
		splitPane.getItems().addAll(
			labelledTreeView("Function arguments", argTermsTreeView), 
			labelledTreeView("Result", resultTermsTreeView)
		);
		
		Stage termsStage;
    
		termsStage = new Stage();
		termsStage.setScene(new Scene(splitPane));
        termsStage.setWidth(800);
        termsStage.setHeight(600);

        termsStage.show();
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
		ModFunc value = modulesTree.getSelectionModel().getSelectedItem().getValue();
		
		if(value == null || value.getFuncName() == null) {
			return;
		}
		
		ErlyBerly.nodeAPI().startTrace(value);
		
		dbgController.setCollectingTraces(true);
	}

	private void onConnected() {
		if(!ErlyBerly.nodeAPI().connectedProperty().get())
			return;
		
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
