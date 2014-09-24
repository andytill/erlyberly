package erlyberly;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
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
import javafx.scene.control.SplitPane;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import de.jensd.fx.fontawesome.AwesomeIcon;
import de.jensd.fx.fontawesome.Icon;
import erlyberly.node.OtpUtil;


public class DbgView implements Initializable {
	
	private static final String ICON_STYLE = "-fx-font-family: FontAwesome; -fx-font-size: 1em;";
	private static final OtpErlangAtom RESULT_ATOM = new OtpErlangAtom("result");
	private static final OtpErlangAtom ATOM_PID = new OtpErlangAtom("pid");
	private static final OtpErlangAtom ATOM_REG_NAME = new OtpErlangAtom("reg_name");
	private static final OtpErlangAtom ATOM_UNDEFINED = new OtpErlangAtom("undefined");
	
	private final ObservableList<TreeItem<ModFunc>> treeModules = FXCollections.observableArrayList();
	
	private final SortedList<TreeItem<ModFunc>> sortedTreeModules = new SortedList<TreeItem<ModFunc>>(treeModules);
	
	private final FilteredList<TreeItem<ModFunc>> filteredTreeModules = new FilteredList<TreeItem<ModFunc>>(sortedTreeModules);
	
	@FXML
	private TreeView<ModFunc> modulesTree;
	@FXML
	private Button goButton;
	@FXML
	private VBox tracesBox;
	@FXML
	private TextField searchField;
	@FXML
	private VBox modulesBox;
	
	private final DbgController dbgController = new DbgController();
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
		sortedTreeModules.setComparator(new Comparator<TreeItem<ModFunc>>() {
			@Override
			public int compare(TreeItem<ModFunc> o1, TreeItem<ModFunc> o2) {
				return o1.getValue().compareTo(o2.getValue());
			}});
		
		SplitPane.setResizableWithParent(modulesBox, Boolean.FALSE);
		
		searchField.textProperty().addListener(new InvalidationListener() {
			@Override
			public void invalidated(Observable o) {
				filteredTreeModules.setPredicate(DbgView.this::modulePredicate);
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
						String tracePropsToString = tracePropsToString(map);
						
						Label label;
						
						label = new Label(tracePropsToString);
						label.setStyle("-fx-font-smoothing-type:lcd; ");
						label.setOnMouseClicked((me) -> { 
							if(me.getButton().equals(MouseButton.PRIMARY)){
					            if(me.getClickCount() == 2){
					            	showTraceTermView(argsFromTraceMap(map), (OtpErlangObject) map.get(RESULT_ATOM)); 
				            	}
				            }
				        });
							                              
						tracesBox.getChildren().add(0, label);
					}
				}
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
		
		dbgController.initialize(url, r);
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
		if(searchField.getText().isEmpty())
			return true;
		return t.getValue().toString().contains(searchField.getText());
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
			
			ArrayList<ModFunc> modFuncs = new ArrayList<ModFunc>();

			isExported = true;			
			modFuncs.addAll(toModFuncs(moduleNameAtom, exportedFuncs, isExported));

			isExported = false;
			modFuncs.addAll(toModFuncs(moduleNameAtom, localFuncs, isExported));

			Collections.sort(modFuncs);
			
			addTreeItems(modFuncs, moduleItem);
			
			treeModules.add(moduleItem);
		}
		Bindings.bindContentBidirectional(root.getChildren(), filteredTreeModules);
		
		return root;
	}

	private void addTreeItems(ArrayList<ModFunc> modFuncs, TreeItem<ModFunc> moduleItem) {
		for (ModFunc modFunc : modFuncs) {
			if(!modFunc.isSynthetic()) {
				TreeItem<ModFunc> item = new TreeItem<ModFunc>(modFunc);

				Icon icon;
				
				if(modFunc.isExported()) {
					icon = treeIcon(AwesomeIcon.SQUARE);
				}
				else {
					icon = treeIcon(AwesomeIcon.SQUARE_ALT);
				}
				item.setGraphic(icon);
				
				moduleItem.getChildren().add(item);
			}
		}
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
	
	private String tracePropsToString(HashMap<Object, Object> map) {
		String trace = "";
		
		OtpErlangAtom regName = (OtpErlangAtom) map.get(ATOM_REG_NAME);
		
		if(!ATOM_UNDEFINED.equals(regName)) {
			trace += regName.atomValue();
		}
		else {
			OtpErlangString pidString = (OtpErlangString) map.get(ATOM_PID);
			trace += pidString.stringValue();
		}
		trace += " ";
		trace += fnToFunctionString((OtpErlangTuple)map.get(new OtpErlangAtom("fn")));
		trace += " => ";
		trace += OtpUtil.otpObjectToString((OtpErlangObject) map.get(RESULT_ATOM));
		
		return trace;
	}
	
	private String fnToFunctionString(OtpErlangTuple tuple) {
		OtpErlangAtom mod = (OtpErlangAtom) tuple.elementAt(0);
		OtpErlangAtom func = (OtpErlangAtom) tuple.elementAt(1);
		OtpErlangList args = (OtpErlangList) tuple.elementAt(2);
		ArrayList<String> sargs = new ArrayList<String>();
		for (OtpErlangObject otpErlangObject : args) {
			sargs.add(otpErlangObject.toString());
		}
		
		String join = String.join(", ", sargs);
		
		String fn = mod.atomValue() + ":" + func.atomValue() + "(" + join + ")";
		
		return fn;
	}


	private OtpErlangObject argsFromTraceMap(HashMap<Object, Object> map) {
		OtpErlangTuple tuple = (OtpErlangTuple) map.get(new OtpErlangAtom("fn"));
		OtpErlangList args = (OtpErlangList) tuple.elementAt(2);
		return args;
	}
}
