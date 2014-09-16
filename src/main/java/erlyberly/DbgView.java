package erlyberly;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.ResourceBundle;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.collections.ListChangeListener;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.layout.VBox;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class DbgView implements Initializable {
	
	private static final OtpErlangAtom ATOM_PID = new OtpErlangAtom("pid");
	private static final OtpErlangAtom ATOM_REG_NAME = new OtpErlangAtom("reg_name");
	private static final OtpErlangAtom ATOM_UNDEFINED = new OtpErlangAtom("undefined");
	@FXML
	private TreeView<ModFunc> modulesTree;
	@FXML
	private Label traceTargetLabel;
	@FXML
	private Button goButton;
	@FXML
	private VBox tracesBox;
	
	private final DbgController dbgController = new DbgController();
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
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
					traceTargetLabel.setText("Choose a function");
				}
				else {
					traceTargetLabel.setText("Trace " + modFunc.toFullString());
				}
			}});
		
		dbgController.initialize(url, r);
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
			
			TreeItem<ModFunc> moduleItem = new TreeItem<ModFunc>(ModFunc.toModule(moduleNameAtom));

			ArrayList<ModFunc> modFuncs;

			isExported = true;			
			modFuncs = toModFuncs(moduleNameAtom, exportedFuncs, isExported);
			addTreeItems(modFuncs, moduleItem);

			isExported = false;
			modFuncs = toModFuncs(moduleNameAtom, localFuncs, isExported);
			addTreeItems(modFuncs, moduleItem);
			
			root.getChildren().add(moduleItem);
			Collections.sort(root.getChildren(), new Comparator<TreeItem<ModFunc>>() {
				@Override
				public int compare(TreeItem<ModFunc> o1, TreeItem<ModFunc> o2) {
					return o1.getValue().compareTo(o2.getValue());
				}});
		}
		return root;
	}

	private void addTreeItems(ArrayList<ModFunc> modFuncs, TreeItem<ModFunc> moduleItem) {
		for (ModFunc modFunc : modFuncs) {
			if(!modFunc.isSynthetic()) {
				moduleItem.getChildren().add(new TreeItem<ModFunc>(modFunc));
			}
		}
	}

	private ArrayList<ModFunc> toModFuncs(OtpErlangAtom moduleNameAtom, OtpErlangList exportedFuncs, boolean isExported) throws OtpErlangRangeException {
		ArrayList<ModFunc> mfs = new ArrayList<>();
		for (OtpErlangObject exported : exportedFuncs) {
			ModFunc modFunc = ModFunc.toFunc(moduleNameAtom, exported, isExported);
			mfs.add(modFunc);
		}
		Collections.sort(mfs);
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
		trace += map.get(new OtpErlangAtom("result")).toString();
		
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
}
