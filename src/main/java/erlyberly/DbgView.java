package erlyberly;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.ResourceBundle;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DbgView implements Initializable {
	
	@FXML
	private TreeView<ModFunc> modulesTree;
	@FXML
	private ListView<String> traceLogsListView;
	@FXML
	private Label traceTargetLabel;

	//private final DbgController dbgController;
	
	public DbgView() {
		//dbgController = new DbgController();
	}
	
	@Override
	public void initialize(URL url, ResourceBundle r) {
		ErlyBerly.nodeAPI().connectedProperty().addListener(new InvalidationListener() {
			@Override
			public void invalidated(Observable o) {
				onConnected();
			}});
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
}
