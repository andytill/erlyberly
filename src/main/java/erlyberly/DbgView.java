package erlyberly;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DbgView implements Initializable {
	
	@FXML
	private TreeView<Object> modulesTree;

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
	
	private TreeItem<Object> buildObjectTreeRoot() throws Exception {		
		TreeItem<Object> root;
		
		root = new TreeItem<Object>();
		root.setExpanded(true);
		
		OtpErlangList requestFunctions = ErlyBerly.nodeAPI().requestFunctions();
		
		for (OtpErlangObject e : requestFunctions) {
			OtpErlangTuple tuple = (OtpErlangTuple) e;
			
			OtpErlangAtom moduleNameAtom = (OtpErlangAtom) tuple.elementAt(0);
			OtpErlangList exportedFuncs = (OtpErlangList) tuple.elementAt(1);
			OtpErlangList localFuncs = (OtpErlangList) tuple.elementAt(2);
			
			TreeItem<Object> moduleItem = new TreeItem<Object>(moduleNameAtom.atomValue());
			
			for (OtpErlangObject exported : exportedFuncs) {
				ModFunc modFunc = ModFunc.toModFunc(exported, true);
				moduleItem.getChildren().add(new TreeItem<Object>(modFunc));
			}

			for (OtpErlangObject local : localFuncs) {
				ModFunc modFunc = ModFunc.toModFunc(local, false);
				moduleItem.getChildren().add(new TreeItem<Object>(modFunc));
			}
			
			root.getChildren().add(moduleItem);
		}
		return root;
	}
}
