package erlyberly;

import javafx.collections.ObservableList;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

@SuppressWarnings("rawtypes")
public class TermTreeView extends TreeView {
	
	@SuppressWarnings("unchecked")
	public TermTreeView() {
		setRoot(new TreeItem());
	}
	
	public void populateFromTerm(OtpErlangObject obj) {
		setShowRoot(false);
		
		TreeItem root = getRoot();
		ObservableList children = root.getChildren();
		
		children.clear();
		
		addToTreeItem(root, obj);
	}
	
	@SuppressWarnings("unchecked")
	private void addToTreeItem(TreeItem parent, OtpErlangObject obj) {
		
		if(obj instanceof OtpErlangBinary) {
			TreeItem item = new TreeItem(OtpUtil.binaryToString((OtpErlangBinary) obj));
			parent.getChildren().add(item);
		}
		else if(obj instanceof OtpErlangTuple) {
			TreeItem tupleItem;
			
			tupleItem = new TreeItem("{");
			tupleItem.setExpanded(true);
			
			parent.getChildren().add(tupleItem);
			
			for (OtpErlangObject e : ((OtpErlangTuple) obj).elements()) {
				addToTreeItem(tupleItem, e);
			}
			
			parent.getChildren().add(new TreeItem("}"));
		}
		else if(obj instanceof OtpErlangList) {
			TreeItem tupleItem;
			
			tupleItem = new TreeItem("[");
			tupleItem.setExpanded(true);
			
			parent.getChildren().add(tupleItem);
			
			for (OtpErlangObject e : ((OtpErlangList) obj).elements()) {
				addToTreeItem(tupleItem, e);
			}
			
			parent.getChildren().add(new TreeItem("]"));
		}
		else {
			parent.getChildren().add(new TreeItem(OtpUtil.otpObjectToString(obj)));
		}
	}

}
