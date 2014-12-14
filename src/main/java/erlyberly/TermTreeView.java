package erlyberly;

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
		
		addToTreeItem(getRoot(), obj);
	}
	
	@SuppressWarnings("unchecked")
	private void addToTreeItem(TreeItem parent, OtpErlangObject obj) {
		
		if(obj instanceof OtpErlangBinary) {
			TreeItem item = new TreeItem(OtpUtil.binaryToString((OtpErlangBinary) obj));
			parent.getChildren().add(item);
		}
		else if(obj instanceof OtpErlangTuple) {
			OtpErlangObject[] elements = ((OtpErlangTuple) obj).elements();
			
			if(elements.length == 0) {
				parent.getChildren().add(new TreeItem("{ }"));
			}
			else {
				TreeItem tupleItem;
				
				tupleItem = new TreeItem("{");
				tupleItem.setExpanded(true);
				
				parent.getChildren().add(tupleItem);
				for (OtpErlangObject e : elements) {
					addToTreeItem(tupleItem, e);
				}
				
				parent.getChildren().add(new TreeItem("}"));
			}
		}
		else if(obj instanceof OtpErlangList) {
			OtpErlangObject[] elements = ((OtpErlangList) obj).elements();
			
			if(elements.length == 0) {
				parent.getChildren().add(new TreeItem("[ ]"));
			}
			else {
				TreeItem listItem;
				
				listItem = new TreeItem("[");
				listItem.setExpanded(true);
				
				parent.getChildren().add(listItem);
				
				for (OtpErlangObject e : elements) {
					addToTreeItem(listItem, e);
				}
				
				parent.getChildren().add(new TreeItem("]"));
			}
		}
		else {
			StringBuilder stringBuilder = new StringBuilder();
			OtpUtil.otpObjectToString(obj, stringBuilder);
			parent.getChildren().add(new TreeItem(stringBuilder.toString()));
		}
	}

	public void populateFromListContents(OtpErlangList list) {
		for (OtpErlangObject a : list) {
			populateFromTerm(a);
		}
	}

}
