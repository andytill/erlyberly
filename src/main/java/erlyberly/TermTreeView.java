package erlyberly;

import javafx.scene.control.Label;
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
				
				if(isRecord(obj)) {
                    String recordNameText = "#" + OtpUtil.tupleElement(1, obj) + " ";
                    
					tupleItem = new TreeItem("{");
                    tupleItem.setGraphic(recordLabel(recordNameText));
					parent.getChildren().add(tupleItem);
					tupleItem.setExpanded(true);
					elements = OtpUtil.iterableElements(OtpUtil.tupleElement(2, obj));// ((OtpErlangList) OtpUtil.tupleElement(2, obj)).elements();
					for (OtpErlangObject e : elements) {
						addToTreeItem(tupleItem, e);
					}
					parent.getChildren().add(new TreeItem("}"));
				}
				else if(isRecordField(obj)) {
				    
					tupleItem = new TreeItem(" ");
					tupleItem.setGraphic(recordLabel(OtpUtil.tupleElement(1, obj) + " =  "));
                    tupleItem.setExpanded(true);
					
					parent.getChildren().add(tupleItem);
					
					OtpErlangObject value = OtpUtil.tupleElement(2, obj);
					if(OtpUtil.isLittleTerm(value))
					    tupleItem.setValue(value);
					else
					    addToTreeItem(tupleItem, value);
					
				}
				else {
					tupleItem = new TreeItem();
					tupleItem.setExpanded(true);
					
					if(OtpUtil.isLittleTerm(obj)) {
					    tupleItem.setValue(obj);
					}
					else {
                        tupleItem.setValue("{");
	                    for (OtpErlangObject e : elements) {
	                        addToTreeItem(tupleItem, e);
	                    }
	                    parent.getChildren().add(new TreeItem("}"));
					}
					
                    parent.getChildren().add(tupleItem);
				}
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
				

                if(OtpUtil.isLittleTerm(obj)) {
                    listItem.setValue(obj);
                }
                else {
                    listItem.setValue("[");
                    for (OtpErlangObject e : elements) {
                        addToTreeItem(listItem, e);
                    }
                    parent.getChildren().add(new TreeItem("]"));
                }
				
				parent.getChildren().add(listItem);
			}
		}
		else {
			StringBuilder stringBuilder = new StringBuilder();
			OtpUtil.otpObjectToString(obj, stringBuilder);
			parent.getChildren().add(new TreeItem(stringBuilder.toString()));
		}
	}

    private Label recordLabel(String recordNameText) {
        Label label;
        label = new Label(recordNameText);
        label.getStyleClass().add("record-label");
        return label;
    }

	private boolean isRecordField(OtpErlangObject obj) {
		return OtpUtil.isTupleTagged(OtpUtil.atom("erlyberly_record_field"), obj);
	}

	private boolean isRecord(OtpErlangObject obj) {
		return OtpUtil.isTupleTagged(OtpUtil.atom("erlyberly_record"), obj);
	}

	public void populateFromListContents(OtpErlangList list) {
		for (OtpErlangObject a : list) {
			populateFromTerm(a);
		}
	}

}
