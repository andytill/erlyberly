package erlyberly;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;
import javafx.event.ActionEvent;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.KeyCombination;

@SuppressWarnings("rawtypes")
public class TermTreeView extends TreeView<OtpErlangObject> {
	
	@SuppressWarnings("unchecked")
	public TermTreeView() {
		setRoot(new TreeItem());

		MenuItem copyMenuItem = new MenuItem("Copy");
		copyMenuItem.setAccelerator(KeyCombination.keyCombination("shortcut+c"));
		copyMenuItem.setOnAction(this::onCopyCalls);
		setContextMenu(new ContextMenu(copyMenuItem));
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
					    tupleItem.setValue(otpObjectToString(value));
					else
					    addToTreeItem(tupleItem, value);
					
				}
				else {
					tupleItem = new TreeItem();
					tupleItem.setExpanded(true);
					
					if(OtpUtil.isLittleTerm(obj)) {
					    tupleItem.setValue(otpObjectToString(obj));
	                    parent.getChildren().add(tupleItem);
					}
					else {
                        tupleItem.setValue("{");
	                    for (OtpErlangObject e : elements) {
	                        addToTreeItem(tupleItem, e);
	                    }
	                    parent.getChildren().add(tupleItem);
	                    parent.getChildren().add(new TreeItem("}"));
					}
					
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
                    listItem.setValue(otpObjectToString(obj));
                    parent.getChildren().add(listItem);
                }
                else {
                    listItem.setValue("[");
                    for (OtpErlangObject e : elements) {
                        addToTreeItem(listItem, e);
                    }
                    parent.getChildren().add(listItem);
                    parent.getChildren().add(new TreeItem("]"));
                }
			}
		}
		else {
            parent.getChildren().add(new TreeItem(otpObjectToString(obj)));
		}
	}

    private String otpObjectToString(OtpErlangObject obj) {
        StringBuilder stringBuilder = new StringBuilder();
        OtpUtil.otpObjectToString(obj, stringBuilder);
        String string = stringBuilder.toString();
        return string;
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

    private void onCopyCalls(ActionEvent e) {
        StringBuilder sbuilder = new StringBuilder();

        for (TreeItem obj : getSelectionModel().getSelectedItems()) {
            sbuilder.append(obj.getValue()).append("\n");
            }

        copyToClipboard(sbuilder);
    }

    private void copyToClipboard(StringBuilder sbuilder) {
        final Clipboard clipboard = Clipboard.getSystemClipboard();
        final ClipboardContent content = new ClipboardContent();
        content.putString(sbuilder.toString());
        clipboard.setContent(content);
    }
}
